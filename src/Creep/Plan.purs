module Creep.Plan where

import Blockable (runBlockableT, unblock)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (withExceptT)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Free (Free, liftF, resume, runFreeM)
import Control.Monad.State (execStateT, get, put)
import Control.Monad.Writer (execWriter, tell)
import Creep.Exec (ExecError(ErrorMessage, BadReturnCode), catchReturnCode, harvestSource, moveTo, transferToStructure)
import Data.Argonaut.Core (foldJsonArray, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (head, singleton)
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, flip, map, pure, show, unit, ($), (*>), (<), (<$>), (<<<), (<=<), (<>), (=<<), (>), (>>=))
import Screeps (CMD, Creep, MEMORY, TICK, TargetPosition(..))
import Screeps.Creep (amtCarrying, carryCapacity, name)
import Screeps.FindType (find_my_spawns, find_sources)
import Screeps.Resource (resource_energy)
import Screeps.ReturnCode (err_not_in_range)
import Screeps.Room (find)
import Screeps.RoomObject (room)

data PlanF a
  = HarvestEnergy a
  | TransferEnergyToBase a
  | Repeat (Plan Unit)
  | Interrupt (Plan Unit) (Plan Unit) a

instance functorPlanF :: Functor PlanF where
  map k f = case f of
    HarvestEnergy f'        -> HarvestEnergy $ k f'
    TransferEnergyToBase f' -> TransferEnergyToBase $ k f'
    Repeat x                -> Repeat x
    Interrupt x y f'        -> Interrupt x y $ k f'

newtype Plan a = Plan (Free PlanF a)

derive instance newtypePlan :: Newtype (Plan a) _

derive newtype instance functorPlan :: Functor Plan
derive newtype instance applyPlan :: Apply Plan
derive newtype instance applicativePlan :: Applicative Plan
derive newtype instance bindPlan :: Bind Plan
derive newtype instance monadPlan :: Monad Plan

instance encodeJsonPlan :: EncodeJson (Plan Unit) where
  encodeJson plan =
    encodeJson $ execWriter $ flip runFreeM (unwrap plan) $ case _ of
      HarvestEnergy next -> do
        tellObject  $ "action" := "harvestEnergy"
                   ~> jsonEmptyObject
        pure next
      TransferEnergyToBase next -> do
        tellObject  $ "action" := "transferEnergyToBase"
                   ~> jsonEmptyObject
        pure next
      Repeat block -> do
        tellObject  $ "action" := "repeat"
                   ~> "block"  := encodeJson block
                   ~> jsonEmptyObject
        pure $ pure unit
      Interrupt interruptee interrupter next -> do
        tellObject  $ "action" := "interrupt"
                   ~> "interruptee"  := encodeJson interruptee
                   ~> "interrupter"  := encodeJson interrupter
                   ~> jsonEmptyObject
        pure next
      where
        tellObject = tell <<< singleton

instance decodeJsonPlan :: DecodeJson (Plan Unit) where
  decodeJson = foldJsonArray (throwError "expected an array") decodeFromArray
    where
      decodeFromArray = map sequence_ <<< traverse decodeAction
      decodeAction =
        foldJsonObject (throwError "expected an object") $ \object ->
          object .? "action" >>= case _ of
            "harvestEnergy" -> pure harvestEnergy
            "transferEnergyToBase" -> pure transferEnergyToBase
            "repeat" -> pure <<< repeat =<< decodeField object "block"
            "interrupt" -> do
              interruptee <- decodeField object "interruptee"
              interrupter <- decodeField object "interrupter"
              pure $ interrupt interruptee interrupter
            cmd -> throwError $ "unrecognized command: " <> cmd
      decodeField object = decodeJson <=< (object .? _)

harvestEnergy :: Plan Unit
harvestEnergy = Plan $ liftF $ HarvestEnergy unit

transferEnergyToBase :: Plan Unit
transferEnergyToBase = Plan $ liftF $ TransferEnergyToBase unit

repeat :: Plan Unit -> Plan Unit
repeat block = Plan $ liftF $ Repeat block

interrupt :: Plan Unit -> Plan Unit -> Plan Unit
interrupt interrupted interruptee =
  Plan $ liftF $ Interrupt interrupted interruptee unit

executePlan ::
  forall e.
    Creep -> Plan Unit ->
    ExceptT String
            (Eff (cmd :: CMD, memory :: MEMORY, tick :: TICK | e))
            (Plan Unit)
executePlan creep =
  renderError <<< flip execStateT (pure unit) <<< runBlockableT <<< executePlan'
  where
    renderError = withExceptT \error ->
      "error in creep " <> (show $ name creep) <> ": " <>
        renderDetails error
      where
        renderDetails details = case details of
          ErrorMessage message -> message
          BadReturnCode code -> show code
    executePlan' plan' = do
      put plan'
      runFreeM peel $ unwrap plan'
      where
        peel = case _ of
          HarvestEnergy next -> do
            if amtCarrying creep resource_energy < carryCapacity creep
              -- TODO: Use findClosestByPath instead.
              then case head $ find (room creep) find_sources of
                Just source -> do
                  harvestSource creep source
                    `catchNotInRange` (moveTo creep $ TargetObj source)
                  stay
                Nothing -> throwError $ ErrorMessage "source not found"
              else transition next
          TransferEnergyToBase next -> do
            if amtCarrying creep resource_energy > 0
              then case head $ find (room creep) find_my_spawns of
                Just spawn -> do
                  transferToStructure creep spawn resource_energy
                    `catchNotInRange` (moveTo creep $ TargetObj spawn)
                  stay
                Nothing -> throwError $ ErrorMessage "spawn not found"
              else transition next
          Repeat block -> do
            -- Execute just the block to avoid infinite loops.
            block' <- map (_.plan) $ localExec $ executePlan' block
            prependPlan block'
          Interrupt interruptee interrupter next -> do
            interrupter' <-
              map (_.plan) $ localExec $ executePlan' interrupter
            case resume $ unwrap interrupter' of
              Left _ -> do
                -- Interruption - switch to interrupter.
                prependPlan $ interrupter'
              Right _ -> do
                -- No interruption - continue running interruptee.
                interruptee' <-
                  map (_.plan) $ localExec $ executePlan' interruptee
                changePlan $ (interruptee' `interrupt` interrupter) *> Plan next
          where
            catchNotInRange = catchReturnCode err_not_in_range
            stay = unwrap <$> get
            transition next = do
              put $ Plan next
              pure next
            changePlan plan'' = do
              put plan''
              pure $ pure unit
            prependPlan prefixPlan = do
              oldPlan <- get
              changePlan $ prefixPlan *> oldPlan
            localExec exec = do
              outerPlan <- get
              blocked <- isNothing <$> unblock exec
              innerPlan <- get
              put outerPlan
              pure {plan: innerPlan, blocked: blocked}
