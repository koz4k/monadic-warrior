module Creep.Plan where

import Blockable (runBlockableT)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (withExceptT)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Free (Free, liftF, resume, runFreeM)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (execStateT, get, put)
import Control.Monad.Writer (execWriter, tell)
import Creep.Exec (ExecError(ErrorMessage, BadReturnCode), catchReturnCode, harvestSource, moveTo, transferToStructure)
import Data.Argonaut.Core (foldJsonArray, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (head, length, singleton, snoc, uncons)
import Data.Either (Either(Right, Left), isRight)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, flip, map, pure, show, unit, ($), (*>), (-), (<), (<$>), (<<<), (<=<), (<>), (=<<), (==), (>), (>>=))
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

type ThreadQueue = Array (Plan Unit)

executePlan ::
  forall e.
    Creep -> ThreadQueue ->
    ExceptT String
            (Eff (cmd :: CMD, memory :: MEMORY, tick :: TICK | e))
            ThreadQueue
executePlan creep threadQueue =
  renderError $ flip execStateT threadQueue $ runBlockableT executeThreads
  where
    renderError = withExceptT \error ->
      "error in creep " <> (show $ name creep) <> ": " <>
        renderDetails error
      where
        renderDetails details = case details of
          ErrorMessage message -> message
          BadReturnCode code -> show code
    executeThreads = do
      threadCount <- length <$> get
      flip tailRecM threadCount \i ->
        if i == 0
          then pure $ Done unit
          else do
            threadQueue' <- get
            case uncons threadQueue' of
              Nothing -> throwError $ ErrorMessage "empty thread queue"
              Just {head: plan, tail: threadQueue''} -> do
                plan' <- executeSingleAction plan
                put $ snoc threadQueue'' plan'
                pure $ Loop $ i - 1
    executeSingleAction plan' = case resume $ unwrap plan' of
      Left action -> peel action
      Right _ -> pure $ pure unit
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
            block' <- executeSingleAction block
            pure $ block' *> plan'
          Interrupt interruptee interrupter next -> do
            interrupter' <- executeSingleAction interrupter
            if isPure interrupter'
              then do
                -- No interruption - continue running interruptee.
                interruptee' <- executeSingleAction interruptee
                if isPure interruptee'
                  then
                    -- Interruptee finished - transition to the next action.
                    transition next
                  else
                    -- Interruptee is still running - update it in the plan.
                    pure $ (interruptee' `interrupt` interrupter) *> Plan next
              else
                -- Interruption - switch to interrupter.
                pure $ interrupter' *> plan'
          where
            catchNotInRange = catchReturnCode err_not_in_range
            stay = pure plan'
            transition = executeSingleAction <<< Plan
            isPure = isRight <<< resume <<< unwrap
