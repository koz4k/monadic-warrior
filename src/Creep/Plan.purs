module Creep.Plan where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Free (Free, liftF, resume, runFreeM)
import Control.Monad.State.Trans (execStateT, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (execWriter, tell)
import Data.Argonaut.Core (foldJsonArray, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (head, singleton)
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, flip, map, pure, show, unit, void, when, ($), (*>), (/=), (<), (<<<), (<=<), (<>), (=<<), (==), (>), (>>=))
import Screeps (CMD, Creep, MEMORY, TICK, TargetPosition(..))
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, transferToStructure)
import Screeps.FindType (find_my_spawns, find_sources)
import Screeps.Resource (resource_energy)
import Screeps.ReturnCode (err_not_in_range, ok)
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

instance newtypePlan :: Newtype (Plan a) (Free PlanF a) where
  wrap = Plan
  unwrap (Plan x) = x

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
  forall e. Creep -> Plan Unit ->
            ExceptT String
                    (Eff (cmd :: CMD, memory :: MEMORY, tick :: TICK | e))
                    (Plan Unit)
executePlan creep plan = do
  flip execStateT plan $ flip runFreeM (unwrap plan) $ case _ of
    HarvestEnergy next -> do
      if amtCarrying creep resource_energy < carryCapacity creep
        then case head $ find (room creep) find_sources of
          Just source -> do
            code <- liftEff $ harvestSource creep source
            if code == err_not_in_range
              then void $ liftEff $ moveTo creep $ TargetObj source
              else when (code /= ok) $
                throwError $ "error in harvestSource: " <> show code
            stay
          Nothing -> throwError "source not found"
        else transition next
    TransferEnergyToBase next -> do
      if amtCarrying creep resource_energy > 0
        then case head $ find (room creep) find_my_spawns of
          Just spawn -> do
            code <- liftEff $ transferToStructure creep spawn resource_energy
            if code == err_not_in_range
              then void $ liftEff $ moveTo creep $ TargetObj spawn
              else when (code /= ok) $
                throwError $ "error in transferToStructure: " <> show code
            stay
          Nothing -> throwError "spawn not found"
        else transition next
    Repeat block -> do
      -- Execute just the block to avoid infinite loops.
      block' <- lift $ executePlan creep $ block
      changePlan $ block' *> plan
    Interrupt interruptee interrupter next -> do
      interrupter' <- lift $ executePlan creep interrupter
      case resume $ unwrap interrupter' of
        Left _ ->
          -- Interruption - switch to interrupter.
          changePlan $ interrupter' *> plan
        Right _ -> do
          -- No interruption - continue running interruptee.
          interruptee' <- lift $ executePlan creep interruptee
          changePlan $ interruptee' `interrupt` interrupter
    where
      stay = done
      transition next = do
        next' <- lift $ executePlan creep $ Plan next
        changePlan next'
      changePlan plan' = do
        put plan'
        done
      done = pure $ pure unit
