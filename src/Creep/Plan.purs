module Creep.Plan where

import Data.Argonaut.Core (foldJsonArray, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (head, singleton)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Unfoldable (replicate)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State.Trans (execStateT, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (execWriter, tell)
import Prelude (
  class Applicative, class Apply, class Bind, class Functor, class Monad,
  class Show, Unit, bind, discard, flip, map, pure, unit, void, when, ($), (*>),
  (+), (<), (<<<), (<>), (=<<), (==), (>), (>>=)
)

import Screeps (CMD, Creep, MEMORY, TICK, TargetPosition(..))
import Screeps.Creep (
  amtCarrying, carryCapacity, harvestSource, moveTo, transferToStructure
)
import Screeps.FindType (find_my_spawns, find_sources)
import Screeps.Resource (resource_energy)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.ReturnCode (err_not_in_range)

data PlanF a
  = HarvestEnergy a
  | TransferEnergyToBase a
  | Repeat (Plan Unit)

instance functorPlanF :: Functor PlanF where
  map k f = case f of
    HarvestEnergy f'        -> HarvestEnergy $ k f'
    TransferEnergyToBase f' -> TransferEnergyToBase $ k f'
    Repeat x                -> Repeat x

newtype Plan a = Plan (Free PlanF a)

instance newtypePlan :: Newtype (Plan a) (Free PlanF a) where
  wrap = Plan
  unwrap (Plan x) = x

derive newtype instance functorPlan :: Functor Plan
derive newtype instance applyPlan :: Apply Plan
derive newtype instance applicativePlan :: Applicative Plan
derive newtype instance bindPlan :: Bind Plan
derive newtype instance monadPlan :: Monad Plan

instance showPlan :: Show (Plan Unit) where
  show = execWriter <<< go 0
    where
      go level plan = flip runFreeM (unwrap plan) $ case _ of
        HarvestEnergy next -> do
          tellLine "harvestEnergy"
          pure next
        TransferEnergyToBase next -> do
          tellLine "transferEnergyToBase"
          pure next
        Repeat block -> do
          tellLine "repeat"
          go (level + 1) block
          pure $ pure unit
        where
         tellLine line = tell $ indent <> line <> "\n"
         indent = joinWith "" $ replicate level "  "

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
            "repeat" -> pure <<< repeat =<< decodeJson =<< object .? "block"
            cmd -> throwError $ "unrecognized command: " <> cmd

harvestEnergy :: Plan Unit
harvestEnergy = Plan $ liftF $ HarvestEnergy unit

transferEnergyToBase :: Plan Unit
transferEnergyToBase = Plan $ liftF $ TransferEnergyToBase unit

repeat :: Plan Unit -> Plan Unit
repeat block = Plan $ liftF $ Repeat block

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
            when (code == err_not_in_range) $
              void $ liftEff $ moveTo creep $ TargetObj source
            stay
          Nothing -> throwError "source not found"
        else transition next
    TransferEnergyToBase next -> do
      if amtCarrying creep resource_energy > 0
        then case head $ find (room creep) find_my_spawns of
          Just spawn -> do
            code <- liftEff $ transferToStructure creep spawn resource_energy
            when (code == err_not_in_range) $
              void $ liftEff $ moveTo creep $ TargetObj spawn
            stay
          Nothing -> throwError "spawn not found"
        else transition next
    Repeat block -> do
      plan' <- lift $ executePlan creep $ block *> plan
      changePlan plan'
    where
      stay = done
      transition next = do
        put $ Plan next
        done
      changePlan plan' = do
        put plan'
        done
      done = pure $ pure unit
