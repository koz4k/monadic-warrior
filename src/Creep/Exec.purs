module Creep.Exec where

import Blockable (class PartialMonoid, BlockableT, reserve)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), curry)
import Prelude (($), (&&), (/=))
import Screeps (CMD, Creep, MEMORY, TargetPosition)
import Screeps.Creep (harvestSource, moveTo, transferToStructure) as Creep
import Screeps.Resource (ResourceType)
import Screeps.ReturnCode (ReturnCode, err_tired, ok)
import Screeps.Source (Source)
import Screeps.Structure (class Structure)

data ExecStatus
  = NotExecuted
  | Executed

instance partialMonoidExecStatus :: PartialMonoid ExecStatus where
  partialEmpty = NotExecuted
  partialAppend = curry $ case _ of
    Tuple NotExecuted NotExecuted -> Just NotExecuted
    Tuple Executed Executed -> Nothing
    _ -> Just Executed

type Exec = BlockableT ExecStatus

harvestSource ::
  forall e m.
    MonadEff (cmd :: CMD | e) m =>
    Creep -> Source -> Exec m ReturnCode
harvestSource creep source = liftSubAction $ Creep.harvestSource creep source

moveTo ::
  forall e m a.
    MonadEff (cmd :: CMD, memory :: MEMORY | e) m =>
    Creep -> TargetPosition a -> Exec m ReturnCode
moveTo creep target = liftSubAction $ Creep.moveTo creep target

transferToStructure ::
  forall e m a.
    MonadEff (cmd :: CMD | e) m => Structure a =>
    Creep -> a -> ResourceType -> Exec m ReturnCode
transferToStructure creep structure resourceType =
  liftSubAction $ Creep.transferToStructure creep structure resourceType

liftSubAction ::
  forall e m. MonadEff e m => Eff e ReturnCode -> Exec m ReturnCode
liftSubAction subAction =
  reserve Executed (\code -> code /= ok && code /= err_tired) $
    liftEff subAction
