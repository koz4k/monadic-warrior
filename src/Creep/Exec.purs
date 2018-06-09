module Creep.Exec (Exec, ExecError(..), ExecStatus, catchReturnCode, harvestSource, moveTo, transferToStructure, upgradeController) where

import Control.Monad.Blockable (class PartialMonoid, BlockableT, reserve)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, catchJust, throwError)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), curry)
import Prelude (class Eq, Unit, bind, const, pure, unit, ($), (==))
import Screeps (CMD, Creep, MEMORY, TargetPosition)
import Screeps.Controller (Controller)
import Screeps.Creep (harvestSource, moveTo, transferToStructure, upgradeController) as Creep
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

data ExecError
  = ErrorMessage String
  | BadReturnCode ReturnCode

derive instance eqExecError :: Eq ExecError

type Exec = BlockableT ExecStatus

harvestSource ::
  forall e m.
    MonadError ExecError m => MonadEff (cmd :: CMD | e) m =>
      Creep -> Source -> Exec m Unit
harvestSource creep source =
  liftSubAction creep $ Creep.harvestSource creep source

moveTo ::
  forall e m a.
    MonadError ExecError m =>
    MonadEff (cmd :: CMD, memory :: MEMORY | e) m =>
      Creep -> TargetPosition a -> Exec m Unit
moveTo creep target =
  liftSubAction creep do
    code <- Creep.moveTo creep target
    -- Ignore err_tired so that running moveTo twice blocks in this case.
    pure $ if code == err_tired
      then ok
      else code

transferToStructure ::
  forall e m a.
    MonadError ExecError m => MonadEff (cmd :: CMD | e) m => Structure a =>
      Creep -> a -> ResourceType -> Exec m Unit
transferToStructure creep structure resourceType =
  liftSubAction creep $ Creep.transferToStructure creep structure resourceType

upgradeController ::
  forall e m.
    MonadError ExecError m => MonadEff (cmd :: CMD | e) m =>
      Creep -> Controller -> Exec m Unit
upgradeController creep controller =
  liftSubAction creep $ Creep.upgradeController creep controller

liftSubAction ::
  forall e m.
    MonadError ExecError m => MonadEff e m =>
      Creep -> Eff e ReturnCode -> Exec m Unit
liftSubAction creep subAction =
  reserve Executed do
    code <- liftEff subAction
    if code == ok
      then pure unit
      else throwError $ BadReturnCode code

catchReturnCode ::
  forall m.
    MonadError ExecError m =>
      ReturnCode -> Exec m Unit -> Exec m Unit -> Exec m Unit
catchReturnCode code action handler = action `catchIt` const handler
  where
    catchIt = catchJust \error ->
      if error == BadReturnCode code
        then Just error
        else Nothing
