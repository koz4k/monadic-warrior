module Creep.Exec (Exec, ExecError(..), ExecStatus, build, catchReturnCode, harvestSource, moveTo, rangedAttackCreep, transferToStructure, upgradeController) where

import Control.Monad.Blockable (class PartialMonoid, BlockableT, reserve)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, catchJust, throwError)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Prelude (class Eq, Unit, bind, const, pure, unit, ($), (==))
import Screeps (CMD, Creep, MEMORY, TargetPosition)
import Screeps.ConstructionSite (ConstructionSite)
import Screeps.Controller (Controller)
import Screeps.Creep (build, harvestSource, moveTo, rangedAttackCreep, transferToStructure, upgradeController) as Creep
import Screeps.Resource (ResourceType)
import Screeps.ReturnCode (ReturnCode, err_tired, ok)
import Screeps.Source (Source)
import Screeps.Structure (class Structure)

newtype ExecStatus
  = ExecStatus { moved :: Boolean
               , other :: Boolean
               }

instance partialMonoidExecStatus :: PartialMonoid ExecStatus where
  partialEmpty = ExecStatus {moved: false, other: false}
  -- TODO: Rewrite using purescript-record (PureScript 0.12 needed).
  partialAppend (ExecStatus rec1) (ExecStatus rec2) = do
    union1 <- union getMoved setMoved rec1 rec2
    union2 <- union getOther setOther union1 rec2
    pure $ ExecStatus union2
    where
      union get set rec1' rec2' =
        case Tuple (get rec1') (get rec2') of
          Tuple true true -> Nothing
          Tuple false false -> Just rec1'
          _ -> Just $ set rec1' true
      getMoved = (_.moved)
      setMoved r = r {moved = _}
      getOther = (_.other)
      setOther r = r {other = _}

statusMoved :: ExecStatus
statusMoved = ExecStatus {moved: true, other: false}

statusOther :: ExecStatus
statusOther = ExecStatus {moved: false, other: true}

data ExecError
  = ErrorMessage String
  | BadReturnCode ReturnCode

derive instance eqExecError :: Eq ExecError

type Exec = BlockableT ExecStatus

build ::
  forall e m.
    MonadError ExecError m => MonadEff (cmd :: CMD | e) m =>
      Creep -> ConstructionSite -> Exec m Unit
build creep site =
  liftSubAction creep statusOther $ Creep.build creep site

rangedAttackCreep ::
  forall e m.
    MonadError ExecError m => MonadEff (cmd :: CMD | e) m =>
      Creep -> Creep -> Exec m Unit
rangedAttackCreep creep enemy =
  liftSubAction creep statusOther $ Creep.rangedAttackCreep creep enemy

harvestSource ::
  forall e m.
    MonadError ExecError m => MonadEff (cmd :: CMD | e) m =>
      Creep -> Source -> Exec m Unit
harvestSource creep source =
  liftSubAction creep statusOther $ Creep.harvestSource creep source

moveTo ::
  forall e m a.
    MonadError ExecError m =>
    MonadEff (cmd :: CMD, memory :: MEMORY | e) m =>
      Creep -> TargetPosition a -> Exec m Unit
moveTo creep target =
  liftSubAction creep statusMoved do
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
  liftSubAction creep statusOther $
    Creep.transferToStructure creep structure resourceType

upgradeController ::
  forall e m.
    MonadError ExecError m => MonadEff (cmd :: CMD | e) m =>
      Creep -> Controller -> Exec m Unit
upgradeController creep controller =
  liftSubAction creep statusOther $ Creep.upgradeController creep controller

liftSubAction ::
  forall e m.
    MonadError ExecError m => MonadEff e m =>
      Creep -> ExecStatus -> Eff e ReturnCode -> Exec m Unit
liftSubAction creep status subAction =
  reserve status do
    code <- liftEff subAction
    if code == ok
      then pure unit
      else throwError $ BadReturnCode code

catchReturnCode ::
  forall m a.
    MonadError ExecError m =>
      ReturnCode -> m a -> m a -> m a
catchReturnCode code action handler = action `catchIt` const handler
  where
    catchIt = catchJust \error ->
      if error == BadReturnCode code
        then Just error
        else Nothing
