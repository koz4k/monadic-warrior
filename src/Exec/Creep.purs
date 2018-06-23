module Exec.Creep (ExecStatus, build, harvestSource, moveTo, rangedAttackCreep, transferToStructure, upgradeController) where

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Holder (class MonadHolder, class PartialMonoid)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Exec (ExecError, liftSubAction)
import Prelude (Unit, bind, pure, ($), (==))
import Screeps (Creep, TargetPosition)
import Screeps.ConstructionSite (ConstructionSite)
import Screeps.Controller (Controller)
import Screeps.Creep (build, harvestSource, moveTo, rangedAttackCreep, transferToStructure, upgradeController) as Creep
import Screeps.Resource (ResourceType)
import Screeps.ReturnCode (err_tired, ok)
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

build ::
  forall m.
    MonadHolder ExecStatus m => MonadError ExecError m =>
    MonadEffect m =>
      Creep -> ConstructionSite -> m Unit
build creep site =
  liftSubAction statusOther $ Creep.build creep site

rangedAttackCreep ::
  forall m.
    MonadHolder ExecStatus m => MonadError ExecError m =>
    MonadEffect m =>
      Creep -> Creep -> m Unit
rangedAttackCreep creep enemy =
  liftSubAction statusOther $ Creep.rangedAttackCreep creep enemy

harvestSource ::
  forall m.
    MonadHolder ExecStatus m => MonadError ExecError m =>
    MonadEffect m =>
      Creep -> Source -> m Unit
harvestSource creep source =
  liftSubAction statusOther $ Creep.harvestSource creep source

moveTo ::
  forall m a.
    MonadHolder ExecStatus m => MonadError ExecError m =>
    MonadEffect m =>
      Creep -> TargetPosition a -> m Unit
moveTo creep target =
  liftSubAction statusMoved do
    code <- Creep.moveTo creep target
    -- Ignore err_tired so that running moveTo twice blocks in this case.
    pure $ if code == err_tired
      then ok
      else code

transferToStructure ::
  forall m a.
    MonadHolder ExecStatus m => MonadError ExecError m =>
    MonadEffect m => Structure a =>
      Creep -> a -> ResourceType -> m Unit
transferToStructure creep structure resourceType =
  liftSubAction statusOther $
    Creep.transferToStructure creep structure resourceType

upgradeController ::
  forall m.
    MonadHolder ExecStatus m => MonadError ExecError m =>
    MonadEffect m =>
      Creep -> Controller -> m Unit
upgradeController creep controller =
  liftSubAction statusOther $ Creep.upgradeController creep controller
