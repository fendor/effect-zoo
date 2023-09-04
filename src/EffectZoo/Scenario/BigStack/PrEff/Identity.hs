{-# LANGUAGE DataKinds #-}
module EffectZoo.Scenario.BigStack.PrEff.Identity where

import PrEff
import Data.Kind
import qualified Control.IxMonad as Ix

data Identity a where
  Noop :: Identity ()

runIdentity :: PrEff (Identity : effs) IVoid p q a -> PrEff effs IVoid p q a
runIdentity = interpret $ \case
  Noop -> pure ()
