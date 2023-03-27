{-# LANGUAGE DataKinds #-}
module EffectZoo.Scenario.BigStack.PrEff.Identity where

import PrEff
import Data.Kind
import qualified Control.IxMonad as Ix

data Identity a where
  Noop :: a -> Identity a

runIdentity :: PrEff (Identity : effs) IVoid p q a -> PrEff effs IVoid p q a
runIdentity (Value a) = Ix.return a
runIdentity (Impure (OHere (Noop a)) k) = runIdentity (runIKleisliTupled k a)
runIdentity (Impure (OThere cmd) k) = Impure cmd $ hdl runIdentity k
