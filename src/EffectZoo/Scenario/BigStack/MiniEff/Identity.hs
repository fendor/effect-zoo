{-# LANGUAGE DataKinds #-}
module EffectZoo.Scenario.BigStack.MiniEff.Identity where

import MiniEff
import Data.Kind
import qualified Control.IxMonad as Ix

data Identity a where
  Noop :: a -> Identity a

runIdentity :: MiniEff (Identity : effs) IVoid p q a -> MiniEff effs IVoid p q a
runIdentity (Value a) = Ix.return a
runIdentity (Impure (OHere (Noop a)) k) = runIdentity (runIKleisliTupled k a)
runIdentity (Impure (OThere cmd) k) = Impure cmd $ hdl runIdentity k
