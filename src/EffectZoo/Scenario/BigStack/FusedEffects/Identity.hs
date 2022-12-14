{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances, KindSignatures #-}
module EffectZoo.Scenario.BigStack.FusedEffects.Identity where

import "fused-effects" Control.Algebra
import "fused-effects" Control.Effect.Sum
import Data.Kind

data Identity (m :: Type -> Type) k where
  Noop :: m k -> Identity m k

newtype IdentityC m a = IdentityC { runIdentity :: m a }
  deriving (Functor, Applicative, Monad)

instance Algebra sig m => Algebra (Identity :+: sig) (IdentityC m) where
  alg hdl sig ctx = case sig of
    L (Noop m)      -> hdl $ m <$ ctx
    R other         -> IdentityC $ alg (runIdentity . hdl) other ctx
