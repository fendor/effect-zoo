{-# language KindSignatures, FlexibleContexts, DeriveFunctor, TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module EffectZoo.Scenario.Reinterpretation.FusedEffects.HTTP where

import "fused-effects" Control.Algebra
import "fused-effects" Control.Effect.Sum
import "fused-effects" Control.Effect.Reader
import Data.Kind

data HTTP (m :: Type -> Type) a where
  GET :: String -> HTTP m String

httpGET :: Has HTTP sig m => String -> m String
httpGET url = send (GET url)

newtype ReaderHTTPC m a = ReaderHTTPC { runReaderHTTPC :: m a }
  deriving (Functor, Applicative, Monad)

instance (Monad m, Algebra sig m, Has (Reader String) sig m) => Algebra (HTTP :+: sig) (ReaderHTTPC m) where
  alg hdl sig ctx = case sig of
    (L (GET _path)) -> (<$ ctx) <$> ask
    (R other) -> ReaderHTTPC (alg (runReaderHTTPC . hdl) other ctx)

mockResponses :: ReaderHTTPC m a -> m a
mockResponses = runReaderHTTPC
