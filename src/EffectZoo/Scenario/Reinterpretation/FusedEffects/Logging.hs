{-# language KindSignatures, FlexibleContexts, TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DeriveFunctor #-}
module EffectZoo.Scenario.Reinterpretation.FusedEffects.Logging where

import "fused-effects" Control.Algebra
import "fused-effects" Control.Effect.Sum
import "fused-effects" Control.Effect.Writer
import Data.Kind

data Logging (m :: Type -> Type) k where
  LogMsg :: String -> Logging m ()

logMsg :: Has Logging sig m => String -> m ()
logMsg msg = send (LogMsg msg)

newtype WriterLoggingC m a = WriterLoggingC { runWriterLoggingC :: m a }
  deriving (Functor, Applicative, Monad)

instance Has (Writer [String]) sig m => Algebra (Logging :+: sig) (WriterLoggingC m) where
  alg hdl sig ctx = case sig of
    (L (LogMsg msg)) -> ctx <$ tell [msg]
    (R other) -> WriterLoggingC (alg (runWriterLoggingC . hdl) other ctx)

accumulateLogMessages
  :: Has (Writer [String]) sig m
  => WriterLoggingC m a
  -> m a
accumulateLogMessages = runWriterLoggingC
