{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators,
  KindSignatures, FlexibleInstances, MultiParamTypeClasses,
  UndecidableInstances, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module EffectZoo.Scenario.FileSizes.FusedEffects.Logging where

import "fused-effects" Control.Algebra
import "fused-effects" Control.Effect.Sum
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified EffectZoo.Scenario.FileSizes.Shared
                                               as Shared
import Data.Kind

data Logging (m :: Type -> Type) a where
  LogMsg :: String -> Logging m ()

logMsg :: Has Logging sig m => String -> m ()
logMsg msg = send (LogMsg msg)

newtype LogIOC m a = LogIOC
  { unLogIOC :: ReaderT (IORef [String]) m a
  } deriving (Functor, Applicative, Monad, MonadIO)

runLogIOC :: IORef [String] -> LogIOC m a -> m a
runLogIOC r (LogIOC (ReaderT m)) = m r

instance (Algebra sig m, MonadIO m) => Algebra (Logging :+: sig) (LogIOC m) where
  alg hdl sig ctx = case sig of
    L (LogMsg msg) -> (<$ ctx) <$> (LogIOC $ ReaderT $ \r -> liftIO (Shared.logToIORef r msg))
    R other -> LogIOC $ ReaderT $ \r -> (alg ((`runReaderT` r) . unLogIOC . hdl) other ctx)
