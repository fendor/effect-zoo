{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators,
  KindSignatures, FlexibleInstances, MultiParamTypeClasses,
  UndecidableInstances, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module EffectZoo.Scenario.FileSizes.FusedEffects.File where

import "fused-effects" Control.Algebra
import "fused-effects" Control.Effect.Sum
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified EffectZoo.Scenario.FileSizes.Shared
                                               as Shared

data File m k = TryFileSize FilePath (Maybe Int -> m k)

tryFileSize :: Has File sig m => FilePath -> m (Maybe Int)
tryFileSize path = send (TryFileSize path pure)

newtype FileIOC m a = FileIOC
  { runFileIOC :: m a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (File :+: sig) (FileIOC m) where
  alg hdl sig ctx = case sig of
    L (TryFileSize path k) -> FileIOC $ do
      msize <- liftIO (Shared.tryGetFileSize path)
      runFileIOC (hdl (k msize <$ ctx))
    R other -> FileIOC (alg (runFileIOC . hdl) other ctx)
