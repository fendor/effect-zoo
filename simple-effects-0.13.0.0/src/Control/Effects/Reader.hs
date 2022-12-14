{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
-- | The regular old 'MonadReader' effect with some differences. First, there's no functional
--   dependency limiting your stack to a single environment type. This means less type inference so
--   it might not be enough to just write 'readEnv'. Write 'readEnv @MyEnvType' instead using
--   TypeApplications.
--
--   Second, the function has a less generic name and is called 'readEnv'.
--
--   Third, since it's a part of this effect framework, you get a 'implementReadEnv' function with
--   which you can provide a different environment implementation _at runtime_.
module Control.Effects.Reader (module Control.Effects.Reader, module Control.Effects) where

import Control.Effects
import GHC.Generics

newtype ReadEnv e m = ReadEnvMethods
    { _readEnv :: m e }
    deriving (Generic)
instance Effect (ReadEnv e)

-- | Read a value of type 'e'. Use with the TypeApplications extension to
--   help with type inference
--   @readEnv \@Int@
readEnv :: forall e m. MonadEffect (ReadEnv e) m => m e
readEnv = _readEnv effect

-- | Use the given action in the underlying monad to provide environment
--   values. You can think of @implementReadEnv x m@ as replacing all 'readEnv' calls
--   in 'm' with 'x'.
implementReadEnv :: Functor m => m e -> RuntimeImplemented (ReadEnv e) m a -> m a
implementReadEnv m = implement (ReadEnvMethods m)
