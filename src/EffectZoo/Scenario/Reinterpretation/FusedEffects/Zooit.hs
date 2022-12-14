{-# language KindSignatures, TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DeriveFunctor, FlexibleContexts #-}
module EffectZoo.Scenario.Reinterpretation.FusedEffects.Zooit where

import GHC.Generics (Generic1)
import "fused-effects" Control.Algebra
import "fused-effects" Control.Effect.Sum
import           EffectZoo.Scenario.Reinterpretation.FusedEffects.HTTP
import           EffectZoo.Scenario.Reinterpretation.FusedEffects.Logging
import Data.Kind

data Zooit (m :: Type -> Type) k where
    ListScenarios :: Zooit m [String]

listScenarios :: Has Zooit sig m => m [String]
listScenarios = send ListScenarios

newtype LoggedHTTPC m a = LoggedHTTPC { runLoggedHTTPC :: m a }
  deriving (Functor, Applicative, Monad)

instance (Monad m, Has Logging sig m, Has HTTP sig m) => Algebra (Zooit :+: sig) (LoggedHTTPC m) where
  alg hdl sig ctx = case sig of
    L (ListScenarios) -> do
      logMsg "Fetching a list of scenarios"
      scenarios <- lines <$> httpGET "/scenarios"
      pure (scenarios <$ ctx)
    R other -> LoggedHTTPC (alg (runLoggedHTTPC . hdl) other ctx)

toLoggedHTTP :: LoggedHTTPC m a -> m a
toLoggedHTTP = runLoggedHTTPC
