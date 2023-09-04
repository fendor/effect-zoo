{-# language DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module EffectZoo.Scenario.Reinterpretation.PrEff.HTTP where

import PrEff
import PrEff.Simple.Reader

data HTTP a where
  HttpGET :: String -> HTTP String

httpGET :: Member HTTP effs => String -> PrEff effs s p p String
httpGET = send . HttpGET

mockResponses :: PrEff (HTTP ': effs) IVoid () () a -> PrEff (Reader String ': effs) IVoid () () a
mockResponses = reinterpret $ \(HttpGET _path) -> ask
