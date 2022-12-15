{-# language DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module EffectZoo.Scenario.Reinterpretation.MiniEff.HTTP where

import MiniEff
import Simple.Reader

data HTTP a where
  HttpGET :: String -> HTTP String

httpGET :: Member HTTP effs => String -> MiniEff effs s p p String
httpGET = send . HttpGET

mockResponses :: MiniEff (HTTP ': effs) IVoid () () a -> MiniEff (Reader String ': effs) IVoid () () a
mockResponses = reinterpret $ \(HttpGET _path) -> ask
