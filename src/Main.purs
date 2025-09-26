module Main where

import Prelude

import Component.Counter as Counter
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI (Counter.component 3) 0 body
    void $ liftEffect $ HS.subscribe io.messages \currentCount ->
      log $ "Received Count from Top-level Counter: " <> show currentCount
