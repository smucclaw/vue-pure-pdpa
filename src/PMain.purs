module PMain where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"

str :: String
str = "hello from purescript!"