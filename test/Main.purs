module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Index (str)

main :: Effect Unit
main = do
  log "🍝"
  log str
  log "You should add some tests."
