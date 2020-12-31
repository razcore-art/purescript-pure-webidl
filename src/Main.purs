module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Text.Parsing.Parser (runParser)
import WebGPU.Parser (parser)

main :: Effect Unit
main = logShow $ runParser "\"test\"" parser
