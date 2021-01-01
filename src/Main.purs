module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Text.Parsing.Parser (runParser)
import WebIDL.Parser (webIDLParser)

testString :: String
testString = "long"

main :: Effect Unit
main = logShow $ runParser testString webIDLParser

-- filepath :: FilePath
-- filepath = "gpuweb/spec/webgpu.idl"
--
-- main :: Effect Unit
-- main = launchAff_ do
  -- text <- readTextFile UTF8 filepath
  -- logShow $ runParser text webIDLParser
