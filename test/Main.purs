module Test.Main where

import Prelude

import Data.Array (fold)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser (runParser)
import WebIDL.AST as AST
import WebIDL.Parser (webIDLParser)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
   describeUnsignedInteger


describeUnsignedInteger :: Spec Unit
describeUnsignedInteger
   = describe "UnsignedInteger" do
      let inputs    = [ "short", "long", "long long", "unsigned short", "unsigned long", "unsigned long long" ]
          before    = (" " <> _)
          after     = (_ <> " after")
          nullable  = (_ <> "?")
          alteredBy = alteredTuple
                  <$> [ identity
                      , before
                      , after
                      , nullable
                      , after    <<< before
                      , nullable <<< before
                      , after    <<< nullable
                      , after    <<< nullable <<< before
                      ]
      it "should succeed on well formed input" $ fold do
         Tuple input alteredInput <- alteredBy <*> inputs
         let parsed   = hush $ runParser alteredInput webIDLParser
             expected = Just <<< (if contains (Pattern "?") alteredInput
                                     then AST.IDLTypeNullable
                                     else identity) <<< AST.IDLTypeNamed $ input
         pure $ parsed `shouldEqual` expected
   where alteredTuple f s = Tuple s $ f s
