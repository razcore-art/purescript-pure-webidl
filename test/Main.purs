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
import Text.Parsing.Parser (Parser, runParser)
import WebIDL.AST as AST
import WebIDL.Parser as P


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
   let unsignedIntegers   = [ identity, ("unsigned " <> _)]     <*> P.integers
       unrestrictedFloats = [ identity, ("unrestricted " <> _)] <*> P.floats

   describePrimitiveType "PrimitiveBasic"   P.typePrimitiveBasic    P.basics
   describePrimitiveType "UnsignedInteger"  P.typeUnsignedInteger   unsignedIntegers
   describePrimitiveType "UrestrictedFloat" P.typeUnrestrictedFloat unrestrictedFloats
   describePrimitiveType "Primitive"        P.typePrimitive       $ P.basics <> unsignedIntegers <> unrestrictedFloats


describePrimitiveType :: String -> Parser String AST.IDLType -> Array String -> Spec Unit
describePrimitiveType description parser inputs
   = describe description do
      let post      = (_ <> " post")
          nullable  = (_ <> "?")
          alteredBy = alteredTuple <$> [ identity, post, nullable, post <<< nullable ]
      it "should succeed on well formed input" $ fold do
         Tuple input alteredInput <- alteredBy <*> inputs
         let parsed   = hush $ runParser alteredInput parser
             expected = Just <<< (if contains (Pattern "?") alteredInput
                                     then AST.IDLTypeNullable
                                     else identity) <<< AST.IDLTypeNamed $ input
         pure $ parsed `shouldEqual` expected
   where alteredTuple f s = Tuple s $ f s
