module Test.Main where

import Prelude

import Data.Array (concatMap, fold, intercalate, replicate)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), contains, replace)
import Data.String.CodeUnits as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (choice)
import WebIDL.AST as AST
import WebIDL.Parser as P


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $ parallel do
   let duplicateLong x    = if x == "long" then [ x, intercalate " " $ replicate 2 x ] else [ x ]
       unsignedIntegers   = [ identity, ("unsigned " <> _) ]     <*> concatMap duplicateLong P.integers
       unrestrictedFloats = [ identity, ("unrestricted " <> _) ] <*> P.floats
       allSimpleInputs    = P.basics <> unsignedIntegers <> unrestrictedFloats <> P.strings

   describeSimpleType "Primitive <|> String" simpleParser allSimpleInputs
   describeIdentifier [ "some", "identifiers", "_andWithIdentStartChar" ]
   where simpleParser = choice [ P.typePrimitive, P.typeString ]


describeSimpleType :: String -> Parser String AST.IDLType -> Array String -> Spec Unit
describeSimpleType description parser inputs
   = describe description do
      it "should succeed on well formed input" $ fold do
         Tuple input alteredInput <- alteredBy <*> inputs
         let parsed   = hush $ runParser alteredInput parser
             expected = Just <<< (if contains (Pattern "?") alteredInput
                                     then AST.IDLTypeNullable
                                     else identity) <<< AST.IDLTypeNamed <<< unDuplicateLong $ input
         pure $ parsed `shouldEqual` expected
   where unDuplicateLong  = replace (Pattern "long long") (Replacement "long")

describeIdentifier :: Array String -> Spec Unit
describeIdentifier inputs
   = describe "Identifier" do
      it "should succeed on well formed input" $ fold do
         Tuple input alteredInput <- alteredBy <*> inputs
         let parsed = hush $ runParser alteredInput P.typeIdentifier
             expected = Just <<< (if contains (Pattern "?") alteredInput
                                     then AST.IDLTypeNullable
                                     else identity) <<< AST.IDLTypeNamed <<< dropIdentStart $ input
         pure $ parsed `shouldEqual` expected
   where dropIdentStart = S.dropWhile (_ == P.identStartChar)


post :: String -> String
post = (_ <> " post")

nullable :: String -> String
nullable = (_ <> "?")

alteredBy :: Array (String -> Tuple String String)
alteredBy = alteredTuple <$> [ identity, post, nullable, post <<< nullable ]
   where alteredTuple f s = Tuple s $ f s
