module WebIDL.Parser
  ( basics
  , floats
  , integers
  , identStartChar
  , strings
  , parser
  , typeIdentifier
  , typePrimitive
  , typeString
  , unwords
  , words
  ) where
  -- where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (class Foldable, intercalate)
import Data.String.CodeUnits as S
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Data.String.Regex.Unsafe (unsafeRegex)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice, option, try)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (GenLanguageDef(..), TokenParser, letter, makeTokenParser, unGenLanguageDef)
import WebIDL.AST as AST


whitespaceRegex :: Regex
whitespaceRegex = unsafeRegex """\s+""" RF.noFlags

words :: String -> Array String
words "" = empty
words s  = R.split whitespaceRegex s

unwords :: ∀ f. Foldable f => f String -> String
unwords = intercalate " "

basics :: Array String
basics
  = [ "undefined"
    , "boolean"
    , "byte"
    , "octet"
    , "bigint"
    ]

integers :: Array String
integers
  = [ "short"
    , "long"
    ]

floats :: Array String
floats
  = [ "float"
    , "double"
    ]

strings :: Array String
strings
  = [ "ByteString"
    , "DOMString"
    , "USVString"
    ]

others :: Array String
others
  = [ "unsigned"
    , "unrestricted"
    ]


reservedNames :: Array String
reservedNames = integers <> floats <> strings <> others

identStartChar :: Char
identStartChar = '_'

tokenParser :: TokenParser
tokenParser
  = makeTokenParser
  $ LanguageDef (unGenLanguageDef javaStyle)
  { identStart      = letter <|> char identStartChar
  , reservedNames   = reservedNames
  , reservedOpNames = [ "?" ]
  }


retainReserved :: String -> Parser String String
-- "long" is special because it can be "long IDENTIFIER" or "long long IDENTIFIER"
retainReserved s | s == "long" = s <$ choice [ try (tokenParser.reserved s <* tokenParser.reserved s), tokenParser.reserved s ]
                 | otherwise   = s <$ tokenParser.reserved s

typeSimple :: ∀ f. Functor f => Foldable f => f String -> Parser String String
typeSimple = choice <<< map retainReserved


typePrimitiveBuilder :: ∀ f. Functor f => Foldable f => f String -> String -> Parser String AST.IDLType
typePrimitiveBuilder types
  = case _ of
         ""           -> parserImpl ""
         optionalType -> (option "" $ (_ <> " ") <$> retainReserved optionalType) >>= parserImpl
  where parserImpl preName = do
          name     <- typeSimple types
          nullable <- option false $ true <$ tokenParser.reservedOp "?"
          let idlTypeNamed = AST.IDLTypeNamed $ preName <> name
          pure $ (if nullable
                    then AST.IDLTypeNullable
                    else identity) idlTypeNamed


typePrimitiveBasic :: Parser String AST.IDLType
typePrimitiveBasic = typePrimitiveBuilder basics ""

typeUnsignedInteger :: Parser String AST.IDLType
typeUnsignedInteger = typePrimitiveBuilder integers "unsigned"

typeUnrestrictedFloat :: Parser String AST.IDLType
typeUnrestrictedFloat = typePrimitiveBuilder floats "unrestricted"

typePrimitive :: Parser String AST.IDLType
typePrimitive = choice [ typePrimitiveBasic, typeUnsignedInteger, typeUnrestrictedFloat ]

typeString :: Parser String AST.IDLType
typeString = typePrimitiveBuilder strings ""

typeIdentifier :: Parser String AST.IDLType
typeIdentifier = do
  identifier <- tokenParser.identifier
  nullable   <- option false $ true <$ tokenParser.reservedOp "?"
  let idlTypeNamed = AST.IDLTypeNamed <<< S.dropWhile (_ == identStartChar) $ identifier
  pure $ (if nullable
             then AST.IDLTypeNullable
             else identity) idlTypeNamed

parser :: Parser String AST.IDLType
parser = choice [ typePrimitive, typeString, typeIdentifier ]
