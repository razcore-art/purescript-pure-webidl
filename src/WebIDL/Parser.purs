module WebIDL.Parser
  ( basics
  , integers
  , floats
  , typePrimitiveBasic
  , typeUnsignedInteger
  , typeUnrestrictedFloat
  , typePrimitive
  , words
  ) where
  -- where

import Prelude

import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice, option, try)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.Token (GenLanguageDef(..), TokenParser, makeTokenParser, unGenLanguageDef)
import WebIDL.AST as AST


whitespaceRegex :: Regex
whitespaceRegex
  = case R.regex """\s+""" RF.noFlags of
         Left  err -> unsafeCrashWith $ "[whitespaceRegex] `\\s+` seems to be invalid, err: " <> err
         Right r   -> r

words :: String -> Array String
words "" = empty
words s  = R.split whitespaceRegex s

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

tokenParser :: TokenParser
tokenParser
  = makeTokenParser
  $ LanguageDef (unGenLanguageDef javaStyle)
  { reservedNames = reservedNames
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
         ""           -> parser ""
         optionalType -> (option "" $ (_ <> " ") <$> retainReserved optionalType) >>= parser
  where parser preName = do
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

-- TODO: start working on the bigger parser that includes other non PrimitiveType parsers
typeString :: Parser String AST.IDLType
typeString = typePrimitiveBuilder strings ""

typePrimitive :: Parser String AST.IDLType
typePrimitive = choice [ typePrimitiveBasic, typeUnsignedInteger, typeUnrestrictedFloat ]
