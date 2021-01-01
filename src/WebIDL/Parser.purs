module WebIDL.Parser
  ( webIDLParser
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (filter)
import Data.Foldable (class Foldable)
import Data.Functor (voidLeft, voidRight)
import Data.String as S
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice, try)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.Token (GenLanguageDef(..), TokenParser, makeTokenParser, unGenLanguageDef)
import WebIDL.AST as AST

reservedUnsignedInteger :: Array String
reservedUnsignedInteger = [ "short", "long long", "long" ]

reservedString :: Array String
reservedString = [ "ByteString", "DOMString", "USVString" ]


reservedFloat :: Array String
reservedFloat = [ "float", "double" ]

tokenParser :: TokenParser
tokenParser
  = makeTokenParser $ LanguageDef (unGenLanguageDef javaStyle)
  { reservedNames = 
    [ "dictionary"
    , "interface"
    , "mixin"
    , "typedef"
    , "unsigned"
    ]
    <> filter ((_ < 2)<<< S.length) reservedUnsignedInteger
    <> reservedString
    <> reservedFloat
  }

reserved :: String -> Parser String Unit
reserved = tokenParser.reserved

reservedRetain :: String -> Parser String String
reservedRetain s = reserved s $> s

parseTypeSimple :: ∀ f. Functor f => Foldable f => f String -> Parser String String
parseTypeSimple = choice <<< map reservedRetain

parseTypeUnsignedInteger :: Parser String AST.Type
parseTypeUnsignedInteger = AST.TypeUnsignedInteger <$> parseTypeSimple reservedUnsignedInteger

parseTypeFloat :: Parser String AST.Type
parseTypeFloat = AST.TypeFloat <$> parseTypeSimple reservedFloat

parseTypeString :: Parser String AST.Type
parseTypeString = AST.TypeString <$> parseTypeSimple reservedString

webIDLParser ::Parser String AST.Type
webIDLParser
  =  tokenParser.whiteSpace
  *>  parseTypeFloat
  <|> parseTypeString
  <|> parseTypeUnsignedInteger
