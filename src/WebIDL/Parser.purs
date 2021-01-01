module WebIDL.Parser
  ( webIDLParser
  ) where

import Prelude

import Data.Array (filter)
import Data.Foldable (class Foldable)
import Data.String as S
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice, option)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.Token (GenLanguageDef(..), TokenParser, makeTokenParser, unGenLanguageDef)
import WebIDL.AST as AST

unsignedIntegers :: Array String
unsignedIntegers
  = [ "short"
    , "long long"
    , "long"
    ]

strings :: Array String
strings
  = [ "ByteString"
    , "DOMString"
    , "USVString"
    ]

floats :: Array String
floats
  = [ "float"
    , "double"
    ]

reservedNames :: Array String
reservedNames = unsignedIntegers <> strings <> floats

tokenParser :: TokenParser
tokenParser
  = makeTokenParser
  $ LanguageDef (unGenLanguageDef javaStyle)
  { reservedNames =
    [ "unsigned"
    ] <> filter ((_ < 2) <<< S.length) reservedNames
  , reservedOpNames = [ "?" ]
  }

retainReserved :: String -> Parser String String
retainReserved s = tokenParser.reserved s $> s

typeSimple :: ∀ f. Functor f => Foldable f => f String -> Parser String String
typeSimple = choice <<< map retainReserved

typeUnsignedInteger :: Parser String AST.IDLType
typeUnsignedInteger = do
  unsigned <- option "" $ (_ <> " ") <$> retainReserved "unsigned"
  name     <- typeSimple unsignedIntegers
  nullable <- option false $ tokenParser.reservedOp "?" $> true
  let idlTypeNamed = AST.IDLTypeNamed $ unsigned <> name
  pure if nullable
    then AST.IDLTypeNullable idlTypeNamed
    else idlTypeNamed

webIDLParser ::Parser String AST.IDLType
webIDLParser = tokenParser.whiteSpace *> typeUnsignedInteger
