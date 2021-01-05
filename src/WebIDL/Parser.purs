module WebIDL.Parser
  ( basics
  , integers
  , floats
  , typePrimitiveBasic
  , typeUnsignedInteger
  , typeUnrestrictedFloat
  , typePrimitive
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
    , "long long"
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
reservedNames = integers <> strings <> floats <> others

tokenParser :: TokenParser
tokenParser
  = makeTokenParser
  $ LanguageDef (unGenLanguageDef javaStyle)
  { reservedNames = filter ((_ < 2) <<< S.length) reservedNames
  , reservedOpNames = [ "?" ]
  }


retainReserved :: String -> Parser String String
retainReserved s = tokenParser.reserved s $> s

typeSimple :: ∀ f. Functor f => Foldable f => f String -> Parser String String
typeSimple = choice <<< map retainReserved


typePrimitiveBuilder :: ∀ f. Functor f => Foldable f => f String -> String -> Parser String AST.IDLType
typePrimitiveBuilder types
  = case _ of
         ""           -> parser ""
         optionalType -> (option "" $ (_ <> " ") <$> retainReserved optionalType) >>= parser
  where parser preName = do
          name     <- typeSimple types
          nullable <- option false $ tokenParser.reservedOp "?" $> true
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
