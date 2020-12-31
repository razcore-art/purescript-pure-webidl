module WebGPU.Parser where

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser)

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

parser ::Parser String String
parser = tokenParser.stringLiteral
