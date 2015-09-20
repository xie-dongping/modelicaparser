

module Language.Modelica.Parser.Programme where


import Language.Modelica.Syntax.Programme

import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Lexer
import Language.Modelica.Parser.Utility (until)

import Prelude hiding (until)

import Text.ParserCombinators.Parsec
  ((<|>), try, manyTill, anyChar, lookAhead, string, char, eof)


import Control.Applicative (liftA)

code :: Parser TextSegment
code = liftA Code $ manyTill anyChar (lookAhead p)
  where p = string "\""
            <|> try (string "//")
            <|> try (string "/*")
            <|> (eof >> return "")

block :: Parser TextSegment
block =
  (char '\"' >> Str `until` (char '\"'))
  <|> (try (string "//") >> LineComment `until` eol_or_eof)
  <|> (try (string "/*") >> BlockComment `until` try (string "*/"))
  <|> code

modelica_programme :: Parser [TextSegment]
modelica_programme = manyTill block eof


