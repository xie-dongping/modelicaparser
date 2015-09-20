

module Language.Modelica.Test.Lexer (test) where

import Language.Modelica.Syntax.ToString (toString)

import qualified Language.Modelica.Parser.Lexer as Lexer

import Language.Modelica.Test.Utility (testparser)

import Text.ParserCombinators.Parsec (eof)

import Text.Printf (printf)

import Control.Applicative ((<*), Applicative)


tests_unsigned_number :: [(String, Double)]
tests_unsigned_number =
  ("123", 123) :
  ("123   ", 123) :
  ("123.567", 123.567) :
  ("123.", 123) :
  ("123e0", 123) :
  ("123.e10", 1.23e12) :
  ("123.5e10", 1.235e12) :
  ("123e+10", 1.23e12) :
  ("123.e+10", 1.23e12) :
  ("123.5e+10", 1.235e12) :
  ("123E-10", 1.23e-8) :
  ("123.E-10", 1.23e-8) :
  ("123.5e-10", 1.235e-8) :
  []

tests_unicode_string :: [String]
tests_unicode_string =
  "\"def\"" :
  "\"äüß³¼æðſđilha\"" :
  "\"   ֆդսիոասԱԴՍՁՅ՞՞ՉՋ1Ձյձ  \"" :
  []


tests_ident :: [String]
tests_ident =
  "x" :
  "a23aa2" :
  "'bla'" :
  []


test :: IO [Bool]
test = do
  let f (str, res) = (str, testparser (Lexer.unsigned_number <* eof) str == res)
      h str = (str, printf "\"%s\"" (testparser (Lexer.unicode_string <* eof) str) == str)
      i str = (str, toString (testparser (Lexer.ident <* eof) str) == str)
      as = map f tests_unsigned_number
      cs = map h tests_unicode_string
      ds = map i tests_ident
      pr str res = do
        print str
        print res
        putStrLn "-----------------------------------------------------------"

  let res = as ++ cs ++ ds
  mapM_ (uncurry pr) res
  return (map snd res)

