

module Language.Modelica.Parser where


import Language.Modelica.Syntax.Programme
import Language.Modelica.Syntax.Modelica
import Language.Modelica.Syntax.ToString (toString)

import Language.Modelica.Parser.ClassDefinition
import Language.Modelica.Parser.Programme 
import Language.Modelica.Parser.Option (OptionSet, defaultOptions)
import Language.Modelica.Parser.Utility (stringParser)

import Text.Parsec.Prim (runP)


commentsAndCode ::
  OptionSet -> FilePath -> String -> [TextSegment]
commentsAndCode opts file str =
  case runP (stringParser modelica_programme) opts file str of
       Right x -> x
       Left err -> error (show err)

simple :: FilePath -> String -> StoredDefinition
simple = withOptions defaultOptions

withOptions :: OptionSet -> FilePath -> String -> StoredDefinition
withOptions opts file str =
  let txt = concatMap f $ commentsAndCode opts file str
      f xs@(Str _) = toString xs
      f xs@(Code _) = toString xs
      f (LineComment _) = "\n"
      f (BlockComment xs) = filter (== '\n') xs
  in case runP stored_definition opts file txt of
          Right ast -> ast
          Left err -> error $ show err
