

module Language.Modelica.Parser.Basic where


import Language.Modelica.Syntax.Modelica

import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Lexer
import Language.Modelica.Parser.Expression


import Text.ParserCombinators.Parsec ((<|>), try, optionMaybe)

import Control.Applicative (liftA, liftA2, liftA3, (<*))



class_prefixes :: Parser ClassPrefixes
class_prefixes = liftA2 ClassPrefixes
  (optionMaybe partial_)
  ( class_
    <|> model_
    <|> block_
    <|> type_
    <|> package_
    <|> class_prefixes_connector
    <|> try class_prefixes_function
    <|> try class_prefixes_record
    <|> operator_)


class_prefixes_function :: Parser Prefix
class_prefixes_function = liftA2 FunctionPrefix
  (optionMaybe (pure_ <|> impure_))
  (optionMaybe operatorfunction_ <* function_)

class_prefixes_record :: Parser Prefix
class_prefixes_record = 
  liftA Record (optionMaybe operatorrecord_ <* record_)

class_prefixes_connector :: Parser Prefix
class_prefixes_connector = 
  liftA Connector (optionMaybe expandable_ <* connector_)

type_prefix :: Parser TypePrefix
type_prefix = liftA3 TypePrefix
  (optionMaybe (flow_ <|> stream_))
  (optionMaybe (discrete_ <|> parameter_ <|> constant_))
  (optionMaybe (input_ <|> output_))

type_specifier :: Parser Name
type_specifier = name

base_prefix :: Parser BasePrefix
base_prefix = type_prefix

string_comment :: Parser StringComment
string_comment = liftA StringComment $
  optionMaybe (liftA2 (,) unicode_string (plusList unicode_string))
