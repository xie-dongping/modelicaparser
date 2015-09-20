

module Language.Modelica.Parser.ComponentClause where


import Language.Modelica.Syntax.Modelica

import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Expression
import Language.Modelica.Parser.Modification
import Language.Modelica.Parser.Lexer
import Language.Modelica.Parser.Basic


import Control.Applicative
  (liftA, liftA2, liftA3, (*>), (<$>), (<*>))

import Text.ParserCombinators.Parsec (optionMaybe)



component_clause :: Parser ComponentClause
component_clause = ComponentClause <$>
  type_prefix <*>
  type_specifier <*>
  (optionMaybe array_subscripts) <*>
  component_list


component_list :: Parser ComponentList
component_list = liftA2 ComponentList
  component_declaration (commaList component_declaration)

component_declaration :: Parser ComponentDeclaration
component_declaration = liftA3 ComponentDeclaration
  declaration (optionMaybe condition_attribute) comment

condition_attribute :: Parser ConditionAttribute
condition_attribute =
  liftA ConditionAttribute $ if_ *> expression
