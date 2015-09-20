

module Language.Modelica.Parser.Modification where

import Language.Modelica.Syntax.Modelica

import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Lexer
import Language.Modelica.Parser.Basic
import Language.Modelica.Parser.Expression
import Language.Modelica.Parser.Utility (eitherOr, eitherOrOr)


import Control.Applicative
  (liftA, liftA2, liftA3, (*>), (<$>), (<*>))

import Text.ParserCombinators.Parsec ((<|>), optionMaybe)



modification :: Parser Modification
modification = 
  liftA ModificationAssign (assign *> expression)
  <|> liftA ModificationColonAssign (colon_assign *> expression)
  <|> liftA2 Modification 
             class_modification (optionMaybe (assign *> expression))



class_modification :: Parser ClassModification
class_modification =
  liftA ClassModification (parens $ optionMaybe argument_list)


argument_list :: Parser ArgumentList
argument_list = liftA2 ArgumentList
  argument (commaList argument)

argument :: Parser Argument
argument =
  liftA ArgElementRedeclaration element_redeclaration
  <|> liftA ArgElementModOrRep element_modification_or_replacable

element_modification_or_replacable :: Parser ElementModOrRep
element_modification_or_replacable =
  liftA3 ElementModOrRep
    (optionMaybe each_)
    (optionMaybe final_)
    (element_modification `eitherOr` element_replaceable)

element_modification :: Parser ElementModification
element_modification = liftA3 ElementModification
  name (optionMaybe modification) (optionMaybe string_comment)

element_redeclaration :: Parser ElementRedeclaration
element_redeclaration =
  liftA3 ElementRedeclaration
  (redeclare_ *> optionMaybe each_) 
  (optionMaybe final_)
  (eitherOrOr short_class_definition component_clause1 element_replaceable)


element_replaceable :: Parser ElementReplaceableShort
element_replaceable = liftA2 ElementReplaceableShort
  (replaceable_ *> (short_class_definition `eitherOr` component_clause1))
  (optionMaybe constraining_clause)

short_class_definition :: Parser ShortClassDefinition
short_class_definition = liftA3 ShortClassDefinition
  class_prefixes
  ident
  (assign *> (short_class_def_1 <|> short_class_def_2))

short_class_def_1 :: Parser ShortClassDef
short_class_def_1 = liftA2 ShortClassDef1
  (enumeration_ *> enum_list_or_colon)
  comment

enum_list_or_colon :: Parser (Either Colon (Maybe EnumList))
enum_list_or_colon = parens $ colon `eitherOr` optionMaybe enum_list

enum_list :: Parser EnumList
enum_list = liftA2 EnumList
  enumeration_literal (commaList enumeration_literal)

enumeration_literal :: Parser EnumerationLiteral
enumeration_literal =
  liftA2 EnumerationLiteral ident comment


short_class_def_2 :: Parser ShortClassDef
short_class_def_2 = ShortClassDef2 <$>
  base_prefix <*>
  name <*>
  (optionMaybe array_subscripts) <*>
  (optionMaybe class_modification) <*>
  comment

component_clause1 :: Parser ComponentClause1
component_clause1 = liftA3 ComponentClause1
  type_prefix type_specifier component_declaration1

component_declaration1 :: Parser ComponentDeclaration1
component_declaration1 =
  liftA2 ComponentDeclaration1 declaration comment


extends_clause :: Parser ExtendsClause
extends_clause = liftA3 ExtendsClause
  (extends_ *> name)
  (optionMaybe class_modification)
  (optionMaybe annotation)

constraining_clause :: Parser ConstrainingClause
constraining_clause = liftA2 ConstrainingClause
  (constrainedby_ *> name) (optionMaybe class_modification)

declaration :: Parser Declaration
declaration = liftA3 Declaration
  ident (optionMaybe array_subscripts) (optionMaybe modification)

annotation :: Parser Annotation
annotation =
  liftA Annotation (annotation_ *> class_modification)

comment :: Parser Comment
comment =
  liftA2 Comment (optionMaybe string_comment) (optionMaybe annotation)
