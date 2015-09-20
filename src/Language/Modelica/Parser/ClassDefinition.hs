
module Language.Modelica.Parser.ClassDefinition where

import Language.Modelica.Syntax.Modelica

import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Lexer
import Language.Modelica.Parser.Expression
import Language.Modelica.Parser.Modification
import Language.Modelica.Parser.ComponentClause
import Language.Modelica.Parser.Equation
import Language.Modelica.Parser.Basic
import Language.Modelica.Parser.Option
import Language.Modelica.Parser.Utility (eitherOr, (~*))

import Control.Applicative
  (liftA, liftA2, liftA3, (*>), (<*), (<*>), (<$>))

import Text.ParserCombinators.Parsec
  ((<|>), optionMaybe, try, many, getState)

import qualified Data.Set as Set


import_clause :: Parser ImportClause
import_clause = liftA2 ImportClause
  (import_ *> (try import_clause1 <|> import_clause2)) comment


import_clause1 :: Parser Import
import_clause1 = liftA2 Assign
  ident (assign *> name)

import_clause2 :: Parser Import
import_clause2 = liftA2 IList
  name
  (optionMaybe $ (symbol "." *> (star `eitherOr` braces import_list)))

import_list :: Parser ImportList
import_list = liftA2 ImportList
  ident (commaList ident)

element :: Parser Element
element =
  liftA ElementImportClause (try import_clause)
  <|> liftA ElementExtendsClause (try extends_clause)
  <|> element_help

element_help :: Parser Element
element_help = element_options >>= \opts ->
  liftA (Element opts) classDefOrCompClause
  <|> liftA2 (ElementReplaceable opts) (replaceable_ *> classDefOrCompClause)
             (optionMaybe (liftA2 (,) constraining_clause comment))

element_options :: Parser ElementOptions
element_options = ElementOptions <$>
  optionMaybe redeclare_ <*>
  optionMaybe final_ <*>
  optionMaybe inner_ <*>
  optionMaybe outer_

classDefOrCompClause :: Parser (Either ClassDefinition ComponentClause)
classDefOrCompClause =
  liftA Left class_definition
  <|> liftA Right component_clause

element_list :: Parser ElementList
element_list =
  liftA (ElementList . map fst) (element ~* semicolon)

external_function_call :: Parser ExternalFunctionCall
external_function_call = liftA3 ExternalFunctionCall
  (optionMaybe (try $ component_reference <* assign))
  ident
  (parens (optionMaybe expression_list))

language_specification :: Parser LanguageSpecification
language_specification = unicode_string


composition_list :: Parser [CompositionList]
composition_list = many $
  liftA PublicElementList (public_ *> element_list)
  <|> liftA ProtectedElementList (protected_ *> element_list)
  <|> liftA ESec (try equation_section)
  <|> liftA ASec (try algorithm_section)


composition_external :: Parser CompositionExternal
composition_external = liftA3 CompositionExternal
  (external_ *> optionMaybe language_specification)
  (optionMaybe external_function_call)
  (optionMaybe annotation <* semicolon)


-- | This parser behaves according to the specification.
composition_annotation_last :: Parser Composition
composition_annotation_last = Composition <$>
  element_list <*>
  composition_list <*>
  (optionMaybe composition_external) <*>
  (optionMaybe (annotation <* semicolon))

-- | In order to succeed, this parser needs the /annotation/ keyword first.
composition_annotation_first :: Parser Composition
composition_annotation_first =
  let comp ann el cl ce = Composition el cl ce ann
  in comp <$>
       (liftA Just (annotation <* semicolon)) <*>
       element_list <*>
       composition_list <*>
       (optionMaybe composition_external)

composition :: Parser Composition
composition = do
  opt <- getState
  if Set.member PermitAnnotationFirst opt
     then composition_annotation_first <|> composition_annotation_last
     else composition_annotation_last


class_specifier :: Parser ClassSpecifier
class_specifier =
  class_specifier_extends
  <|> try class_specifier_a
  <|> try class_specifier_enum
  <|> try class_specifier_der
  <|> class_specifier_end

class_specifier_a :: Parser ClassSpecifier
class_specifier_a = ClassSpecifierA <$>
  ident <*>
  (assign *> base_prefix) <*>
  name <*>
  (optionMaybe array_subscripts) <*>
  (optionMaybe class_modification) <*>
  comment

class_specifier_enum :: Parser ClassSpecifier
class_specifier_enum = liftA3 ClassSpecifierEnum
  ident
  (assign *> enumeration_ *> enum_list_or_colon)
  comment

class_specifier_der :: Parser ClassSpecifier
class_specifier_der = liftA3 ClassSpecifierDer
  ident
  (assign *> der_ *> parens p)
  comment
  where p = liftA3 (,,) name (comma *> ident) (commaList ident)

class_specifier_end :: Parser ClassSpecifier
class_specifier_end = ClassSpecifierEnd <$>
  ident <*>
  string_comment <*>
  composition <*>
  (end_ *> ident) 

class_specifier_extends :: Parser ClassSpecifier
class_specifier_extends = ClassSpecifierExtends <$>
  (extends_ *> ident) <*>
  (optionMaybe class_modification) <*>
  string_comment <*>
  composition <*>
  (end_ *> ident)

class_definition :: Parser ClassDefinition
class_definition = liftA3 ClassDefinition
  (optionMaybe encapsulated_)
  class_prefixes
  class_specifier

-- | Main entry point for parsing
stored_definition :: Parser StoredDefinition
stored_definition = liftA2 StoredDefinition
  (optionMaybe (within_ *> optionMaybe name <* semicolon)) (many p) 
  where p = liftA2 (,) (optionMaybe final_) (class_definition <* semicolon)
