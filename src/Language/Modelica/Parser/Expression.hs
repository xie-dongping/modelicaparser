
module Language.Modelica.Parser.Expression where


import Language.Modelica.Syntax.Modelica

import Language.Modelica.Parser.Lexer
import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Utility (followedBy, (~+), (~*))


import Control.Applicative
  (liftA, liftA2, liftA3, (*>), (<*>), (<$>))

import Text.ParserCombinators.Parsec
  ((<|>), (<?>), optionMaybe, sepBy, many, try, getPosition)


expression :: Parser Expression
expression =
  if_expression
  <|> liftA Expression simple_expression

if_expression :: Parser Expression
if_expression = IfExpression <$>
  (if_ *> expression) <*>
  (then_ *> expression) <*>
  (many else_if_expression) <*>
  (else_ *> expression)

else_if_expression :: Parser ElseIfExpression
else_if_expression = liftA2 ElseIfExpression
  (elseif_ *> expression)
  (then_ *> expression)

-- Is this order of parsing efficient?
simple_expression :: Parser SimpleExpression
simple_expression =
  try (liftA3 SimpleExpression3 logical_expression logexpr logexpr)
  <|> try (liftA2 SimpleExpression2 logical_expression logexpr)
  <|> liftA SimpleExpression1 logical_expression
  where logexpr = colon *> logical_expression



logical_expression :: Parser LogicalExpression
logical_expression = liftA2 LogicalExpression
  logical_term (or_ `followedBy` logical_term)


logical_term :: Parser LogicalTerm
logical_term = liftA2 LogicalTerm
  logical_factor (and_ `followedBy` logical_factor)

logical_factor :: Parser LogicalFactor
logical_factor = liftA2 LogicalFactor
  (optionMaybe not_) relation

relation :: Parser Relation
relation = liftA2 Relation
  arithmetic_expression
  (optionMaybe (rel_op ~+ arithmetic_expression))

arithmetic_expression :: Parser ArithmeticExpression
arithmetic_expression = liftA3 ArithmeticExpression
  (optionMaybe add_op) term (add_op ~* term)

term :: Parser Term
term = liftA2 Term factor (mul_op ~* factor)

factor :: Parser Factor
factor = liftA2 Factor
  primary (optionMaybe (pot_op ~+ primary))

primary :: Parser Primary
primary = do
  liftA2 PUnsignedNumber getPosition (try unsigned_number)
  <|> liftA2 PModelicaStr getPosition (try unicode_string)
  <|> liftA2 PBoolValue getPosition (false_ <|> true_)
  <|> try (liftA2 PFuncCall din function_call_args)
  <|> liftA PCompRef (try component_reference)
  <|> primary_output_expression_list
  <|> primary_expression_list
  <|> liftA PFuncArgs (braces function_arguments)
  <|> end_ *> return PEnd

din :: Parser DIN
din = der_ <|> initial_ <|> liftA FuncCallName (try name)

primary_output_expression_list :: Parser Primary
primary_output_expression_list = parens $
  liftA POutputExprList output_expression_list

primary_expression_list :: Parser Primary
primary_expression_list = brackets $ liftA2 PExpressionList
  expression_list (semiList expression_list)

component_reference :: Parser ComponentReference
component_reference = ComponentReference <$>
  (optionMaybe dot) <*>
  ident <*>
  (optionMaybe array_subscripts) <*>
  (dotList (ident ~+ optionMaybe array_subscripts))

named_arguments :: Parser NamedArguments
named_arguments =
  liftA2 NamedArguments named_argument (commaList named_argument)

named_argument :: Parser NamedArgument
named_argument =
  liftA2 NamedArgument ident (assign *> function_argument)

function_call_args :: Parser FunctionCallArgs
function_call_args = parens $
  liftA FunctionCallArgs (optionMaybe function_arguments)

function_arguments :: Parser FunctionArguments
function_arguments =
  liftA FANamedArguments (try named_arguments)
  <|>
  liftA2 FunctionArguments
         function_argument
         (optionMaybe $
           liftA Left (comma *> function_arguments)
           <|> liftA Right (for_ *> for_indices))

function_argument :: Parser FunctionArgument
function_argument =
  function_ *> liftA2 Function name (parens (optionMaybe named_arguments))
  <|> liftA FAExpression expression

output_expression_list :: Parser OutputExpressionList
output_expression_list = liftA OutputExpressionList $
  (optionMaybe expression) `sepBy` comma

expression_list :: Parser ExpressionList
expression_list =
  liftA2 ExpressionList expression (commaList expression)

-- try because (dot *> star) could be allowed sometimes (import list)
name :: Parser Name
name = liftA3 Name (optionMaybe dot) ident (many (dot *> ident))

for_indices :: Parser ForIndices
for_indices = liftA2 ForIndices
  for_index (commaList for_index)

for_index :: Parser ForIndex
for_index = liftA2 ForIndex
  ident (optionMaybe (in_ *> expression))


array_subscripts :: Parser ArraySubscripts
array_subscripts = brackets $ liftA2 ArraySubscripts
  subscript (commaList subscript)
  

subscript :: Parser Subscript
subscript =
  (colon *> return SubscriptColon)
  <|> liftA Subscript expression
  <?> "subscript"
