

module Language.Modelica.Parser.Equation where

import Language.Modelica.Syntax.Modelica

import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Lexer
import Language.Modelica.Parser.Expression
import Language.Modelica.Parser.Modification
import Language.Modelica.Parser.Utility (eitherOr)

import Control.Applicative
  (liftA2, liftA3, (*>), (<$>), (<*>), (<*))

import Text.ParserCombinators.Parsec ((<|>), many, optionMaybe, try)



equations :: Parser [Equation]
equations = many (try equation <* semicolon)

equation_section :: Parser EquationSection
equation_section = liftA2 EquationSection
  (optionMaybe init_) (equation_ *> equations)

equation :: Parser Equation
equation = liftA2 Equation
  ( if_equation
    <|> for_equation
    <|> connect_clause
    <|> when_equation
    <|> try (liftA2 Eqn simple_expression (assign *> expression) )
    <|> liftA2 EqFunctionCall name function_call_args )
  comment

if_equation :: Parser Eqn
if_equation = IfEquation <$>
  (if_ *> expression <* then_) <*>
  equations <*>
  (many else_if_equation) <*>
  (optionMaybe (else_ *> equations) <* end_if_)

else_if_equation :: Parser ElseIfEquation
else_if_equation = liftA2 ElseIfEquation
  (elseif_ *> expression <* then_) equations


for_equation :: Parser Eqn
for_equation = liftA2 ForEquation
  (for_ *> for_indices <* loop_)
  (equations <* end_for_)

when_equation :: Parser Eqn
when_equation = liftA3 WhenEquation
  (when_ *> expression <* then_)
  equations
  (many else_when_equation <* end_when_)

else_when_equation :: Parser ElseWhenEquation
else_when_equation = liftA2 ElseWhenEquation
  (elsewhen_ *> expression <* then_)
  equations

connect_clause :: Parser Eqn
connect_clause = 
  connect_ *> (parens $
  liftA2 ConnectClause component_reference (comma *> component_reference))


statements :: Parser [Statement]
statements = many (try statement <* semicolon)

algorithm_section :: Parser AlgorithmSection
algorithm_section = liftA2 AlgorithmSection
  (optionMaybe init_) (algorithm_ *> statements)

statement :: Parser Statement
statement = liftA2 Statement
  ( break_
    <|> return_
    <|> if_statement
    <|> for_statement
    <|> when_statement
    <|> while_statement
    <|> comp_ref_statement
    <|> output_list_statement )
  comment


if_statement :: Parser Stmt
if_statement = IfStatement <$>
  (if_ *> expression <* then_) <*>
  statements <*>
  (many else_if_statement) <*>
  (optionMaybe (else_ *> statements) <* end_if_)

else_if_statement :: Parser ElseIfStatement
else_if_statement = liftA2 ElseIfStatement
  (elseif_ *> expression <* then_) statements


for_statement :: Parser Stmt
for_statement = liftA2 ForStatement
  (for_ *> for_indices <* loop_)
  (statements <* end_for_)


when_statement :: Parser Stmt
when_statement = liftA3 WhenStatement
  (when_ *> expression <* then_)
  statements
  (many else_when_statement <* end_when_)

else_when_statement :: Parser ElseWhenStatement
else_when_statement = liftA2 ElseWhenStatement
  (elsewhen_ *> expression <* then_)
  statements

while_statement :: Parser Stmt
while_statement = liftA2 WhileStatement
  (while_ *> expression <* loop_)
  (statements <* end_while_)


comp_ref_statement :: Parser Stmt
comp_ref_statement = liftA2 CompRefStatement
  component_reference
  ((colon_assign *> expression) `eitherOr` function_call_args)

output_list_statement :: Parser Stmt
output_list_statement = liftA3 OutputListStatement
  (parens output_expression_list)
  (colon_assign *> component_reference)
  function_call_args
  
