

module Language.Modelica.Test.Expression (test) where

import qualified Language.Modelica.Parser.Expression as Expr

import Language.Modelica.Test.Utility (testFunc)


test :: IO [Bool]
test = do

  res1 <- mapM (testFunc Expr.expression) $
    "true <= (false * (5.0^(, , ))^(, \"bla\", 2.0))" :
    "3.0 : 7.0 + 8.0 : 9.0" :
    "(3.0 : 7.0) + 8.0 : 9.0" :
    "3.0 : 7.0 + (8.0 : 9.0)" :
    "(3.0 : 7.0) + (8.0 : 9.0)" :
    "3.0 : (7.0 + 8.0) : 9.0" :
    "(3.0 : 7.0 + 8.0) : 9.0" :
    "3.0 : (7.0 + 8.0 : 9.0)" :
    "(3.0 : 7.0 + 8.0 : 9.0)" :
    "(3.0) : (7.0) + (8.0 : 9.0)" :
    "[true, .bla[x]; false]" :
    "()" :
    "{x, y}" :
    "{x = 9.0, y = 9.0}" :
    "f(9.0)" :
    "f(9.0, g(), x)" :
    "if true then x else y" :
    "if true then x elseif false then z else y" :
    "if true then x elseif false then z elseif x < y then 7.0 else y" :
    "x.y" :
    "x   .y" :
    "x. y" :
    "x   . y" :
    "3 + 4" :
    "3 .+ 4" :
    "x + y" :
    "x .+ y" :
    "points[m:end] + (x1-x0)" :
    "end" : []

  res2 <- mapM (testFunc Expr.function_arguments) $
    "x, y for x in 9.0" :
    "function x()" :
    "function x(), function y()" :
    "x, function y()" :
    "function x() for bla in 1, blub in true" :
    "x, y = 7" :
    "function x.y(), 9+4, x = 8" :
    "function x.y(), 9+4 for x in 8" :
    "x, y, a for y, x in (-9.0 + (-a)), a" :
    "x, y" :
    "true, \"BlaBlub\", level = Error.level" :
    "engineFile = \"maps/engine/base.txt\"" :
    "bla = \"äüöß\"" :
    "x = 9.0, y = 9.0" :
    "x = function b()" :
    "x = function b(), y = 7" : []

  res3 <- mapM (testFunc Expr.for_indices) $
    "bla in 3.0, blub, x in (3.0, 3.0, (, , ), , )" : []

  res4 <- mapM (testFunc Expr.output_expression_list) $
    "(, (, , , ), )" : 
    ",,," :
    "x,,x,," : []

  res5 <- mapM (testFunc Expr.named_arguments) $
    "bla = blub" :
    "x = y, y = z" :
    "x = y, y = z, z = x" : []

  res7 <- mapM (testFunc Expr.expression) $
    "not 5" :
    "not true :5" :
    "true : 6   :3" :
    "true or (6   : 3 and 7)" :
    "(.bla[x.x,:], x.y,,).^ 4.e-3  " :
    "(b0[1:n] - b0[na]/a[na]*a[1:n])*x + b0[na]/a[na]*u" : []

  res8 <- mapM (testFunc Expr.function_argument) $
    "function b.x()" :
    "function b(x = -0)" :
    "x + y" : []

  res9 <- mapM (testFunc Expr.expression_list) $
    "(, ( ,,), 3 , 3.0   )" :
    "a,  b" : []

  res10 <- mapM (testFunc Expr.array_subscripts) $
    "[ 3^2, true  : false : 3, 4, :  , :,:, 3:3 ]  " : []

  res11 <- mapM (testFunc Expr.component_reference) $
    ".bla.dfs[3,: ,:].ads[ :,3 : true].x" : []

  res12 <- mapM (testFunc Expr.named_arguments) $
    "bla = blub, x = .x[(,,)]" :
    "y = function f(x =9)" :
    "y = function .f(x= function g.x(a=7, b=-4.0)), z = -1.0" : []


  res13 <- mapM (testFunc Expr.name) $
    "bla" :
    ".a.b.c" : []

  res14 <- mapM (testFunc Expr.primary) $
    "3" :
    "\"bla\"" :
    "\"äöüß\"" :
    "false" :
    "true" :
    "der(1)" :
    "f(3)" :
    "points[m:end]" :
    "cat(1, {0}, points[m:end] .+ (x1-x0), {1})" :
    []

  return $ concat [res1, res2, res3, res4, res5, res7, res8, res9, res10, res11, res12, res13, res14]
