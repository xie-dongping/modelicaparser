

module Language.Modelica.Test.Equation (test) where

import qualified Language.Modelica.Parser.Equation as Eq

import Language.Modelica.Test.Utility (testFunc)


test :: IO [Bool]
test = do
  res0 <- mapM (testFunc Eq.connect_clause) $
    "connect(x, y)" : 
    "connect(a[1.0], .b.x)" : []

  res1 <- mapM (testFunc Eq.equation_section) $
    "equation" :
    "initial equation" :
    "equation x = 12.9;" :
    "equation x = 1.0; y = true and false; z = .x[(, 7.7)];" :
    "equation f(x, y for x in 9.0);" :
    "equation assert (true, \"Bla\", level = Error.level);" :
    "equation assert (x, y for x in [1, 2, 3]);" :
    "initial equation true = not false \"bla\" + \"blub\";" :
    []

  res2 <- mapM (testFunc Eq.if_equation) $
    "if true then end if" :
    "if true then else end if" :
    "if true then x = 1.0 \"comment\"; end if" :
    "if true then elseif false then end if" :
    "if true then x = 2.0; elseif false then y = 8.0; end if" :
    "if true then x = 3.0; elseif false then end if" :
    "if true then x = 4.0; elseif false then elseif false then end if" :
    "if true then x = 5.0; elseif false then else end if" :
    "if true then elseif false then elseif true then else end if" :
    "if true then x = 6.0; elseif false then y = 8.0; else z = 9.0; end if" :
    "if true then x = 6.0; elseif false then y = 8.0; z = 1.2; elseif x == y then z = 1.5; else z = 9.0; end if" : []

  res3 <- mapM (testFunc Eq.for_equation) $
    "for y in true loop end for" :
    "for y in true, x loop x = y; end for" :
    "for x, y loop x = 7.0; y = 8.0; end for" : []

  res4 <- mapM (testFunc Eq.when_equation) $
    "when not x == true then end when" :
    "when truebla then elsewhen falseblub then elsewhen 7.0 then end when" :
    "when not false then elsewhen not true then x = 0.0; y = 1.0; end when" : []

  res5 <- mapM (testFunc Eq.algorithm_section) $
    "algorithm break;" :
    "algorithm return \"bla\";" :
    "initial algorithm break \"bla\" + \"blub\";" :
    "initial algorithm for x in y loop break; end for;" :
    "algorithm when true then elsewhen false then return; end when;" :
    "algorithm while 1.0 <= 2.0 loop break \"bla\"; return \"blub\"; end while;" :
    "algorithm .a.b[1.0] := 15.0;" :
    "algorithm a.b(bla = true, x = .x[(, , )]);" :
    "algorithm a.b(bla = function f(x = 1.0));" :
    "initial algorithm a.b(12.0, function f(x = function g(y = 1.99)));" :
    "algorithm () := a();" :
    "algorithm (x, y) := .a.x[3.0, :, :](x = 7.0, y = function f(z = 8.99)) \"comment\";" :
    "algorithm assert(1, 2, 3);" :
    "algorithm assert(1, 2, 3); break; return; if true then f(1); else f(2); end if; break;" :
    "algorithm if a then break; else break; end if; when x then break; end when;" :

    []

  res6 <- mapM (testFunc Eq.if_statement) $
    "if true then break \"bla\"; end if" :
    "if true then break; elseif false then return \"bla\"; end if" :
    "if x>x0 then m:=size(points, 1); elseif x<x0 then return; end if" : []

  res7 <- mapM (testFunc Eq.while_statement) $
    "while true loop break; end while" :
    "while 1.0 <= 2.0 loop break; return; end while" :
    "while true loop m:=m+1; end while" : []


  res8 <- mapM (testFunc Eq.when_statement) $
    "when acceptedStep then if x>x0 then m:=size(points, 1); elseif x<x0 then while true loop m:=m+1; end while; end if; x0:=x; end when" : []

  res9 <- mapM (testFunc Eq.statement) $
    "values:=cat(1, {in0}, values[1:m], {interpolate(points, values,1-(x-x0))})" :
    "points:=cat(1, {0}, points[m:end] .+ (x1-x0), {1})" : []

  return (res0 ++ res1 ++ res2 ++ res3 ++ res4 ++ res5 ++ res6 ++ res7 ++ res8 ++ res9)