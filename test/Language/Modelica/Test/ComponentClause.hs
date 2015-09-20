

module Language.Modelica.Test.ComponentClause (test) where

import qualified Language.Modelica.Parser.ComponentClause as CC

import Language.Modelica.Test.Utility (testFunc)



test :: IO [Bool]
test = do
  res1 <- mapM (testFunc CC.condition_attribute) $
    "if 7.0 + x" :
    "if (7.0 + true)" : []

  res2 <- mapM (testFunc CC.component_declaration) $
    "x" : 
    "x[:, :]()" :
    "x[:, 4.0](x := 6.0)" :
    "x[:, 4.0](x := false .* 8.0) = true" :
    "x[:, 4.0](x := 6.0) = f(7.0 == x + 8.0)" :
    "x[:, 4.0]() = true if (7.0 == (false))" :
    "x[:, 4.0]() = true if if (7.0 == (false)) then true else false" :
    "x[:, 4.0]() = true if if if (x + y == (false)) then true else false then true else false" :
    "x[:, :]() = 7.9 \"bla\" + \"blub\"" :
    []

  res3 <- mapM (testFunc CC.component_list) $
    "x" :
    "x, y" : 
    "x[:, 4.0, :](x := 6.0), x[4.0]()" :
    "x[:, 4.0, :](x := 6.0), x[false](), y" :
    []

  res4 <- mapM (testFunc CC.component_clause) $
    "flow parameter bla x" :
    "stream constant output bla x, y" :
    "stream constant output bla x, y" :
    "stream constant output bla[:, 5.0] x, y" :
    "Engine engine1(parMaxTorque = 170, engineFile = \"maps/engiääne/base.txt\") annotation(Placement(visible = true, transformation(origin = {-72.9833,-44.9117})))" :
    []

  return $ res1 ++ res2 ++ res3 ++ res4
