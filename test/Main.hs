

module Main where


import qualified Language.Modelica.Test.Lexer as Lexer
import qualified Language.Modelica.Test.Expression as Expr
import qualified Language.Modelica.Test.Modification as Mod
import qualified Language.Modelica.Test.Basic as Basic
import qualified Language.Modelica.Test.ClassDefinition as ClassDef
import qualified Language.Modelica.Test.Equation as Equation
import qualified Language.Modelica.Test.ComponentClause as CompClause
import qualified Language.Modelica.Test.Programme as Prog

import qualified Language.Modelica.Test.Utility as Utility

import Control.Monad (when)

main :: IO ()
main = do

  Utility.banner "Lexer"
  as <- Lexer.test

  Utility.banner "Basic"
  bs <- Basic.test

  Utility.banner "Expression"
  cs <- Expr.test

  Utility.banner "Modification"
  ds <- Mod.test

  Utility.banner "Equation"
  es <- Equation.test

  Utility.banner "Component Clause"
  fs <- CompClause.test

  Utility.banner "Class Definition"
  gs <- ClassDef.test

  Utility.banner "Comment"
  hs <- Prog.test

  Utility.banner  "QuickCheck tests"
  i <- Prog.runTests

  let tests = as ++ bs ++ cs ++ ds ++ es ++ fs ++ gs ++ hs ++ [i]
      testsLen = length tests
      passedLen = length $ filter (== True) tests

  Utility.banner "Final Result"

  putStrLn $ "Have we successfully passed all " ++ (show testsLen) ++ " tests? "
  when (testsLen /= passedLen)
       (putStrLn $ "Passed only " ++ show passedLen ++ " tests!")
  Utility.printBoolOrExit (and tests)


  putStrLn ""
