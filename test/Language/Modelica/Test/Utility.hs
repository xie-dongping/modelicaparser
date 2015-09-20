

module Language.Modelica.Test.Utility where

import Language.Modelica.Syntax.ToString (ToString, toString)

import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Option (defaultOptions, OptionSet)
import Language.Modelica.Parser.Utility (stringParser)

import Text.Parsec.Prim (runP)

import Control.Monad (when)

import System.Exit (exitFailure)




import System.Console.ANSI
  ( Color(Green, Red, Yellow, White),
    SGR(SetColor),
    ConsoleLayer(Foreground),
    ColorIntensity(Vivid, Dull),
    setSGR)


printBool :: Bool -> IO ()
printBool b = do
  let colour = if b then Green else Red
  setSGR [ SetColor Foreground Vivid colour ]
  print b
  setSGR [ SetColor Foreground Dull White ]


printBoolOrExit :: Bool -> IO ()
printBoolOrExit b = do
  let colour = if b then Green else Red
  setSGR [ SetColor Foreground Vivid colour ]
  print b
  setSGR [ SetColor Foreground Dull White ]
  when (not b) exitFailure



highlight :: String -> IO ()
highlight str = do
  setSGR [ SetColor Foreground Vivid Yellow ]
  putStr str
  setSGR [ SetColor Foreground Dull White ]

banner :: String -> IO ()
banner title = do
  putStrLn "\n==========================================================="
  highlight $ "*** " ++ title ++ " ***\n"
  putStrLn "===========================================================\n"




roundTrip :: (ToString a) => Parser a -> String -> String
roundTrip p = toString . testparser (stringParser p)

testFunc :: (ToString a, Eq a, Show a) => Parser a -> String -> IO Bool
testFunc = testFuncWithOpts defaultOptions


testFuncWithOpts ::
  (ToString a, Eq a, Show a) => OptionSet -> Parser a -> String -> IO Bool
testFuncWithOpts opts p str0 = do
  let ast1 = testparserWithOpts opts (stringParser p) str0
      str1 = toString ast1
      ast2 = testparserWithOpts opts (stringParser p) str1
      bool = ast1 == ast2
  putStrLn $
    str0 ++ "\n\n"
    ++ "   " ++ show ast1 ++ "\n== " ++ show ast2
  printBool bool
  putStrLn "-----------------------------------------------------------"
  return bool




testFuncTwoParsers ::
  (ToString a, Eq a, Show a) => Parser a -> Parser a -> String -> IO Bool
testFuncTwoParsers p0 p1 str0 = do
  let ast1 = testparser (stringParser p0) str0
      str1 = toString ast1
      ast2 = testparser (stringParser p1) str1
      bool = ast1 == ast2

  putStrLn $ str0 ++ "\n\n"
  putStrLn $ "   " ++ show ast1 ++ "\n== " ++ show ast2
  printBool bool
  putStrLn "-----------------------------------------------------------"
  return bool

testparserWithOpts :: OptionSet -> Parser a -> String -> a
testparserWithOpts opts p str =
  case runP p opts "(testparser)" str of
       Left err -> error $ show err
       Right t -> t


testparser :: Parser a -> String -> a
testparser = testparserWithOpts defaultOptions
