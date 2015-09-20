

module Main where

import Language.Modelica.Syntax.ToString (toString)

import Language.Modelica.Parser.Option
  (OptionSet, defaultOptions, Option(PermitAnnotationFirst))

import Language.Modelica.Parser (withOptions)

import Language.Modelica.Test.Utility
  (highlight, printBoolOrExit)
import Language.Modelica.Test.Paths_Main2 (getDataFileName)

import qualified Data.Set as Set
import System.FilePath.Posix ((</>))

import Control.Monad (void)

testFile :: FilePath -> IO ()
testFile = testFileOpts defaultOptions

testFileOpts :: OptionSet -> FilePath -> IO ()
testFileOpts opts file = void $ do
  txt <- readFile file
  highlight file
  putStrLn $ "\n" ++ replicate (length file) '='

  let a = withOptions opts file txt
      b = withOptions opts (file ++ ", 2nd time") $ toString a

  putStrLn $ txt
  putStrLn $ "fst round:\t" ++ toString a
  putStrLn $ "snd round:\t" ++ toString b

  printBoolOrExit (toString a == toString b)




files :: [String]
files =
  ("test" </> "tests" </> "test.mo") :
  ("test" </> "tests" </> "test2.mo") :
  ("test" </> "tests" </> "test3.mo") :
  ("test" </> "tests" </> "test4.mo") :
  ("test" </> "tests" </> "capacitator.mo") :
  ("test" </> "tests" </> "connector.mo") :
  ("test" </> "tests" </> "ThrowingBall.mo") :
  ("test" </> "tests" </> "M.mo") :
  ("test" </> "tests" </> "spatialDistribution.mo") :
  ("test" </> "tests" </> "caf2.mo") :
  []


files_comment :: [String]
files_comment =
  ("test" </> "tests" </> "comment.mo") :
  ("test" </> "tests" </> "comment2.mo") :
  []

files_caf :: [String]
files_caf =
  ("test" </> "tests" </> "caf1.mo") :
  ("test" </> "tests" </> "caf2.mo") :
  ("test" </> "tests" </> "vehicle.mo") :
  ("test" </> "tests" </> "vehicle_parallelHybrid.mo") :
  []


main :: IO ()
main = do
  mapM_ ((testFile =<<) . getDataFileName) files
  mapM_ ((testFile =<<) . getDataFileName) files_comment

  let paf = Set.singleton PermitAnnotationFirst
  mapM_ ((testFileOpts paf =<<) . getDataFileName) files_caf
