{-# LANGUAGE TemplateHaskell #-}

module Language.Modelica.Test.Programme where


import qualified Language.Modelica.Parser.Programme as Prog
import Language.Modelica.Syntax.ToString (toString)

import Language.Modelica.Parser.Option (defaultOptions)

import Language.Modelica.Test.Utility (testFunc)

--import Language.Modelica.Parser.Parser (Parser)
--import Language.Modelica.Parser.Option (OptionSet)
import Text.Parsec.Prim (runP)

import qualified Test.QuickCheck as QC
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.List as List

test :: IO [Bool]
test = do

  res1 <- mapM (testFunc Prog.modelica_programme) $
    "/* bla  */" :
    "/* bla \n blub */" :
    "/* bla \r\n blub */" :
    "/* bla */    " :
    []


  res2 <- mapM (testFunc Prog.modelica_programme) $
    "// bla blub" :
    "// blub\n" :
    []

  res3 <- mapM (testFunc Prog.modelica_programme) $
    "abc(\"def\")" :
    "\"/**/\"" :
    "// /*\n" :
    "// b" :
    "\"//blub\"" : []

  return $ concat [res1, res2, res3]




-- Spaces before and after are important,
-- because /*/*/* would be the legal block comment "/*/*/"
bcb, bce, lcb, strb, slash, star :: String
bcb = " /* "
bce = " */ "
lcb = " // "
strb = " \" "
slash = " / "
star = " * "

chars :: [String]
chars =
  replicate 2 "\n" ++
  replicate 8 " "  ++
  map (:[]) ['a'..'z']


data TextSegment = Str String
                 | LineComment String
                 | BlockComment String
                 | Code String
                 deriving (Show)

newtype SomeText = SomeText String deriving (Show)

instance QC.Arbitrary SomeText where
  arbitrary = do
    len <- QC.choose (0, 100 :: Int)
    str <- QC.vectorOf len (QC.elements chars)
    return (SomeText $ concat str)

segmentsToString :: [TextSegment] -> String
segmentsToString = concatMap f
  where f (Str str) = "\"" ++ str ++ "\""
        f (LineComment str) = "//" ++ str ++ "\n"
        f (BlockComment str) = "/*" ++ str ++ "*/"
        f (Code str) = str

insert :: String -> Int -> String -> String
insert ts n str = as ++ str ++ bs
  where (as, bs) = splitAt n ts


instance QC.Arbitrary TextSegment where
  arbitrary = do
    seg <- QC.choose (0, 3 :: Int)
    n <- QC.choose (0, 4 :: Int)
    SomeText str <- QC.arbitrary

    let len = length str

        g acc (pos, e) = insert acc pos e

        h 0 = (Str, [bcb, bce, lcb, slash, star])
        h 1 = (LineComment, [bcb, bce, lcb, strb, slash, star])
        h 2 = (BlockComment, [bcb, lcb, strb, slash, star])
        h 3 = (Code, [""])
        h x = error $ "TextSegment: Saw " ++ show x
                      ++ "; no such number!"

        f x = do
          let (constr, lst) = h x
          pos <- QC.vectorOf n (QC.elements [0..len])
          es <- QC.vectorOf n (QC.elements lst)
          return $ case constr (List.foldl' g str (zip pos es)) of
                        LineComment xs -> LineComment (filter (/= '\n') xs)
                        xs -> xs
    f seg

newtype RandomProgramme = RandomProgramme String deriving (Show)

instance QC.Arbitrary RandomProgramme where
  arbitrary = do
    segs <- QC.choose (0, 20 :: Int)
    blocks <- QC.vectorOf segs QC.arbitrary
    let str = segmentsToString blocks
    return (RandomProgramme str)

prop_roundTrip :: RandomProgramme -> Bool
prop_roundTrip (RandomProgramme txt) =
  case runP Prog.modelica_programme defaultOptions "" txt of
       Right res -> toString res == txt
       Left err -> error (show err)

runTests :: IO Bool
runTests = $quickCheckAll
