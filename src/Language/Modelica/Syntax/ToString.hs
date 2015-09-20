

module Language.Modelica.Syntax.ToString where

import System.IO (nativeNewline, Newline(LF, CRLF))

import qualified Data.List as List


newline :: String
newline = case nativeNewline of
               LF -> "\n"
               CRLF -> "\r\n"

class ToString a where
      toString :: a -> String

instance ToString a => ToString (Maybe a) where
         toString (Just e) = toString e
         toString Nothing = ""

instance (ToString a, ToString b) => ToString (Either a b) where
         toString (Left x) = toString x
         toString (Right x) = toString x

instance (ToString a, ToString b) => ToString (a, b) where
         toString (x, y) = toString x ++ " " ++ toString y


maybeSpace :: (ToString a) => Maybe a -> String
maybeSpace Nothing = ""
maybeSpace (Just x) = toString x ++ " "

spaceMaybe ::  (ToString a) => Maybe a -> String
spaceMaybe Nothing = ""
spaceMaybe (Just x) = " " ++ toString x

listSpace :: (ToString a) => [a] -> String
listSpace xs = toCommaList xs ++ " "

spaceList :: (ToString a) => [a] -> String
spaceList xs = " " ++ toCommaList xs

toSemiListSpace :: (ToString a) => [a] -> String
toSemiListSpace = (" " ++) . concatMap ((++ "; ") . toString)

toSemiList :: (ToString a) => [a] -> String
toSemiList [] = ""
toSemiList xs =
  List.intercalate "; " (map toString xs) ++ ";"

toCommaList :: (ToString a) => [a] -> String
toCommaList = List.intercalate ", " . map toString
