{-# LANGUAGE FlexibleInstances #-}

module Language.Modelica.Syntax.Programme where

import Language.Modelica.Syntax.ToString (toString, ToString, newline)


data TextSegment = Str String
                 | LineComment String
                 | BlockComment String
                 | Code String
                 deriving (Show, Eq)


instance ToString TextSegment where
  toString (Str str) = "\"" ++ str ++ "\""
  toString (LineComment str) = "//" ++ str ++ newline
  toString (BlockComment str) = "/*" ++ str ++ "*/"
  toString (Code str) = str

instance ToString [TextSegment] where
  toString = concatMap toString