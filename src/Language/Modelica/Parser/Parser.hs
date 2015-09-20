

module Language.Modelica.Parser.Parser where

import Language.Modelica.Parser.Option

import qualified Text.Parsec.Prim as Prim

type Parser = Prim.Parsec String OptionSet
