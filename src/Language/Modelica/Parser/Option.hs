

module Language.Modelica.Parser.Option where


import qualified Data.Set as Set
import Data.Set (Set)

data Option =
  PermitAnnotationFirst -- ^ Usefull for files generated with <https://openmodelica.org/>
  deriving (Show, Eq, Ord)



type OptionSet = Set Option

defaultOptions :: OptionSet
defaultOptions = Set.empty
