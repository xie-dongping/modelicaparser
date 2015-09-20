
module Language.Modelica.Test.Basic (test) where


import qualified Language.Modelica.Parser.Basic as Basic

import Language.Modelica.Test.Utility (testFunc)



test :: IO [Bool]
test = do
  res0 <- mapM (testFunc Basic.class_prefixes) $
    "class" :
    "model" :
    "partial class" :
    "partial model" :
    "package" :
    "operator" :
    "function" :
    "operator function" :
    "pure function" :
    "impure operator function" :
    "operator record" :
    "record" :
    "connector" :
    "expandable connector" :
    "partial operator record" :
    "partial pure function" : []

  res1 <- mapM (testFunc Basic.type_prefix) $
    "flow parameter input " :
    "constant input " :
    "output " : []


  res2 <- mapM (testFunc Basic.string_comment) $
    "" :
    "\"bla\"" :
    "\"bla\" + \"blub\"" :
    "\"äü*?\" + \"püÄÖ\"" :
    "\"bla\" + \"blub\" + \"hello!\"" : []


  return (res0 ++ res1 ++ res2)