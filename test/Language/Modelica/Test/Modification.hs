

module Language.Modelica.Test.Modification (test) where

import qualified Language.Modelica.Parser.Modification as Mod
import Language.Modelica.Parser.Lexer (whiteSpace)

import Language.Modelica.Test.Utility (testFunc)


import Control.Applicative ((*>))



test :: IO [Bool]
test = do

  res1 <- mapM (testFunc (whiteSpace *> Mod.modification)) $
    " = 3.0" :
    " := a" : []

  res2 <- mapM (testFunc Mod.modification) $
    "()" :
    "(x \"bla\")" :
    "(x \"bla\") = 3.0" :
    "(x \"bla\", x := 8.0) = 3.0" :
    "(x = 8.0 \"bla\")" :
    "(x = 8.0 \"bla\") = 9.7" :
    "(x)" :
    "(each x)" :
    "(each x = 3.0 \"bla\")" :
    "(each final x = 3.0 \"bla\")" :
    "(final x = 3.0 \"bla\")" :
    "(x = 3.0 \"bla\" + \"blub\") = (true, 7.0, , , y)" : 
    "(x = \"blaääß\")" :
    []

  res3 <- mapM (testFunc Mod.element_modification) $
    "x \"bla\"" : []

  res4 <- mapM (testFunc Mod.declaration) $
    "x" :
    "x[:, :, :]" :
    "x[:, :, :]()" :
    "x[:, :, :](final x := 4.0) = x" : []

  res5 <- mapM (testFunc Mod.enumeration_literal) $
    "x" :
    "x \"bla\"" : 
    "x \"bla\" + \"blub\"" : []

  res6 <- mapM (testFunc Mod.enum_list) $
    "x \"bla\", y \"bla\" + \"blub\", c" :
    "x \"bla\" annotation (x = 7.0, z = abc[:, 4.0])" :
    "x \"bla\" annotation (x = 8.0 \"abc\")" : []


  res7 <- mapM (testFunc Mod.annotation) $
    "annotation ()" :
    "annotation (each y = 3.0)" :
    "annotation (each final replaceable class con = enumeration ())" :
    "annotation (redeclare each final class cc = flow parameter output g[4.0, x[(, 1.0)]] (each x = 99.0) \"bla\" + \"blub\")" :
    "annotation (Icon(coordinateSystem(extent={{-100,-100}, {100,100}}),graphics={Rectangle(extent={{-5,-5},{7,7}}, __NameOfVendor_shadow=2)}))" :
    []

  res8 <- mapM (testFunc Mod.short_class_definition) $
    "class con = enumeration ()" :
    "partial expandable connector con = enumeration () \"bla\" + \"blub\"" :
    "partial expandable connector con = enumeration (:) \"bla\" + \"blub\"" :
    "impure function f = enumeration (x, y \"comment\") \"bla\" + \"blub\"" : []

  res9 <- mapM (testFunc Mod.element_replaceable) $
    "replaceable flow constant c x" :
    "replaceable class ccc = enumeration ()" :
    "replaceable pure function f = stream input .g.h \"comment\"" :
    "replaceable operator record rec = enumeration (:) \"bla\" constrainedby .a.b.c (each x = 7.0)" :

    []

  res10 <- mapM (testFunc Mod.enumeration_literal) $
    "x" :
    "x \"bla\"" : 
    "x \"bla\" + \"blub\"" : []

  res11 <- mapM (testFunc Mod.enum_list) $
    "x \"bla\", y \"bla\" + \"blub\", c" :
    "x \"bla\" annotation (x = 7.0, z = abc[:, 4.0])" :
    "x \"bla\" annotation (x = 8.0 \"abc\")" : []

  res12 <- mapM (testFunc Mod.extends_clause) $
    "extends blub" :
    "extends blub.z" :
    "extends .blub.z (each z := 0.0)" :
    "extends .blub.z (z) annotation (each y = false)" :
    "extends .blub.k.z (z) annotation (each y = false \"bla\" + \"blub\")" : []



  return $ res1 ++ res2 ++ res3 ++ res4 ++ res5 ++ res6 ++ res7 ++ res8 ++ res9 ++ res10 ++ res11 ++ res12

