

module Language.Modelica.Test.ClassDefinition (test) where

import qualified Language.Modelica.Parser.ClassDefinition as CD
import Language.Modelica.Parser.Option (Option(PermitAnnotationFirst))

import Language.Modelica.Test.Utility (testFunc, testFuncWithOpts, testFuncTwoParsers)

import qualified Data.Set as Set



test :: IO [Bool]
test = do

  res1 <- mapM (testFunc CD.import_list) $
    "x" :
    "x, y" : []

  res2 <- mapM (testFunc CD.import_clause) $
    "import x = .f.g" :
    "import x = .f.g \"bla\"" :
    "import x = .f.g \"bla\" + \"blub\"" :
    "import x" :
    "import x.*" :
    "import x.y.*" :
    "import x . y . *" :
    "import x.y.{a, b}" :
    "import x . y . {   a, b}" :
    []

  res3 <- mapM (testFunc CD.element) $
    "import x" :
    "extends bla" :
    "redeclare outer flow input x x, u, w" :
    "outer flow input x[:, 7.0] x, u, w" :
    "x[:, :, 7.0] x, u[true, a + b] = 4.5, w = if true then false else x + y" :
    "replaceable x y constrainedby blub \"bla\" + \"bla\"" :
    "replaceable x.y u, v" :
    "inner outer replaceable flow output x.y u[:, :], v" :
    "inner outer replaceable encapsulated partial class x end x" :
    []

  res4 <- mapM (testFunc CD.element_list) $
    "" :
    "import x;" :
    "import x; extends bla;" :
    "import x; extends bla; redeclare x w;" :
    []

  res5 <- mapM (testFunc CD.external_function_call) $
    "f()" :
    ".f = f()" :
    "x.f = f(1.0, 2.0)" :
    "f(true, false)" :
    []

  res6 <- mapM (testFunc CD.composition_list) $
    "" : 
    "public " : 
    "protected " :
    "public public " :
    "public import x; import t; protected import y; protected import z; " : []


  res7 <- mapM (testFunc CD.composition_external) $
    "external;" :
    "external \"bla\";" :
    "external f();" :
    "external \"bla\" annotation ();" :
    "external \"bla\" f(true, 1.0) annotation (each final replaceable class con = enumeration ());" : []

  res8 <- mapM (testFunc CD.composition) $
    "" :
    "import x;" :
    "annotation ();" :
    "import x; import y; public import x; protected import t; external \"blub\"; annotation ();" : []

  res9 <- mapM (testFunc CD.class_specifier) $
    "extends x \"bla\" + \"blub\" import m1; import m2; end y" :
    "extends x \"bla\" + \"blub\" end y" :
    "x \"bla\" + \"blub\" import m1; extends m2.m3; end y" :
    "y = flow input l.m.n  \"bla\" + \"blub\" annotation ()" :
    "y = enumeration(:) \"bla\"" :
    "y = enumeration() \"bla\"" :
    "y = enumeration(x \"bla\", y)" :
    "y = der(x, x)" :
    "y = der(x.y, f)" :
    "y = der(x.y, f, x)" :
    "y = der(x.y, f, x) \"bla\" + \"blub\"" : []

  res10 <- mapM (testFunc CD.class_definition) $
    "partial model x \"bla\" + \"blub\" import m1; extends m2.m3; end y" :
    "class extends x end x" :
    "encapsulated operator extends x \"bla\" end x" : []

  res11 <- mapM (testFunc CD.stored_definition) $
    "" :
    "within;" : 
    "within x.f;" :
    "class extends x end x;" :
    "within x; class extends x end x;" :
    "within x; class extends x end x; class extends y end y;" :

    "within x; final class extends x end x;" :
    "model M Real r; annotation (); end M;" :
    "model M annotation (); end M;" : []


  res12 <- mapM (testFuncTwoParsers CD.composition_annotation_first CD.composition) $
    "annotation (); Real v;" :
    "annotation (); import x;" :
    "annotation (); import x; import y; public import x; protected import t; external \"blub\";" :

    []

  let opts = Set.singleton PermitAnnotationFirst

  res13 <- mapM (testFuncWithOpts opts CD.composition) $
    "annotation (); import x;" :
    "import x; annotation ();" :
    []

  return $ res1 ++ res2 ++ res3 ++ res4 ++ res5 ++ res6
                ++ res7 ++ res8 ++ res9 ++ res10 ++ res11 ++ res12 ++ res13
