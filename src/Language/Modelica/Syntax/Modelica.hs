{-# LANGUAGE FlexibleInstances #-}

module Language.Modelica.Syntax.Modelica where


import Language.Modelica.Syntax.ToString
  ( ToString, toString, spaceMaybe, maybeSpace,
    toCommaList, toSemiList, toSemiListSpace )

import Text.Parsec.Pos (SourcePos)

import qualified Data.List as List

import Text.Printf (printf)

import Data.Char (toLower)

--------------------------------------------------------------

type EitherOrOr a b c = Either a (Either b c)

--------------------------------------------------------------

type TypeSpecifier = Name

--------------------------------------------------------------

data Ident = Ident SourcePos String
           | QIdent SourcePos String deriving (Show)

instance Eq Ident where
  (Ident _ a) == (Ident _ b) = a == b
  (QIdent _ a) == (QIdent _ b) = a == b
  _ == _ = False

instance ToString Ident where
         toString (Ident _pos str) = str
         toString (QIdent _pos str) = "'" ++ str ++ "'"

--------------------------------------------------------------

data Dot = Dot deriving (Show, Eq)

instance ToString Dot where
         toString = const "."


data Star = Star deriving (Show, Eq)

instance ToString Star where
         toString = const "*"


data Colon = Colon deriving (Show, Eq)
instance ToString Colon where
         toString = const ":"


data Each = Each deriving (Show, Eq)

instance ToString Each where
         toString = const "each"


data Final = Final deriving (Show, Eq)

instance ToString Final where
         toString = const "final"


data Partial = Partial deriving (Show, Eq)

instance ToString Partial where
         toString = const "partial"


data Redeclare = Redeclare deriving (Show, Eq)

instance ToString Redeclare where
         toString = const "redeclare"


data Inner = Inner deriving (Show, Eq)

instance ToString Inner where
         toString = const "inner"


data Outer = Outer deriving (Show, Eq)

instance ToString Outer where
         toString = const "outer"


data Init = Init deriving (Show, Eq)

instance ToString Init where
         toString = const "initial"

data Encapsulated = Encapsulated deriving (Show, Eq)

instance ToString Encapsulated where
         toString = const "encapsulated"

--------------------------------------------------------------



data Name = Name (Maybe Dot) Ident [Ident] deriving (Show, Eq)

instance ToString Name where
  toString (Name dot i is) =
    toString dot ++ List.intercalate "." (map toString (i:is))

--------------------------------------------------------------

data Modification =
  Modification ClassModification (Maybe Expression)
  | ModificationAssign Expression
  | ModificationColonAssign Expression
  deriving (Show, Eq)

instance ToString Modification where
         toString (Modification cm ex) =
           toString cm ++ maybe "" ((" = " ++) . toString) ex
         toString (ModificationAssign ex) = " = " ++ toString ex
         toString (ModificationColonAssign as) = " := " ++ toString as


--------------------------------------------------------------


data ClassModification = ClassModification (Maybe ArgumentList) deriving (Show, Eq)

instance ToString ClassModification where
         toString (ClassModification al) = "(" ++ toString al ++ ")"

---------------------------------------------------------------

data ArgumentList =
  ArgumentList Argument [Argument] deriving (Show, Eq)

instance ToString ArgumentList where
         toString (ArgumentList e es) = toCommaList (e:es)

--------------------------------------------------------------

data Argument = 
  ArgElementModOrRep ElementModOrRep
  | ArgElementRedeclaration ElementRedeclaration deriving (Show, Eq)

instance ToString Argument where
         toString (ArgElementModOrRep emor) = toString emor
         toString (ArgElementRedeclaration er) = toString er

--------------------------------------------------------------


data ElementModOrRep =
  ElementModOrRep
    (Maybe Each)
    (Maybe Final)
    (Either ElementModification ElementReplaceableShort) deriving (Show, Eq)

instance ToString ElementModOrRep where
         toString (ElementModOrRep each final x) =
           maybeSpace each ++ maybeSpace final ++ toString x

--------------------------------------------------------------


data ElementModification =
  ElementModification Name (Maybe Modification) (Maybe StringComment)
  deriving (Show, Eq)

instance ToString ElementModification where
         toString (ElementModification n m sc) =
           toString n ++ toString m ++ toString sc

--------------------------------------------------------------

data ElementRedeclaration =
  ElementRedeclaration
    (Maybe Each)
    (Maybe Final)
    (EitherOrOr ShortClassDefinition ComponentClause1 ElementReplaceableShort)
    deriving (Show, Eq)

instance ToString ElementRedeclaration where
         toString (ElementRedeclaration each final expr) =
           "redeclare " ++ maybeSpace each ++ maybeSpace final ++ toString expr

--------------------------------------------------------------

data ElementReplaceableShort =
  ElementReplaceableShort
    (Either ShortClassDefinition ComponentClause1)
    (Maybe ConstrainingClause)
  deriving (Show, Eq)

instance ToString ElementReplaceableShort where
         toString (ElementReplaceableShort oot cc) =
           "replaceable " ++ toString oot ++ spaceMaybe cc

--------------------------------------------------------------

data ShortClassDefinition =
  ShortClassDefinition ClassPrefixes Ident ShortClassDef deriving (Show, Eq)

instance ToString ShortClassDefinition where
         toString (ShortClassDefinition pref i scd) =
           toString pref ++ " " ++ toString i ++ " = " ++ toString scd


data ShortClassDef =
  ShortClassDef1 (Either Colon (Maybe EnumList)) Comment
  | ShortClassDef2
      TypePrefix
      Name 
      (Maybe ArraySubscripts)
      (Maybe ClassModification)
      Comment
  deriving (Show, Eq)

instance ToString ShortClassDef where
         toString (ShortClassDef1 x c) =
           "enumeration (" ++ toString x ++ ")" ++ toString c
         toString (ShortClassDef2 t n as cm c) =
           toString t ++ toString n
             ++ toString as ++ spaceMaybe cm ++ toString c


--------------------------------------------------------------


data ComponentClause1 =
  ComponentClause1 TypePrefix TypeSpecifier ComponentDeclaration1 
  deriving (Show, Eq)

instance ToString ComponentClause1 where
         toString (ComponentClause1 tp n cd) =
           toString tp ++ toString n ++ " " ++ toString cd

--------------------------------------------------------------

data ComponentDeclaration1 =
  ComponentDeclaration1 Declaration Comment deriving (Show, Eq)


instance ToString ComponentDeclaration1 where
         toString (ComponentDeclaration1 d c) = 
           toString d ++ toString c

--------------------------------------------------------------

data ConstrainingClause =
  ConstrainingClause Name (Maybe ClassModification) deriving (Show, Eq)


instance ToString ConstrainingClause where
         toString (ConstrainingClause n cm) =
           "constrainedby " ++ toString n ++ spaceMaybe cm

--------------------------------------------------------------

data ExtendsClause =
  ExtendsClause Name (Maybe ClassModification) (Maybe Annotation) deriving (Show, Eq)


instance ToString ExtendsClause where
         toString (ExtendsClause n cm an) =
           "extends " ++ toString n ++ spaceMaybe cm ++ spaceMaybe an

--------------------------------------------------------------

data Comment = Comment (Maybe StringComment) (Maybe Annotation)
               deriving (Show, Eq)


instance ToString Comment where
         toString (Comment sc an) = toString sc ++ spaceMaybe an


--------------------------------------------------------------

data StringComment = StringComment (Maybe (String, [String])) deriving (Show, Eq)

instance ToString StringComment where
  toString (StringComment Nothing) = ""
  toString (StringComment (Just (s, ss))) =
    " " ++ List.intercalate " + " (map (printf "\"%s\"") (s:ss))

--------------------------------------------------------------

newtype Annotation = Annotation ClassModification deriving (Show, Eq)

instance ToString Annotation where
         toString (Annotation cm) = "annotation " ++ toString cm

--------------------------------------------------------------

data Expression =
  IfExpression Expression Expression [ElseIfExpression] Expression
  | Expression SimpleExpression deriving (Show, Eq)

instance ToString Expression where
  toString (Expression e) = toString e
  toString (IfExpression cond th eie els) =
    "if " ++ toString cond ++ " then " ++ toString th
          ++ concatMap toString eie ++ " else " ++ toString els

--------------------------------------------------------------

data ElseIfExpression = ElseIfExpression Expression Expression deriving (Show, Eq)

instance ToString ElseIfExpression where
  toString (ElseIfExpression e1 e2) =
    " elseif " ++ toString e1 ++ " then " ++ toString e2

--------------------------------------------------------------

data SimpleExpression = 
  SimpleExpression1 LogicalExpression 
  | SimpleExpression2 LogicalExpression LogicalExpression
  | SimpleExpression3 LogicalExpression LogicalExpression LogicalExpression
  deriving (Show, Eq)

instance ToString SimpleExpression where
         toString (SimpleExpression1 le) = toString le
         toString (SimpleExpression2 le1 le2) =
           toString le1 ++ " : " ++ toString le2
         toString (SimpleExpression3 le1 le2 le3) =
           toString le1 ++ " : " ++ toString le2 ++ " : " ++ toString le3


--------------------------------------------------------------

data LogicalExpression =
  LogicalExpression LogicalTerm [LogicalTerm] deriving (Show, Eq)

instance ToString LogicalExpression where
         toString (LogicalExpression lt lts) =
           toString lt ++ concatMap ((" or " ++) . toString) lts


--------------------------------------------------------------

data LogicalTerm =
  LogicalTerm LogicalFactor [LogicalFactor] deriving (Show, Eq)

instance ToString LogicalTerm where
         toString (LogicalTerm lf lfs) =
           toString lf ++ concatMap ((" and " ++) . toString) lfs

--------------------------------------------------------------

data Not = Not deriving (Show, Eq)

data LogicalFactor = LogicalFactor (Maybe Not) Relation deriving (Show, Eq)

instance ToString LogicalFactor where
         toString (LogicalFactor b rel) =
           (++ toString rel) $
             case b of
                  Just Not -> "not "
                  _ -> ""

--------------------------------------------------------------

data Relation =
  Relation ArithmeticExpression (Maybe (RelOp, ArithmeticExpression))
  deriving (Show, Eq)

instance ToString Relation where
         toString (Relation ae ro) = toString ae ++ maybe "" f ro
           where f (r, a) = " " ++ toString r ++ " " ++ toString a

--------------------------------------------------------------

data RelOp = Equal | UnEqual | GTH | LTH | GEQ | LEQ deriving (Show, Eq)

instance ToString RelOp where
         toString Equal = "=="
         toString UnEqual = "<>"
         toString GTH = ">"
         toString LTH = "<"
         toString GEQ = ">="
         toString LEQ = "<="

--------------------------------------------------------------

data ArithmeticExpression =
  ArithmeticExpression (Maybe AddOp) Term [(AddOp, Term)] deriving (Show, Eq)

instance ToString ArithmeticExpression where
         toString (ArithmeticExpression ao t ls) =
           toString ao ++ toString t ++ concatMap f ls
           where f (a, s) = " " ++ toString a ++ " " ++ toString s

--------------------------------------------------------------

data AddOp = Plus | Minus | DotPlus | DotMinus deriving (Show, Eq)

instance ToString AddOp where
         toString Plus = "+"
         toString Minus = "-"
         toString DotPlus = ".+"
         toString DotMinus = ".-"

--------------------------------------------------------------

data Term = Term Factor [(MulOp, Factor)] deriving (Show, Eq)

instance ToString Term where
         toString (Term fac ls) =
           toString fac ++ concatMap f ls
           where f (mo, g) = " " ++ toString mo ++ " " ++ toString g

--------------------------------------------------------------

data MulOp = Mul | Div | DotMul | DotDiv deriving (Show, Eq)

instance ToString MulOp where
         toString Mul = "*"
         toString Div = "/"
         toString DotMul = ".*"
         toString DotDiv = "./"

--------------------------------------------------------------

data Factor = Factor Primary (Maybe (PotOp, Primary)) deriving (Show, Eq)

instance ToString Factor where
         toString (Factor prim m) = toString prim ++ maybe "" f m
           where f (po, p) = toString po ++ toString p
         -- toString (Factor prim m) = toString prim ++ spaceMaybe m

--------------------------------------------------------------

data PotOp = Pot | DotPot deriving (Show, Eq)

instance ToString PotOp where
         toString Pot = "^"
         toString DotPot = ".^"

--------------------------------------------------------------

data DIN = Der | Initial | FuncCallName Name deriving (Show, Eq)

instance ToString DIN where
         toString Der = "der"
         toString Initial = "initial"
         toString (FuncCallName n) = toString n

--------------------------------------------------------------

data Primary =
  PUnsignedNumber SourcePos Double
  | PModelicaStr SourcePos String
  | PBoolValue SourcePos Bool
  | PFuncCall DIN FunctionCallArgs
  | PCompRef ComponentReference
  | PFuncArgs FunctionArguments
  | POutputExprList OutputExpressionList
  | PExpressionList ExpressionList [ExpressionList]
  | PEnd
  deriving (Show)


instance Eq Primary where
  (PUnsignedNumber _ a) == (PUnsignedNumber _ b) = a == b
  (PModelicaStr _ a) == (PModelicaStr _ b) = a == b
  (PBoolValue _ a) == (PBoolValue _ b) = a == b
  (PFuncCall u v) == (PFuncCall x y) = u == x && v == y
  (PCompRef a) == (PCompRef b) = a == b
  (PFuncArgs a) == (PFuncArgs b) = a == b
  (POutputExprList a) == (POutputExprList b) = a == b
  (PExpressionList u v) == (PExpressionList x y) = u == x && v == y
  PEnd == PEnd = True
  _ == _ = False

instance ToString Primary where
         toString (PUnsignedNumber _pos d) = show d
         toString (PModelicaStr _pos str) = printf "\"%s\"" str
         toString (PBoolValue _pos b) = map toLower $ show b
         toString (PFuncCall din args) = toString din ++ toString args
         toString (PCompRef cr) = toString cr
         toString (PFuncArgs pfa) = "{" ++ toString pfa ++ "}"
         toString (POutputExprList oel) = "(" ++ toString oel ++ ")"
         toString (PExpressionList el els) =
           "[" ++ toString el ++ concatMap f els ++ "]"
           where f = ("; " ++) . toString
         toString PEnd = "end"

---------------------------------------------------------------

data NamedArguments =
  NamedArguments NamedArgument [NamedArgument] deriving (Show, Eq)

instance ToString NamedArguments where
         toString (NamedArguments a as) = toCommaList (a:as)

---------------------------------------------------------------

data FunctionCallArgs =
  FunctionCallArgs (Maybe FunctionArguments) deriving (Show, Eq)

instance ToString FunctionCallArgs where
         toString (FunctionCallArgs fca) = "(" ++ toString fca ++ ")"

---------------------------------------------------------------

data FunctionArguments =
  FunctionArguments FunctionArgument (Maybe (Either FunctionArguments ForIndices))
  | FANamedArguments NamedArguments
  deriving (Show, Eq)


instance ToString FunctionArguments where
         toString (FunctionArguments fa rs) =
           toString fa ++ maybe "" f rs
           where f x = case x of
                            Left fargs -> ", " ++ toString fargs
                            Right foridx -> " for " ++ toString foridx
         toString (FANamedArguments nargs) = toString nargs

---------------------------------------------------------------

data FunctionArgument =
  FAExpression Expression
  | Function Name (Maybe NamedArguments) deriving (Show, Eq)

instance ToString FunctionArgument where
         toString (FAExpression ex) = toString ex
         toString (Function name nargs) =
           "function " ++ toString name ++ "(" ++ toString nargs ++ ")"

---------------------------------------------------------------

data NamedArgument = NamedArgument Ident FunctionArgument deriving (Show, Eq)

instance ToString NamedArgument where
         toString (NamedArgument n fa) =
           toString n ++ " = " ++ toString fa

---------------------------------------------------------------

data OutputExpressionList =
  OutputExpressionList [Maybe Expression] deriving (Show, Eq)

instance ToString OutputExpressionList where
         toString (OutputExpressionList exs) = toCommaList exs

---------------------------------------------------------------


data ExpressionList =
  ExpressionList Expression [Expression] deriving (Show, Eq)

instance ToString ExpressionList where
         toString (ExpressionList e es) = toCommaList (e:es)

---------------------------------------------------------------

data Subscript =
     Subscript Expression
     | SubscriptColon deriving (Show, Eq)

instance ToString Subscript where
         toString (Subscript ex) = toString ex
         toString SubscriptColon = ":"

---------------------------------------------------------------

data ArraySubscripts = ArraySubscripts Subscript [Subscript] deriving (Show, Eq)

instance ToString ArraySubscripts where
         toString (ArraySubscripts a as) = "[" ++ toCommaList (a:as) ++ "]"

---------------------------------------------------------------

data ComponentReference =
  ComponentReference (Maybe Dot) Ident (Maybe ArraySubscripts)
                     [(Ident, Maybe ArraySubscripts)] deriving (Show, Eq)

instance ToString ComponentReference where
         toString (ComponentReference dot n as lst) =
           toString dot ++ toString n ++ toString as ++ concatMap f lst
           where f (na, a) = "." ++ toString na ++ toString a


---------------------------------------------------------------

data ForIndices = ForIndices ForIndex [ForIndex] deriving (Show, Eq)

instance ToString ForIndices where
         toString (ForIndices i is) = toCommaList (i:is)

---------------------------------------------------------------

data ForIndex = ForIndex Ident (Maybe Expression) deriving (Show, Eq)

instance ToString ForIndex where
         toString (ForIndex n e) =
           toString n ++ maybe "" ((" in " ++) . toString) e


---------------------------------------------------------------

data TypePrefix =
  TypePrefix (Maybe FS) (Maybe DPC) (Maybe OI) deriving (Show, Eq)

type BasePrefix = TypePrefix

instance ToString TypePrefix where
         toString (TypePrefix fs dpc oi) =
           maybeSpace fs ++ maybeSpace dpc ++ maybeSpace oi

data FS = Flow | Stream deriving (Show, Eq)

instance ToString FS where
         toString = map toLower . show

data DPC = Discrete | Parameter | Constant deriving (Show, Eq)

instance ToString DPC where
         toString = map toLower . show

data OI = Output | Input deriving (Show, Eq)

instance ToString OI where
         toString = map toLower . show


---------------------------------------------------------------

data ClassPrefixes =
  ClassPrefixes (Maybe Partial) Prefix deriving (Show, Eq)

instance ToString ClassPrefixes where
         toString (ClassPrefixes part pre) = maybeSpace part ++ toString pre

---------------------------------------------------------------

data Prefix = Class
            | Model
            | Block
            | Type
            | Package
            | Operator
            | Record (Maybe OperatorRecord)
            | Connector (Maybe Expandable)
            | FunctionPrefix (Maybe PureImpure) (Maybe OperatorFunction)
            deriving (Show, Eq)

instance ToString Prefix where
         toString (FunctionPrefix puim opfunc) =
           maybeSpace puim ++ maybeSpace opfunc ++ "function"
         toString (Record orec) = maybeSpace orec ++ "record"
         toString (Connector ex) = maybeSpace ex ++ "connector"
         toString pref = map toLower (show pref)

---------------------------------------------------------------

data PureImpure = Pure | Impure deriving (Show, Eq)

instance ToString PureImpure where
         toString = map toLower . show

---------------------------------------------------------------

data OperatorFunction = OperatorFunction deriving (Show, Eq)

instance ToString OperatorFunction where
         toString = const "operator"

---------------------------------------------------------------

data OperatorRecord = OperatorRecord deriving (Show, Eq)

instance ToString OperatorRecord where
         toString = const "operator"

---------------------------------------------------------------

data Expandable = Expandable deriving (Show, Eq)

instance ToString Expandable where
         toString = const "expandable"

---------------------------------------------------------------

data Declaration =
  Declaration Ident (Maybe ArraySubscripts) (Maybe Modification) deriving (Show, Eq)

instance ToString Declaration where
         toString (Declaration n as m) =
           toString n ++ toString as ++ toString m

---------------------------------------------------------------

data ConditionAttribute =
  ConditionAttribute Expression deriving (Show, Eq)

instance ToString ConditionAttribute where
  toString (ConditionAttribute expr) = "if " ++ toString expr


---------------------------------------------------------------

data ComponentDeclaration =
  ComponentDeclaration Declaration (Maybe ConditionAttribute) Comment deriving (Show, Eq)


instance ToString ComponentDeclaration where
         toString (ComponentDeclaration decl ifex com) =
           toString decl ++ spaceMaybe ifex ++ toString com

---------------------------------------------------------------

data ComponentList =
  ComponentList ComponentDeclaration [ComponentDeclaration] deriving (Show, Eq)

instance ToString ComponentList where
         toString (ComponentList decl decls) = toString decl ++ concatMap f decls
           where f = (", " ++) . toString

---------------------------------------------------------------

data ComponentClause =
  ComponentClause TypePrefix TypeSpecifier (Maybe ArraySubscripts) ComponentList
  deriving (Show, Eq)

instance ToString ComponentClause where
  toString (ComponentClause tp n as cl) =
    toString tp ++ toString n ++ toString as ++ " " ++ toString cl


---------------------------------------------------------------

data EnumerationLiteral =
  EnumerationLiteral Ident Comment deriving (Show, Eq)

instance ToString EnumerationLiteral where
         toString (EnumerationLiteral i c) =
           toString i ++ toString c

---------------------------------------------------------------

data EnumList =
  EnumList EnumerationLiteral [EnumerationLiteral] deriving (Show, Eq)

instance ToString EnumList where
         toString (EnumList e es) = toCommaList (e:es)

---------------------------------------------------------------

data EquationSection =
  EquationSection (Maybe Init) [Equation] deriving (Show, Eq)

instance ToString EquationSection where
         toString (EquationSection ini []) =
           maybeSpace ini ++ "equation"
         toString (EquationSection ini eqs) =
           maybeSpace ini ++ "equation " ++ toSemiList eqs

---------------------------------------------------------------

data Equation = Equation Eqn Comment deriving (Show, Eq)

instance ToString Equation where
  toString (Equation e c) = toString e ++ toString c

---------------------------------------------------------------

data Eqn =
  IfEquation Expression [Equation] [ElseIfEquation] (Maybe [Equation])
  | ForEquation ForIndices [Equation]
  | WhenEquation Expression [Equation] [ElseWhenEquation]
  | ConnectClause ComponentReference ComponentReference
  | EqFunctionCall Name FunctionCallArgs
  | Eqn SimpleExpression Expression deriving (Show, Eq)

instance ToString Eqn where
  toString (Eqn se e) = toString se ++ " = " ++ toString e

  toString (IfEquation e ieqs eie eeqs) =
    "if " ++ toString e ++ " then"
          ++ toSemiListSpace ieqs
    ++ concatMap toString eie
    ++ case eeqs of
            Nothing -> ""
            Just es -> "else" ++ toSemiListSpace es
    ++ "end if"

  toString (ForEquation fi eqs) =
    "for " ++ toString fi ++ " loop" ++ toSemiListSpace eqs ++ "end for"

  toString (WhenEquation e eqs ewe) =
    "when " ++ toString e ++ " then" 
            ++ toSemiListSpace eqs ++ concatMap toString ewe ++ "end when"

  toString (ConnectClause a b) =
    "connect(" ++ toString a ++ ", " ++ toString b ++ ")"

  toString (EqFunctionCall n fca) =
    toString n ++ toString fca


---------------------------------------------------------------

data ElseIfEquation =
  ElseIfEquation Expression [Equation] deriving (Show, Eq)

instance ToString ElseIfEquation where
  toString (ElseIfEquation e eqs) =
    "elseif " ++ toString e ++ " then" ++ toSemiListSpace eqs

---------------------------------------------------------------

data ElseWhenEquation =
  ElseWhenEquation Expression [Equation] deriving (Show, Eq)

instance ToString ElseWhenEquation where
  toString (ElseWhenEquation e eqs) =
    "elsewhen " ++ toString e ++ " then" ++ toSemiListSpace eqs

---------------------------------------------------------------

data AlgorithmSection =
  AlgorithmSection (Maybe Init) [Statement] deriving (Show, Eq)

instance ToString AlgorithmSection where
         toString (AlgorithmSection ini []) =
           maybeSpace ini ++ "algorithm"
         toString (AlgorithmSection ini eqs) =
           maybeSpace ini ++ "algorithm " ++ toSemiList eqs

---------------------------------------------------------------

data Statement = Statement Stmt Comment deriving (Show, Eq)

instance ToString Statement where
  toString (Statement e c) = toString e ++ toString c

---------------------------------------------------------------

data Stmt =
  Break
  | Return
  | IfStatement Expression [Statement] [ElseIfStatement] (Maybe [Statement])
  | ForStatement ForIndices [Statement]
  | WhenStatement Expression [Statement] [ElseWhenStatement]
  | WhileStatement Expression [Statement]
  | CompRefStatement ComponentReference (Either Expression FunctionCallArgs)
  | OutputListStatement OutputExpressionList ComponentReference FunctionCallArgs
  deriving (Show, Eq)

instance ToString Stmt where
  toString Break = "break"
  toString Return = "return"

  toString (IfStatement e istmts eie estmts) =
    "if " ++ toString e ++ " then"
          ++ toSemiListSpace istmts
    ++ concatMap toString eie
    ++ case estmts of
            Nothing -> ""
            Just es -> "else" ++ toSemiListSpace es
    ++ "end if"

  toString (ForStatement fi stmts) =
    "for " ++ toString fi ++ " loop" ++ toSemiListSpace stmts ++ "end for"

  toString (WhenStatement e stmts ewe) =
    "when " ++ toString e ++ " then" 
            ++ toSemiListSpace stmts ++ concatMap toString ewe ++ "end when"

  toString (WhileStatement e stmts) =
    "while " ++ toString e ++ " loop"
             ++ toSemiListSpace stmts ++ "end while"

  toString (CompRefStatement cr (Left e)) =
    toString cr ++ " := " ++ toString e

  toString (CompRefStatement cr (Right fca)) =
    toString cr ++ toString fca

  toString (OutputListStatement oel cr fca) =
    "(" ++ toString oel ++ ") := " ++ toString cr ++ toString fca



---------------------------------------------------------------

data ElseIfStatement =
  ElseIfStatement Expression [Statement] deriving (Show, Eq)

instance ToString ElseIfStatement where
  toString (ElseIfStatement e stmts) =
    "elseif " ++ toString e ++ " then" ++ toSemiListSpace stmts

---------------------------------------------------------------

data ElseWhenStatement =
  ElseWhenStatement Expression [Statement] deriving (Show, Eq)

instance ToString ElseWhenStatement where
  toString (ElseWhenStatement e stmts) =
    "elsewhen " ++ toString e ++ " then" ++ toSemiListSpace stmts

---------------------------------------------------------------

data ImportList =
  ImportList Ident [Ident] deriving (Show, Eq)

instance ToString ImportList where
         toString (ImportList i is) = toCommaList (i:is)

---------------------------------------------------------------

data Import =
  Assign Ident Name
  | IList Name (Maybe (Either Star ImportList)) deriving (Show, Eq)

instance ToString Import where
  toString (Assign i n) = toString i ++ " = " ++ toString n
  toString (IList n Nothing) = toString n
  toString (IList n (Just (Left Star))) = toString n ++ ".*"
  toString (IList n (Just (Right il))) = toString n ++ ".{" ++ toString il ++ "}"


data ImportClause = ImportClause Import Comment deriving (Show, Eq)

instance ToString ImportClause where
  toString (ImportClause i c) =
    "import " ++ toString i ++ toString c

---------------------------------------------------------------

data ElementOptions =
  ElementOptions (Maybe Redeclare) (Maybe Final) (Maybe Inner) (Maybe Outer)
  deriving (Show, Eq)

instance ToString ElementOptions where
  toString (ElementOptions r f i o) =
    maybeSpace r ++ maybeSpace f ++ maybeSpace i ++ maybeSpace o

data Element =
  ElementImportClause ImportClause
  | ElementExtendsClause ExtendsClause
  | Element ElementOptions (Either ClassDefinition ComponentClause)
  | ElementReplaceable ElementOptions (Either ClassDefinition ComponentClause)
      (Maybe (ConstrainingClause, Comment))
  deriving (Show, Eq)

instance ToString Element where
  toString (ElementImportClause ic) = toString ic
  toString (ElementExtendsClause ec) = toString ec
  toString (Element opts oot) = toString opts ++ toString oot
  toString (ElementReplaceable opts oot ccc) =
    toString opts ++ "replaceable " ++ toString oot ++ spaceMaybe ccc

---------------------------------------------------------------

data ElementList = ElementList [Element] deriving (Show, Eq)

instance ToString ElementList where
  toString (ElementList el) = toSemiList el

---------------------------------------------------------------

data ExternalFunctionCall =
  ExternalFunctionCall (Maybe ComponentReference) Ident (Maybe ExpressionList)
  deriving (Show, Eq)

instance ToString ExternalFunctionCall where
  toString (ExternalFunctionCall (Just cr) i el) =
    toString cr ++ " = " ++ toString i ++ "(" ++ toString el ++ ")"
  toString (ExternalFunctionCall Nothing i el) =
    toString i ++ "(" ++ toString el ++ ")"

---------------------------------------------------------------

type LanguageSpecification = String

---------------------------------------------------------------


data Composition =
  Composition
    ElementList
    [CompositionList]
    (Maybe CompositionExternal)
    (Maybe Annotation)
  deriving (Show, Eq)


instance ToString Composition where
  toString (Composition el cls ce a) =
    toString el ++ g cls ++ maybe "" toString ce ++ maybe "" f a
      where f = (' ':) . (++ ";") . toString
            g [] = ""
            g xs = " " ++ toString xs

data CompositionList =
  PublicElementList ElementList
  | ProtectedElementList ElementList
  | ESec EquationSection
  | ASec AlgorithmSection
  deriving (Show, Eq)

instance ToString [CompositionList] where
  toString xs = concatMap toString xs


instance ToString CompositionList where
  toString (PublicElementList (ElementList [])) = "public "
  toString (PublicElementList el) = "public " ++ toString el ++ " "
  toString (ProtectedElementList (ElementList [])) = "protected "
  toString (ProtectedElementList el) = "protected " ++ toString el ++ " "
  toString (ESec es) = toString es
  toString (ASec as) = toString as

data CompositionExternal =
  CompositionExternal
    (Maybe LanguageSpecification)
    (Maybe ExternalFunctionCall)
    (Maybe Annotation)
  deriving (Show, Eq)

instance ToString CompositionExternal where
  toString (CompositionExternal ls efc a) =
    "external" ++ maybe "" f ls ++ spaceMaybe efc ++ spaceMaybe a ++ ";"
      where f = (" \"" ++) . (++"\"") . id

---------------------------------------------------------------


data ClassSpecifier =
  ClassSpecifierEnd Ident StringComment Composition Ident
  | ClassSpecifierA Ident BasePrefix Name
                    (Maybe ArraySubscripts) (Maybe ClassModification) Comment
  | ClassSpecifierEnum Ident (Either Colon (Maybe EnumList)) Comment
  | ClassSpecifierDer Ident (Name, Ident, [Ident]) Comment
  | ClassSpecifierExtends Ident (Maybe ClassModification)
                          StringComment Composition Ident
  deriving (Show, Eq)


instance ToString ClassSpecifier where
  toString (ClassSpecifierEnd i1 sc c i2) =
    toString i1 ++ toString sc ++ " " ++ toString c ++ " end " ++ toString i2
  toString (ClassSpecifierA i bp n as cm c) =
    toString i ++ " = " ++ toString bp ++ toString n ++ spaceMaybe as
               ++ spaceMaybe cm ++ " " ++ toString c
  toString (ClassSpecifierEnum i x c) =
    toString i ++ " = enumeration(" ++ toString x ++ ")" ++ toString c
  toString (ClassSpecifierDer i (n, j, js) c) =
    toString i ++ " = der(" ++ toString n ++ ", " ++ toCommaList (j:js) ++ ")"
               ++ toString c
  toString (ClassSpecifierExtends i1 cm sc c i2) =
    "extends " ++ toString i1 ++ spaceMaybe cm ++ toString sc
               ++ (if emptyComposition c then "" else " ")
               ++ toString c
               ++ " end " ++ toString i2
    where emptyComposition (Composition (ElementList []) [] Nothing Nothing) = True
          emptyComposition _ = False

---------------------------------------------------------------

data ClassDefinition =
  ClassDefinition (Maybe Encapsulated) ClassPrefixes ClassSpecifier
  deriving (Show, Eq)

instance ToString ClassDefinition where
  toString (ClassDefinition enc cp cs) =
    maybeSpace enc ++ toString cp ++ " " ++ toString cs

---------------------------------------------------------------

data StoredDefinition =
  StoredDefinition (Maybe (Maybe Name)) [(Maybe Final, ClassDefinition)]
  deriving (Show, Eq)

instance ToString StoredDefinition where
  toString (StoredDefinition (Just n) cd) =
    "within" ++ spaceMaybe n ++ "; " ++ toSemiList cd
  toString (StoredDefinition _ cd) = toSemiList cd