

module Language.Modelica.Parser.Lexer where

import Language.Modelica.Syntax.Modelica

import Language.Modelica.Parser.Parser (Parser)
import Language.Modelica.Parser.Utility (followedBy)

import Text.ParserCombinators.Parsec
  ( (<|>), (<?>), try, between,
    oneOf, noneOf, string, option, skipMany,
    many, many1, notFollowedBy, satisfy, choice,
    char, digit, unexpected, getPosition, eof )


import qualified Data.Set as Set; import Data.Set (Set)
import qualified Data.Char as Char


import Control.Applicative
  (liftA, liftA2, liftA3, (*>), (<*), Applicative)

import Control.Monad (void)


---------------------------------------------------------

eol :: Parser ()
eol = void $
  try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"

---------------------------------------------------------

nondigit :: Parser Char
nondigit =
  oneOf ('_' : ['a'..'z'] ++ ['A'..'Z']) <?> "nondigit"

schar :: Parser String
schar = liftA (:[]) $ noneOf "\"\\"

qchar :: Parser String
qchar = liftA (:[])
  ( nondigit
    <|> digit
    <|> oneOf "!#$%&()*+,-./:;<>=?@[]^{}|~ ")

sescape :: Parser String
sescape = choice $ map string
  [ "\\'", "\\\"", "\\?", "\\\\", "\\a",
    "\\b", "\\f", "\\n", "\\r", "\\t", "\\v" ]

qident :: Parser String
qident = liftA concat $ quotes (many1 (qchar <|> sescape))

unicode_string :: Parser String
unicode_string = liftA concat $
  quotation $ many (schar <|> sescape)

ident :: Parser Ident
ident = 
  liftA2 Ident getPosition (try ident')
  <|> liftA2 QIdent getPosition qident
  <?> "ident"

ident' :: Parser String
ident' = do
  i <- lexeme $ liftA2 (:) nondigit (many (digit <|> nondigit))
  if isKeyword i
     then unexpected ("keyword " ++ show i)
     else return i


identChar :: Parser String
identChar = 
  liftA (:[]) nondigit
  <|> liftA (:[]) digit


---------------------------------------------------------


unsigned_integer :: Parser Integer
unsigned_integer = liftA read (many1 digit)

makeNumber :: Integer -> Integer -> Integer -> Double
makeNumber x y z = read (show x ++ "." ++ show y ++ "e" ++ show z)

unsigned_number :: Parser Double
unsigned_number = lexeme $
  liftA3 makeNumber unsigned_integer fraction expo

fraction :: Parser Integer
fraction = try (char '.' *> option 0 unsigned_integer) <|> return 0

eE :: Parser Char
eE = char 'e' <|> char 'E' <?> "expected \"e\" or \"E\""

plusMinus :: Parser Integer
plusMinus = option 1 $
  (char '+' >> return 1) <|> (char '-' >> return (-1))

expo :: Parser Integer
expo = option 0 $
  liftA2 (*) (eE *> plusMinus) unsigned_integer

---------------------------------------------------------

symbol :: String -> Parser String
symbol name = lexeme (string name)

lexeme :: Parser a -> Parser a
lexeme = (<* whiteSpace)

whiteSpace :: Parser ()
whiteSpace = skipMany (satisfy Char.isSpace) <?> "whitespace"

parens, braces, brackets, quotes, quotation :: Parser a -> Parser a
parens p        = between (symbol "(") (symbol ")") p
braces p        = between (symbol "{") (symbol "}") p
brackets p      = between (symbol "[") (symbol "]") p
quotes p        = between (string "'") (symbol "'") p
quotation p     = between (string "\"") (symbol "\"") p

---------------------------------------------------------

dot :: Parser Dot
dot = lexeme $ try $ do
  void $ symbol "."
  notFollowedBy (oneOf "+/^*{")
  return Dot

star :: Parser Star
star = symbol "*" *> return Star

colon :: Parser Colon
colon = symbol ":" *> return Colon

comma, plus, semicolon, assign, colon_assign :: Parser String
comma = symbol ","
plus = symbol "+"
semicolon = symbol ";"
assign = symbol "="
colon_assign = symbol ":="


cpp_block_cmt_start, cpp_block_cmt_end :: Parser String
cpp_block_cmt_start = symbol "/*"
cpp_block_cmt_end = symbol "*/"

cpp_line_cmt_start :: Parser String
cpp_line_cmt_start = symbol "//"

slash :: Parser String
slash = symbol "/"

eol_or_eof :: Parser ()
eol_or_eof = eol <|> eof

---------------------------------------------------------

keyword :: String -> Parser ()
keyword kw = lexeme $ try $ do
  _ <- string kw
  notFollowedBy identChar <?> ("end of " ++ show kw)

-- Total of 60 keywords
kwds :: Set String
kwds = Set.fromList [
     "algorithm", "discrete", "false", "loop", "pure",
     "and", "each", "final", "model", "record",
     "annotation", "else", "flow", "not", "redeclare",
     "elseif", "for", "operator", "replaceable",
     "block", "elsewhen", "function", "or", "return",
     "break", "encapsulated", "if", "outer", "stream",
     "class", "end", "import", "output", "then",
     "connect", "enumeration", "impure", "package", "true",
     "connector", "equation", "in", "parameter", "type",
     "constant", "expandable", "initial", "partial", "when",
     "constrainedby", "extends", "inner", "protected", "while",
     "der", "external", "input", "public", "within" ]


isKeyword :: String -> Bool
isKeyword = flip Set.member kwds


in_, if_, then_, else_, elseif_, for_, when_, while_,
  loop_, end_, connect_, and_, or_,
  function_, annotation_,
  end_for_, end_if_, end_while_, end_when_,
  equation_, algorithm_, replaceable_, record_,
  connector_, constrainedby_, enumeration_,
  elsewhen_, extends_, import_, public_, protected_,
  external_, within_ :: Parser ()


in_               = keyword "in"
if_               = keyword "if"
then_             = keyword "then"
else_             = keyword "else"
elseif_           = keyword "elseif"
for_              = keyword "for"
when_             = keyword "when"
elsewhen_         = keyword "elsewhen"
while_            = keyword "while"
loop_             = keyword "loop"
end_              = keyword "end"
connect_          = keyword "connect"
and_              = keyword "and"
or_               = keyword "or"
function_         = keyword "function"
record_           = keyword "record"
connector_        = keyword "connector"
annotation_       = keyword "annotation"
end_for_          = end_ *> for_
end_if_           = end_ *> if_
end_while_        = end_ *> while_
end_when_         = end_ *> when_
equation_         = keyword "equation"
algorithm_        = keyword "algorithm"
replaceable_      = keyword "replaceable"
constrainedby_    = keyword "constrainedby"
extends_          = keyword "extends"
enumeration_      = keyword "enumeration"
import_           = keyword "import"
public_           = keyword "public"
protected_        = keyword "protected"
external_         = keyword "external"
within_           = keyword "within"

not_ :: Parser Not
not_              = keyword "not" *> return Not

true_, false_ :: Parser Bool
false_            = keyword "false" *> return False
true_             = keyword "true" *> return True

der_, initial_ :: Parser DIN
der_              = keyword "der" *> return Der
initial_          = keyword "initial" *> return Initial

init_ :: Parser Init
init_             = keyword "initial" *> return Init

each_ :: Parser Each
each_             = keyword "each" *> return Each

final_ :: Parser Final
final_            = keyword "final" *> return Final

redeclare_ :: Parser Redeclare
redeclare_        = keyword "redeclare" *> return Redeclare

inner_ :: Parser Inner
inner_            = keyword "inner" *> return Inner


outer_ :: Parser Outer
outer_            = keyword "outer" *> return Outer


flow_, stream_ :: Parser FS
flow_             = keyword "flow" *> return Flow
stream_           = keyword "stream" *> return Stream

discrete_, parameter_, constant_ :: Parser DPC
discrete_         = keyword "discrete" *> return Discrete
parameter_        = keyword "parameter" *> return Parameter
constant_         = keyword "constant" *> return Constant

input_, output_ :: Parser OI
input_            = keyword "input" *> return Input
output_           = keyword "output" *> return Output

partial_ :: Parser Partial
partial_          = keyword "partial" *> return Partial

encapsulated_ :: Parser Encapsulated
encapsulated_     = keyword "encapsulated" *> return Encapsulated

class_, model_, block_, type_, package_, operator_ :: Parser Prefix
class_            = keyword "class" *> return Class
model_            = keyword "model" *> return Model
block_            = keyword "block" *> return Block
type_             = keyword "type" *> return Type
package_          = keyword "package" *> return Package
operator_         = keyword "operator" *> return Operator

pure_, impure_ :: Parser PureImpure
pure_             = keyword "pure" *> return Pure
impure_           = keyword "impure" *> return Impure

operatorfunction_ :: Parser OperatorFunction
operatorfunction_ = keyword "operator" *> return OperatorFunction

operatorrecord_ :: Parser OperatorRecord
operatorrecord_ = keyword "operator" *> return OperatorRecord

expandable_ :: Parser Expandable
expandable_     = keyword "expandable" *> return Expandable

break_, return_ :: Parser Stmt
break_          = keyword "break" *> return Break
return_         = keyword "return" *> return Return


---------------------------------------------------------

rel_op :: Parser RelOp
rel_op = operator $
  ("==", Equal) :
  ("<>", UnEqual) : 
  ("<=", LEQ) :
  (">=", GEQ) :
  ("<" , LTH) :
  (">" , GTH) :
  []

add_op :: Parser AddOp
add_op = operator $
  ("+" , Plus) :
  (".+", DotPlus) :
  ("-" , Minus) :
  (".-", DotMinus) :
  []
 
mul_op :: Parser MulOp
mul_op = operator $
  ("*", Mul) :
  (".*", DotMul) :
  ("/", Div) :
  ("./", DotDiv) :
  []


pot_op :: Parser PotOp
pot_op = operator $
  ("^", Pot) :
  (".^", DotPot) :
  []

operator :: [(String, a)] -> Parser a
operator = choice . map (\(x, y) -> try (symbol x) >> return y)

------------------------------------------------------------------

commaList :: Parser a -> Parser [a]
commaList = followedBy comma

semiList :: Parser a -> Parser [a]
semiList = followedBy semicolon

dotList :: Parser a -> Parser [a]
dotList = followedBy dot

plusList :: Parser a -> Parser [a]
plusList = followedBy plus
