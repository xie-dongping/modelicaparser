

module Language.Modelica.Parser.Utility where

import Language.Modelica.Parser.Parser (Parser)

import Text.ParserCombinators.Parsec
  ( (<|>), try, many, skipMany, satisfy, eof, manyTill, anyChar)

import Control.Applicative
  (liftA, liftA2, (*>), (<*), Applicative)

import qualified Data.Char as Char

import Prelude hiding (until)

(~+) :: Parser a -> Parser b -> Parser (a, b)
(~+) = liftA2 (,)

(~*) :: Parser a -> Parser b -> Parser [(a, b)]
(~*) s p = many (s ~+ p)

followedBy :: Parser a -> Parser b -> Parser [b]
followedBy s p = liftA (map snd) (s ~* p)

cst :: Applicative f => b -> f a -> f b
cst v = liftA (const v)


eitherOr :: Parser a -> Parser b -> Parser (Either a b)
eitherOr pa pb =
  try (liftA Left pa)
  <|> liftA Right pb

eitherOrOr :: Parser a -> Parser b -> Parser c -> Parser (Either a (Either b c))
eitherOrOr pa pb pc =
  try (liftA Left pa)
  <|> liftA Right (eitherOr pb pc)

alternate1 :: Parser a -> Parser a -> Parser [a]
alternate1 p sep =
  do { x <- p;
       do { y <- sep;
            xs <- alternate p sep;
            return (x:y:xs) } <|> return [x] }

alternate :: Parser a -> Parser a -> Parser [a]
alternate p sep = alternate1 p sep <|> return []

stringParser :: Parser a -> Parser a
stringParser p =
  skipMany (satisfy Char.isSpace) *> p <* eof

until :: (String -> b) -> Parser a -> Parser b
until constr endp = liftA constr (manyTill anyChar endp)
