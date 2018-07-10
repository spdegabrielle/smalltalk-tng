-- Generic S-expression reader, adapted from from RosettaCode:
-- https://rosettacode.org/wiki/S-Expressions#Haskell

module SimpleSexp (Atom(..), Sexp(..), readSexps, showSexp) where

import Data.Functor
import Data.List
import Text.Parsec ((<|>), (<?>), many, many1, char, try, parse, sepBy, choice, between)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Token (integer, float, whiteSpace, stringLiteral, makeTokenParser)
import Text.Parsec.Language (haskell)

data Atom = Int Integer
          | Float Double
          | String String
          | Symbol String
            deriving (Eq, Show)
data Sexp = Atom Atom
          | List [Sexp]
            deriving (Eq, Show)

tProg = many tExpr <?> "program"
  where tExpr = between ws ws (tList <|> tAtom) <?> "expression"
        ws = whiteSpace haskell
        tAtom  =  (try (Atom . Float <$> float haskell) <?> "floating point number")
              <|> (try (Atom . Int <$> integer haskell) <?> "integer")
              <|> (Atom . String <$> stringLiteral haskell <?> "string")
              <|> (Atom . Symbol <$> many1 (noneOf "()\"\t\n\r ") <?> "symbol")
              <?> "atomic expression"
        tList = List <$> between (char '(') (char ')') (many tExpr) <?> "list"

readSexps = parse tProg ""

showSexp (Atom (Int i)) = show i
showSexp (Atom (Float d)) = show d
showSexp (Atom (String s)) = show s
showSexp (Atom (Symbol s)) = s
showSexp (List sexps) = "(" ++ intercalate " " (map showSexp sexps) ++ ")"
