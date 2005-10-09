module Main where

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Map as Map

data Value = VTuple [Value]
           | VMap (Map Value Value)
           | VApp Value Value
           | VQuote Value
           | VVar String
           | VLiteral Literal
             deriving (Show, Eq, Ord)

data Literal = LInteger Integer
               deriving (Show, Eq, Ord)

tngDef = P.LanguageDef
         { P.commentStart = "\""
         , P.commentEnd = "\""
         , P.commentLine = ""
         , P.nestedComments = False
         , P.identStart = letter
         , P.identLetter = alphaNum
         , P.opStart = (oneOf ":!#$%&*+./<=>?@\\^|-~")
         , P.opLetter = (oneOf ":!#$%&*+./<=>?@\\^|-~" <|> alphaNum)
         , P.reservedNames = []
         , P.reservedOpNames = []
         , P.caseSensitive = True
         }

tngTokenizer = P.makeTokenParser tngDef

whiteSpace = P.whiteSpace tngTokenizer
ident = P.identifier tngTokenizer
integer = P.integer tngTokenizer
punct s = do whiteSpace; string s; return ()

readValue = do vs <- sepBy1 readMap (punct ",")
               return (case vs of
                               [v] -> v
                               _ -> VTuple vs)
readMap =    try (do entries <- sepBy1 readMapEntry whiteSpace; return (VMap (fromList entries)))
         <|> readApp
readMapEntry = do p <- readApp; punct ":"; v <- readApp; return (p, v)
readApp = do (part : parts) <- sepBy1 readSimple whiteSpace; return (foldl VApp part parts)
readSimple =    do punct "("; v <- readValue; punct ")"; return v
            <|> do punct "{"; v <- readValue; punct "}"; return (VQuote v)
            <|> do i <- ident; return (VVar i)
            <|> do n <- integer; return (VLiteral (LInteger n))

parseValueFromString = parse (do v <- readValue; eof; return v) ""

main = print 123
