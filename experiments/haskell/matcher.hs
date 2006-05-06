module Main where

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

data Value = VAtom String
           | VBinding String
           | VObject [(Value, Value)]
           | VApp Value Value
             deriving (Eq, Ord)

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
punct s = do string s; whiteSpace; return ()

readValue = readApp
readApp = do (part : parts) <- sepBy1 readSimple whiteSpace; return $ foldl VApp part parts
readMap = try (do entries <- sepBy readMapEntry whiteSpace; return $ VObject entries)
readMapEntry = do l <- readSimple; punct ":"; r <- readSimple; return (l, r)
readSimple =    do punct "("; v <- readValue; punct ")"; return v
            <|> do punct "["; m <- readMap; punct "]"; return m
            <|> do punct "+"; i <- ident; return $ VBinding i
            <|> do i <- ident; return $ VAtom i

instance Show Value where
    show v = showValue v

showValue (VAtom s) = s
showValue (VBinding s) = "+" ++ s
showValue (VObject clauses) =
    "[" ++ tail (foldr (\(l,r) tl -> " " ++ show l ++ ": " ++ show r ++ tl) "]" clauses)
showValue (VApp v1 v2) = "(" ++ show v1 ++ " " ++ show v2 ++ ")"

parseValueFromString = parse (do whiteSpace; v <- readValue; eof; return v) ""

---------------------------------------------------------------------------

match' ps vs =
    case parseValueFromString ps of
      Right p -> case parseValueFromString vs of
                   Right v -> Right $ match p v
                   Left err -> Left err
      Left err -> Left err

bindingUnion Nothing _ = Nothing
bindingUnion _ Nothing = Nothing
bindingUnion (Just b1) (Just b2) = Just (b1 ++ b2)

match (VAtom a) (VAtom b) = if a == b then Just [] else Nothing
match (VBinding n) v = Just [(n, v)]
match (VObject []) v = Just []  -- maybe remove if atoms are properly disjoint?
match (VObject patternClauses) (VObject valueClauses) =
    foldr bindingUnion (Just []) $ map (flip match1 valueClauses) patternClauses
match (VApp _ _) _ = error "There really shouldn't be a VApp in pattern-position in match"
match _ _ = Nothing

match1 (pval, ppat) [] = Nothing
match1 (pval, ppat) ((vpat, vval) : valueClauses) =
    case match vpat pval of
      Nothing -> match1 (pval, ppat) valueClauses
      Just bs -> case match ppat (eval vval bs) of
                   Nothing -> match1 (pval, ppat) valueClauses
                   x -> x

eval o@(VAtom s) bs =
    case lookup s bs of
      Just v -> v
      Nothing -> o
eval o@(VBinding _) bs = o
eval o@(VObject clauses) bs =
    VObject $ map (\(v, p) -> (eval v bs, eval p bs)) clauses

substBindings o@(VAtom s) bs =
    case lookup s bs of
      Just v -> v
      Nothing -> o
substBindings o@(VBinding _) bs = o
substBindings (VObject clauses) bs =
    VObject (map (\(v, p) -> (substBindings v bs, substBindings p bs)) clauses)

