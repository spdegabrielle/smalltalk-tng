module Main where

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified List
import Debug.Trace

type Env = [(String, Value)]

data AST = AstAtom String
         | AstBinding String
         | AstObject [(AST, AST)]
         | AstApp AST AST
           deriving (Eq, Ord)

data Value = VAtom String
           | VBinding String
           | VClosure Env [(Value, Value)]
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

readAST = readApp
readApp = do (part : parts) <- sepBy1 readSimple whiteSpace; return $ foldl AstApp part parts
readMap = try (do entries <- sepBy readMapEntry whiteSpace; return $ AstObject entries)
readMapEntry = do l <- readSimple; punct ":"; r <- readSimple; return (l, r)
readSimple =    do punct "("; v <- readAST; punct ")"; return v
            <|> do punct "["; m <- readMap; punct "]"; return m
            <|> do punct "+"; i <- ident; return $ AstBinding i
            <|> do i <- ident; return $ AstAtom i

showClause (l, r) = show l ++ ": " ++ show r
showClauses [] = ""
showClauses [clause] = showClause clause
showClauses (clause:clauses) =
    (showClause clause) ++ (concatMap (\c -> " " ++ showClause c) clauses)

instance Show AST where
    show v = showAST v

showAST (AstAtom s) = s
showAST (AstBinding s) = "+" ++ s
showAST (AstObject clauses) = "[" ++ showClauses clauses ++ "]"
showAST (AstApp v1 v2) = "(" ++ show v1 ++ " " ++ show v2 ++ ")"

instance Show Value where
    show v = showValue v

showValue (VAtom s) = s
showValue (VBinding s) = "+" ++ s
showValue (VClosure env clauses) = showEnv env ++ showClauses clauses

showEnv bindings = "{" ++ showClauses bindings ++ "}"

parseASTFromString = parse (do whiteSpace; v <- readAST; eof; return v) ""

readTng s = case parseASTFromString s of
              Right v -> v
              Left err -> error $ "Parse error: " ++ show err

---------------------------------------------------------------------------

freeVars env exp =
    case exp of
      AstAtom s -> if any (s ==) env then [] else [s]
      AstBinding _ -> []
      AstObject clauses -> foldr collect [] clauses
          where collect clause acc = List.union (clauseFree clause) acc
                clauseFree (pat, val) = foldr List.union (valsFree val) patsFree
                    where valsFree = freeVars ((patternBound pat) ++ env)
                patsFree = map (freeVars env . fst) clauses
      AstApp rator rand -> List.union (freeVars env rator) (freeVars env rand)

patternBound (AstAtom s) = []
patternBound (AstBinding b) = [b]
patternBound (AstObject clauses) = concatMap (patternBound . snd) clauses
patternBound (AstApp _ _) = error "Unreduced pattern in patternBound"

---------------------------------------------------------------------------

match' ps vs = match (readTng ps) (readTng vs)
freeVars' env exp = freeVars env (readTng exp)
eval' env exp = eval env (readTng exp)

---------------------------------------------------------------------------

bindingUnion Nothing _ = Nothing
bindingUnion _ Nothing = Nothing
bindingUnion (Just b1) (Just b2) = Just (b1 ++ b2)

match (AstAtom a) (AstAtom b) = if a == b then Just [] else Nothing
match (AstBinding n) v = Just [(n, v)]
match (AstObject []) v = Just []  -- maybe remove if atoms are properly disjoint?
match (AstObject patternClauses) (AstObject valueClauses) =
    foldr bindingUnion (Just []) $ map (flip match1 valueClauses) patternClauses
match (AstApp _ _) _ = error "There really shouldn't be a AstApp in pattern-position in match"
match _ _ = Nothing

match1 (pval, ppat) [] = Nothing
match1 (pval, ppat) ((vpat, vval) : valueClauses) =
    case match vpat pval of
      Nothing -> match1 (pval, ppat) valueClauses
      Just bs -> case match ppat (substBindings bs vval) of
                   Nothing -> match1 (pval, ppat) valueClauses
                   x -> x

eval bs o =
    case o of
      AstAtom s -> case lookup s bs of
                     Just v -> v
                     Nothing -> o
      AstBinding _ -> o
      AstObject clauses -> AstObject $ map evalClause clauses
          where evalClause (pat, val) = (eval bs pat, substBindings rhsBindings val)
                    where rhsBindings = filter notShadowed bs
                          notShadowed (var, val) = not $ any (== var) patBound
                          patBound = patternBound pat
      AstApp rator rand -> applyTng bs (eval bs rator) (eval bs rand)

applyTng bs (AstObject patternClauses) value = apply1 bs value patternClauses

apply1 bs val [] = error "applyTng match failed"
apply1 bs val ((ppat, pval) : patternClauses) =
    case match ppat val of
      Nothing -> apply1 bs val patternClauses
      Just bs' -> eval (bs' ++ bs) pval

substBindings bs o =
    case o of
      AstAtom s -> case lookup s bs of
                     Just v -> v
                     Nothing -> o
      AstBinding _ -> o
      AstObject clauses -> AstObject $ map substClause clauses
          where substClause (v, p) = (substBindings bs v, substBindings bs p)
