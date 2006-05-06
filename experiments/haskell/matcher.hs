module Main where

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified List
import Debug.Trace

type Env = [(Bool, String, Value)]

data AST = AstAtom String
         | AstBinding String
         | AstDiscard
         | AstLet String AST
         | AstObject [(AST, AST)]
         | AstApp AST AST
           deriving (Eq, Ord)

data Value = VAtom String
           | VBinding String
           | VDiscard
           | VObject [(Value, Closure)]
             deriving (Eq, Ord)

data Closure = Closure Env AST
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
            <|> do punct "_"; return AstDiscard
            <|> do i <- ident; readLet i
readLet i =     do punct "="; v <- readSimple; return $ AstLet i v
            <|> (return $ AstAtom i)

sepList s [] = ""
sepList s [x] = x
sepList s (x:xs) = x ++ s ++ sepList s xs

showClause (l, r) = show l ++ ": " ++ show r
showClauses clauses = sepList " " (map showClause clauses)

instance Show AST where
    show v = showAST v

showAST (AstAtom s) = s
showAST (AstBinding s) = "+" ++ s
showAST (AstDiscard) = "_"
showAST (AstLet s v) = s ++ "=" ++ show v
showAST (AstObject clauses) = "[" ++ showClauses clauses ++ "]"
showAST (AstApp v1 v2) = "(" ++ show v1 ++ " " ++ show v2 ++ ")"

instance Show Value where
    show v = showValue v

showValue (VAtom s) = s
showValue (VBinding s) = "+" ++ s
showValue (VDiscard) = "_"
showValue (VObject clauses) = "[" ++ showClauses clauses ++ "]"

instance Show Closure where
    show (Closure e v) = showEnv e ++ show v
    -- show (Closure e v) = show v

showEnvEntry (False, n, v) = n ++ "->" ++ show v
showEnvEntry (True, n, v) = n
showEnv [] = ""
showEnv bindings = "{" ++ sepList ", " (map showEnvEntry bindings) ++ "} "

parseASTFromString = parse (do whiteSpace; v <- readAST; eof; return v) ""

readTng s = case parseASTFromString s of
              Right v -> v
              Left err -> error $ "Parse error: " ++ show err

---------------------------------------------------------------------------

freeVars env exp =
    case exp of
      AstAtom s -> if any (s ==) env then [] else [s]
      AstBinding _ -> []
      AstDiscard -> []
      AstLet s v -> freeVars (s:env) v
      AstObject clauses -> foldr collect [] clauses
          where collect clause acc = List.union (clauseFree clause) acc
                clauseFree (pat, val) = foldr List.union (valsFree val) patsFree
                    where valsFree = freeVars ((patternBound pat) ++ env)
                patsFree = map (freeVars env . fst) clauses
      AstApp rator rand -> List.union (freeVars env rator) (freeVars env rand)

patternBound (AstAtom s) = []
patternBound (AstBinding b) = [b]
patternBound (AstDiscard) = []
patternBound (AstLet s v) = patternBound v
patternBound (AstObject clauses) = concatMap (patternBound . snd) clauses
patternBound (AstApp _ _) = error "Unreduced pattern in patternBound"

---------------------------------------------------------------------------

freeVars' env exp = freeVars env (readTng exp)
eval' exp = eval [] (readTng exp)

---------------------------------------------------------------------------

bindingUnion Nothing _ = Nothing
bindingUnion _ Nothing = Nothing
bindingUnion (Just b1) (Just b2) = Just (b1 ++ b2)

eLookup s [] = Nothing
eLookup s ((_, n, v):bs) = if n == s then Just v else eLookup s bs

lookupVal s bs =
    case eLookup s bs of
      Just v -> v
      Nothing -> case eLookup s baseEnv of
                   Just v -> v
                   Nothing -> VAtom s

match (VAtom a) (VAtom b) = if a == b then Just [] else Nothing
match (VBinding n) v = Just [(False, n, v)]
match (VDiscard) v = Just []
match (VObject patternClauses) (VObject valueClauses) =
    foldr bindingUnion (Just []) $ map (match1 valueClauses) patternClauses
match _ _ = Nothing

firstThat p [] = Nothing
firstThat p (x:xs) = case p x of
                       Nothing -> firstThat p xs
                       j -> j

match1 valueClauses (pval, ppat) =
    firstThat firstMatch valueClauses
    where firstMatch (vpat, vval) =
              do bs' <- match vpat pval
                 bs'' <- match (reduce ppat []) (reduce vval bs')
                 return bs''

reduce (Closure env v) bs = eval (bs ++ env) v

eval bs o =
    case o of
      AstAtom s -> lookupVal s bs
      AstBinding s -> VBinding s
      AstDiscard -> VDiscard
      AstLet s v -> result
          where result = eval bs' v
                bs' = (True, s, result) : bs
      AstObject clauses -> VObject $ map evalClause clauses
          where evalClause (pat, val) = (eval bs pat, Closure bs val)
      AstApp rator rand -> applyTng bs (eval bs rator) (eval bs rand)

dnu function value = error $ "DNU: " ++ show function ++ " " ++ show value

applyTng bs function@(VObject patternClauses) value =
    case firstThat matches patternClauses of
      Nothing -> dnu function value
      Just result -> result
    where matches (ppat, pval) = case match ppat value of
                                   Nothing -> Nothing
                                   Just bs' -> Just $ reduce pval bs'
applyTng bs function value = dnu function value

baseEnv = [ def "cons" "[+car: [+cdr: [First: car Rest: cdr]]]"
          , def "map" "[+f: loop=[(cons +a +d): (cons (f a) (loop d)) +x:x]]"
          ]
    where def nm exp = (False, nm, eval' exp)
