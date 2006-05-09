module Matcher where

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified List
import Debug.Trace

type Env = [(Bool, String, Value)]

data AST = AstAtom String
         | AstBinding String AST
         | AstDiscard
         | AstLet String AST
         | AstObject [(AST, AST)]
         | AstApp AST AST
           deriving (Eq, Ord)

data Value = VAtom String
           | VBinding String Value
           | VDiscard
           | VObject [(Pattern, Closure)]
             deriving (Eq, Ord)

data Pattern = PAtom String
             | PBinding String Pattern
             | PDiscard
             | PObject [(Value, Pattern)]
               deriving (Eq, Ord)

data Closure = Closure Env AST
             | Constant Value
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
            <|> do punct "+"; i <- ident; readBinding i
            <|> do punct "_"; return AstDiscard
            <|> do i <- ident; readLet i
readLet i =     do punct "="; v <- readSimple; return $ AstLet i v
            <|> (return $ AstAtom i)
readBinding i =     do punct "@"; v <- readSimple; return $ AstBinding i v
                <|> (return $ AstBinding i AstDiscard)

sepList s [] = ""
sepList s [x] = x
sepList s (x:xs) = x ++ s ++ sepList s xs

showClause (l, r) = show l ++ ": " ++ show r
showClauses clauses = sepList " " (map showClause clauses)

instance Show AST where
    show v = showAST v

showAST (AstAtom s) = s
showAST (AstBinding s AstDiscard) = "+" ++ s
showAST (AstBinding s v) = "+" ++ s ++ "@" ++ show v
showAST (AstDiscard) = "_"
showAST (AstLet s v) = s ++ "=" ++ show v
showAST (AstObject clauses) = "[" ++ showClauses clauses ++ "]"
showAST (AstApp v1 v2) = "(" ++ show v1 ++ " " ++ show v2 ++ ")"

instance Show Value where
    show v = showValue v

showValue (VAtom s) = s
showValue (VBinding s VDiscard) = "+" ++ s
showValue (VBinding s v) = "+" ++ s ++ "@" ++ show v
showValue (VDiscard) = "_"
showValue (VObject clauses) = "[" ++ showClauses clauses ++ "]"

instance Show Pattern where
    show v = showPattern v

showPattern (PAtom s) = s
showPattern (PBinding s PDiscard) = "+" ++ s
showPattern (PBinding s v) = "+" ++ s ++ "@" ++ show v
showPattern (PDiscard) = "_"
showPattern (PObject clauses) = "[" ++ showClauses clauses ++ "]"

instance Show Closure where
    show (Closure e v) = showEnv e ++ show v
    show (Constant v) = show v
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

eLookup s [] = Nothing
eLookup s ((_, n, v):bs) = if n == s then Just v else eLookup s bs

lookupVal s bs =
    case eLookup s bs of
      Just v -> v
      Nothing -> case eLookup s baseEnv of
                   Just v -> v
                   Nothing -> VAtom s

match (PAtom a) (VAtom b) = if a == b then Just [] else Nothing
match (PBinding n p) v = do bs <- match p v; return ((False, n, v) : bs)
match (PDiscard) v = Just []
match (PObject patternClauses) (VObject valueClauses) =
    foldr bindingUnion (Just []) $ map (match1 valueClauses) patternClauses
        where bindingUnion j1 j2 = do b1 <- j1; b2 <- j2; return (b1 ++ b2)
match _ _ = Nothing

firstThat p [] = Nothing
firstThat p (x:xs) = case p x of
                       Nothing -> firstThat p xs
                       j -> j

match1 valueClauses (pval, ppat) =
    firstThat firstMatch valueClauses
    where firstMatch (vpat, vval) =
              do bs' <- match vpat pval
                 bs'' <- match ppat (reduce vval bs')
                 return bs''

toPattern v =
    case v of
      VAtom s -> PAtom s
      VBinding s v' -> PBinding s (toPattern v')
      VDiscard -> PDiscard
      VObject clauses -> PObject $ [(toValue p, toPattern $ reduce cl []) | (p, cl) <- clauses]

toValue p =
    case p of
      PAtom s -> VAtom s
      PBinding s p' -> VBinding s (toValue p')
      PDiscard -> VDiscard
      PObject clauses -> VObject $ [(toPattern v, Constant $ toValue p) | (v, p) <- clauses]

reduce (Closure env v) bs = eval (bs ++ env) v
reduce (Constant v) bs = v

eval bs o =
    case o of
      AstAtom s -> lookupVal s bs
      AstBinding s v -> VBinding s (eval bs v)
      AstDiscard -> VDiscard
      AstLet s v -> result
          where result = eval bs' v
                bs' = (True, s, result) : bs
      AstObject clauses -> VObject $ map evalClause clauses
          where evalClause (patexp, val) = (pat, maybeClose pat bs val)
                    where pat = toPattern $ eval bs patexp
      AstApp rator rand -> applyTng bs (eval bs rator) (eval bs rand)

maybeClose pat bs o =
    case patBound pat of
      [] -> Constant $ eval bs o
      _ -> Closure bs o

patBound (PAtom s) = []
patBound (PBinding s p) = [s] ++ patBound p
patBound (PDiscard) = []
patBound (PObject clauses) = concatMap (patBound . snd) clauses

dnu function value = error $ "DNU: " ++ show function ++ " " ++ show value

applyTng bs function@(VObject patternClauses) value =
    case firstThat matches patternClauses of
      Nothing -> dnu function value
      Just result -> result
    where matches (ppat, pval) = case match ppat value of
                                   Nothing -> Nothing
                                   Just bs' -> Just $ reduce pval bs'
applyTng bs function value = dnu function value

---------------------------------------------------------------------------

eval' exp = eval [] (readTng exp)

baseEnv = [ def "cons" "[+car: [+cdr: [First: car Rest: cdr]]]"
          , def "map" "[+f: loop=[(cons +a +d): (cons (f a) (loop d)) +x:x]]"
          ]
    where def nm exp = (False, nm, eval' exp)
