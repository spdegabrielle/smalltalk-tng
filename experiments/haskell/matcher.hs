module Matcher where

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified List
import qualified Maybe
import Debug.Trace

type Env = [(Bool, String, Value)]

data AST = AstAtom String
         | AstBinding String AST
         | AstDiscard
         | AstObject [(AST, AST)]
         | AstLet String AST
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
            <|> do string "+"; i <- ident; readBinding i
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
showAST (AstObject clauses) = "[" ++ showClauses clauses ++ "]"
showAST (AstLet s v) = s ++ "=" ++ show v
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
      AstObject clauses -> VObject $ map evalClause clauses
          where evalClause (patexp, val) = (pat, maybeClose pat bs val)
                    where pat = toPattern $ eval bs patexp
      AstLet s v -> result
          where result = eval bs' v
                bs' = (True, s, result) : bs
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

infix 4 <:      -- accepts strictly more values
infix 4 <=:     -- either <: or =:
infix 4 =:      -- is essentially "the same" pattern

(=:) = patEqv
(<:) = stricter
p1 <=: p2 = (p1 <: p2) || (p1 =: p2)

p1 `overlaps` p2 = (p1 <: p2) || (p1 =: p2) || (p2 <: p1)

patEqv (PAtom s1) (PAtom s2) = s1 == s2
patEqv (PDiscard) (PDiscard) = True
patEqv (PBinding s p1) p2 = patEqv p1 p2
patEqv p1 (PBinding s p2) = patEqv p1 p2
patEqv (PObject c1) (PObject c2) = clausesMatchBy clauseEqv c1 c2 && clausesMatchBy clauseEqv c2 c1
patEqv _ _ = False

clauseEqv (v1, p1) (v2, p2) = (toPattern v1 `patEqv` toPattern v2) && (p1 `patEqv` p2)

tracev s v = trace (s ++ ": " ++ show v) v
tracev2 s f v1 v2 = let r = f v1 v2 in trace (s ++ ": " ++ show ((v1, v2), r)) r

clausesMatchBy pred c1 c2 = null $ remaining
    where remaining = foldl removeClauses c2 c1
          removeClauses cs c = filter (not . pred c) cs

data Failure = Failure { lhs :: Pattern, rhs :: Pattern, expected :: Bool, got :: Bool }
               deriving (Show)
strictFailures = Maybe.mapMaybe fails tests
    where fails (lhs, rhs, expected) = let result = (lhs <:: rhs) in
                                       if result == expected
                                       then Nothing
                                       else Just $ Failure { lhs = ePat lhs,
                                                             rhs = ePat rhs,
                                                             expected = expected,
                                                             got = result }
          tests = [("cons _ (cons _ _)", "cons (cons _ _) _", False),
                   ("cons _ (cons _ _)", "cons _ _", True),
                   ("cons _ _", "cons _ (cons _ _)", False),
                   ("[]", "[]", False),
                   ("[]", "cons _ _", False),
                   ("cons _ _", "cons _ _", False),
                   ("cons +a +b", "cons +c +d", False),
                   ("[First: _]", "[First: _ Rest: _]", False),
                   ("[A: _ B: _]", "[B: _ A: _]", False),
                   ("[A: _ B: _]", "[A: _]", True),
                   ("[A: _ B: _]", "[C: _]", False),
                   ("[A: _ B: _]", "[C: _ A: _]", False),
                   ("[First: _ Rest: _]", "[First: _]", True)]

stricter (PBinding s p1) p2 = stricter p1 p2
stricter p1 (PBinding s p2) = stricter p1 p2
stricter a b | a == b = False
stricter _ (PDiscard) = True
stricter (PObject c1) (PObject c2) = any (surviveAfterRemoving c2) c1 &&
                                     not (any (surviveAfterRemoving c1) c2)
    where surviveAfterRemoving clausesToRemove clause =
              not $ any (`clauseStricterOrEqv` clause) clausesToRemove
stricter _ _ = False

clauseStricterOrEqvToAnyOf clauses clause = any (clauseStricterOrEqv clause) clauses
clauseStricterOrEqv c1 c2 = (c1 `clauseStricter` c2) || (c1 `clauseEqv` c2)

clauseStricter (v1, p1) (v2, p2) = ((v1 `valEqv` v2) && (p1 `stricter` p2)) ||
                                   ((v1 `coStricter` v2) && ((p1 `stricter` p2) ||
                                                             (p1 `patEqv` p2)))
    where v1 `coStricter` v2 = (toPattern v2 `stricter` toPattern v1)
          v1 `valEqv` v2 = (toPattern v1 `patEqv` toPattern v2)

infix 4 <::
infix 4 <=::
infix 4 =::

ePat exp = toPattern $ eval' exp
a <:: b = (ePat a) <: (ePat b)
a <=:: b = (ePat a) <=: (ePat b)
a =:: b = (ePat a) =: (ePat b)

---------------------------------------------------------------------------

eval' exp = eval [] (readTng exp)

baseEnv = [ def "cons" "[+car: [+cdr: [First: car Rest: cdr]]]"
          , def "map" "[+f: loop=[(cons +a +d): (cons (f a) (loop d)) +x:x]]"
          ]
    where def nm exp = (False, nm, eval' exp)
