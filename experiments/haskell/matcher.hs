module Matcher where

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified List
import qualified Maybe
import Debug.Trace

type Env = [(Bool, String, Value)]

data AST = AstAtom String
         | AstLiteral Literal
         | AstBinding String AST
         | AstDiscard
         | AstObject [(AST, AST)]
         | AstLet String AST
         | AstApp AST AST
           deriving (Eq, Ord)

data Literal = LitInt Integer
               deriving (Eq, Ord)

data Value = VAtom String
           | VLiteral Literal
           | VBinding String Value
           | VDiscard
           | VObject [(Value, Closure)]
           | VPrimitive Primitive
             deriving (Eq, Ord)

data Primitive = Primitive String (Value -> Value)

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
            <|> do i <- integer; return $ AstLiteral $ LitInt i
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
showAST (AstLiteral l) = show l
showAST (AstBinding s AstDiscard) = "+" ++ s
showAST (AstBinding s v) = "+" ++ s ++ "@" ++ show v
showAST (AstDiscard) = "_"
showAST (AstObject clauses) = "[" ++ showClauses clauses ++ "]"
showAST (AstLet s v) = s ++ "=" ++ show v
showAST (AstApp v1 v2) = "(" ++ show v1 ++ " " ++ show v2 ++ ")"

instance Show Literal where
    show l = showLiteral l

showLiteral (LitInt i) = show i

instance Show Value where
    show v = showValue v

showValue (VAtom s) = s
showValue (VLiteral l) = show l
showValue (VBinding s VDiscard) = "+" ++ s
showValue (VBinding s v) = "+" ++ s ++ "@" ++ show v
showValue (VDiscard) = "_"
showValue (VObject clauses) = "[" ++ showClauses clauses ++ "]"
showValue (VPrimitive p) = show p

instance Eq Primitive where
    (Primitive n1 _) == (Primitive n2 _) = n1 == n2

instance Ord Primitive where
    (Primitive n1 _) `compare` (Primitive n2 _) = n1 `compare` n2

instance Show Primitive where
    show (Primitive name _) = "#<" ++ name ++ ">"

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

eLookup s [] defval = defval
eLookup s ((_, n, v):bs) defval = if n == s then v else eLookup s bs defval
lookupVal s bs = eLookup s bs (eLookup s baseEnv (VAtom s))

-- match pattern value -> maybe bindings
match (VAtom a) (VAtom b) = if a == b then Just [] else Nothing
match (VLiteral a) (VLiteral b) = if a == b then Just [] else Nothing
match (VBinding n p) v = do bs <- match p v; return ((False, n, v) : bs)
match (VDiscard) v = Just []
match (VObject patternClauses) (VObject valueClauses) =
    foldr bindingUnion (Just []) $ map (match1 valueClauses) patternClauses
        where bindingUnion j1 j2 = do b1 <- j1; b2 <- j2; return (b1 ++ b2)
match (VPrimitive (Primitive n1 _)) (VPrimitive (Primitive n2 _)) = if n1 == n2 then Just [] else Nothing
match _ _ = Nothing

firstThat p [] = Nothing
firstThat p (x:xs) = case p x of
                       Nothing -> firstThat p xs
                       j -> j

match1 valueClauses (pval, ppat) =
    firstThat firstMatch valueClauses
    where firstMatch (vpat, vval) =
              do bs' <- match vpat pval
                 bs'' <- match (forcePattern ppat) (reduce vval bs')
                 return bs''

forcePattern clo = reduce clo []

reduce (Closure env v) bs = eval (bs ++ env) v
reduce (Constant v) bs = v

eval bs o =
    case o of
      AstAtom s -> lookupVal s bs
      AstLiteral l -> VLiteral l
      AstBinding s v -> VBinding s (eval bs v)
      AstDiscard -> VDiscard
      AstObject clauses -> VObject $ map evalClause clauses
          where evalClause (patexp, val) = (pat, maybeClose pat bs val)
                    where pat = eval bs patexp
      AstLet s v -> result
          where result = eval bs' v
                bs' = (True, s, result) : bs
      AstApp rator rand -> applyTng bs (eval bs rator) (eval bs rand)

maybeClose pat bs o =
    case patBound pat of
      [] -> Constant $ eval bs o
      _ -> Closure bs o

patBound (VAtom s) = []
patBound (VLiteral l) = []
patBound (VBinding s p) = [s] ++ patBound p
patBound (VDiscard) = []
patBound (VObject clauses) = concatMap clauseBound clauses
    where clauseBound (_, clo) = patBound $ forcePattern clo
patBound (VPrimitive (Primitive n _)) = []

dnu function value = error $ "DNU: " ++ show function ++ " " ++ show value

applyTng bs function@(VObject patternClauses) value =
    case firstThat matches patternClauses of
      Nothing -> dnu function value
      Just result -> result
    where matches (ppat, pval) = case match ppat value of
                                   Nothing -> Nothing
                                   Just bs' -> Just $ reduce pval bs'
applyTng bs function@(VPrimitive (Primitive _ f)) value = f value
applyTng bs function value = dnu function value

---------------------------------------------------------------------------

infix 4 <:      -- accepts strictly more values
infix 4 <=:     -- either <: or =:
infix 4 =:      -- is essentially "the same" pattern

(=:) = patEqv
(<:) = stricter
p1 <=: p2 = (p1 <: p2) || (p1 =: p2)

p1 `overlaps` p2 = (p1 <: p2) || (p1 =: p2) || (p2 <: p1)

patEqv (VAtom s1) (VAtom s2) = s1 == s2
patEqv (VLiteral l1) (VLiteral l2) = l1 == l2
patEqv (VDiscard) (VDiscard) = True
patEqv (VBinding s p1) p2 = patEqv p1 p2
patEqv p1 (VBinding s p2) = patEqv p1 p2
patEqv (VObject c1) (VObject c2) = clausesMatchBy c1 c2 && clausesMatchBy c2 c1
patEqv (VPrimitive (Primitive n1 _)) (VPrimitive (Primitive n2 _)) = n1 == n2
patEqv _ _ = False

clauseEqv (v1, p1) (v2, p2) = (v1 `patEqv` v2) && (forcePattern p1 `patEqv` forcePattern p2)

clausesMatchBy c1 c2 = null $ remaining
    where remaining = foldl removeClauses c2 c1
          removeClauses cs c = filter (not . clauseEqv c) cs

stricter (VBinding s p1) p2 = stricter p1 p2
stricter p1 (VBinding s p2) = stricter p1 p2
stricter a b | a == b = False
stricter _ (VDiscard) = True
stricter (VObject c1) (VObject c2) = any (surviveAfterRemoving c2) c1 &&
                                     not (any (surviveAfterRemoving c1) c2)
    where surviveAfterRemoving clausesToRemove clause =
              not $ any (`clauseStricterOrEqv` clause) clausesToRemove
stricter _ _ = False

clauseStricterOrEqv c1 c2 = (c1 `clauseStricter` c2) || (c1 `clauseEqv` c2)

clauseStricter (v1, p1) (v2, p2) = ((v1 `valEqv` v2) && (p1' `stricter` p2')) ||
                                   ((v1 `coStricter` v2) && ((p1' `stricter` p2') ||
                                                             (p1' `patEqv` p2')))
    where v1 `coStricter` v2 = (v2 `stricter` v1)
          v1 `valEqv` v2 = (v1 `patEqv` v2)
          p1' = forcePattern p1
          p2' = forcePattern p2

---------------------------------------------------------------------------

infix 4 <::
infix 4 <=::
infix 4 =::

a <:: b = (eval' a) <: (eval' b)
a <=:: b = (eval' a) <=: (eval' b)
a =:: b = (eval' a) =: (eval' b)

data Failure = Failure { lhs :: Value, rhs :: Value, expected :: Bool, got :: Bool }
               deriving (Show)
strictFailures = Maybe.mapMaybe fails tests
    where fails (lhs, rhs, expected) = let result = (lhs <:: rhs) in
                                       if result == expected
                                       then Nothing
                                       else Just $ Failure { lhs = eval' lhs,
                                                             rhs = eval' rhs,
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

---------------------------------------------------------------------------

eval' exp = eval [] (readTng exp)

baseEnv = [ def "cons" "[+car: [+cdr: [First: car Rest: cdr]]]"
          , def "map" "[+f: loop=[(cons +a +d): (cons (f a) (loop d)) Nil:Nil]]"
          , defPrim "add" $ \(VLiteral (LitInt i)) -> p $ \(VLiteral (LitInt j)) -> VLiteral (LitInt (i + j))
          ]
    where def nm exp = def' nm $ eval' exp
          def' nm val = (False, nm, val)
          p fn = VPrimitive $ Primitive "(anon)" fn
          defPrim nm fn = def' nm $ VPrimitive $ Primitive nm fn
