module CoreETng where

---------------------------------------------------------------------------
-- If you don't want to support *groups* of clauses natively, then
-- rewrite them to share a binding for self/super.
--
--    [.x=.result]
--  + [.y=.foo, .b=self.y]
--  + [vv=[1=vv]]
--
-- becomes
--
--    [.x=.result]
--  + [msg = [xself = [xsuper = [.y=.foo] + [.b=xself.y] + [other=xsuper other]]] self super msg]
--  + [vv=[1=vv]]
---------------------------------------------------------------------------
-- It's clear that messages (<exp1 exp2 ...>) can be rewritten into
-- regular closures (let val1 = exp1 in let val2 = exp2 in
-- ... {receiver -> receiver val1 val2 ...}). Message patterns can be
-- rewritten, too:
--
--   [<pat1 pat2 ...> = exp]
--
-- becomes (using {} for closure-clauses, as for the full eTNG)
--
--   [msg -> msg {pat1 -> {pat2 -> {... -> exp}}}]
--
-- Note there's no default clause {_ -> super msg} here. That's up to
-- the user. We don't automatically backtrack.
--
-- Of course, adjacent clauses in a group need to be rewritten
-- carefully:
--
-- [.x 1 -> .a;
--  .x 2 -> .b;
--  <.c 1> -> .z;
--  <.c 2> -> .w;
--  v -> v]
--
-- becomes
--
-- [.x -> {1 -> .a;
--         2 -> .b};
--  msg -> msg {.c -> {1 -> .z;     "where msg is fresh"
--                     2 -> .w};
--              _ -> [v -> v] msg}]
--
-- Eww. I don't think that last catch-all line is a good idea. It
-- should probably be illegal to to have multiple catch-all clauses in
-- a group that can't be merged - so either the <.c ...>'s or the v->v
-- would need to be removed to make that legal. This decision is in
-- keeping with the don't-backtrack principle. Assuming we remove the
-- v->v from the example, the result becomes
--
-- [.x -> {1 -> .a;
--         2 -> .b};
--  msg -> msg {.c -> {1 -> .z;   "where msg is fresh"
--                     2 -> .w}}]
--
-- All this rewriting means that the debug-information used to report
-- DNU may need to be a set of character ranges in disjoint pieces of
-- code, since there'll be no single coherent place we can point to
-- and say "this is the receiver of the failed message"!
---------------------------------------------------------------------------

import qualified Maybe
import List

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

data Literal = SymLit String
             | IntLit Integer
               deriving Eq

type Varname = String

type Environment = [(Varname, Val)]

data Exp = Clause Pat Exp
         | Send Exp Exp
         | Extend Exp Exp
         | LitExp Literal
         | Ref Varname
         | Self
         | Super

data Pat = Discard
         | LitPat Literal
         | Def Varname Pat

type ClosureClause = (Environment, Pat, Exp)

data Val = Closure [ClosureClause]
         | LitVal Literal

---------------------------------------------------------------------------
-- Evaluator

eval env self super = eval'
    where eval' (Clause p e) = Closure [(env, p, e)]
          eval' (Send e1 e2) = send (eval' e1) (eval' e2)
          eval' (Extend e1 e2) = extendClosure (eval' e1) (eval' e2)
          eval' (LitExp l) = LitVal l
          eval' (Ref v) = maybe (unboundvariable v) id (lookup v env)
          eval' Self = self
          eval' Super = super

send receiver@(Closure clauses) message = search clauses
    where search [] = doesnotunderstand receiver message
          search ((env, pat, body):rest) = maybe (search rest) deliver (match pat message)
              where deliver bindings = eval (bindings ++ env) receiver (Closure rest) body
send receiver message = error $ "PrimitiveReceiver:" ++ show (receiver, message)

match Discard _ = Just []
match (LitPat l1) (LitVal l2)
    | l1 == l2 = Just []
    | otherwise = Nothing
match (Def v p) val = maybe Nothing (\inner -> Just ((v,val):inner)) (match p val)
match _ _ = Nothing

extendClosure (LitVal _) c = c
extendClosure c (LitVal _) = c
extendClosure (Closure c1) (Closure c2) = Closure (c1 ++ c2)

doesnotunderstand receiver message = error $ "DNU:" ++ show (receiver, message)
unboundvariable v = error $ "Unbound:" ++ show v

---------------------------------------------------------------------------
-- Reasoning

freeIn (Clause p e) = freeIn e \\ boundIn p
freeIn (Send e1 e2) = freeIn e1 `union` freeIn e2
freeIn (Extend e1 e2) = freeIn e1 `union` freeIn e2
freeIn (LitExp _) = []
freeIn (Ref v) = [v]
freeIn Self = [] -- hmm.
freeIn Super = [] -- hmm.

boundIn Discard = []
boundIn (LitPat _) = []
boundIn (Def v p) = [v] `union` boundIn p

---------------------------------------------------------------------------
-- Reader

tngDef = P.LanguageDef
         { P.commentStart = "\""
         , P.commentEnd = "\""
         , P.commentLine = ""
         , P.nestedComments = False
         , P.identStart = letter
         , P.identLetter = alphaNum
         , P.opStart = (oneOf ":!#$%&*+./<=>?@\\^|-~")
         , P.opLetter = (oneOf ":!#$%&*+./<=>?@\\^|-~" <|> alphaNum)
         , P.reservedNames = ["self", "super"]
         , P.reservedOpNames = []
         , P.caseSensitive = True
         }

tngTokenizer = P.makeTokenParser tngDef

reserved = P.reserved tngTokenizer
whiteSpace = P.whiteSpace tngTokenizer
ident = P.identifier tngTokenizer
natural = P.natural tngTokenizer
punct s = do string s; whiteSpace; return ()
operator = P.operator tngTokenizer
comma = P.comma tngTokenizer

readExp = readExtend
readExtend = do (sub : supers) <- sepBy1 readApp (punct "+"); return $ foldl Extend sub supers
readApp = do (part : parts) <- sepBy1 readSimple whiteSpace; return $ foldl Send part parts
readSimple =     do punct "("; e <- readExp; punct ")"; return e
             <|> do punct "["; p <- readPat; punct "="; e <- readExp; punct "]"; return $ Clause p e
             <|> do punct "."; i <- ident; return $ LitExp $ SymLit i
             <|> do i <- natural; return $ LitExp $ IntLit i
             <|> try (do reserved "self"; return Self)
             <|> try (do reserved "super"; return Super)
             <|> do i <- ident; return $ Ref i

readPat =     do punct "_"; return Discard
          <|> do punct "."; i <- ident; return $ LitPat $ SymLit i
          <|> do i <- natural; return $ LitPat $ IntLit i
          <|> try (do i <- ident; punct "@"; p <- readPat; return $ Def i p)
          <|> do i <- ident; return $ Def i Discard

parseExpFromString = parse (do whiteSpace; v <- readExp; eof; return v) ""

read s = case parseExpFromString s of
           Right v -> v
           Left err -> error $ "Parse error: " ++ show err

---------------------------------------------------------------------------
-- Writer

sepList t s [] = ""
sepList t s [x] = x ++ t
sepList t s (x:xs) = x ++ s ++ sepList t s xs

instance Show Exp where show v = showExp v
showExp (Clause p e) = "[" ++ show p ++ "=" ++ show e ++ "]"
showExp (Send e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
showExp (Extend e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
showExp (LitExp l) = show l
showExp (Ref v) = v
showExp Self = "self"
showExp Super = "super"

instance Show Pat where show v = showPat v
showPat Discard = "_"
showPat (LitPat l) = show l
showPat (Def v Discard) = v
showPat (Def v p) = v ++ "@" ++ show p

instance Show Val where show v = showVal v
showVal (Closure clauses) = "[" ++ sepList "" ", " (map showClause clauses) ++ "]"
showVal (LitVal l) = show l

instance Show Literal where
    show (SymLit s) = "." ++ s
    show (IntLit i) = show i

showClause ([], p, e) = show p ++ "=" ++ show e
showClause (env, p, e) = sepList "::" ";" (Maybe.catMaybes $ map (showEnv e) env)
                         ++ show p ++ "=" ++ show e

showEnv exp (var, val) | var `elem` freeIn exp = Just $ var ++ "/" ++ show val
                       | otherwise = Nothing

---------------------------------------------------------------------------
-- Driver

evalTop = eval [] empty empty
    where empty = Closure []

readEval = evalTop . CoreETng.read

main = do putStr "> "
          input <- getLine
          putStrLn $ show $ readEval input
          main
