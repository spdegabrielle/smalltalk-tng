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
-- Here's a piece of (pseudo-)code from test3.tng:
--
-- {<.s:empty>, <.s:empty> -> .bothEmpty;
--           _, <.s:empty> -> .secondEmpty;
--  <.s:empty>,          _ -> .firstEmpty;
--           _,          _ -> .neitherEmpty}
--
-- There are a couple of interesting things here. Our rule about never
-- backtracking means that some of the clauses, as written, are
-- unreachable. Also, the presence of tuples *containing*
-- message-pattern-matches could be awkward.
--
-- Let's pull out as much shared structure as possible between
-- neighbouring clauses - here, it's the single outermost duple - and
-- put fresh pattern variables in the slots where neighbours
-- vary. Then reinject the pattern variables *in left to right order*
-- using currying rather than structure. If we do things that way,
-- then the last two clauses above are unreachable because of the
-- discard in the left cell of the tuple pattern in the second clause.
--
-- {(t1, t2) -> {<.s:empty> -> {<.s:empty> -> .bothEmpty};
--                        _ -> {<.s:empty> -> .secondEmpty};
--               "No other rules - the remaining clauses are unreachable!"}
--                    t1 t2}
--
-- Simplest might be just to plain not implement message-patterns
-- yet. After all, any message-pattern can be easily rewritten away
-- into cases - less convenient, but certainly clearer:
--
-- {<.a(x)> -> x;
--  <.b(y)> -> y}
--
-- becomes
--
-- {value -> value {.a(x) -> x;
--                  .b(y) -> y}}
--
-- which actually can also be written
--
-- <{.a(x) -> x;
--   .b(y) -> y}>
--
-- Hmm!
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
         | TupleExp [Exp]
         | LitExp Literal
         | Ref Varname
         | Self
         | Super

data Pat = Discard
         | LitPat Literal
         | Def Varname Pat
         | TuplePat [Pat]

type ClosureClause = (Environment, Pat, Exp)

data Val = Closure [ClosureClause]
         | LitVal Literal
         | TupleVal [Val]

---------------------------------------------------------------------------
-- Evaluator

eval env self super = eval'
    where eval' (Clause p e) = Closure [(env, p, e)]
          eval' (Send e1 e2) = send (eval' e1) (eval' e2)
          eval' (Extend e1 e2) = extendClosure (eval' e1) (eval' e2)
          eval' (TupleExp es) = TupleVal $ map eval' es
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
match (TuplePat ps) (TupleVal vs)
    | length ps /= length vs = Nothing
    | otherwise = foldr accumulate (Just []) $ zip ps vs
    where accumulate _ Nothing = Nothing
          accumulate (p, v) (Just rightBindings) =
              maybe Nothing (\inner -> Just (inner ++ rightBindings)) (match p v)
match _ _ = Nothing

extendClosure (Closure c1) (Closure c2) = Closure (c1 ++ c2)
extendClosure c@(Closure _) _ = c
extendClosure _ any = any

doesnotunderstand receiver message = error $ "DNU:" ++ show (receiver, message)
unboundvariable v = error $ "Unbound:" ++ show v

---------------------------------------------------------------------------
-- Reasoning

freeIn (Clause p e) = freeIn e \\ boundIn p
freeIn (Send e1 e2) = freeIn e1 `union` freeIn e2
freeIn (Extend e1 e2) = freeIn e1 `union` freeIn e2
freeIn (TupleExp es) = foldl union [] $ map freeIn es
freeIn (LitExp _) = []
freeIn (Ref v) = [v]
freeIn Self = [] -- hmm.
freeIn Super = [] -- hmm.

boundIn Discard = []
boundIn (LitPat _) = []
boundIn (Def v p) = [v] `union` boundIn p
boundIn (TuplePat ps) = foldl union [] $ map boundIn ps -- duplicates?

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

mktup ctor [part] = part
mktup ctor parts = ctor parts

readExp = readExtend
readExtend = do (sub : supers) <- sepBy1 readTuple (punct "+"); return $ foldl Extend sub supers
readTuple = do parts <- sepBy1 readApp (punct ","); return $ mktup TupleExp parts
readApp = do (part : parts) <- sepBy1 readSimple whiteSpace; return $ foldl Send part parts
readSimple =     do punct "("; e <- readExp; punct ")"; return e
             <|> do punct "["; p <- readPat; punct "="; e <- readExp; punct "]"; return $ Clause p e
             <|> do punct "."; i <- ident; return $ LitExp $ SymLit i
             <|> do i <- natural; return $ LitExp $ IntLit i
             <|> try (do reserved "self"; return Self)
             <|> try (do reserved "super"; return Super)
             <|> do i <- ident; return $ Ref i

readPat = do parts <- sepBy1 readSimplePat (punct ","); return $ mktup TuplePat parts
readSimplePat =     do punct "("; p <- readPat; punct ")"; return p
                <|> do punct "_"; return Discard
                <|> do punct "."; i <- ident; return $ LitPat $ SymLit i
                <|> do i <- natural; return $ LitPat $ IntLit i
                <|> try (do i <- ident; punct "@"; p <- readSimplePat; return $ Def i p)
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
showExp (TupleExp es) = "(" ++ sepList "" ", " (map showExp es) ++ ")"
showExp (LitExp l) = show l
showExp (Ref v) = v
showExp Self = "self"
showExp Super = "super"

instance Show Pat where show v = showPat v
showPat Discard = "_"
showPat (LitPat l) = show l
showPat (Def v Discard) = v
showPat (Def v p) = v ++ "@" ++ show p
showPat (TuplePat ps) = "(" ++ sepList "" ", " (map showPat ps) ++ ")"

instance Show Val where show v = showVal v
showVal (Closure clauses) = "[" ++ sepList "" ", " (map showClause clauses) ++ "]"
showVal (LitVal l) = show l
showVal (TupleVal vs) = "(" ++ sepList "" ", " (map showVal vs) ++ ")"

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
