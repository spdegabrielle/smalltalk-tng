import System.IO
import SimpleSexp
import Text.Show.Functions
import Data.Set as Set
import Data.List as List
import Control.Monad.State

type Handler = Immediate -> [AbsVal] -> PE AbsVal
type Symbol = String

data Immediate = A Atom
               | P Symbol Handler
                 deriving Show

data AST = Lit Immediate
         | Ref Symbol
         | If AST AST AST
         | Lambda [Symbol] AST
         | Apply AST [AST]
         | Bind Symbol AST AST
         -- | Letrec [Symbol] [AST] AST
           deriving Show

data AbsVal = Unknown Symbol
            | Runtime Symbol Description
            | Compiletime Description
              deriving Show

data Description = Simple Immediate
                 | Pair AbsVal AbsVal
                 | Closure [Symbol] AST Environment
                   deriving Show

type Environment = [(Symbol, AbsVal)]

type Era = [(Symbol, (AST, AbsVal))]
type History = [Era]

type PEState = (Integer, History)
type PE a = State PEState a

data MaybeKnown = NotKnown Symbol
                | Known Description

freeNames (Lit v) = Set.empty
freeNames (Ref id) = Set.singleton id
freeNames (If test true false) = Set.unions $ List.map freeNames [test, true, false]
freeNames (Lambda formals body) = Set.difference (freeNames body) (Set.fromList formals)
freeNames (Apply rator rands) = Set.unions $ List.map freeNames (rator : rands)
freeNames (Bind name init body) = Set.union (freeNames init) (Set.delete name (freeNames body))

getHistory :: PE History
getHistory = do
  (_, hs) <- get
  return hs

nextId :: String -> PE Symbol
nextId base = do
  (n, hs) <- get
  put (n + 1, hs)
  return $ base ++ show n

emit :: String -> AST -> (Symbol -> PE AbsVal) -> PE AbsVal
emit base ast gen_av = do
  id <- nextId base
  av <- gen_av id
  (n, (h : hs)) <- get
  put (n, ((id, (ast, av)) : h) : hs)
  return av

lookupEnv' :: Environment -> History -> Symbol -> AbsVal
lookupEnv' env hs id = case lookup id env of
                         Nothing -> do
                           case lookup id (concat hs) of
                             Nothing -> error $ "Unbound variable: " ++ id
                             Just (ast, av) -> av
                         Just av -> av

lookupEnv :: Environment -> Symbol -> PE AbsVal
lookupEnv env id = do hs <- getHistory
                      return $ lookupEnv' env hs id

maybeKnown :: AbsVal -> MaybeKnown
maybeKnown (Unknown id) = NotKnown id
maybeKnown (Runtime id d) = Known d
maybeKnown (Compiletime d) = Known d

pushHistory :: PE ()
pushHistory = do
  (n, hs) <- get
  put (n, [] : hs)

popHistory :: PE Era
popHistory = do
  (n, h : hs) <- get
  put (n, hs)
  return h

codegen :: PE AbsVal -> PE AST
codegen block = do
  pushHistory
  av <- block
  h <- popHistory
  return $ wrapEra h (codegenAbsVal av)

wrapEra :: Era -> AST -> AST
wrapEra [] body = body
wrapEra ((id, (ast, av)) : h) (Ref ref) | id == ref = wrapEra h ast
wrapEra ((id, (ast, av)) : h) body = wrapEra h (Bind id ast body)

codegenAbsVal :: AbsVal -> AST
codegenAbsVal (Unknown id) = Ref id
codegenAbsVal (Runtime id _) = Ref id
codegenAbsVal (Compiletime d) = codegenDescription d

codegenDescription :: Description -> AST
codegenDescription (Simple i) = Lit i
codegenDescription (Pair a d) = Apply (Lit primCons) $ List.map codegenAbsVal [a, d]
codegenDescription (Closure formals body env) = Lambda formals body

primCons = P "cons" primConsHandler
primConsHandler self [a, d] = emit "pair"
                                (Apply (Lit self) $ List.map codegenAbsVal [a, d])
                                (\pairid -> return $ Runtime pairid $ Pair a d)

pe :: Environment -> AST -> PE AbsVal
pe env (Lit v) = return $ Compiletime $ Simple v
pe env (Ref id) = lookupEnv env id
pe env (If test true false) = do
  testv <- pe env test
  case maybeKnown testv of
    NotKnown testid -> do t <- codegen $ pe env true
                          f <- codegen $ pe env false
                          emit "if" (If (Ref testid) t f) (return . Unknown)
    Known (Simple (A (Int 0))) -> pe env false
    Known _ -> pe env true
pe env p@(Lambda formals body) = do
  hs <- getHistory
  let cloenv = [(id, lookupEnv' env hs id) | id <- Set.toList $ freeNames p, boundIn env id]
  b <- codegen $ pe (bindFormals cloenv formals) body
  let clo = Closure formals b cloenv
  emit "lam" (codegenDescription clo) (\lamid -> return $ Runtime lamid clo)
pe env (Apply rator rands) = do
  ratorv <- pe env rator
  randsv <- mapM (pe env) rands
  case maybeKnown ratorv of
    NotKnown ratorid ->
        emit "app" (Apply (Ref ratorid) (List.map codegenAbsVal randsv)) (return . Unknown)
    Known (Closure formals body cloenv) ->
        pe (bindActuals env formals randsv) body
    Known (Simple p@(P name handler)) ->
        handler p randsv
    Known other -> error ("Cannot apply non-procedure: " ++ show other)
pe env (Bind formal init body) = do
  initv <- pe env init
  pe (bindActuals env [formal] [initv]) body

boundIn :: Environment -> Symbol -> Bool
boundIn env id = case lookup id env of
                   Nothing -> False
                   Just _ -> True

bindFormals :: Environment -> [Symbol] -> Environment
bindFormals env formals = [(f, Unknown f) | f <- formals] ++ env

bindActuals :: Environment -> [Symbol] -> [AbsVal] -> Environment
bindActuals env formals actuals = zip formals actuals ++ env

partialEval :: Environment -> AST -> AST
partialEval env term =
    let (ast, (_n, [])) = runState (codegen $ pe env term) (0, []) in ast

reconstruct :: AST -> Sexp
reconstruct (Lit (A a)) = Atom a
reconstruct (Lit (P name _)) = Atom (Symbol $ "#%" ++ name)
reconstruct (Ref id) = Atom (Symbol id)
reconstruct (If test true false) = List [Atom (Symbol "if"),
                                         reconstruct test,
                                         reconstruct true,
                                         reconstruct false]
reconstruct (Lambda formals body) = List [Atom (Symbol "lambda"),
                                          List $ List.map (Atom . Symbol) formals,
                                          reconstruct body]
reconstruct (Apply rator rands) = List ([reconstruct rator] ++ List.map reconstruct rands)
reconstruct (Bind name init body) = reconstructBinds [(name, init)] body

reconstructBinds :: [(Symbol, AST)] -> AST -> Sexp
reconstructBinds bs (Bind name init body) = reconstructBinds ((name, init) : bs) body
reconstructBinds bs body =
    List [Atom (Symbol "let*"),
          List [List [Atom (Symbol name), reconstruct init] | (name, init) <- reverse bs],
          reconstruct body]

parse (Atom (Symbol s)) = Ref s
parse (List [Atom (Symbol "quote"), Atom e]) = Lit (A e)
parse (List [Atom (Symbol "if"), test, true, false]) = If (parse test) (parse true) (parse false)
parse (List (Atom (Symbol "lambda") : List formals : es)) =
    Lambda (List.map parseSym formals) (parseSeq es)
parse (List (Atom (Symbol "let") : List bindings : es)) =
    Apply (Lambda (List.map bindingName bindings) (parseSeq es))
          (List.map (parse . bindingInit) bindings)
parse (List (Atom (Symbol "let*") : List bindings : es)) =
    List.foldl (\ e b -> Bind (bindingName b) (parse $ bindingInit b) e) (parseSeq es) bindings
parse (List (rator : rands)) = Apply (parse rator) (List.map parse rands)

parseSeq [e] = parse e
parseSeq (e:es) = Apply (Lambda ["__ ignored __"] (parseSeq es)) [parse e]

parseSym (Atom (Symbol s)) = s

bindingName (List [Atom (Symbol n), e]) = n
bindingInit (List [Atom (Symbol n), e]) = e

main = do
  let exampleSource = "(lambda (bb) ((lambda (k b) (k (lambda () b))) (lambda (f) (f)) (bb)))"
  let Right [exampleSexp] = readSexps exampleSource
  let example = parse exampleSexp
  -- let example = Lambda ["bb"] $ Apply (Lambda ["k", "b"] $ Apply (Ref "k") [Lambda [] (Ref "b")])
  --               [Lambda ["f"] (Ref "f"),
  --                Apply (Ref "bb") []]
  putStrLn $ showSexp $ reconstruct $ partialEval [] example