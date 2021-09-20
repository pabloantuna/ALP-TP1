module Eval2
  ( eval,
    State,
  )
where

import AST
import qualified Data.Map.Strict as M
import Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case s M.!? v of
  Nothing -> Left UndefVar
  Just x -> Right x

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s = Right (Skip :!: s)
stepComm (Let v i) s = do
  valor :!: estadonuevo <- evalExp i s
  return (Skip :!: update v valor estadonuevo)
stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = do
  c1' :!: s' <- stepComm c1 s
  return (Seq c1' c2 :!: s')
stepComm (IfThenElse b c1 c2) s = do
  (vcond :!: s') <- evalExp b s
  return (if vcond then c1 :!: s' else c2 :!: s')
stepComm (Repeat c b) s = Right (Seq c (IfThenElse b Skip (Repeat c b)) :!: s)

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const x) s = Right (x :!: s)
evalExp (Var x) s = do
  v <- lookfor x s
  return (v :!: s)
evalExp (EAssgn x e) s = do
  v :!: s' <- evalExp e s
  return (v :!: update x v s')
evalExp (ESeq e1 e2) s = do
  _ :!: s' <- evalExp e1 s
  evalExp e2 s'
evalExp (UMinus e) s = do
  v :!: s' <- evalExp e s
  return (- v :!: s')
evalExp (Plus e1 e2) s = opBinPuro e1 e2 (+) s
evalExp (Minus e1 e2) s = opBinPuro e1 e2 (-) s
evalExp (Times e1 e2) s = opBinPuro e1 e2 (*) s
evalExp (Div e1 e2) s = do
  (v1 :!: s') <- evalExp e1 s
  (v2 :!: s'') <- evalExp e2 s'
  if v2 == 0 then Left DivByZero else Right (v1 `div` v2 :!: s'')
evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)
evalExp (And e1 e2) s = opBinPuro e1 e2 (&&) s
evalExp (Or e1 e2) s = opBinPuro e1 e2 (||) s
evalExp (Not e) s = do
  (v :!: s') <- evalExp e s
  return (not v :!: s')
evalExp (Eq e1 e2) s = opBinPuro e1 e2 (==) s
evalExp (NEq e1 e2) s = opBinPuro e1 e2 (/=) s
evalExp (Lt e1 e2) s = opBinPuro e1 e2 (<) s
evalExp (Gt e1 e2) s = opBinPuro e1 e2 (>) s

opBinPuro :: Exp a -> Exp b -> (a -> b -> c) -> State -> Either Error (Pair c State)
opBinPuro e1 e2 f s = do
  (v1 :!: s') <- evalExp e1 s
  (v2 :!: s'') <- evalExp e2 s'
  return (f v1 v2 :!: s'')
