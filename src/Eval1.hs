module Eval1
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
lookfor :: Variable -> State -> Int
lookfor = flip (M.!)

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s
stepComm (Let v i) s = Skip :!: update v valor estadonuevo where valor :!: estadonuevo = evalExp i s
stepComm (Seq Skip c2) s = c2 :!: s
stepComm (Seq c1 c2) s = let c1' :!: s' = stepComm c1 s in Seq c1' c2 :!: s'
stepComm (IfThenElse b c1 c2) s = let (vcond :!: s') = evalExp b s in if vcond then c1 :!: s' else c2 :!: s'
stepComm (Repeat c b) s = Seq c (IfThenElse b Skip (Repeat c b)) :!: s

-- Evalua una expresion
-- Completar la definición

evalExp :: Exp a -> State -> Pair a State
evalExp (Const x) s = x :!: s
evalExp (Var x) s = lookfor x s :!: s
evalExp (EAssgn x e) s = let v :!: s' = evalExp e s in (v :!: update x v s')
evalExp (ESeq e1 e2) s = let _ :!: s' = evalExp e1 s in evalExp e2 s'
evalExp (UMinus e) s = let v :!: s' = evalExp e s in - v :!: s'
evalExp (Plus e1 e2) s = opBinPuro e1 e2 (+) s
evalExp (Minus e1 e2) s = opBinPuro e1 e2 (-) s
evalExp (Times e1 e2) s = opBinPuro e1 e2 (*) s
evalExp (Div e1 e2) s = opBinPuro e1 e2 div s
evalExp BTrue s = True :!: s
evalExp BFalse s = False :!: s
evalExp (And e1 e2) s = opBinPuro e1 e2 (&&) s
evalExp (Or e1 e2) s = opBinPuro e1 e2 (||) s
evalExp (Not e) s = let (v :!: s') = evalExp e s in not v :!: s'
evalExp (Eq e1 e2) s = opBinPuro e1 e2 (==) s
evalExp (NEq e1 e2) s = opBinPuro e1 e2 (/=) s
evalExp (Lt e1 e2) s = opBinPuro e1 e2 (<) s
evalExp (Gt e1 e2) s = opBinPuro e1 e2 (>) s

opBinPuro :: Exp a -> Exp b -> (a -> b -> c) -> State -> Pair c State
opBinPuro e1 e2 f s =
  let (v1 :!: s') = evalExp e1 s
      (v2 :!: s'') = evalExp e2 s'
   in f v1 v2 :!: s''
