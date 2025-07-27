{-# OPTIONS_GHC -Wall #-}
module Contex where 

import Data.Maybe()
import Syntax 

distinct :: Eq a => [a] -> Bool 
distinct []     = True 
distinct (v:vs) = notElem v vs && distinct vs 

iswfProgram :: Program -> Bool
iswfProgram pr = iswfStmt pr []
{-
iswfStmt :: Stmt -> [String] -> Bool 
iswfStmt (Assign var e) vs  = elem var vs && iswfExpr e vs 
iswfStmt (If e s) vs        = iswfExpr e vs && iswfStmt s vs
iswfStmt (While e s) vs     = iswfExpr e vs && iswfStmt s vs  
iswfStmt (Block vs1 sts) vs = distinct vs1 && all (flip iswfStmt (vs1++vs)) sts 
iswfStmt (Read var) vs      = elem var vs
iswfStmt (Write e) vs       = iswfExpr e vs
-}
{-
buildEs :: Maybe Int -> Sctp
--buildEs (Just _) = Ar
--buildEs Nothing  = Sc
buildEs mi = maybe Sc (\_ -> Ar) mi
-}
iswfStmt :: Stmt -> EnvSt -> Bool
iswfStmt (Block vdl sl) env = 
   (distinct (map fst vdl)) &&
   (let env1 = [(v,(maybe Sc (\_->Ar)) d) | (v,d) <- vdl] ++ env
    in all (flip iswfStmt env1) sl)
iswfStmt (Assign (v,Just ei) e) env = 
   (lookup v env == Just Ar) && (iswfExpr ei env == Just Sc) && (iswfExpr e env == Just Sc)
iswfStmt (Assign (v,Nothing) e) env = 
   (lookup v env == Just Sc) && (iswfExpr e env == Just Sc)  
iswfStmt (Read (v, Just ei)) env  = (lookup v env == Just Ar) && (iswfExpr ei env == Just Sc)
iswfStmt (Read (v, Nothing)) env  = (lookup v env == Just Sc)
iswfStmt (Write e) env            = (iswfExpr e env == Just Sc)
iswfStmt (If e s) env        = (iswfExpr e env == Just Sc) && iswfStmt s env
iswfStmt (While e s) env     = (iswfExpr e env == Just Sc) && iswfStmt s env

{-
iswfExpr :: Expr -> [String] -> Bool 
iswfExpr (Var s) vs         = elem s vs
iswfExpr (Const _) _        = True
iswfExpr (BinOp _ e1 e2) vs = iswfExpr e1 vs && iswfExpr e2 vs
-}

iswfExpr :: Expr -> EnvSt -> Maybe Sctp
iswfExpr (VarOp (v,Just e)) env  = 
  if (lookup v env == Just Ar) && (iswfExpr e env == Just Sc) then Just Sc else Nothing
iswfExpr (VarOp (v,Nothing)) env = lookup v env 
iswfExpr (Const _) _             = Just Sc
iswfExpr (BinOp _ e1 e2) env     =   
  if (iswfExpr e1 env == Just Sc) && (iswfExpr e2 env == Just Sc) then Just Sc else Nothing 
