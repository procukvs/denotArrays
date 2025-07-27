{-# OPTIONS_GHC -Wall #-}
module Semantics where 

import Data.Maybe ( fromJust )
import Syntax 


-- int a - скалярна = з ним зв"язано (a,(Nothing,3)) stg[3] => містить значення a
-- int b[4] - масив = з ним зв2язано (b,(Just 4,7)) stg[7] => містить значення b[0]
--                     stg[4] => b[3], stg[5] => b[2], stg[6] => b[1]

getLocation :: String -> Env -> (Int,Int)
getLocation s env = case fromJust $ lookup s env of 
                     ((Just n), k) -> (n,k)              -- Just k 
                     (Nothing,  k) -> (1,k)    

getValue:: Int -> Work -> Integer
getValue k = \(_,stg,_) -> 
    let p = length stg - k
    in case stg!!p of
         Just v  -> v 
         Nothing -> error "valueNothing"

updValue :: Int ->  Integer -> Work -> Work
updValue k v  = \(inp,stg,out) ->
     let p = length stg - k
         (beg,end) = splitAt p stg 
         stg1 = beg ++ [Just v] ++ (tail end)
     in  (inp,stg1,out) 

allocate :: Int -> Work -> Work 
allocate k = \(inp,stg,out) -> 
    let beg = [Nothing | _ <- [1..k]]
    in (inp, beg++stg,out)

getBase :: Work -> Int 
getBase = \(_,stg,_) -> length stg 

free :: Int -> Work -> Work 
free t = \(inp,stg, out) -> (inp, drop t stg,out)

writeValue :: Integer -> Work -> Work 
writeValue v = \(inp, stg, out) -> (inp, stg, out ++ [v])

readInput :: Work -> Integer
readInput = \(inp,_,_) -> if null inp then error "readInput" else head inp 

dropInput :: Work -> Work 
dropInput = \(inp,stg,out) -> (tail inp,stg,out)

applyBo :: Op -> Integer -> Integer -> Integer 
applyBo Plus v1 v2  = v1 + v2  
applyBo Minus v1 v2 = v1 - v2
applyBo Times v1 v2 = v1 * v2
applyBo Div v1 v2   = if v2 /= 0 then div v1 v2 else error "DivOnZero"
applyBo Mod v1 v2   = if v2 /= 0 then mod v1 v2 else error "ModOnZero" 

-------------------------------------

eLocation :: Var -> Env -> Work -> Int
eLocation (s, Just ei) env = 
          \w -> let i     = fromIntegral (eExpr ei env w) 
                    (n,k) = getLocation s env
                in if i>=0 && i<n then k+i else error "Index"
                       --in if i>0 && i<=n then k+i else error "Index"
eLocation (s, Nothing) env = \_w -> let (_,k) = getLocation s env in k

eExpr :: Expr -> Env -> Work -> Integer
eExpr (VarOp var) env      = \w -> getValue (eLocation var env w) w
eExpr (Const v) _          = \_ -> v 
eExpr (BinOp op e1 e2) env = \w -> applyBo op (eExpr e1 env w) (eExpr e2 env w) 

alloc :: Int -> (Maybe Int) -> Int 
alloc s mi = s+(maybe 1 id mi)
--alloc s (Just t) = s+t 
--alloc s Nothing  = s+1

extEnv :: [VarDef] -> Int -> Env -> Env
extEnv vs b = \env -> let (ns,mi) = unzip vs
                          mls  = zip mi (scanl alloc 0 mi) 
                          nenv1 = (zip ns [(m, (b+i+1)) | (m,i)<- mls]) ++ env             
                      in nenv1

iStmt :: Stmt -> Env -> Work -> Work 
iStmt (Assign var e) env = \w -> updValue (eLocation var env w)(eExpr e env w) w   -- updDValue var (eExpr e env w) env w 
iStmt (If e s) env       = \w -> if eExpr e env w > 0 then iStmt s env w else w
iStmt wh@(While e s) env = \w -> if eExpr e env w > 0 then iStmt wh env (iStmt s env w) else w 
iStmt (Block vs sts) env 
             = \w -> let k     = foldl alloc 0 (map snd vs) 
                         w1    = allocate k w   
                         nenv = extEnv vs (getBase w) env
                         w2 = foldl (\wr s -> iStmt s nenv wr) w1 sts               
                     in free k w2
iStmt (Read var) env     = \w -> let v = readInput w
                                     w1 = updValue (eLocation var env w) v  w     -- updDValue var v env w     
                                 in dropInput w1
iStmt (Write e) env      = \w -> writeValue (eExpr e env w) w

iProgram :: Program -> [Integer] -> [Integer]
iProgram prog ix = let w = (ix, [],[])          
                       (_,_,ox) = iStmt prog [] w  
                   in ox

{-
getValue:: String -> Work -> Integer
getValue s = \(_,mem,_) -> case lookup s mem of
                              Just v  -> v 
                              Nothing -> error "getValue"

updValue :: String -> Integer -> Work -> Work
updValue s v = \(inp,mem,out) -> (inp, update mem s v, out)
  where update :: [(String, Integer)] -> String -> Integer ->  [(String, Integer)]
        update [] _ _           = error "updValue" 
        update ((x,_):sx) s1 v1 | x==s1 = (x,v1): sx
        update (x:sx) s1 v1     = x:(update sx s1 v1) 


extMemory :: [String] -> Work -> Work 
extMemory vs = \(inp,mem,out) -> (inp,[(v,0)|v<-vs]++mem,out)
-}