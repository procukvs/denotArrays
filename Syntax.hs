{-# OPTIONS_GHC -Wall #-}
module Syntax where 

--Одновимірні масиви індексуються як в Сі
-- масив a[4] має 4 елементи, які індексуються a[0], a[1], a[2], a[3]!!!
type Var  = (String, Maybe Expr)
data Expr = VarOp Var  | Const Integer   | BinOp Op Expr Expr  
             deriving (Show, Eq)
-- Бінарні (2-аргумента) оператори
data Op =  Plus | Minus | Times | Div | Mod  
             deriving (Show, Eq)
type VarDef = (String, Maybe Int)             
data Stmt = Assign Var Expr  | Read Var |  Write Expr | If Expr Stmt 
          | While Expr Stmt  | Block [VarDef] [Stmt]        
             deriving (Show, Eq)
--  В операторах if (iv) s і while (iv) s значення v>0 цілого виразу iv 
--                еквівалентно логічному значенню True 			 
type Program = Stmt

type Work = ([Integer], [Maybe Integer], [Integer])

data Sctp  = Ar | Sc deriving (Show, Eq)
type EnvSt = [(String, Sctp)]

type Env = [(String,(Maybe Int, Int))]

bubbleArTest :: String
bubbleArTest = 
 " { int i, a[10];  \n\
   \ \n\
   \  i := 0; while (10 - i) {  read a[i]; i:= i+1}; \n\
   \ { int is, c; is := 1; \n \
   \    while (is) { is := 0; i:= 0;  \n\
   \     while (9 - i) {\n\
   \          if (a[i]-a[i+1]) {\n\
   \           is:=1; c:=a[i+1]; a[i+1]:=a[i]; a[i]:=c }; \n\
   \          i:= i+1 } }\n\
   \ }; \n\
   \ i := 0; while (10 - i) {  write a[i]; i:= i+1} \n\
  \}" 

-- Програми -------------------------------------------

{- Вводить ціле масив а з 10 елементів,
   виконує його бульбашкове сортування  
   і виводить впорядкований масив a.   
   
   { int i, a[10]; 
     i := 0; while (10 - i) { i:= i+1; read a[i]};
	   { int is, c; is := 1; 
       while (is) { is := 0; i:= 1;
         while (10 - i) {
           if (a[i]-a[i+1]) {
             is:=1; c:=a[i+1]; a[i+1]:=a[i]; a[i]:=c }
	         i:= i+1 } }
     };  
     i := 0; while (10 - i) { i:= i+1; write a[i]}
    }  
-} 

