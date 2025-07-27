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



{-


--data ParseOpt = ParserO | LibraryO deriving (Eq, Show) 
--data SemanOpt = WorkO | StateO | ApplicO deriving (Eq, Show)

-- Програми -------------------------------------------
{- Вводить два цілі значення b і e, 
   якщо вони додатні, то в змінній out
   обчислення значення b в степені e, 
   в інших випадках значення змінної out=0. 
   Значення out виводиться

   { int b, e, out;  
     read b; read e; out:= 0;
	   if (b)
       if (e)
	     {int i; i:=0; out := 1; 
		    while (e-i) {out := out*b; i := i+1}; }; 
     write out  
   }
-}
power :: Program
power = Block [("b",Nothing),("e",Nothing),("out",Nothing)]
              [ Read ("b",Nothing), Read ("e",Nothing)
              , Assign ("out",Nothing) (Const 0)
              , If (VarOp ("b",Nothing)) 
                  (If (VarOp ("e",Nothing))
                    (Block [("i",Nothing)]
                        [ Assign ("i",Nothing) (Const 0)
                        , Assign ("out",Nothing) (Const 1)
                        , While (BinOp Minus (VarOp ("e",Nothing)) (VarOp ("i",Nothing)))
                           (Block [] 
                                  [ Assign ("out",Nothing) (BinOp Times (VarOp ("out",Nothing)) (VarOp ("b",Nothing)))
                                  , Assign ("i",Nothing) (BinOp Plus (VarOp ("i",Nothing)) (Const 1))])
                        ]
                     )
                  )
              , Write (VarOp ("out",Nothing))
              ]

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

bubbleAr :: Program
bubbleAr = Block [("i",Nothing),("a",Just 10)]
                 [ Assign ("i",Nothing) (Const 0)
                 , While (BinOp Minus (Const 10) (VarOp ("i",Nothing)))
                     (Block [] 
                            [ Read ("a", Just (VarOp ("i",Nothing)))
                            , Assign ("i",Nothing) (BinOp Plus (VarOp ("i",Nothing)) (Const 1))
                            ])
                 , Block [("is",Nothing),("c",Nothing)]
                         [ Assign ("is",Nothing) (Const 1)
                         , While (VarOp ("is",Nothing)) 
                            (Block [] 
                                   [ Assign ("is",Nothing) (Const 0)
                                   , Assign ("i",Nothing) (Const 0)
                                   , While (BinOp Minus (Const 9) (VarOp ("i",Nothing)))
                                      (Block [] 
                                             [ If (BinOp Minus (VarOp ("a",Just (VarOp ("i",Nothing))))
                                                               (VarOp ("a",Just (BinOp Plus (VarOp ("i",Nothing)) (Const 1)))))
                                                 (Block []
                                                        [ Assign ("is",Nothing) (Const 1)  
                                                        , Assign ("c",Nothing) (VarOp ("a",Just (BinOp Plus (VarOp ("i",Nothing)) (Const 1)))) 
                                                        , Assign ("a",Just (BinOp Plus (VarOp ("i",Nothing))(Const 1))) (VarOp ("a",Just (VarOp ("i",Nothing))))
                                                        , Assign ("a",Just (VarOp ("i",Nothing))) (VarOp ("c",Nothing))
                                                        ])
                                             , Assign ("i",Nothing) (BinOp Plus (VarOp ("i",Nothing)) (Const 1))    
                                             ])
                                   ])
                         ]
                 , Assign ("i",Nothing) (Const 0) 
                 , While (BinOp Minus (Const 10) (VarOp ("i",Nothing)))
                     (Block [] 
                            [ Write (VarOp ("a", Just (VarOp ("i",Nothing))))
                            ,  Assign ("i",Nothing) (BinOp Plus (VarOp ("i",Nothing)) (Const 1))
                            ])         
                 ]
 
{- Вводить ціле значення in,
   якщо воно невід"ємне, то обчислює відповідне число 
   Фібонначі в змінній out і виводить його.
   При від"ємному in виводиться 0.
   
   {int in, out; 
    read in; out := 0; 
	if (in+1)                            -- in>=0 
      {int f0, f1, c; 
	   f0 := 1; f1 := 1; out := 1;
       if (in-1)                       -- in>1
         { c := 1; while (in-c) 
		   {out := f0 + f1; f0 := f1; f1 := out; c := c+1}
		 } 
	 };
    write out	 
  }
-}
{-
fibonacci :: Program
fibonacci = 
    Block ["in", "out"]
          [ Read "in",  Assign "out" (Const 0)
          , If (BinOp Plus (Var "in") (Const 1)) 
               (Block ["f0", "f1", "c"]
                      [Assign "f0" (Const  1), Assign "f1" (Const 1), Assign "out" (Const 1),
                       If (BinOp Minus (Var "in") (Const 1))
                         (Block [] [ Assign "c" (Const 1)
                                   , While (BinOp Minus (Var "in") (Var "c")) 
                                       (Block []
                                              [ Assign "out" (BinOp Plus (Var "f0") (Var "f1"))
                                              , Assign "f0" (Var "f1")
                                              , Assign "f1" (Var "out")
                                              , Assign "c" (BinOp Plus (Var "c") (Const 1))
                                              ]
                                       )
                                   ]
                         )
                     ]
               )
          , Write (Var "out")
          ]
-}
powerTest :: String
powerTest =
   "{ int b, e, out; \n\
   \  read b; read e; out:= 0;\n\
   \  if (b) \n \
   \    if (e)\n \
   \     {int i; i:=0; out := 1;\n\
   \      while (e-i) {out := out*b; i := i+1} }; \n\
   \  write out \n\
   \}"

squareRootTest :: String
squareRootTest =
   "{int a, b;\n\
   \ read a; a:= a+1; b := 0;\n\
   \ if (a) \n\
   \    while(a-b*b) b:= b+1;\n\
   \  write (b-1)\n\
   \ }"

fibonacciTest :: String
fibonacciTest = 
 " {int in, out; read in; out := 0; \n\
   \if (in+1){int f0, f1,c; \n\
   \           f0 := 1; f1 := 1; out := 1; \n\
   \           if(in-1) \n \
   \              { c := 1; while (in-c)  \n\
   \                  {out := f0 + f1; f0 := f1; f1 := out; c := c+1}\n\
   \              }\n\
   \          }; \n\
   \write out \n\
  \}" 

{-
program=  stmt ;
stmt   = 'while' , '(' , expr , ')' , stmt ; 
       | 'if' , '(' , expr , ')' , stmt ;
       | 'read' , var ; 
       | 'write' , expr ; 
       | var , ':=' , expr ;
       | '{' , [defin] ,  stmt , {';' , stmt} , '}' ;
defin  = 'int' , defvar , {',' , defvar} , ';' ;
defvar = identifier , ['[' , decimal , ']']
expr   = term , {addOp , term} ;
term   = factor , {mulOp , factor} ;
factor = decimal | '(' , expr , ')' | var;
var    = identifier , [ '[' , expr , ']' ];
addOp  = '+' | '_' ;
mulOp  = '*' | '/' | '%' ;
identifier = letter , {(digit | letter)} ;  
             крім 'int' 'if' 'while' 'read' 'write' 
decimal = digit , {digit} ;
letter  = 'A' | ... | 'Z' | 'a' | ... | 'z' ;
digit   = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ; 
-}
{-
data WorkSp = WorkSp { input :: [Integer]
                     , memory :: [(String,Integer)]
                     , output :: [Integer]}
-}
-}