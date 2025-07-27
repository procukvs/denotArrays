{-# OPTIONS_GHC -Wall #-}
module DenotArFull where

import Text.ParserCombinators.Parsec
import Data.Maybe ( fromJust )

-- Абстракний синтаксис і типи, що використовуються: Work, Sctp, EnvSt, Env
type Var  = (String, Maybe Expr)
data Expr = VarOp Var  | Const Integer   | BinOp Op Expr Expr  
             deriving (Show, Eq)
data Op =  Plus | Minus | Times | Div | Mod  
             deriving (Show, Eq)
type VarDef = (String, Maybe Int)             
data Stmt = Assign Var Expr  | Read Var |  Write Expr | If Expr Stmt 
          | While Expr Stmt  | Block [VarDef] [Stmt]        
             deriving (Show, Eq)
type Program = Stmt

type Work = ([Integer], [Maybe Integer], [Integer])
data Sctp  = Ar | Sc deriving (Show, Eq)
type EnvSt = [(String, Sctp)]
type Env = [(String,(Maybe Int, Int))]

-- Синтаксичні аналізатори і Конкретний синтаксис БНФ в коментарях
{- лексика  
  symbol = ';' | '{' | '}' | '(' | ')' | ','
  addOp  = '+' | '_' ;
  mulOp  = '*' | '/' | '%' ;
  identifier = letter , {(digit | letter)} ;  
             крім 'int' 'if' 'while' 'read' 'write' 
  decimal = digit , {digit} ;
  letter  = 'A' | ... | 'Z' | 'a' | ... | 'z' ;
  digit   = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ; 
  identif =  char {digit | char}
  reserved= 'int' | 'read' | 'write' | 'if' | 'while' 
-}

identif :: Parser String
identif = do {c <- letter; cs <- many alphaNum; return (c:cs)}

lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}      

identifier :: Parser String
identifier = try (do {nm <- lexem identif;
      if (any(nm==) ["int","read","write","if","while"])
       then unexpected ("reserved word " ++ show nm) else return nm} ) 

reserved :: String -> Parser ()
reserved st = try( lexem (do {_ <- string st; notFollowedBy alphaNum})) 

symbol :: String ->  Parser ()
symbol st = lexem (do {_ <- string st; return ()})

{- вирази 
  var    = identifier , [ '[' , expr , ']' ];
  expr   = term , {addOp , term} ;
  term   = factor , {mulOp , factor} ;
  factor = decimal | '(' , expr , ')' | var;
-}

oper  :: String -> Op -> Parser (Expr -> Expr -> Expr)
oper str bop = do {symbol str; return (BinOp bop)} 

mulOp, addOp :: Parser (Expr -> Expr -> Expr)   
mulOp = (oper "*" Times) <|> (oper "/" Div) <|> (oper "%" Mod)
addOp = (oper "+" Plus) <|> (oper "-" Minus)

parens, brackets :: Parser a -> Parser a
parens p = do {symbol "("; e <- p; symbol ")"; return e} 
brackets p = do {symbol "["; e <- p; symbol "]"; return e} 

decimal :: Parser Integer
decimal = do {ds <- many1 digit; return (read ds)} 

dimension :: Parser Int
dimension = do {ds <- many1 digit; return (read ds)} 

var  :: Parser Var 
var  = do v  <- identifier
          me <- option Nothing (do {e<-brackets expr; return (Just e)}) 
          return (v,me) 

factor, term, expr :: Parser Expr
factor  = parens expr  
       <|> do {v <- lexem decimal; return (Const v)} 
       <|> do {v <- var; return (VarOp v)} 
       <?> "factor"

term = chainl1 factor mulOp   
expr = chainl1 term addOp 

{- оператори
  program=  stmt ;
  stmt   = 'while' , '(' , expr , ')' , stmt ; 
         | 'if' , '(' , expr , ')' , stmt ;
         | 'read' , var  ; 
         | 'write' , expr ; 
         |  var , ':=' , expr ;
         | '{' , [defin] ,  stmt , {';' , stmt} , '}' ;
  defin  = 'int' , defvar , {',' , defvar} , ';' ;
  defvar = identifier , ['[' , decimal , ']'] ;
-}   

braces :: Parser a -> Parser a
braces p = do {symbol "{"; e <- p; symbol "}"; return e} 

semiSep1, commaSep1 :: Parser a -> Parser [a] 
semiSep1 p = sepBy p (symbol ";")
commaSep1 p = sepBy p (symbol ",")

stmt :: Parser Stmt 
stmt = do {reserved "while"; e <- parens expr; s <- stmt; return (While e s)}
   <|> do {reserved "if"; e <- parens expr; s <- stmt; return (If e s)}
   <|> do {reserved "read"; v <- var; return (Read v)}
   <|> do {reserved "write"; e <- expr; return (Write e)} 
   <|> do {v <- var; symbol ":="; e <- expr; return (Assign v e)}
   <|> braces (do {dll <- option [] defin; sl <- semiSep1 stmt; return (Block dll sl)})
   <?> "statement"
   
defin :: Parser [VarDef] 
defin = do {reserved "int"; il <- commaSep1 varDef; symbol ";"; return il}

varDef :: Parser VarDef 
varDef = do v  <- identifier
            mi <- option Nothing (do {i<- brackets dimension; return (Just i)}) 
            return (v,mi)   

program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseProgram :: String -> Program
parseProgram s = case parse program "" s  of
              {Left er -> error (show er); Right p -> p}

-- Контекстні умови
distinct :: Eq a => [a] -> Bool 
distinct []     = True 
distinct (v:vs) = notElem v vs && distinct vs 

iswfProgram :: Program -> Bool
iswfProgram pr = iswfStmt pr []
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

iswfExpr :: Expr -> EnvSt -> Maybe Sctp
iswfExpr (VarOp (v,Just e)) env  = 
  if (lookup v env == Just Ar) && (iswfExpr e env == Just Sc) then Just Sc else Nothing
iswfExpr (VarOp (v,Nothing)) env = lookup v env 
iswfExpr (Const _) _             = Just Sc
iswfExpr (BinOp _ e1 e2) env     =   
  if (iswfExpr e1 env == Just Sc) && (iswfExpr e2 env == Just Sc) then Just Sc else Nothing 

---- Семантичні функції
getLocation :: String -> Env -> (Int,Int)
getLocation s env = case fromJust $ lookup s env of 
           { ((Just n), k) -> (n,k); (Nothing,  k) -> (1,k) }   

getValue:: Int -> Work -> Integer
getValue k = \(_,stg,_) -> 
   let p = length stg - k in case stg!!p of
     {Just v  -> v; Nothing -> error "valueNothing"}

updValue :: Int ->  Integer -> Work -> Work
updValue k v  = \(inp,stg,out) ->
   let { p = length stg - k; (beg,end) = splitAt p stg; 
         stg1 = beg ++ [Just v] ++ (tail end)}
   in  (inp,stg1,out) 

allocate :: Int -> Work -> Work 
allocate k = \(inp,stg,out) -> 
   let beg = [Nothing | _ <- [1..k]] in (inp, beg++stg,out)

getBase :: Work -> Int 
getBase = \(_,stg,_) -> length stg 

free :: Int -> Work -> Work 
free t = \(inp,stg, out) -> (inp, drop t stg,out)

writeValue :: Integer -> Work -> Work 
writeValue v = \(inp, stg, out) -> (inp, stg, out ++ [v])

readInput :: Work -> Integer
readInput = \(inp,_,_) -> 
   if null inp then error "readInput" else head inp 

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
eLocation (s, Just ei) env = \w -> 
        let {i = fromIntegral (eExpr ei env w); (n,k) = getLocation s env}
        in if i>=0 && i<n then k+i else error "Index"
eLocation (s, Nothing) env = \_w -> let (_,k) = getLocation s env in k

eExpr :: Expr -> Env -> Work -> Integer
eExpr (VarOp var1) env      = \w -> getValue (eLocation var1 env w) w
eExpr (Const v) _          = \_ -> v 
eExpr (BinOp op e1 e2) env = \w -> applyBo op (eExpr e1 env w) (eExpr e2 env w) 

alloc :: Int -> (Maybe Int) -> Int 
alloc s mi = s+(maybe 1 id mi)

extEnv :: [VarDef] -> Int -> Env -> Env
extEnv vs b = \env -> 
      let {(ns,mi) = unzip vs; mls  = zip mi (scanl alloc 0 mi); 
            nenv1 = (zip ns [(m, (b+i+1)) | (m,i)<- mls]) ++ env }            
      in nenv1

iStmt :: Stmt -> Env -> Work -> Work 
iStmt (Assign var2 e) env = \w -> 
         updValue (eLocation var2 env w)(eExpr e env w) w   
iStmt (If e s) env       = \w ->      
         if eExpr e env w > 0 then iStmt s env w else w
iStmt wh@(While e s) env = \w -> 
         if eExpr e env w > 0 then iStmt wh env (iStmt s env w) else w 
iStmt (Block vs sts) env = \w -> 
         let {k = foldl alloc 0 (map snd vs); w1 = allocate k w;   
              nenv = extEnv vs (getBase w) env;
              w2 = foldl (\wr s -> iStmt s nenv wr) w1 sts}               
         in free k w2
iStmt (Read vr) env     = \w -> 
         let {v = readInput w;
              w1 = updValue (eLocation vr env w) v  w }     
         in dropInput w1
iStmt (Write e) env      = \w -> writeValue (eExpr e env w) w

iProgram :: Program -> [Integer] -> [Integer]
iProgram prog ix = 
        let {w = (ix, [],[]); (_,_,ox) = iStmt prog [] w} in ox

-- Інтерпретація
interpret :: String -> [Integer] -> [Integer]
interpret st ix = let {pr = parseProgram st; wf = iswfProgram pr}
                  in if wf then iProgram pr ix  else error "Contex"

interpretFile :: String -> [Integer] -> IO()  
interpretFile sf ix = do {st <- readFile sf; print (interpret st ix)} 
