{-# OPTIONS_GHC -Wall #-}
module Parse where

import Text.ParserCombinators.Parsec
   
import Syntax

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
var  = do {v  <- identifier; 
        me <- option Nothing (do {e<-brackets expr; return (Just e)}) ;
        return (v,me) }

factor, term, expr :: Parser Expr
factor  = parens expr  <|> do {v <- lexem decimal; return (Const v)} 
       <|> do {v <- var; return (VarOp v)}  <?> "factor"

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
varDef = do {v  <- identifier;  mi <- option Nothing 
                (do {i<- brackets dimension; return (Just i)}) ;
             return (v,mi) }  

program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseProgram :: String -> Program
parseProgram s = case parse program "" s  of
              {Left er -> error (show er); Right p -> p}

--parseSPL :: String -> Either String Program
parseSPL :: String -> Program
parseSPL s = case parse program "" s  of
              {Left er -> error (show er); Right p -> p}

