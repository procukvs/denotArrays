{-# OPTIONS_GHC -Wall #-}

module DenotAr where

--import Text.Parsec.String  
--import Syntax 
import Semantics 
import Parse
import Contex

-- only in ghci :for testing 
interpret :: String -> [Integer] -> [Integer]
interpret st ix = let {pr = parseProgram st; wf = iswfProgram pr}
                  in if wf then iProgram pr ix  else error "Contex"

interpretFile :: String -> [Integer] -> IO()  
interpretFile sf ix = do {st <- readFile sf; print (interpret st ix)} 
{-
parseFile :: String -> IO(Program)
parseFile s = do {res <- parseFromFile program s; 
                  case res of 
                    {Left er -> error (show er); Right p -> return p} } 

interpretFile1 :: String -> [Integer] -> IO()  
interpretFile1 sf ix = do pr <- parseFile sf
                         if iswfProgram pr 
                            then print (iProgram pr ix)
                            else print "error Contex"   

                         --print (iProgram pr ix)
-}