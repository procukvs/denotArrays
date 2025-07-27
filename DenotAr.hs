{-# OPTIONS_GHC -Wall #-}

module DenotAr where

import Semantics 
import Parse
import Contex
import Syntax

-- only in ghci :for testing 
interpret :: String -> [Integer] -> [Integer]
interpret st ix = let {pr = parseProgram st; wf = iswfProgram pr}
                  in if wf then iProgram pr ix  else error "Contex"

interpretFile :: String -> [Integer] -> IO()  
interpretFile sf ix = do {st <- readFile sf; print (interpret st ix)} 
