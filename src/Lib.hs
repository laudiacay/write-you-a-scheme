module Lib
    ( realMain 
    ) where

import System.Environment
import Parser

realMain :: IO ()
{--
realMain = do
    args <- getArgs
    putStrLn ("enter your name...")
    name <- getLine
    summed <- return (foldr (+) 0 (map read args))
    putStrLn ("Hello, " ++ name ++ ", your sum issss: " ++ show summed)
--}

realMain =  do
  args <- getArgs
  lisp_str <- readFile (args !! 0)
  mapM_ putStrLn (map readExpr (lines lisp_str))
