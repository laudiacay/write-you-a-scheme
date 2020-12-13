module Lib
    ( realMain 
    ) where

import System.Environment

realMain :: IO ()
realMain = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)
