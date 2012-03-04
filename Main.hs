
module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.Cmd
import System.Environment
import System.Exit

import Development.NSIS
import Examples.Example1
import Examples.Example2
import Examples.Primes


examples = let (*) = (,) in ["example1" * example1, "example2" * example2, "primes" * primes]


main = do
    args <- getArgs
    let (flags,names) = partition ("-" `isPrefixOf`) args
    names <- return $ concatMap (\x -> if x == "all" then map fst examples else [x]) names
    if null names then
        putStrLn $ "Type the name of an example: " ++ unwords (map fst examples)
     else do
        forM_ names $ \name -> do
            let script = fromMaybe (error $ "Unknown example: " ++ name) $ lookup name examples
            unless ("--nowrite" `elem` flags) $ writeFile (name ++ ".nsi") $ nsis script
            unless ("--nobuild" `elem` flags) $ do
                r <- system $ "\"C:/Program Files/NSIS/makensis.exe\" " ++ name ++ ".nsi"
                when (r /= ExitSuccess) $ error "NSIS FAILED"
            when ("--run" `elem` flags) $ do
                system $ name ++ ".exe"
                return ()
