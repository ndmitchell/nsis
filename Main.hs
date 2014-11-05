
module Main(main) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Process
import System.Environment
import System.Exit
import System.Info

import Development.NSIS
import Examples.Base64
import Examples.Example1
import Examples.Example2
import Examples.Finish
import Examples.Primes
import Examples.Taskbar


examples = let (*) = (,) in
    ["base64" * base64, "example1" * example1, "example2" * example2
    ,"finish" * finish, "primes" * primes, "taskbar" * taskbar]


main = do
    args <- getArgs
    let (flags,names) = partition ("-" `isPrefixOf`) args
    when ("--help" `elem` flags) $ do
        putStr $ unlines
            ["nsis-test [FLAGS] [EXAMPLES]"
            ,"Examples:"
            ,"  " ++ unwords (map fst examples)
            ,"Flags:"
            ,"  --help     Show this message"
            ,"  --nowrite  Don't write out the scripts"
            ,"  --nobuild  Don't build"
            ,"  --run      Run the result"
            ]
        exitSuccess
    when (null args) $ do
        putStrLn "*****************************************************************"
        putStrLn "** Running nsis test suite, run with '--help' to see arguments **"
        putStrLn "*****************************************************************"
    names <- return $ if null names then map fst examples else names
    forM_ names $ \name -> do
        let script = fromMaybe (error $ "Unknown example: " ++ name) $ lookup name examples
        unless ("--nowrite" `elem` flags) $ writeFile (name ++ ".nsi") $ nsis script
        unless ("--nobuild" `elem` flags) $
            if os == "mingw32" then do
                r <- system $ "makensis " ++ name ++ ".nsi"
                when (r /= ExitSuccess) $ error "NSIS FAILED"
            else
                putStrLn "Not building because not on Windows"
        when ("--run" `elem` flags) $ do
            system $ name ++ ".exe"
            return ()
