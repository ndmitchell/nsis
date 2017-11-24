
module Main(main) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Process
import System.Directory
import System.Environment
import System.Exit

import Development.NSIS
import Examples.Base64
import Examples.Example1
import Examples.Example2
import Examples.Finish
import Examples.Primes
import Examples.Radio
import Examples.Taskbar
import Examples.WinMessages
import Examples.EnvVarUpdate


examples = let (*) = (,) in
    ["example1" * void example1, "example2" * void example2, "base64" * void base64
    ,"finish" * void finish, "primes" * void primes, "radio" * void radio, "taskbar" * void taskbar
    ,"winmessages" * void winmessages, "envvarupdate" * void envvarupdate]


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
            ,"  --build    Do build"
            ,"  --run      Run the result"
            ]
        exitSuccess
    when (null args) $ do
        putStrLn "*****************************************************************"
        putStrLn "** Running nsis test suite, run with '--help' to see arguments **"
        putStrLn "*****************************************************************"
    names <- return $ if null names then map fst examples else names

    b <- findExecutable "makensis"
    let build | "--build" `elem` flags = True
              | "--nobuild" `elem` flags = False
              | otherwise = isJust b

    forM_ names $ \name -> do
        let script = fromMaybe (error $ "Unknown example: " ++ name) $ lookup name examples
        unless ("--nowrite" `elem` flags) $ writeFile (name ++ ".nsi") $ nsis script
        when build $ do
            r <- system $ "makensis -V3 " ++ name ++ ".nsi"
            when (r /= ExitSuccess) $ error "NSIS FAILED"
        when ("--run" `elem` flags) $ do
            system $ name ++ ".exe"
            return ()
    when (isNothing b) $
        putStrLn "Warning: No nsis on the PATH, files were not built"
