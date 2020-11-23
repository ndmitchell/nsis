
import Control.Exception.Extra
import System.Process.Extra
import System.Info
import Control.Monad

main = do
    putStrLn $ "Only running tests on linux, OS = " ++ show os
    when (os == "linux") $ do
        retry 3 $ system_ "sudo apt-get install nsis"
        system_ "cabal test --test-option=--build"
