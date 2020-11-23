
import Control.Exception.Extra
import System.Process.Extra
import System.Info
import Control.Monad

main =
    when (os == "linux") $ do
        retry 3 $ system_ "sudo apt-get install nsis"
        system_ "cabal test --test-option=--build"
