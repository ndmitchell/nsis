
import Control.Exception.Extra
import System.Process.Extra

main = do
    retry 3 $ system_ "sudo apt-get install nsis"
    system_ "cabal test --test-option=--build"
