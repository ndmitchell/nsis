
import Control.Exception.Extra
import System.Process.Extra
import System.Info.Extra
import Control.Monad

main =
    unless isMac $ do
        unless isWindows $
            retry 3 $ system_ "sudo apt-get install nsis"
        system_ "cabal test --test-option=--build"
