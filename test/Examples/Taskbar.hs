{-# LANGUAGE OverloadedStrings #-}

module Examples.Taskbar(taskbar) where

import Control.Monad
import Development.NSIS
import qualified Development.NSIS.Plugins.Taskbar as T


taskbar = do
    name "taskbar"
    allowRootDirInstall True
    outFile "taskbar.exe"
    caption "Taskbar test"
    showInstDetails Show
    installDir "$EXEDIR"
    requestExecutionLevel User
    addPluginDir "."
    T.taskbar

    page Directory
    page InstFiles

    section "" [] $
        replicateM_ 20 $ do
            sleep 100
            detailPrint "hello"
