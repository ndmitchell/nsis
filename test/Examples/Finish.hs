{-# LANGUAGE OverloadedStrings #-}

module Examples.Finish(finish) where

import Development.NSIS


finish = do
    name "Finish"
    outFile "finish.exe"
    requestExecutionLevel User
    installDir "$DESKTOP"

    page Directory
    page InstFiles
    page $ Finish finishOptions
        {finRun="$WINDIR/notepad.exe"
        ,finRunText="Run Notepad"
        ,finLink="http://google.com/"
        ,finLinkText="Visit Google!"
        }

    section "" [] $ return ()
