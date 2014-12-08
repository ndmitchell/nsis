{-# LANGUAGE OverloadedStrings #-}

module Examples.WinMessages(winmessages) where

import Development.NSIS
import Development.NSIS.Plugins.WinMessages


winmessages = do
    name "winmessages"
    outFile "winmessages.exe"
    requestExecutionLevel User

    section "" [] $ do
        wnd <- findWindow "#32770" "" (Just hwndParent)
        ctl <- getDlgItem wnd 1027
        sendMessage [] ctl wm_SETTEXT (0 :: Exp Int) ("STR:MyText" :: Exp String)
        return ()

{-
Section
    FindWindow $0 '#32770' '' $HWNDPARENT
    GetDlgItem $1 $0 1027
    SendMessage $1 ${WM_SETTEXT} 0 'STR:MyText'
SectionEnd
-}
