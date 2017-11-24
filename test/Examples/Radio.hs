{-# LANGUAGE OverloadedStrings #-}

module Examples.Radio(radio) where

import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate
import Development.NSIS.Plugins.Sections


radio = do
    name "Radio"
    outFile "radio.exe"
    installDir "$DESKTOP/Radio"
    requestExecutionLevel User

    -- page Directory
    page Components
    page InstFiles

    section "Core files" [Required] $ do
        setOutPath "$INSTDIR"
        file [] "test/Examples/Radio.hs"

    local <- section "Add to user %PATH%" [] $ do
        setEnvVarPrepend HKCU "PATH" "$INSTDIR"
    global <- section "Add to system %PATH%" [] $ do
        setEnvVarPrepend HKLM "PATH" "$INSTDIR"
    atMostOneSection [local,global]

    a <- section "I like Marmite" [] $ return ()
    b <- section "I hate Marmite" [] $ return ()
    c <- section "I don't care" [] $ return ()
    exactlyOneSection [a,b,c]
