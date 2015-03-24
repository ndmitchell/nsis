{-# LANGUAGE OverloadedStrings #-}

module Examples.Radio(radio) where

import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate


radio = do
    name "Radio"
    outFile "radio.exe"
    installDir "$DESKTOP/Radio"
    requestExecutionLevel User

    -- page Directory
    page Components
    page InstFiles

    -- which index is currently selected
    isLocal <- mutable_ true
    local <- newSectionId
    global <- newSectionId

    section "Core files" [Required] $ do
        setOutPath "$INSTDIR"
        file [] "Examples/Radio.hs"

    section "Add to user %PATH%" [Id local] $ do
        setEnvVarPrepend HKCU "PATH" "$INSTDIR"
    section "Add to system %PATH%" [Unselected, Id global] $ do
        setEnvVarPrepend HKLM "PATH" "$INSTDIR"

    onSelChange $ do
        bLocal <- sectionGet local SF_Selected
        bGlobal <- sectionGet global SF_Selected
        iff isLocal
            (iff_ (sectionGet global SF_Selected) $ do
                isLocal @= false
                sectionSet local SF_Selected false)
            (iff_ (sectionGet local SF_Selected) $ do
                isLocal @= true
                sectionSet global SF_Selected false)
