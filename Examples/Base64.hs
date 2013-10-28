{-# LANGUAGE OverloadedStrings #-}

module Examples.Base64(base64) where

import Development.NSIS
import Development.NSIS.Plugins.Base64


base64 = do
    name "base64"
    allowRootDirInstall True
    outFile "base64.exe"
    caption "Base64 test"
    showInstDetails Show
    installDir "$EXEDIR"
    requestExecutionLevel User
    addPluginDir "."

    page Directory
    page InstFiles

    section "" [] $ do
        setOutPath "$INSTDIR"
        let src = "Hello NSIS Plugin!"
        enc <- constant_ $ encrypt src
        dec <- constant_ $ decrypt enc
        alert $ "Source: " & src & "\nEncrypted: " & enc & "\nDecrypted: " & dec
