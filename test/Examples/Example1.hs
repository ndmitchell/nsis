{-# LANGUAGE OverloadedStrings #-}

module Examples.Example1(example1) where

import Development.NSIS


-- Based on example1.nsi from NSIS
--
-- This script is perhaps one of the simplest NSIs you can make. All of the
-- optional settings are left to their default settings. The installer simply 
-- prompts the user asking them where to install, and drops a copy of example1.hs
-- there. 
example1 = do
    
    -- The name of the installer
    name "Example1"

    -- The file to write
    outFile "example1.exe"

    -- The default installation directory
    installDir "$DESKTOP/Example1"

    -- Request application privileges for Windows Vista
    requestExecutionLevel User

    ---------------------------------

    -- Pages
    
    page Directory
    page InstFiles

    ---------------------------------

    -- The stuff to install
    section "" [] $ do -- No components page, name is not important

        -- Set output path to the installation directory.
        setOutPath "$INSTDIR"

        -- Put file there
        file [] "test/Examples/Example1.hs"
