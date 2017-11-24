{-# LANGUAGE OverloadedStrings #-}

module Examples.Example2(example2) where

import Development.NSIS

-- Based on example2.nsi from NSIS
--
-- This script is based on example1.nsi, but it remember the directory, 
-- has uninstall support and (optionally) installs start menu shortcuts.
--
-- It will install example2.nsi into a directory that the user selects,

----------------------------------

example2 = do
    -- The name of the installer
    _ <- constantStr "Ex" "2"

    name "Example$Ex"

    -- The file to write
    outFile "example$Ex.exe"

    -- The default installation directory
    installDir "$PROGRAMFILES64/Example2"

    -- Registry key to check for directory (so if you install again, it will 
    -- overwrite the old one automatically)
    installDirRegKey HKLM "Software/NSIS_Example2" "Install_Dir"

    -- Request application privileges for Windows Vista
    requestExecutionLevel Admin

    -- Inject a literal setting that's not currently supported by the DSL
    unsafeInjectGlobal "# ignore me (could be an injected literal)"

    ----------------------------------

    -- Pages

    page Components
    page Directory
    page InstFiles

    unpage Confirm
    unpage InstFiles

    ----------------------------------

    -- The stuff to install
    section "Example2 (required)" [Required] $ do

        -- Set output path to the installation directory.
        setOutPath "$INSTDIR"

        -- Put file there
        file [] "test/Examples/Example$Ex.hs"

        -- Inject a non-global literal setting
        unsafeInject "# ignore me (could be an injected literal)"

        -- Write the installation path into the registry
        writeRegStr HKLM "SOFTWARE/NSIS_Example2" "Install_Dir" "$INSTDIR"

        -- Write the uninstall keys for Windows
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Example2" "DisplayName" "NSIS Example2"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Example2" "UninstallString" "\"$INSTDIR/uninstall.exe\""
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Example2" "NoModify" 1
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Example2" "NoRepair" 1
        writeUninstaller "uninstall.exe"

    -- Optional section (can be disabled by the user)
    section "Start Menu Shortcuts" [] $ do

        createDirectory "$SMPROGRAMS/Example2"
        createShortcut "$SMPROGRAMS/Example2/Uninstall.lnk" [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
        createShortcut "$SMPROGRAMS/Example2/Example2 (MakeNSISW).lnk" [Target "$INSTDIR/example2.nsi", IconFile "$INSTDIR/example2.nsi", IconIndex 0]

    ----------------------------------

    -- Uninstaller

    uninstall $ do

        -- Remove registry keys
        deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Example2"
        deleteRegKey HKLM "SOFTWARE/NSIS_Example2"

        -- Remove files and uninstaller
        delete [] "$INSTDIR/example2.nsi"
        delete [] "$INSTDIR/uninstall.exe"

        -- Remove shortcuts, if any
        delete [] "$SMPROGRAMS/Example2/*.*"

        -- Remove directories used
        rmdir [] "$SMPROGRAMS/Example2"
        rmdir [] "$INSTDIR"
