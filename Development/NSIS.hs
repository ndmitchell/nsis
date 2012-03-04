{-# LANGUAGE OverloadedStrings #-}

-- | NSIS (Nullsoft Scriptable Install System, <http://nsis.sourceforge.net/>) is a tool that allows programmers
--   to create installers for Windows.
--   This library provides an alternative syntax for NSIS scripts, as an embedded Haskell language, removing much
--   of the hard work in developing an install script. Simple NSIS installers should look mostly the same, complex ones should
--   be significantly more maintainable.
--
--   For examples, see the Examples source directory.
--
--   Much of the documentation from the Installer section is taken straight from the NSIS documentation.
module Development.NSIS
    (
    -- * Core types
    nsis, nsisNoOptimise, Action, Exp, Value,
    -- * Scripting
    -- ** Variables
    share, scope, constant, constant_, mutable, mutable_, (@=),
    -- ** Typed variables
    mutableInt, constantInt, mutableInt_, constantInt_, mutableStr, constantStr, mutableStr_, constantStr_,
    -- ** Control Flow
    iff, iff_, while, loop, onError,
    (?), (%&&), (%||),
    Label, newLabel, label, goto,
    -- ** Expressions
    (%==), (%/=), (%<=), (%<), (%>=), (%>),
    true, false, not_,
    strRead, strShow,
    (&), strConcat, strLength, strTake, strDrop, strReplace, strIsPrefixOf, strUnlines,
    -- ** File system manipulation
    FileHandle, fileOpen, fileWrite, fileClose, withFile', writeFile', writeFileLines,
    rmdir, delete, 
    getFileTime, fileExists, findEach, findOnce,
    createDirectory, createShortcut,
    -- ** Registry manipulation
    readRegStr, deleteRegKey, writeRegStr, writeRegDWORD,
    -- ** Process execution
    exec,
    -- * Installer
    -- ** Global installer options
    name, outFile, installDir, setCompressor,
    installIcon, uninstallIcon, headerImage,
    installDirRegKey, allowRootDirInstall, caption, showInstDetails, showUninstDetails,
    -- ** Sections
    SectionId, section, sectionGroup, newSectionId, sectionSetText, sectionGetText, uninstall, page, unpage,
    -- ** Section commands
    file, alwaysNonFatal, writeUninstaller, alert, setOutPath, messageBox, requestExecutionLevel,
    hideProgress, detailPrint,
    -- * Settings
    Compressor(..), HKEY(..), MessageBoxType(..), Attrib(..), Page(..), Level(..), Visibility(..),
    FileMode(..)
    ) where

import Development.NSIS.Sugar
import Development.NSIS.Show
import Development.NSIS.Optimise
import Development.NSIS.Library


-- | Create the contents of an NSIS script from an installer specification.
nsis :: Action () -> String
nsis = unlines . showNSIS . optimise . runAction


-- | Like 'nsis', but don't try and optimise the resulting NSIS script. Useful
--   to figure out how the underlying installer works, or if you believe the
--   optimisations are introducing bugs (but please do report any such bugs!).
nsisNoOptimise :: Action () -> String
nsisNoOptimise = unlines . showNSIS . runAction
