{-# LANGUAGE OverloadedStrings #-}

-- | NSIS (Nullsoft Scriptable Install System, <http://nsis.sourceforge.net/>) is a tool that allows programmers
--   to create installers for Windows.
--   This library provides an alternative syntax for NSIS scripts, as an embedded Haskell language, removing much
--   of the hard work in developing an install script. Simple NSIS installers should look mostly the same, complex ones should
--   be significantly more maintainable.
--
--   As a simple example of using this library:
--
-- @
--import "Development.NSIS"
--
--main = writeFile \"example1.nsi\" $ 'nsis' $ do
--     'name' \"Example1\"                  -- The name of the installer
--     'outFile' \"example1.exe\"           -- Where to produce the installer
--     'installDir' \"$DESKTOP/Example1\"   -- The default installation directory
--     'requestExecutionLevel' 'User'       -- Request application privileges for Windows Vista
--     -- Pages to display
--     'page' 'Directory'                   -- Pick where to install
--     'page' 'InstFiles'                   -- Give a progress bar while installing
--     -- Groups fo files to install
--     'section' \"\" [] $ do
--         'setOutPath' \"$INSTDIR\"        -- Where to install files in this section
--         'file' [] \"Example1.hs\"        -- File to put into this section
-- @
--
--   The file @example1.nsi@ can now be processed with @makensis@ to produce the installer @example1.exe@.
--   For more examples, see the @Examples@ source directory.
--
--   Much of the documentation from the Installer section is taken from the NSIS documentation.
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
    str, int, bool,
    (%==), (%/=), (%<=), (%<), (%>=), (%>),
    true, false, not_,
    strRead, strShow,
    (&), strConcat, strLength, strTake, strDrop, strReplace, strIsPrefixOf, strIsSuffixOf, strUnlines, strCheck,
    -- ** File system manipulation
    FileHandle, fileOpen, fileWrite, fileClose, withFile', writeFile', writeFileLines,
    rmdir, delete, copyFiles,
    getFileTime, fileExists, findEach,
    createDirectory, createShortcut,
    -- ** Registry manipulation
    readRegStr, deleteRegKey, deleteRegValue, writeRegStr, writeRegExpandStr, writeRegDWORD,
    -- ** Environment variables
    envVar,
    -- ** Process execution
    exec, execWait, execShell, sleep, abort,
    -- ** Windows
    HWND, hwndParent, findWindow, getDlgItem, sendMessage,
    -- ** Plugins
    plugin, push, pop, exp_,
    addPluginDir,
    -- * Installer
    -- ** Global installer options
    name, outFile, installDir, setCompressor,
    installIcon, uninstallIcon, headerImage,
    installDirRegKey, allowRootDirInstall, caption,
    showInstDetails, showUninstDetails, unicode,
    -- ** Sections
    SectionId, section, sectionGroup, newSectionId,
    sectionSetText, sectionGetText, sectionSet, sectionGet,
    uninstall, page, unpage, finishOptions,
    -- ** Events
    event, onSelChange,
    onPageShow, onPagePre, onPageLeave,
    -- ** Section commands
    file, alwaysNonFatal, writeUninstaller, alert, setOutPath, messageBox, requestExecutionLevel,
    hideProgress, detailPrint, setDetailsPrint,
    -- * Escape hatch
    unsafeInject, unsafeInjectGlobal,
    -- * Settings
    Compressor(..), HKEY(..), MessageBoxType(..), Attrib(..), Page(..), Level(..), Visibility(..),
    FileMode(..), SectionFlag(..), ShowWindow(..), FinishOptions(..), DetailsPrint(..)
    ) where

import Control.Monad
import Development.NSIS.Sugar
import Development.NSIS.Show
import Development.NSIS.Optimise
import Development.NSIS.Library


-- | Create the contents of an NSIS script from an installer specification.
--
-- Beware, 'unsafeInject' and 'unsafeInjectGlobal' may break 'nsis'. The
-- optimizer relies on invariants that may not hold when arbitrary lines are
-- injected. Consider using 'nsisNoOptimise' if problems arise.
nsis :: Action a -> String
nsis = unlines . showNSIS . optimise . runAction . void


-- | Like 'nsis', but don't try and optimise the resulting NSIS script.
--
-- Useful to figure out how the underlying installer works, or if you believe
-- the optimisations are introducing bugs. Please do report any such bugs,
-- especially if you aren't using 'unsafeInject' or 'unsafeInjectGlobal'!
nsisNoOptimise :: Action a -> String
nsisNoOptimise = unlines . showNSIS . runAction . void
