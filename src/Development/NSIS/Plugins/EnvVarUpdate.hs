{-# LANGUAGE OverloadedStrings #-}

-- | Module for updating environment variables. All functions take either 'HKLM' to modify the
--   variable for the machine, or 'HKCU' to modify for the user.
--
--   /Warning:/ If you are modifying PATH, make sure you use a special build of NSIS which can cope
--   with longer strings, or you will corrupt your users path.
module Development.NSIS.Plugins.EnvVarUpdate(
    getEnvVar, setEnvVar, deleteEnvVar,
    setEnvVarAppend, setEnvVarPrepend, setEnvVarRemove
    ) where

import Development.NSIS
import Development.NSIS.Plugins.WinMessages
import Control.Monad
import Data.String


resolve :: HKEY -> String
resolve h | h == HKLM || h == HKEY_LOCAL_MACHINE = "SYSTEM/CurrentControlSet/Control/Session Manager/Environment"
          | h == HKCU || h == HKEY_CURRENT_USER = "Environment"
          | otherwise = error $ "Development.NSIS.Plugins.EnvVarUpdate, must use either HKLM or HKCU, got: " ++ show h


-- | Given a string, and a ; separated variable, remove the string from it if it is present.
remove :: Exp String -> Exp String -> Exp String
remove x xs = share x $ \x -> share xs $ \xs -> do
    xs <- mutable_ xs
    xs @= strReplace (";" & x & ";") ";" xs
    while ((x & ";") `strIsPrefixOf` xs) $ xs @= strDrop (strLength x + 1) xs
    while ((";" & x) `strIsSuffixOf` xs) $ xs @= strTake (strLength xs - 1 - strLength x) xs
    iff_ (x %== xs) $ xs @= ""
    xs


-- | Set an environment variable by writing to the registry.
setEnvVar :: HKEY -> Exp String -> Exp String -> Action ()
setEnvVar h key val = do
    writeRegExpandStr h (fromString $ resolve h) key (strCheck ("setting environment variable %" & key & "%") val)
    void $ sendMessage [Timeout 5000] hwnd_BROADCAST wm_WININICHANGE (0 :: Exp Int) ("STR:Environment" :: Exp String)


-- | Read a variable from the registry. If you are not modifying the variable
--   you should use 'envVar' instead.
getEnvVar :: HKEY -> Exp String -> Exp String
getEnvVar h key = strCheck ("reading environment variable %" & key & "%") $ readRegStr h (fromString $ resolve h) key


-- | Delete the environment variable in the registry.
deleteEnvVar :: HKEY -> Exp String -> Action ()
deleteEnvVar h key = do
    deleteRegValue h (fromString $ resolve h) key
    void $ sendMessage [Timeout 5000] hwnd_BROADCAST wm_WININICHANGE (0 :: Exp Int) ("STR:Environment" :: Exp String)


setEnvVarAppend :: HKEY -> Exp String -> Exp String -> Action ()
setEnvVarAppend h key val = share val $ \val -> setEnvVar h key $ remove val (getEnvVar h key) & ";" & val

setEnvVarPrepend :: HKEY -> Exp String -> Exp String -> Action ()
setEnvVarPrepend h key val = share val $ \val -> setEnvVar h key $ val & ";" & remove val (getEnvVar h key)

setEnvVarRemove :: HKEY -> Exp String -> Exp String -> Action ()
setEnvVarRemove h key val = setEnvVar h key $ remove val $ getEnvVar h key
