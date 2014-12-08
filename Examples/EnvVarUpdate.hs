{-# LANGUAGE OverloadedStrings #-}

module Examples.EnvVarUpdate(envvarupdate) where

import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate


envvarupdate = do
    name "envvarupdate"
    outFile "envvarupdate.exe"
    requestExecutionLevel User

    section "" [] $ do
        let assert x = iff_ (getEnvVar HKCU "NSIS_TEST" %/= x) $ alert $ "FAILED!"
        deleteEnvVar HKCU "NSIS_TEST"
        setEnvVar HKCU "NSIS_TEST" "This is a;test"
        assert "This is a;test"
        setEnvVarAppend HKCU "NSIS_TEST" "foo bar"
        assert "This is a;test;foo bar"
        setEnvVarPrepend HKCU "NSIS_TEST" "test"
        assert "test;This is a;foo bar"
        setEnvVarRemove HKCU "NSIS_TEST" "test"
        assert "This is a;foo bar"
        setEnvVarRemove HKCU "NSIS_TEST" "foo bar"
        assert "This is a"
        setEnvVarPrepend HKCU "NSIS_TEST" "extra"
        assert "extra;This is a"
        setEnvVarRemove HKCU "NSIS_TEST" "bob"
        assert "extra;This is a"
        setEnvVar HKCU "NSIS_TEST" "bob;bob;bob;x;bob"
        setEnvVarRemove HKCU "NSIS_TEST" "bob"
        assert "x"
        setEnvVarRemove HKCU "NSIS_TEST" "x"
        assert ""
        setEnvVar HKCU "NSIS_TEST" "y"
        deleteEnvVar HKCU "NSIS_TEST"
        assert ""

        alert $ "USER $$PATH = " & getEnvVar HKCU "PATH"
        alert $ "MACHINE $$PATH = " & getEnvVar HKLM "PATH"
