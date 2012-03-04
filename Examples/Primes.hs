{-# LANGUAGE OverloadedStrings #-}

module Examples.Primes(primes) where

import Development.NSIS

-- Based on primes.nsi from NSIS
--
-- This is an example of the possibities of the NSIS Script language.
-- It calculates prime numbers.

----------------------------------

primes = do

    name "primes"
    allowRootDirInstall True
    outFile "primes.exe"
    caption "Prime number generator"
    showInstDetails Show
    installDir "$EXEDIR"
    requestExecutionLevel User

    ----------------------------------

    --Pages

    page Directory
    page InstFiles

    ----------------------------------

    section "" [] $ do
        setOutPath "$INSTDIR"
        hideProgress doPrimes


doPrimes = do
    hdl <- fileOpen ModeWrite "$INSTDIR/primes.txt"

    let output x = do
            detailPrint $ strShow x & " is prime!"
            fileWrite hdl $ strShow x & " is prime!\r\n"
    output 2
    output 3

    ppos <- mutableInt "PPOS" 5 -- position in prime searching
    pdiv <- mutableInt "PDIV" 0 -- divisor
    pcnt <- mutableInt "PCNT" 2 -- count of how many we've printed
    loop $ \breakOuter -> do
        pdiv @= 3
        loop $ \breakInner -> do
            iff_ (ppos `mod` pdiv %== 0) $ do
                ppos @= ppos + 2
                breakInner
            pdiv @= pdiv + 2
            iff_ (pdiv %>= ppos) $ do
                output ppos
                pcnt @= pcnt + 1
                iff_ (pcnt %== 100) $ do
                    pcnt @= 0
                    ans <- messageBox [MB_YESNO] "Process more?"
                    iff_ (ans %== "NO")
                        breakOuter

    fileClose hdl
