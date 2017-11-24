{-# LANGUAGE OverloadedStrings #-}

module Development.NSIS.Library where

import Control.Monad
import Development.NSIS.Sugar


-- | Replace one string with another string, in a target string. As some examples:
--
-- > strReplace "t" "XX" "test" %== "XXesXX"
-- > strReplace "ell" "" "hello world" %== "ho world"
strReplace :: Exp String -> Exp String -> Exp String -> Exp String
strReplace from to str = do
    from <- constant_ from
    to <- constant_ to
    str <- constant_ str
    scope $ do
        rest <- mutable "REST" str
        res <- mutable "RES" ""
        while (rest %/= "") $ do
            iff (from `strIsPrefixOf` rest)
                (do
                    res @= res & to
                    rest @= strDrop (strLength from) rest)
                (do
                    res @= res & strTake 1 rest
                    rest @= strDrop 1 rest)
        res

-- | NSIS (the underlying installer, not this library) uses fixed length string buffers,
--   defaulting to 1024 bytes. Any strings longer than
--   the limit may cause truncation or segfaults. You can get builds supporting longer strings
--   from <http://nsis.sourceforge.net/Special_Builds>.
--
--   Given @strCheck msg val@, if @val@ exceeds the limit it will 'abort' with @msg@, otherwise
--   it will return 'val'.
strCheck :: Exp String -> Exp String -> Exp String
strCheck msg x = share x $ \x -> do
    let special = "@!!_NSIS"
    iff_ (not_ $ special `strIsSuffixOf` (x & special)) $ do
        void $ messageBox [MB_ICONSTOP] $ "ERROR: String limit exceeded,\n" & msg
        abort $ "ERROR: String limit exceeded, " & msg
    x


-- | Is the first string a prefix of the second.
strIsPrefixOf :: Exp String -> Exp String -> Exp Bool
strIsPrefixOf x y = share x $ \x -> share y $ \y ->
    strTake (strLength x) y %== x

-- | Is the first string a prefix of the second.
strIsSuffixOf :: Exp String -> Exp String -> Exp Bool
strIsSuffixOf x y = share x $ \x -> share y $ \y ->
    strDrop (strLength y - strLength x) y %== x


-- | Join together a list of strings with @\\r\\n@ after each line. Note that unlike standard 'unlines',
--   we use the Windows convention line separator.
strUnlines :: [Exp String] -> Exp String
strUnlines = strConcat . map (& "\r\n")


-- | Write a file comprising of a set of lines.
writeFileLines :: Exp FilePath -> [Exp String] -> Action ()
writeFileLines a b = withFile' ModeWrite a $ \hdl ->
    forM_ b $ \s -> fileWrite hdl $ s & "\r\n"


infixr 3 %&&
infixr 2 %||

-- | Short circuiting boolean operators, equivalent to '&&' and '||' but on 'Exp'.
(%&&), (%||) :: Exp Bool -> Exp Bool -> Exp Bool
(%&&) a b = a ? (b, false)
(%||) a b = a ? (true, b)


-- | With a 'fileOpen' perform some action, then automatically call 'fileClose'.
--   If the action argument jumps out of the section then the 'fileClose' call will be missed.
withFile' :: FileMode -> Exp FilePath -> (Exp FileHandle -> Action ()) -> Action ()
withFile' mode name act = do
    hdl <- fileOpen mode name
    act hdl
    fileClose hdl

-- | Write a file, like 'writeFile'.
writeFile' :: Exp FilePath -> Exp String -> Action ()
writeFile' name contents = withFile' ModeWrite name $ \hdl -> fileWrite hdl contents
