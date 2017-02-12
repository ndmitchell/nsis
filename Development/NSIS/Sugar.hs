{-# LANGUAGE OverloadedStrings, EmptyDataDecls, ScopedTypeVariables, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-} -- Bits.popCount only introduced in 7.6
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- Applicative and Monoid required < 7.9

module Development.NSIS.Sugar(
    Compressor(..), HKEY(..), MessageBoxType(..), Page(..), Level(..), Visibility(..), FileMode(..), SectionFlag(..),
    ShowWindow(..), FinishOptions(..), DetailsPrint(..),
    module Development.NSIS.Sugar, Label, SectionId
    ) where

import Development.NSIS.Type
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Data
import Data.Bits
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Generics.Uniplate.Data


---------------------------------------------------------------------
-- INTERNALS

data S = S
    {uniques :: Int
    ,stream :: [NSIS]
    ,scopes :: [[(String,(TypeRep,Val))]] -- nearest scope is here
    }

-- | Monad in which installers are defined. A useful command to start with is 'section'.
newtype Action a = Action (State S a)
    deriving (Functor, Monad, Applicative)


-- | A 'Value', only used by 'Exp', which can be produced using 'return'.
--   The @ty@ argument should be one of 'String', 'Int' or 'Bool'.
newtype Value ty = Value {fromValue :: Val}

tyString = typeOf (undefined :: String)
tyInt = typeOf (undefined :: Int)


unique :: Action Int
unique = Action $ do
    s <- get
    put s{uniques = uniques s + 1}
    return $ uniques s

var :: Action Var
var = fmap Var unique

newSectionId :: Action SectionId
newSectionId = fmap SectionId unique

val x = [Var_ x]
lit x = [Literal x | x /= ""]

-- | Create a new label, used with 'goto' and 'label' to write line jump based programming.
--   Where possible you should use structured alternatives, such as 'iff', 'while' and 'loop'.
--   Each created label must be used with one call to 'label', and any number of calls to
--   'goto'. As an example:
--
-- @
-- abort <- 'newLabel'
-- 'while' var $ do
--     'iff_' cond1 $ 'goto' abort
--     'iff_' cond2 $ 'goto' abort
--     var '@=' 'strDrop' 1 var 
-- 'label' abort
-- @
--
--   Note that the above example could have been written in a simpler manner with 'loop'.
newLabel :: Action Label
newLabel = fmap Label unique

emit :: NSIS -> Action ()
emit x = Action $ modify $ \s -> s{stream = stream s ++ [x]}


rval :: Exp a -> Action Var
rval act = do
    (xs, res) <- capture act
    case res of
        _ | not $ null xs -> error $ "An R-value may not emit any statements: " ++ show xs
        Value [Var_ x] -> return x
        _ -> error $ "An R-value must be a single value, found: " ++ show (fromValue res)


capture :: Action a -> Action ([NSIS], a)
capture (Action act) = Action $ do
    s0 <- get
    put s0{stream=[]}
    res <- act
    s1 <- get
    put s1{stream=stream s0}
    return (stream s1, res)

runAction :: Action () -> [NSIS]
runAction (Action act) = stream $ execState act s0
    where s0 = S 1 [] [("NSISDIR",(tyString,[Builtin "{NSISDIR}"])):[(x, (tyString, [Builtin x])) | x <- builtin]]


builtin = words $
    "ADMINTOOLS APPDATA CDBURN_AREA CMDLINE COMMONFILES COMMONFILES32 COMMONFILES64 COOKIES DESKTOP DOCUMENTS " ++
    "EXEDIR EXEFILE EXEPATH FAVORITES FONTS HISTORY HWNDPARENT INSTDIR INTERNET_CACHE LANGUAGE LOCALAPPDATA " ++
    "MUSIC NETHOOD OUTDIR PICTURES PLUGINSDIR PRINTHOOD PROFILE PROGRAMFILES PROGRAMFILES32 PROGRAMFILES64 " ++
    "QUICKLAUNCH RECENT RESOURCES RESOURCES_LOCALIZED SENDTO SMPROGRAMS SMSTARTUP STARTMENU SYSDIR TEMP " ++
    "TEMPLATES VIDEOS WINDIR"


-- | Set all 'file' actions to automatically take 'NonFatal'.
alwaysNonFatal :: Action () -> Action ()
alwaysNonFatal act = do
    (xs, _) <- capture act
    mapM_ emit $ transformBi f xs
    where
        f (File x) = File x{fileNonFatal=True}
        f x = x


-- | The type of expressions - namely an 'Action' producing a 'Value'. There are instances
--   for 'Num' and 'IsString', and turning on @{-\# LANGUAGE OverloadedStrings \#-}@ is
--   strongly recommended.
--
--   The 'fromString' function converts any embedded @$VAR@ into a variable lookup, which may refer to one of
--   the builtin NSIS variables (e.g. @$SMPROGRAMS@, @$TEMP@, @$PROGRAMFILES@), or a named variable
--   created with 'constant' or 'mutable'. The string @$$@ is used to escape @$@ values.
--   Bracket the variables to put text characters afterwards (e.g. @$(SMPROGRAMS)XXX@). In contrast
--   to standard strings, @\/@ is treated as @\\@ and @\/\/@ is treated as @\/@. Remember to escape any
--   slashes occuring in URLs.
--
--   If the string is @'Exp' 'String'@ then any 'Int' variables used will be automatically shown (see 'strShow').
--   If the string is @'Exp' ty@ then it must be of the form @\"$VAR\"@ where @$VAR@ is a variable of type @ty@.
--
--   The 'Eq' and 'Ord' instances for 'Exp' throw errors for all comparisons (use '%==', '%<=' etc),
--   but 'min' and 'max' are defined. The 'Num' (arithmetic) and 'Monoid' (string concatenation) instances are both
--   fully implemented. From 'Integral' and 'Fractional', only '/', 'mod' and 'div' are implemented, and
--   all as integer arithmetic. No functions from 'Enum' or 'Real' are implemented.
--
--   When using a single expression multiple times, to ensure it is not evaluated
--   repeatedly, use 'share'.
type Exp ty = Action (Value ty)

instance forall a . Typeable a => IsString (Exp a) where
    fromString o = do
        scopes <- Action $ gets scopes
        let rty = typeOf (undefined :: a)

        let grab good name = case lookup name $ concat scopes of
                Nothing -> error $ "Couldn't find variable, $" ++ name ++ ", in " ++ show o
                Just (ty,y)
                    | ty `notElem` good -> error $ "Type mismatch, $" ++ name ++ " has " ++ show ty ++
                                                ", but you want one of " ++ show good ++ ", in " ++ show o
                    | otherwise -> y

        -- "$VAR" permits any type, everything else requires string
        case parseString o of
            [Right var] -> return $ Value $ grab [rty] var
            
            _ | rty /= tyString ->
                error $ "Cannot use concatenated variables/literals to produce anything other than String, but you tried " ++ show rty ++ ", in " ++ show o
            xs -> fmap (Value . fromValue) $ strConcat $ flip map xs $ \i -> return $ Value $ case i of
                    Left x -> lit x
                    Right name -> grab [tyString,tyInt] name


parseString :: String -> [Either String String]
parseString "" = []
parseString ('/':'/':xs) = Left "/" : parseString xs
parseString ('/':xs) = Left "\\" : parseString xs
parseString ('$':'$':xs) = Left "$" : parseString xs
parseString ('$':'(':xs) = Right a : parseString (drop 1 b)
    where (a,b) = break (== ')') xs
parseString ('$':xs) = Right a : parseString b
    where (a,b) = span isAlphaNum xs
parseString (x:xs) = Left [x] : parseString xs


instance Show (Exp a) where
    show _ = error "show is not available for Exp"

instance Eq (Exp a) where
    _ == _ = error "(==) is not available for Exp, try (%==) instead"

instance Num (Exp Int) where
    fromInteger = return . Value . lit . show
    (+) = intOp "+"
    (*) = intOp "*"
    (-) = intOp "-"
    abs a = share a $ \a -> a %< 0 ? (negate a, a)
    signum a = share a $ \a -> a %== 0 ? (0, a %< 0 ? (-1, 1))    

instance Integral (Exp Int) where
    mod = intOp "%"
    toInteger = error "toInteger is not available for Exp"
    div = intOp "/"
    quotRem = error "quotRem is not available for Exp"

instance Enum (Exp Int) where
    toEnum = error "toEnum is not available for Exp"
    fromEnum = error "toEnum is not available for Exp"

instance Real (Exp Int) where
    toRational = error "toRational is not available for Exp"

instance Ord (Exp Int) where
    compare = error "compare is not available for Exp"
    min a b = share a $ \a -> share b $ \b -> a %<= b ? (a, b)
    max a b = share a $ \a -> share b $ \b -> a %<= b ? (b, a)

instance Fractional (Exp Int) where
    fromRational = error "fromRational is not available for Exp, only Int is supported"
    (/) = intOp "/"

instance Monoid (Exp String) where
    mempty = fromString ""
    mappend x y = mconcat [x,y]
    mconcat xs = do
        xs <- sequence xs
        return $ Value $ f $ concatMap fromValue xs
        where
            f (Literal "":xs) = f xs
            f (Literal x:Literal y:zs) = f $ Literal (x++y) : zs
            f (x:xs) = x : f xs
            f [] = []

instance Bits (Exp Int) where
    (.&.) = intOp "&"
    (.|.) = intOp "|"
    xor = intOp "^"
    complement a = intOp "~" a 0
    shiftL a b = intOp "<<" a (fromInteger $ toInteger b)
    shiftR a b = intOp ">>" a (fromInteger $ toInteger b)
    rotate = error "rotate is not available for Exp"
    bitSize = error "bitSize is not available for Exp"
    isSigned _ = True
    testBit i = error "testBit is not available for Exp"
    bit i = fromInteger $ toInteger (bit i :: Int)

intOp :: String -> Exp Int -> Exp Int -> Exp Int
intOp cmd x y = do Value x <- x; Value y <- y; v <- var; emit $ IntOp v x cmd y; return $ Value $ val v

emit1 :: (Val -> NSIS) -> Exp a -> Action ()
emit1 f x1 = do Value x1 <- x1; emit $ f x1

emit2 :: (Val -> Val -> NSIS) -> Exp a -> Exp b -> Action ()
emit2 f x1 x2 = do Value x1 <- x1; Value x2 <- x2; emit $ f x1 x2

emit3 :: (Val -> Val -> Val -> NSIS) -> Exp a -> Exp b -> Exp c -> Action ()
emit3 f x1 x2 x3 = do Value x1 <- x1; Value x2 <- x2; Value x3 <- x3; emit $ f x1 x2 x3


infix 1 @=

-- | Assign a value to a mutable variable. The variable must have been originally created with
--   'mutable' or 'mutable_', or there will be an error when generating the install file.
(@=) :: Exp t -> Exp t -> Action ()
(@=) v w = do v <- rval v; Value w <- w; emit $ Assign v w


-- | Introduce a variable scope. Scopes are automatically introduced by operations
--   such as 'iff', 'loop', 'while' etc. Inside a scope you may define new variables
--   whose names may clash with variables outside the scope, but the local versions will be used.
--
--   If you have any non-evaluated expressions, before introducing any potentially clashing
--   variables in the scope you should 'share' them or use 'constant_' on them. For example:
--
-- @
-- operate x = do
--     x <- 'constant_' x
--     'scope' $ do
--         'constant' \"TEST\" 0
-- @
--
--   It is important to turn @x@ into a 'constant_' before defining a new constant @$TEST@, since
--   if @x@ refers to @$TEST@ after the new definition, it will pick up the wrong variable.
scope :: Action a -> Action a
scope (Action act) = Action $ do
    s0 <- get
    put s0{scopes=[] : scopes s0}
    res <- act
    modify $ \s -> s{scopes = scopes s0}
    return res


addScope :: forall t . Typeable t => String -> Value t -> Action ()
addScope name v = Action $
    modify $ \s -> let now:rest = scopes s in
                   if name `elem` map fst now
                   then error $ "Defined twice in one scope, " ++ name
                   else s{scopes=((name,(typeOf (undefined :: t), fromValue v)):now):rest}
        


-- | Create a mutable variable a name, which can be modified with '@='.
--   After defining the expression, you can refer to it with @$NAME@ in a 'String'.
--   To introduce a new scope, see 'scope'.
--
-- @
-- h <- 'mutable' \"HELLO\" \"Hello World\"
-- \"$HELLO\" '@=' \"$HELLO!\"
-- h        '@=' \"$HELLO!\" -- equivalent to the above
-- 'alert' \"$HELLO\"        -- with 2 exclamation marks
-- @
mutable :: Typeable t => String -> Exp t -> Action (Exp t)
mutable name x = do
    v <- mutable_ x
    vv <- v
    addScope name vv
    return v

-- | Create an unnamed mutable variable, which can be modified with '@='.
--
-- @
-- h <- 'mutable' \"Hello World\"
-- h '@=' 'h' '&' \"!\"
-- 'alert' h
-- @
mutable_ :: Exp t -> Action (Exp t)
mutable_ x = do
    v <- var
    let v2 = return $ Value $ val v
    v2 @= x
    return v2


-- | Create a constant with a name, ensuring the expression is shared.
--   After defining the expression, you can refer to it with @$NAME@ in a 'String'.
--   To introduce a new scope, see 'scope'.
--
-- @
-- 'constant' \"HELLO\" \"Hello World\"
-- 'alert' \"$HELLO!\"
-- @
constant :: Typeable t => String -> Exp t -> Action (Exp t)
constant name x = do x <- constant_ x; xx <- x; addScope name xx; return x

-- | Create a constant with no name, ensuring the expression is shared.
--   Equivalent to @'share' 'return'@.
constant_ :: Exp t -> Action (Exp t)
constant_ x = do
    -- either the expression is entirely constant, or has mutable variables inside it
    Value x <- x
    if null [() | Var_{} <- x] then
        -- if it's totally constant, we want to leave it that way so it works in non-eval settings (e.g. file)
        return $ return $ Value x
     else do
        -- if it's mutable, we want to share the value, but also snapshot it so that the mutable variables
        -- don't change afterwards
        v <- var
        return (Value $ val v) @= return (Value x)
        -- add the Literal so that assignment throws an error in future
        return $ return $ Value [Var_ v, Literal ""]


-- | The 'Exp' language is call-by-name, meaning you must use share to avoid evaluating an exression
--   multiple times. Using 'share', if the expression has any side effects
--   they will be run immediately, but not on subsequent uses. When defining functions operating on
--   'Exp', if you use the same input expression twice, you should share it. For example:
--
-- @
-- strPalindrom x = 'share' x $ \\x -> x '%==' strReverse x
-- @
--
--   If the expression was not shared, and @x@ read from a file, then the file would be read twice.
share :: Exp t -> (Exp t -> Action a) -> Action a
share v act = do v <- constant_ v; act v


-- | Versions of 'mutable' and 'constant' restricted to 'Exp' 'Int', used to avoid
--   ambiguous type errors.
mutableInt, constantInt :: String -> Exp Int -> Action (Exp Int)
mutableInt = mutable
constantInt = constant

-- | Versions of 'mutable_' and 'constant_' restricted to 'Exp' 'Int', used to avoid
--   ambiguous type errors.
mutableInt_, constantInt_ :: Exp Int -> Action (Exp Int)
mutableInt_ = mutable_
constantInt_ = constant_

-- | Versions of 'mutable' and 'constant' restricted to 'Exp' 'String', used to avoid
--   ambiguous type errors.
mutableStr, constantStr :: String -> Exp String -> Action (Exp String)
mutableStr = mutable
constantStr = constant

-- | Versions of 'mutable_' and 'constant_' restricted to 'Exp' 'String', used to avoid
--   ambiguous type errors.
mutableStr_, constantStr_ :: Exp String -> Action (Exp String)
mutableStr_ = mutable_
constantStr_ = constant_


---------------------------------------------------------------------
-- EXPRESSION WRAPPERS

-- | Perform string concatenation on a list of expressions.
strConcat :: [Exp String] -> Exp String
strConcat = mconcat

-- | Boolean negation.
not_ :: Exp Bool -> Exp Bool
not_ a = a ? (false, true)

infix 4 %==, %/=, %<=, %<, %>=, %>

-- | The standard equality operators, lifted to 'Exp'.
(%==), (%/=) :: Exp a -> Exp a -> Exp Bool
(%==) a b = do
    Value a <- a
    Value b <- b
    v <- mutable_ false
    eq <- newLabel
    end <- newLabel
    emit $ StrCmpS a b eq end
    label eq
    v @= true
    label end
    v

(%/=) a b = not_ (a %== b)

-- | The standard comparison operators, lifted to 'Exp'.
(%<=), (%<), (%>=), (%>) :: Exp Int -> Exp Int -> Exp Bool
(%<=) = comp True  True  False
(%<)  = comp False True  False
(%>=) = comp True  False True
(%>)  = comp False False True


comp :: Bool -> Bool -> Bool -> Exp Int -> Exp Int -> Exp Bool
comp eq lt gt a b = do
    Value a <- a
    Value b <- b
    v <- mutable_ false
    yes <- newLabel
    end <- newLabel
    let f b = if b then yes else end
    emit $ IntCmp a b (f eq) (f lt) (f gt)
    label yes
    v @= true
    label end
    v


-- | Boolean constants corresponding to 'True' and 'False'
true, false :: Exp Bool
false = return $ Value []
true = return $ Value [Literal "1"]

-- | Lift a 'Bool' into an 'Exp'
bool :: Bool -> Exp Bool
bool x = if x then true else false

-- | Lift a 'String' into an 'Exp'
str :: String -> Exp String
str = return . Value . lit

-- | Lift an 'Int' into an 'Exp'
int :: Int -> Exp Int
int = return . Value . lit . show

-- | Erase the type of an Exp, only useful with 'plugin'.
exp_ :: Exp a -> Exp ()
exp_ = fmap (Value . fromValue)

-- | Pop a value off the stack, will set an error if there is nothing on the stack.
--   Only useful with 'plugin'.
pop :: Exp String
pop = do v <- var; emit $ Pop v; return $ Value $ val v

-- | Push a value onto the stack. Only useful with 'plugin'.
push :: Exp a -> Action ()
push a = do Value a <- a; emit $ Push a

-- | Call a plugin. If the arguments are of different types use 'exp_'. As an example:
--
-- @
-- encrypt x = 'share' x $ \\x -> do
--     'plugin' \"Base64\" \"Encrypt\" ['exp_' x, 'exp_' $ 'strLength' x]
-- @
--
--   The only thing to be careful about is that we use the @x@ parameter twice, so should 'share'
--   it to ensure it is only evaluated once.
plugin :: String -> String -> [Exp a] -> Action ()
plugin dll name args = do args <- mapM (fmap fromValue) args; emit $ Plugin dll name args

-- | Add a plugin directory
addPluginDir :: Exp String -> Action ()
addPluginDir a = do Value a <- a; emit $ AddPluginDir a


-- | Return the length of a string, @strLength \"test\" '%==' 4@.
strLength :: Exp String -> Exp Int
strLength a = do Value a <- a; v <- var; emit $ StrLen v a; return $ Value $ val v

-- | Take the first @n@ characters from a string, @strTake 2 \"test\" '%==' \"te\"@.
strTake :: Exp Int -> Exp String -> Exp String
strTake n x = do Value n <- n; Value x <- x; v <- var; emit $ StrCpy v x n (lit ""); return $ Value $ val v

-- | Drop the first @n@ characters from a string, @strDrop 2 \"test\" '%==' \"st\"@.
strDrop :: Exp Int -> Exp String -> Exp String
strDrop n x = do Value n <- n; Value x <- x; v <- var; emit $ StrCpy v x (lit "") n; return $ Value $ val v

-- | Gets the last write time of the file, you should only use the result to compare for equality
--   with other results from 'getFileTime'. On failure the error flag is set.
getFileTime :: Exp FilePath -> Exp String
getFileTime x = do Value x <- x; v1 <- var; v2 <- var; emit $ GetFileTime x v1 v2; strConcat [return $ Value $ val v1, "#", return $ Value $ val v2]

readRegStr :: HKEY -> Exp String -> Exp String -> Exp String
readRegStr k a b = do v <- var; emit2 (ReadRegStr v k) a b; return $ Value $ val v

deleteRegKey :: HKEY -> Exp String -> Action ()
deleteRegKey k = emit1 $ DeleteRegKey k

deleteRegValue :: HKEY -> Exp String -> Exp String -> Action ()
deleteRegValue k = emit2 $ DeleteRegValue k

envVar :: Exp String -> Exp String
envVar a = do v <- var; emit1 (ReadEnvStr v) a; return $ Value $ val v


---------------------------------------------------------------------
-- ATTRIBUTES

data Attrib
    = Solid
    | Final
    | RebootOK
    | Silent
    | FilesOnly
    | NonFatal
    | Recursive
    | Unselected
    | Expanded
    | Description (Exp String)
    | Required
    | Target (Exp String)
    | Parameters (Exp String)
    | IconFile (Exp String)
    | IconIndex (Exp Int)
    | StartOptions String
    | KeyboardShortcut String
    | Id SectionId
    | Timeout Int
    | OName (Exp String)
      deriving Show


---------------------------------------------------------------------
-- STATEMENT WRAPPERS

-- | Define the location of a 'label', see 'newLabel' for details. This function will fail
--   if the same 'Label' is passed to 'label' more than once.
label :: Label -> Action ()
label lbl = emit $ Labeled lbl

-- | Jump to a 'label', see 'newLabel' for details. This function will fail
--   if 'label' is not used on the 'Label'.
goto :: Label -> Action ()
goto lbl = emit $ Goto lbl


infix 2 ?

-- | An expression orientated version of 'iff', returns the first component if
--   the first argument is 'true' or the second if it is 'false'.
--
-- @
-- x '%==' 12 '?' (x, x '+' 5)
-- @
(?) :: Exp Bool -> (Exp t, Exp t) -> Exp t
(?) test (true, false) = do
    v <- var
    let v2 = return $ Value $ val v
    iff test (v2 @= true) (v2 @= false)
    v2

-- | Test a boolean expression, reunning the first action if it is 'true' and the second if it is 'false'.
--   The appropriate branch action will be run within a 'scope'. See '?' for an expression orientated version.
--
-- @
-- 'iff' (x '%==' 12) ('alert' \"is 12\") ('alert' \"is not 12\")
-- @
iff :: Exp Bool -> Action () -> Action () -> Action ()
iff test true false = do
    thn <- newLabel
    els <- newLabel
    end <- newLabel
    Value t <- test
    emit $ StrCmpS t (lit "") thn els
    label thn
    scope false
    goto end
    label els
    scope true
    label end


-- | A version of 'iff' where there is no else action.
iff_ :: Exp Bool -> Action () -> Action ()
iff_ test true = iff test true (return ())


-- | A while loop, run the second argument while the first argument is true.
--   The action is run in a 'scope'. See also 'loop'.
--
-- @
-- x <- 'mutable_' x
-- 'while' (x '%<' 10) $ do
--    x '@=' x '+' 1
-- @
while :: Exp Bool -> Action () -> Action ()
while test act = do
    start <- newLabel
    label start
    iff_ test (scope act >> goto start)

-- | A loop with a @break@ command. Run the action repeatedly until the breaking action
--   is called. The action is run in a 'scope'. See also 'while'.
--
-- @
-- x <- 'mutable_' x
-- 'loop' $ \\break -> do
--     'iff_' (x '%>=' 10) break
--     x '@=' x '+' 1
-- @
loop :: (Action () -> Action ()) -> Action ()
loop body = do
    end <- newLabel
    beg <- newLabel
    label beg
    scope $ body $ goto end
    goto beg
    label end

-- | Run an intitial action, and if that action causes an error, run the second action.
--   Unlike other programming languages, any uncaught errors are silently ignored.
--   All actions are run in 'scope'.
--
-- @
-- 'onError' ('exec' \"\\\"$WINDIR/notepad.exe\\\"\") ('alert' \"Failed to run notepad\")
-- @
onError :: Action () -> Action () -> Action ()
onError act catch = do
    emit ClearErrors
    scope act
    end <- newLabel
    err <- newLabel
    emit $ IfErrors err end
    label err
    scope catch
    label end


-- | Checks for existence of file(s) (which can be a wildcard, or a directory).
--   If you want to check to see if a file is a directory, use @fileExists "DIRECTORY/*.*"@.
--
-- > iff_ (fileExists "$WINDIR/notepad.exe") $
-- >     messageBox [MB_OK] "notepad is installed"
fileExists :: Exp FilePath -> Exp Bool
fileExists x = do
    v <- mutable_ false
    Value x <- x
    yes <- newLabel
    end <- newLabel
    emit $ IfFileExists x yes end
    label yes
    v @= true
    label end
    v


-- | Performs a search for filespec, running the action with each file found.
--   If no files are found the error flag is set. Note that the filename output is without path.
--
-- > findEach "$INSTDIR/*.txt" $ \x ->
-- >     detailPrint x
--
--   If you jump from inside the loop to after the loop then you may leak a search handle.
findEach :: Exp FilePath -> (Exp FilePath -> Action ()) -> Action ()
findEach spec act = do
    Value spec <- spec
    hdl <- var
    v <- var
    emit $ FindFirst hdl v spec
    while (return (Value $ val v)) $ do
        scope $ act $ return $ Value $ val v
        emit $ FindNext (val hdl) v
    emit $ FindClose $ val hdl


infixr 5 &

-- | Concatenate two strings, for example @\"$FOO\" & \"$BAR\"@ is equivalent
--   to @\"$FOO$BAR\"@.
(&) :: Exp String -> Exp String -> Exp String
(&) a b = strConcat [a,b]


-- | Convert an 'Int' to a 'String' by showing it.
strShow :: Exp Int -> Exp String
strShow = fmap (Value . fromValue)


-- | Convert a 'String' to an 'Int', any errors are silently ignored.
strRead :: Exp String -> Exp Int
strRead = fmap (Value . fromValue)


-- | Show an alert, equivalent to @messageBox [MB_ICONEXCLAMATION]@.
alert :: Exp String -> Action ()
alert x = do
    _ <- messageBox [MB_ICONEXCLAMATION] x
    return ()




---------------------------------------------------------------------
-- SETTINGS WRAPPERS

-- | Sets the name of the installer. The name is usually simply the product name such as \'MyApp\' or \'Company MyApp\'.
--
-- > name "MyApp"
name :: Exp String -> Action ()
name = emit1 Name

-- | Specifies the output file that @MakeNSIS@ should write the installer to.
--   This is just the file that MakeNSIS writes, it doesn't affect the contents of the installer.
--   Usually should end with @.exe@.
--
-- > outFile "installer.exe"
outFile :: Exp FilePath -> Action ()
outFile = emit1 OutFile

-- | Sets the output path (@$OUTDIR@) and creates it (recursively if necessary), if it does not exist.
--   Must be a full pathname, usually is just @$INSTDIR@.
--
-- > setOutPath "$INSTDIR"
setOutPath :: Exp FilePath -> Action ()
setOutPath = emit1 SetOutPath

-- | Sets the default installation directory.
--   Note that the part of this string following the last @\\@ will be used if the user selects 'browse', and
--   may be appended back on to the string at install time (to disable this, end the directory with a @\\@).
--   If this doesn't make any sense, play around with the browse button a bit.
--
-- > installDir "$PROGRAMFILES/MyApp"
installDir :: Exp FilePath -> Action ()
installDir = emit1 InstallDir

-- | Writes the uninstaller to the filename (and optionally path) specified.
--   Only valid from within an install section, and requires that you have an 'uninstall' section in your script.
--   You can call this one or more times to write out one or more copies of the uninstaller.
--
-- > writeUninstaller "$INSTDIR/uninstaller.exe"
writeUninstaller :: Exp FilePath -> Action ()
writeUninstaller = emit1 WriteUninstaller

-- | Set the icon used for the installer\/uninstaller.
--
-- > installIcon "$NSISDIR/Contrib/Graphics/Icons/modern-install.ico"
installIcon, uninstallIcon :: Exp FilePath -> Action ()
installIcon = emit1 InstallIcon
uninstallIcon = emit1 UninstallIcon

-- | Set the image used for the header splash. Pass 'Nothing' to use the default header image.
--
-- > headerImage $ Just "$NSISDIR/Contrib/Graphics/Header/win.bmp"
headerImage :: Maybe (Exp FilePath) -> Action ()
headerImage Nothing = emit $ HeaderImage Nothing
headerImage (Just x) = emit1 (HeaderImage . Just) x

-- | Creates (recursively if necessary) the specified directory. Errors can be caught
--   using 'onError'. You should always specify an absolute path.
--
-- > createDirectory "$INSTDIR/some/directory"
createDirectory :: Exp FilePath -> Action ()
createDirectory = emit1 CreateDirectory

-- | This attribute tells the installer to check a string in the registry,
--   and use it for the install dir if that string is valid. If this attribute is present,
--   it will override the 'installDir' attribute if the registry key is valid, otherwise
--   it will fall back to the 'installDir' default. When querying the registry, this command
--   will automatically remove any quotes. If the string ends in \".exe\", it will automatically
--   remove the filename component of the string (i.e. if the string is \"C:/program files/foo/foo.exe\",
--   it will know to use \"C:/program files/foo\").
--
-- > installDirRegKey HKLM "Software/NSIS" ""
-- > installDirRegKey HKLM "Software/ACME/Thingy" "InstallLocation"
installDirRegKey :: HKEY -> Exp String -> Exp String -> Action ()
installDirRegKey k = emit2 $ InstallDirRegKey k

-- | Execute the specified program and continue immediately. Note that the file specified
--   must exist on the target system, not the compiling system. @$OUTDIR@ is used for the working
--   directory. Errors can be caught using 'onError'. Note, if the command could have spaces,
--   you should put it in quotes to delimit it from parameters. e.g.: @exec \"\\\"$INSTDIR/command.exe\\\" parameters\"@.
--   If you don't put it in quotes it will not work on Windows 9x with or without parameters.
--
-- > exec "\"$INSTDIR/someprogram.exe\""
-- > exec "\"$INSTDIR/someprogram.exe\" some parameters"
exec :: Exp String -> Action ()
exec = emit1 Exec

execWait :: Exp String -> Action ()
execWait = emit1 ExecWait

execShell :: [ShowWindow] -> Exp String -> Action ()
execShell sw x = do
    Value x <- x
    let d = def{esCommand=x}
    emit $ ExecShell $ if null sw then d else d{esShow=last sw}

sectionSetText :: SectionId -> Exp String -> Action ()
sectionSetText x = emit1 $ SectionSetText x

sectionGetText :: SectionId -> Exp String
sectionGetText x = do v <- var; emit $ SectionGetText x v; return $ Value $ val v

data SectionFlag
    = SF_Selected
    | SF_SectionGroup
    | SF_SectionGroupEnd
    | SF_Bold
    | SF_ReadOnly
    | SF_Expand
    | SF_PartiallySelected
      deriving (Show,Data,Typeable,Read,Bounded,Enum,Eq,Ord)

sectionGet :: SectionId -> SectionFlag -> Exp Bool
sectionGet sec flag = do
    v <- var
    emit $ SectionGetFlags sec v
    let b = bit $ fromEnum flag :: Exp Int
    b %== (return (Value $ val v) .&. b)

sectionSet :: SectionId -> SectionFlag -> Exp Bool -> Action ()
sectionSet sec flag set = do
    v <- var
    emit $ SectionGetFlags sec v
    v <- return (return $ Value $ val v :: Exp Int)
    iff set
        (emit1 (SectionSetFlags sec) $ setBit   v (fromEnum flag))
        (emit1 (SectionSetFlags sec) $ clearBit v (fromEnum flag))


-- don't want to accidentally dupe the message box, so make it in Action Exp
messageBox :: [MessageBoxType] -> Exp String -> Action (Exp String)
messageBox ty x = do
    let a*b = (a, words b)
    let alts = [MB_OK * "OK"
               ,MB_OKCANCEL * "OK CANCEL"
               ,MB_ABORTRETRYIGNORE * "ABORT RETRY IGNORE"
               ,MB_RETRYCANCEL * "RETRY CANCEL"
               ,MB_YESNO * "YES NO"
               ,MB_YESNOCANCEL * "YES NO CANCEL"]
    let (btns,rest) = partition (`elem` map fst alts) ty
    let btn = last $ MB_OK : btns
    let alt = fromJust $ lookup btn alts

    end <- newLabel
    lbls <- replicateM (length alt) newLabel
    v <- mutable_ ""
    Value x <- x
    emit $ MessageBox (btn:rest) x $ zip alt lbls
    forM_ (zip alt lbls) $ \(a,l) -> do
        label l
        v @= fromString a
        goto end
    label end
    return v

writeRegStr :: HKEY -> Exp String -> Exp String -> Exp String -> Action ()
writeRegStr k = emit3 $ WriteRegStr k

writeRegExpandStr :: HKEY -> Exp String -> Exp String -> Exp String -> Action ()
writeRegExpandStr k = emit3 $ WriteRegExpandStr k

writeRegDWORD :: HKEY -> Exp String -> Exp String -> Exp Int -> Action ()
writeRegDWORD k = emit3 $ WriteRegDWORD k

-- | While the action is executing, do not update the progress bar.
--   Useful for functions which do a large amount of computation, or have loops.
hideProgress :: Action a -> Action a
hideProgress act = do
    fun <- fmap newFun unique
    (xs, v) <- capture act
    emit $ Function fun xs
    emit $ Call fun
    return v

-- | Sleep time in milliseconds
sleep :: Exp Int -> Action ()
sleep = emit1 Sleep

-- | Create a function, useful for registering actions
event :: String -> Action () -> Action ()
event name act = do
    (xs, _) <- capture act
    emit $ Function (Fun name) xs

onSelChange :: Action () -> Action ()
onSelChange = event ".onSelChange"

onPageShow, onPagePre, onPageLeave :: Page -> Action () -> Action ()
-- these names are special and bound by Show
onPageShow  p = event $ "Show" ++ showPageCtor p
onPagePre   p = event $ "Pre" ++ showPageCtor p
onPageLeave p = event $ "Show" ++ showPageCtor p

allowRootDirInstall :: Bool -> Action ()
allowRootDirInstall = emit . AllowRootDirInstall

caption :: Exp String -> Action ()
caption = emit1 Caption

detailPrint :: Exp String -> Action ()
detailPrint = emit1 DetailPrint

setDetailsPrint :: DetailsPrint -> Action ()
setDetailsPrint = emit . SetDetailsPrint

showInstDetails :: Visibility -> Action ()
showInstDetails = emit . ShowInstDetails

showUninstDetails :: Visibility -> Action ()
showUninstDetails = emit . ShowUninstDetails


-- | The type of a file handle, created by 'fileOpen'.
data FileHandle deriving Typeable

-- | Open a file, which must be closed explicitly with 'fileClose'.
--   Often it is better to use 'Development.NSIS.Sugar.writeFile'' or
--   'Development.NSIS.Sugar.withFile' instead.
--
-- @
-- h <- 'fileOpen' 'ModeWrite' \"C:/log.txt\"
-- 'fileWrite' h \"Hello world!\"
-- 'fileClose' h
-- @
fileOpen :: FileMode -> Exp FilePath -> Action (Exp FileHandle)
fileOpen mode name = do
    Value name <- name
    v <- var
    emit $ FileOpen v name mode
    return $ return $ Value $ val v

-- | Write a string to a file openned with 'fileOpen'.
fileWrite :: Exp FileHandle -> Exp String -> Action ()
fileWrite = emit2 FileWrite

-- | Close a file file openned with 'fileOpen'.
fileClose :: Exp FileHandle -> Action ()
fileClose = emit1 FileClose

setCompressor :: Compressor -> [Attrib] -> Action ()
setCompressor x as = emit $ SetCompressor $ foldl f def{compType=x} as
    where
        f c Final = c{compFinal=True}
        f c Solid = c{compSolid=True}
        f c x = error $ "Invalid attribute to setCompress: " ++ show x

file :: [Attrib] -> Exp FilePath -> Action ()
file as x = do Value x <- x; emit . File =<< foldM f def{filePath=x} as
    where
        f c Recursive = return c{fileRecursive=True}
        f c NonFatal = return c{fileNonFatal=True}
        f c (OName x) = do Value x <- x; return c{fileOName=Just x}
        f c x = error $ "Invalid attribute to file: " ++ show x

section :: Exp String -> [Attrib] -> Action () -> Action SectionId
section name as act = do
    sec <- newSectionId
    Value name <- name
    (xs, _) <- capture $ scope act
    x <- foldM f def{secId=sec, secName=name} as
    emit $ Section x xs
    return $ secId x
    where
        f c Unselected = return c{secUnselected=True}
        f c Required = return c{secRequired=True}
        f c (Description x) = do Value x <- x; return c{secDescription=x}
        f c (Id x) = return c{secId=x}
        f c x = error $ "Invalid attribute to section: " ++ show x

sectionGroup :: Exp String -> [Attrib] -> Action () -> Action SectionId
sectionGroup name as act = do
    sec <- newSectionId
    Value name <- name
    (xs, _) <- capture $ scope act
    x <- foldM f def{secgId=sec, secgName=name} as
    emit $ SectionGroup x xs
    return $ secgId x
    where
        f c Expanded = return c{secgExpanded=True}
        f c (Description x) = do Value x <- x; return c{secgDescription=x}
        f c (Id x) = return c{secgId=x}
        f c x = error $ "Invalid attribute to sectionGroup: " ++ show x

uninstall :: Action () -> Action ()
uninstall = void . section "Uninstall" []

-- | Delete file (which can be a file or wildcard, but should be specified with a full path) from the target system.
--   If 'RebootOK' is specified and the file cannot be deleted then the file is deleted when the system reboots --
--   if the file will be deleted on a reboot, the reboot flag will be set. The error flag is set if files are found
--   and cannot be deleted. The error flag is not set from trying to delete a file that does not exist.
--
-- > delete [] "$INSTDIR/somefile.dat"
delete :: [Attrib] -> Exp FilePath -> Action ()
delete as x = do
    Value x <- x
    emit $ Delete $ foldl f def{delFile=x} as
    where
        f c RebootOK = c{delRebootOK=True}
        f c x = error $ "Invalid attribute to delete: " ++ show x

-- | Remove the specified directory (fully qualified path with no wildcards). Without 'Recursive',
--   the directory will only be removed if it is completely empty. If 'Recursive' is specified, the
--   directory will be removed recursively, so all directories and files in the specified directory
--   will be removed. If 'RebootOK' is specified, any file or directory which could not have been
--   removed during the process will be removed on reboot -- if any file or directory will be
--   removed on a reboot, the reboot flag will be set.
--   The error flag is set if any file or directory cannot be removed.
--
-- > rmdir [] "$INSTDIR"
-- > rmdir [] "$INSTDIR/data"
-- > rmdir [Recursive, RebootOK] "$INSTDIR"
-- > rmdir [RebootOK] "$INSTDIR/DLLs"
--
--   Note that the current working directory can not be deleted. The current working directory is
--   set by 'setOutPath'. For example, the following example will not delete the directory.
--
-- > setOutPath "$TEMP/dir"
-- > rmdir [] "$TEMP/dir"
--
--   The next example will succeed in deleting the directory.
--
-- > setOutPath "$TEMP/dir"
-- > setOutPath "$TEMP"
-- > rmdir [] "$TEMP/dir"
--
--   Warning: using @rmdir [Recursive] "$INSTDIR"@ in 'uninstall' is not safe. Though it is unlikely,
--   the user might select to install to the Program Files folder and so this command will wipe out
--   the entire Program Files folder, including other programs that has nothing to do with the uninstaller.
--   The user can also put other files but the program's files and would expect them to get deleted with
--   the program. Solutions are available for easily uninstalling only files which were installed by the installer.
rmdir :: [Attrib] -> Exp FilePath -> Action ()
rmdir as x = do
    Value x <- x
    emit $ RMDir $ foldl f def{rmDir=x} as
    where
        f c RebootOK = c{rmRebootOK=True}
        f c Recursive = c{rmRecursive=True}
        f c x = error $ "Invalid attribute to rmdir: " ++ show x

-- | Both file paths are on the installing system. Do not use relative paths.
copyFiles :: [Attrib] -> Exp FilePath -> Exp FilePath -> Action ()
copyFiles as from to = do
    Value from <- from
    Value to <- to
    emit $ CopyFiles $ foldl f def{cpFrom=from, cpTo=to} as
    where
        f c Silent = c{cpSilent=True}
        f c FilesOnly = c{cpFilesOnly=True}
        f c x = error $ "Invalid attribute to copyFiles: " ++ show x

-- | Creates a shortcut file that links to a 'Traget' file, with optional 'Parameters'. The icon used for the shortcut
--   is 'IconFile','IconIndex'. 'StartOptions' should be one of: SW_SHOWNORMAL, SW_SHOWMAXIMIZED, SW_SHOWMINIMIZED.
--   'KeyboardShortcut' should be in the form of 'flag|c' where flag can be a combination (using |) of: ALT, CONTROL, EXT, or SHIFT.
--   c is the character to use (a-z, A-Z, 0-9, F1-F24, etc). Note that no spaces are allowed in this string. A good example is
--   \"ALT|CONTROL|F8\". @$OUTDIR@ is used for the working directory. You can change it by using 'setOutPath' before creating
--   the Shortcut. 'Description' should be the description of the shortcut, or comment as it is called under XP.
--   The error flag is set if the shortcut cannot be created (i.e. either of the paths (link or target) does not exist, or some other error).
--
-- > createDirectory "$SMPROGRAMS/My Company"
-- > createShortcut "$SMPROGRAMS/My Company/My Program.lnk"
-- >    [Target "$INSTDIR/My Program.exe"
-- >    ,Parameters "some command line parameters"
-- >    ,IconFile "$INSTDIR/My Program.exe", IconIndex 2
-- >    ,StartOptions "SW_SHOWNORMAL"
-- >    ,KeyboardShortcut "ALT|CONTROL|SHIFT|F5"
-- >    ,Description "a description"]
createShortcut :: Exp FilePath -> [Attrib] -> Action ()
createShortcut name as = do Value name <- name; x <- foldM f def{scFile=name} as; emit $ CreateShortcut x
    where
        f c (Target x) = do Value x <- x; return c{scTarget=x}
        f c (Parameters x) = do Value x <- x; return c{scParameters=x}
        f c (IconFile x) = do Value x <- x; return c{scIconFile=x}
        f c (IconIndex x) = do Value x <- x; return c{scIconIndex=x}
        f c (StartOptions x) = return c{scStartOptions=x}
        f c (KeyboardShortcut x) = return c{scKeyboardShortcut=x}
        f c (Description x) = do Value x <- x; return c{scDescription=x}
        f c x = error $ "Invalid attribute to shortcut: " ++ show x


page :: Page -> Action ()
page = emit . Page

finishOptions :: FinishOptions
finishOptions = def

unpage :: Page -> Action ()
unpage = emit . Unpage

requestExecutionLevel :: Level -> Action ()
requestExecutionLevel = emit . RequestExecutionLevel

type HWND = Exp Int

hwndParent :: HWND
hwndParent = return $ Value [Builtin "HWNDPARENT"]

findWindow :: Exp String -> Exp String -> Maybe HWND -> Action HWND
findWindow a b c = do
    v <- var
    Value a <- a
    Value b <- b
    c <- maybe (return Nothing) (fmap (Just . fromValue)) c
    emit $ FindWindow v a b c Nothing
    return $ return $ Value $ val v

getDlgItem :: HWND -> Exp Int -> Action HWND
getDlgItem a b = do
    v <- var
    Value a <- a
    Value b <- b
    emit $ GetDlgItem v a b
    return $ return $ Value $ val v

sendMessage :: [Attrib] -> HWND -> Exp Int -> Exp a -> Exp b -> Action (Exp Int)
sendMessage as a b c d = do
    v <- var
    Value a <- a
    Value b <- b
    Value c <- c
    Value d <- d
    as <- return $ foldl f Nothing as
    emit $ SendMessage a b c d v as
    return $ return $ Value $ val v
    where
        f c (Timeout x) = Just x
        f c x = error $ "Invalid attribute to sendMessage: " ++ show x

abort :: Exp String -> Action ()
abort = emit1 Abort
