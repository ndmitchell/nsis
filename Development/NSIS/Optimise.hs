{-# LANGUAGE PatternGuards #-}

module Development.NSIS.Optimise(optimise) where

import Development.NSIS.Type
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe


-- before: secret = 1021, primes = 109

optimise :: [NSIS] -> [NSIS]
optimise =
    -- allow Label 0
    rep (elimDeadLabel . useLabel0) .
    -- disallow Label 0
    rep (elimDeadLabel . elimAfterGoto . deadAssign . assignSwitch . dullGoto . knownCompare . elimLabeledGoto . elimDeadVar)


rep :: ([NSIS] -> [NSIS]) -> [NSIS] -> [NSIS]
rep f x = g (measure x) x
    where
        g n1 x1 = if n2 < n1 then g n2 x2 else x2
            where x2 = f $ f $ f $ f x1
                  n2 = measure x2
        measure x = length (universeBi x :: [NSIS])


useLabel0 :: [NSIS] -> [NSIS]
useLabel0 = map (descendBi useLabel0) . f
    where
        f (x:Labeled next:xs)
            | null (children x :: [NSIS]) -- must not be a block with nested instructions
            = descendBi (\i -> if i == next then Label 0 else i) x : Labeled next : f xs
        f (x:xs) = x : f xs
        f [] = []


-- Label whose next statement is a good, 
elimLabeledGoto :: [NSIS] -> [NSIS]
elimLabeledGoto x = transformBi f x
    where
        f (Labeled x) = Labeled x
        f x | null (children x) = descendBi moveBounce x
            | otherwise = x

        moveBounce x = fromMaybe x $ lookup x bounce
        bounce = flip concatMap (universe x) $ \x -> case x of
            Labeled x:Goto y:_ -> [(x,y)]
            Labeled x:Labeled y:_ -> [(x,y)]
            _ -> []


-- Delete variables which are only assigned, never read from
elimDeadVar :: [NSIS] -> [NSIS]
elimDeadVar x = transform f x
    where
        f (Assign x _:xs) | x `elem` unused = xs
        f xs = xs

        unused = nub assign \\ nub used
        used = every \\ assign
        every = universeBi x
        assign = [x | Assign x _ <- universeBi x]

jumpy Goto{} = True
jumpy StrCmpS{} = True
jumpy IntCmp{} = True
jumpy IfErrors{} = True
jumpy IfFileExists{} = True
jumpy MessageBox{} = True
jumpy _ = False


-- Eliminate any code after a goto, until a label
elimAfterGoto :: [NSIS] -> [NSIS]
elimAfterGoto x = transformBi f x
    where
        f (x:xs) | jumpy x = x : g xs
        f x = x

        g (Labeled x:xs) = Labeled x:xs
        g (x:xs) = g xs
        g x = x


-- Be careful to neither introduce or remove label based errors
elimDeadLabel :: [NSIS] -> [NSIS]
elimDeadLabel x = transform f x
    where
        f (Labeled x:xs) | x `elem` unused = xs
        f xs = xs

        unused = nub label \\ nub gotos
        gotos = every \\ label
        every = universeBi x
        label = [x | Labeled x <- universeBi x]


dullGoto :: [NSIS] -> [NSIS]
dullGoto = transform f
    where
        f (Goto l1:Labeled l2:xs) | l1 == l2 = Labeled l2 : xs
        f x = x


-- A tricky one! Comparison after jump
knownCompare :: [NSIS] -> [NSIS]
knownCompare x = transform f x
    where
        f (Assign var val : StrCmpS a b yes no : xs)
            | a == [Var_ var], Just eq <- isEqual b val
            = Assign var val : Goto (if eq then yes else no) : xs

        -- grows, but only a finite amount
        f (Assign var val : Labeled l : StrCmpS a b yes no : xs)
            | a == [Var_ var], Just eq <- isEqual b val
            = Assign var val : Goto (if eq then yes else no) : Labeled l : StrCmpS a b yes no : xs

        f (Assign var val : c : xs) | jumpy c = Assign var val : transformBi g c : xs
            where
                g l | Just (StrCmpS a b yes no) <- lookup l cmps
                    , a == [Var_ var], Just eq <- isEqual b val
                    = if eq then yes else no
                g l = l
        f x = x

        cmps = [(l,cmp) | Labeled l : cmp@StrCmpS{} : _ <- universeBi x]


isEqual :: Val -> Val -> Maybe Bool
isEqual x y | x == y = Just True
            | isLit x, isLit y = Just False
            | otherwise = Nothing
    where
        isLit = all isLiteral
        isLiteral Literal{} = True
        isLiteral _ = False


assignSwitch :: [NSIS] -> [NSIS]
assignSwitch = transform f
    where
        -- this rule just switches the assignment, back and forth, ad infinitum
        -- not very principled!
        f (IntOp out1 a b c : Assign other ([Var_ out2]) : xs)
            | out1 == out2
            = IntOp other a b c : Assign out1 ([Var_ other]) : xs
        f x = x


deadAssign :: [NSIS] -> [NSIS]
deadAssign = transform f
    where
        f (Assign v x:xs) | isDead v xs = xs
        f xs = xs

        isDead v (Labeled _:xs) = isDead v xs
        isDead v (Assign v2 x:xs) = v `notElem` universeBi x && (v == v2 || isDead v xs)
        isDead v _ = False
