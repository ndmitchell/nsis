{-# LANGUAGE RecordWildCards #-}

module Development.NSIS.Show(showNSIS) where

import Development.NSIS.Type
import Data.Generics.Uniplate.Data
import Data.Char
import Data.List


showNSIS :: [NSIS] -> [String]
showNSIS xs =
    ["!Include MUI2.nsh"] ++
    ["Var _" ++ show v | v <- sort $ nub [i | Var i <- universeBi xs]] ++
    outs (filter isGlobal xs) ++
    ["!insertmacro MUI_LANGUAGE \"English\""] ++
    concat [("Function " ++ show name) : map indent (outs body) ++ ["FunctionEnd"] | Function name body <- universeBi xs] ++
    outs (filter isSection xs) ++
    ["Function .onInit" | not $ null inits] ++
    map indent (outs inits) ++
    ["FunctionEnd" | not $ null inits] ++
    (if null descs then [] else
        ["!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN"] ++
        map indent ["!insertmacro MUI_DESCRIPTION_TEXT " ++ show i ++ " " ++ show d | (i,d) <- descs] ++
        ["!insertmacro MUI_FUNCTION_DESCRIPTION_END"])
    where descs = filter (not . null . snd) $ concatMap secDescs $ universeBi xs
          inits = filter (\x -> not (isSection x) && not (isGlobal x)) xs


secDescs :: NSIS -> [(SectionId, Val)]
secDescs (Section x _) = [(secId x, secDescription x)]
secDescs (SectionGroup x _) = [(secgId x, secgDescription x)]
secDescs _ = []


isGlobal :: NSIS -> Bool
isGlobal x = case x of
    Name{} -> True
    OutFile{} -> True
    InstallDir{} -> True
    SetCompressor{} -> True
    InstallIcon{} -> True
    UninstallIcon{} -> True
    HeaderImage{} -> True
    Page{} -> True
    Unpage{} -> True
    RequestExecutionLevel{} -> True
    InstallDirRegKey{} -> True
    AllowRootDirInstall{} -> True
    ShowInstDetails{} -> True
    ShowUninstDetails{} -> True
    Caption{} -> True
    _ -> False

isSection :: NSIS -> Bool
isSection x = case x of
    Section{} -> True
    SectionGroup{} -> True
    _ -> False


outs :: [NSIS] -> [String]
outs = concatMap out

out :: NSIS -> [String]
out (Assign v x) = ["StrCpy " ++ show v ++ " " ++ show x]
out (SetCompressor ACompressor{..}) = [unwords $ "SetCompressor" : ["/solid"|compSolid] ++ ["/final"|compFinal] ++ [map toLower $ show compType]]
out (Section ASection{secId=SectionId secId, ..} xs) =
    [unwords $ "Section" : ["/o"|secUnselected] ++ [show $ [Literal "!"|secBold] ++ secName, "_sec" ++ show secId]] ++
    map indent (["SectionIn RO" | secRequired] ++ outs xs) ++
    ["SectionEnd"]
out (SectionGroup ASectionGroup{secgId=SectionId secgId, ..} xs) =
    [unwords $ "SectionGroup" : ["/e"|secgExpanded] ++ [show secgName, "_sec" ++ show secgId]] ++
    map indent (outs xs) ++
    ["SectionGroupEnd"]
out (File AFile{..}) = [unwords $ "File" : ["/nonfatal"|fileNonFatal] ++ ["/r"|fileRecursive] ++ [show filePath]]
out (Labeled i) = [show i ++ ":"]
out (CreateShortcut AShortcut{..}) = return $ unwords $
    ["CreateShortcut", show scFile, show scTarget, show scParameters, show scIconFile
    ,show scIconIndex, show scStartOptions, show scKeyboardShortcut, show scDescription]
out (InstallIcon x) = ["!define MUI_ICON " ++ show x]
out (UninstallIcon x) = ["!define MUI_UNICON " ++ show x]
out (HeaderImage x) = "!define MUI_HEADERIMAGE" : ["!define MUI_HEADERIMAGE_BITMAP " ++ show x | Just x <- [x]]
out (Page x) = ["!insertmacro MUI_PAGE_" ++ showPage x]
out (Unpage x) = ["!insertmacro MUI_UNPAGE_" ++ showPage x]
out Function{} = []
out (Delete ADelete{..}) = [unwords $ "Delete" : ["/rebootok"|delRebootOK] ++ [show delFile]]
out (RMDir ARMDir{..}) = [unwords $ "RMDir" : ["/r"|rmRecursive] ++ ["/rebootok"|rmRebootOK] ++ [show rmDir]]
out (CopyFiles ACopyFiles{..}) = [unwords $ "CopyFiles" : ["/silent"|cpSilent] ++ ["/filesonly"|cpFilesOnly] ++ [show cpFrom, show cpTo]]
out (MessageBox flags txt lbls) = [unwords $ "MessageBox" : intercalate "|" (map show flags) : show txt :
    ["ID" ++ a ++ " " ++ show b | (a,b) <- lbls]]
out (Goto x) = ["Goto " ++ show x | x /= Label 0]
out (ExecShell AExecShell{..}) = [unwords ["ExecShell","\"\"",show esCommand,show esShow]]

out x = [show x]


showPage :: Page -> String
showPage (License x) = "LICENSE \"" ++ x ++ "\""
showPage x = map toUpper $ show x


indent x = "  " ++ x
