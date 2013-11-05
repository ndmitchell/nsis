{-# LANGUAGE RecordWildCards #-}

module Development.NSIS.Show(showNSIS) where

import Development.NSIS.Type
import Control.Arrow
import Data.Generics.Uniplate.Data
import Data.Char
import Data.Function
import Data.List


showNSIS :: [NSIS] -> [String]
showNSIS xs =
    ["!Include MUI2.nsh"] ++
    ["Var _" ++ show v | v <- sort $ nub [i | Var i <- universeBi xs]] ++
    outs fs (filter isGlobal xs) ++
    ["!insertmacro MUI_LANGUAGE \"English\""] ++
    (if null plugins then [] else
        ["Function NSIS_UnusedPluginPreload"
        ,"  # Put all plugins are at the start of the archive, ensuring fast extraction (esp. LZMA solid)"] ++
        map indent plugins ++
        ["FunctionEnd"]) ++
    outs fs (filter isSection xs) ++
    concat [("Function " ++ show name) : map indent (outs fs body) ++ ["FunctionEnd"] | (name,body) <- funs] ++
    (if null descs then [] else
        ["!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN"] ++
        map indent ["!insertmacro MUI_DESCRIPTION_TEXT " ++ show i ++ " " ++ show d | (i,d) <- descs] ++
        ["!insertmacro MUI_FUNCTION_DESCRIPTION_END"])
    where descs = filter (not . null . snd) $ concatMap secDescs $ universeBi xs
          inits = filter (\x -> not (isSection x) && not (isGlobal x)) xs
          fs = map fst funs
          funs = map (fst . head &&& concatMap snd) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $
                     [(Fun ".onInit",inits) | not $ null inits] ++ [(name,body) | Function name body <- universeBi xs]
          plugins = sort $ nub [a ++ "::" ++ b | Plugin a b _ <- universeBi xs]


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
    AddPluginDir{} -> True
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


outs :: [Fun] -> [NSIS] -> [String]
outs fs = concatMap (out fs)

out :: [Fun] -> NSIS -> [String]
out fs (Assign v x) = ["StrCpy " ++ show v ++ " " ++ show x]
out fs (SetCompressor ACompressor{..}) = [unwords $ "SetCompressor" : ["/solid"|compSolid] ++ ["/final"|compFinal] ++ [map toLower $ show compType]]
out fs (Section ASection{secId=SectionId secId, ..} xs) =
    [unwords $ "Section" : ["/o"|secUnselected] ++ [show $ [Literal "!"|secBold] ++ secName, "_sec" ++ show secId]] ++
    map indent (["SectionIn RO" | secRequired] ++ outs fs xs) ++
    ["SectionEnd"]
out fs (SectionGroup ASectionGroup{secgId=SectionId secgId, ..} xs) =
    [unwords $ "SectionGroup" : ["/e"|secgExpanded] ++ [show secgName, "_sec" ++ show secgId]] ++
    map indent (outs fs xs) ++
    ["SectionGroupEnd"]
out fs (File AFile{..}) = [unwords $ "File" : ["/nonfatal"|fileNonFatal] ++ ["/r"|fileRecursive] ++ [show filePath]]
out fs (Labeled i) = [show i ++ ":"]
out fs (CreateShortcut AShortcut{..}) = return $ unwords $
    ["CreateShortcut", show scFile, show scTarget, show scParameters, show scIconFile
    ,show scIconIndex, show scStartOptions, show scKeyboardShortcut, show scDescription]
out fs (InstallIcon x) = ["!define MUI_ICON " ++ show x]
out fs (UninstallIcon x) = ["!define MUI_UNICON " ++ show x]
out fs (HeaderImage x) = "!define MUI_HEADERIMAGE" : ["!define MUI_HEADERIMAGE_BITMAP " ++ show x | Just x <- [x]]
out fs (Page x) = let y = showPageCtor x in
    ["!define MUI_PAGE_CUSTOMFUNCTION_PRE Pre" ++ y | "Pre" ++ y `elem` map show fs] ++
    ["!define MUI_PAGE_CUSTOMFUNCTION_SHOW Show" ++ y | "Show" ++ y `elem` map show fs] ++
    ["!define MUI_PAGE_CUSTOMFUNCTION_LEAVE Leave" ++ y | "Leave" ++ y `elem` map show fs] ++
    concat [showFinish x | Finish x <- [x]] ++
    ["!insertmacro MUI_PAGE_" ++ showPage x]
out fs (Unpage x) = ["!insertmacro MUI_UNPAGE_" ++ showPage x]
out fs Function{} = []
out fs (Delete ADelete{..}) = [unwords $ "Delete" : ["/rebootok"|delRebootOK] ++ [show delFile]]
out fs (RMDir ARMDir{..}) = [unwords $ "RMDir" : ["/r"|rmRecursive] ++ ["/rebootok"|rmRebootOK] ++ [show rmDir]]
out fs (CopyFiles ACopyFiles{..}) = [unwords $ "CopyFiles" : ["/silent"|cpSilent] ++ ["/filesonly"|cpFilesOnly] ++ [show cpFrom, show cpTo]]
out fs (MessageBox flags txt lbls) = [unwords $ "MessageBox" : intercalate "|" (map show flags) : show txt :
    ["ID" ++ a ++ " " ++ show b | (a,b) <- lbls]]
out fs (Goto x) = ["Goto " ++ show x | x /= Label 0]
out fs (IntOp a b "~" _) = [unwords $ "IntOp" : [show a, show b, "~"]] -- the only unary IntOp
out fs (ExecShell AExecShell{..}) = [unwords ["ExecShell","\"\"",show esCommand,show esShow]]
out fs (Plugin a b cs) = [unwords $ (a ++ "::" ++ b) : map show cs]
out fs (AddPluginDir a) = [unwords ["!addplugindir",show a]]

out fs x = [show x]


showPage :: Page -> String
showPage (License x) = "LICENSE \"" ++ x ++ "\""
showPage x = map toUpper $ showPageCtor x

showFinish :: FinishOptions -> [String]
showFinish FinishOptions{..} =
    ["!define MUI_FINISHPAGE_RUN " ++ show finRun | finRun /= def] ++
    ["!define MUI_FINISHPAGE_RUN_TEXT " ++ show finRunText | finRunText /= def] ++
    ["!define MUI_FINISHPAGE_RUN_PARAMETERS " ++ show finRunParamters | finRunParamters /= def] ++
    ["!define MUI_FINISHPAGE_RUN_NOTCHECKED" | not finRunChecked && finRun /= def] ++
    ["!define MUI_FINISHPAGE_SHOWREADME " ++ show finReadme | finReadme /= def] ++
    ["!define MUI_FINISHPAGE_SHOWREADME_TEXT " ++ show finReadmeText | finReadmeText /= def] ++
    ["!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED" | not finReadmeChecked && finReadme /= def] ++
    ["!define MUI_FINISHPAGE_LINK_LOCATION " ++ show finLink | finLink /= def] ++
    ["!define MUI_FINISHPAGE_LINK " ++ show finLinkText | finLinkText /= def]


indent x = "  " ++ x
