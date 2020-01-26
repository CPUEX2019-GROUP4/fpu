module AsmConv.Parse.Config (
    ParsedConfig, parseConfig
) where

import Control.Monad
import Data.Foldable
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Text.Read
import AsmConv.Schedule.Config
import AsmConv.Schedule.Type
import AsmConv.Util

type ParsedConfig = Config String String String
data Section = Section {
    secName :: String,
    secBody :: [String]
}

parseConfig :: String -> Either String ParsedConfig
parseConfig str = mapLeft ("Config: " ++) $ do
    let strs = map removeComment $ lines str
        notLabelFirstLineErr = "First line must be label"
    sections <- maybeToEither notLabelFirstLineErr $ toSections strs
    config <- foldM parseSection defaultParsedConfig sections
    return config

defaultKindName :: String
defaultKindName = "default"

defaultParsedConfig :: ParsedConfig
defaultParsedConfig = defaultConfig {
    cfgDefaultKind = defaultKindName
}

toSections :: [String] -> Maybe [Section]
toSections strs =
    if null left then
        Just secs
    else
        Nothing
    where
        go [] = ([], [])
        go (s : ss) =
            let (l, r) = go ss
                tokens = words s
                name = init $ head tokens
                isSecDelim = length tokens == 1 && last (head tokens) == ':'
            in if isSecDelim then
                ([], Section { secName = name, secBody = l } : r)
            else
                (s : l, r)
        (left, secs) = go strs

removeComment :: String -> String
removeComment = takeWhile (/= '#')

parseSection :: ParsedConfig -> Section -> Either String ParsedConfig
parseSection config section =
    let body = secBody section
        name = secName section
    in case name of
        "paraNum" -> do
            paraNum <- parseInt name 1 body
            return $ config { cfgParaNum = paraNum }
        "ignoreRegs" -> do
            let ignoreRegs = parseStrings body
            return $ config { cfgIgnoreRegs = ignoreRegs }
        "argClassify" -> do
            argClassify <- parseArgClassify body
            return $ config { cfgArgClassify = argClassify }
        "implicitReadArg" -> do
            let implicitReadArg = parseStringToStrings body
            return $ config { cfgImplicitReadArg = implicitReadArg }
        "implicitWriteArg" -> do
            let implicitWriteArg = parseStringToStrings body
            return $ config { cfgImplicitWriteArg = implicitWriteArg }
        "latency" -> do
            latency <- parseStringToInt name body
            return $ config { cfgLatency = latency }
        "defaultLatency" -> do
            defaultLatency <- parseInt name 1 body
            return $ config { cfgDefaultLatency = defaultLatency }
        "kind" -> do
            kind <- parseStringToString name body
            return $ config { cfgKind = kind }
        "defaultKind" -> do
            defaultKind <- parseString name defaultKindName body
            return $ config { cfgDefaultKind = defaultKind }
        "kDepsOn" -> do
            kDepsOn <- parseKDepOn body
            return $ config { cfgKDepsOn = kDepsOn }
        "kParaMax" -> do
            kParaMax <- parseStringsToInt name body
            return $ config { cfgKParaMax = kParaMax }
        _ -> Left $ "Unknown config section name: " ++ secName section

parseInt :: String -> Int -> [String] -> Either String Int
parseInt _ i [] = Right i
parseInt e i (l : ls)
    | [] <- ws = parseInt e i ls
    | [w] <- ws = maybeToEither parseErr $ readMaybe w
    | otherwise = Left parseErr
    where
        parseErr = e ++ ": Parse error"
        ws = words l

parseString :: String -> String -> [String] -> Either String String
parseString _ s [] = Right s
parseString e s (l : ls)
    | [] <- ws = parseString e s ls
    | [w] <- ws = Right w
    | otherwise = Left parseErr
    where
        parseErr = e ++ ": Parse error"
        ws = words l

parseStrings :: [String] -> Set String
parseStrings ls = Set.fromList $ concat $ map words ls

parseStringToStrings :: [String] -> Map String (Set String)
parseStringToStrings ls = Map.fromList $ map parse ls'
    where
        ls' = filter (not . null) $ map words ls
        parse l = (head l, Set.fromList $ tail l)

parseStringToInt :: String -> [String] -> Either String (Map String Int)
parseStringToInt e ls = do
    sis <- mapM parse ls'
    return $ Map.fromList sis
    where
        ls' = filter (not . null) $ map words ls
        parse l = do
            eitherGuard (e ++ ": Argument count must be 2: " ++ unwords l) (length l == 2)
            let [str, sint] = l
            int <- maybeToEither (e ++ ": Argument must be number: " ++ sint) $ readMaybe sint
            return (str, int)

parseStringToString :: String -> [String] -> Either String (Map String String)
parseStringToString e ls = do
    sss <- mapM parse ls'
    return $ Map.fromList sss
    where
        ls' = filter (not . null) $ map words ls
        parse l = do
            eitherGuard (e ++ ": Argument count must be 2: " ++ unwords l) (length l == 2)
            let [str1, str2] = l
            return (str1, str2)

parseStringsToInt :: String -> [String] -> Either String [(Set String, Int)]
parseStringsToInt e ls = mapM parse ls'
    where
        ls' = filter (not . null) $ map words ls
        parse l = do
            let strs = init l
                sint = last l
            int <- maybeToEither (e ++ ": Argument must be number: " ++ sint) $ readMaybe sint
            return (Set.fromList strs, int)

parseArgClassify :: [String] -> Either String (Map String [ReadOrWrite])
parseArgClassify ls = do
    irws <- mapM parse ls'
    return $ Map.fromList irws
    where
        ls' = filter (not . null) $ map words ls
        parse l = do
            let inst = head l
                rwss = tail l
                cls "r" = Right RwRead
                cls "w" = Right RwWrite
                cls "_" = Right RwIgnore
                cls s = Left $
                    "argClassify: " ++ inst ++ ": Unknown argument mode: " ++ s
            rws <- mapM cls rwss
            return (inst, rws)

parseKDepOn :: [String] -> Either String (Map (String, String) (Maybe Latency))
parseKDepOn ls = do
    let ls' = filter (not . null) $ map words ls
    eitherGuard "kind: Body is null" (not $ null ls')
    let kinds : table = ls'
        klen = length kinds
    eitherGuard "kind: Mismatch table size" (length table == klen)
    for_ table $ \ row -> do
        eitherGuard "kind: Mismatch table size" (length row == klen)
    let parseElem "_" = Right Nothing
        parseElem s = do
            i <- maybeToEither "kind: Failed to parse table element" $ readMaybe s
            return $ Just i
        flatTable = concat table
        kks = cartesianProduct kinds kinds
    mlats <- mapM parseElem flatTable
    return $ Map.fromList $ zip kks mlats
