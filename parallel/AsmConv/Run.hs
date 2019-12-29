module AsmConv.Run (
    run
    ) where

import qualified Data.HashMap.Strict as Map
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import AsmConv.Parse.Config
import AsmConv.Parse.Inst
import AsmConv.Schedule.Schedule
import AsmConv.Util
-- import Debug.Trace

run :: String -> String -> Either String String
run sconfig sprog = do
    config <- parseConfig sconfig
    let prog = toInsts sprog
        prog' = filter ((/= IkOther) . iKind) prog
        intLabelInstss = (split . dropDelims. whenElt) ((== IkLabel) . iKind) prog'
    scheduledInsts <- for intLabelInstss $ \ is -> do
        let escheduled = schedule config $ map (unIkInst . iKind) is
        scheduled <- flip mapLeft escheduled $ \ (s, i) ->
            let inst = is !! i
            in s ++ ": " ++ iInst inst ++ " at row " ++ show (iRow inst)
        let isMap = Map.fromList $ zip [0 ..] is
            scheduledInst = map (map $ fromJust . flip Map.lookup isMap) scheduled
        return scheduledInst
    let output = emit prog scheduledInsts
    return output

unIkInst :: InstKind -> (String, [String])
unIkInst (IkInst x) = x
unIkInst _ = error "AsmConv.Run.unIkInst: not IkInst"

emit :: [Inst] -> [[[Inst]]] -> String
emit prog scheduledInsts = instStr
    where
        labels = map iInst $ filter ((== IkLabel) . iKind) prog
        dummy = error "dummy"
        dlabels = dummy : labels
        scheduleds = map
            (intercalate "\n" . (intersperse "    ;" . map (intercalate "\n" . map iInst)))
            scheduledInsts
        insts = tail $ concat $ zipWith (\ x y -> [x, y]) dlabels scheduleds
        instStr = unlines $ filter (/= "") insts
