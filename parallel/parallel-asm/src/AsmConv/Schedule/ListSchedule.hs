module AsmConv.Schedule.ListSchedule (
    ScheduleConfig(..), listSchedule, defaultScheduleConfig
) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Maybe
import Data.List
import AsmConv.Schedule.Type
-- import Debug.Trace

type Ready = Map InstId [InstId]

type AccumLatency = Map InstId Latency
type ExecClk = Map InstId Int

data ScheduleConfig = ScheduleConfig {
    scParaNum :: Int,
    scParaMax :: [(Set InstId, Int)],
    scLatency :: Map InstId Latency
} deriving (Show)

data ExecState = ExecState {
    esConfig :: ScheduleConfig,
    esDepMap :: DepMap,
    esAccumLatency :: AccumLatency,
    esClk :: Int,
    esExecClk :: ExecClk,
    esLeftInst :: Set InstId
} deriving (Show)

defaultScheduleConfig :: ScheduleConfig
defaultScheduleConfig = ScheduleConfig {
    scParaNum = 1,
    scParaMax = [],
    scLatency = Map.empty
}

listSchedule :: ScheduleConfig -> [DepEdge] -> [[InstId]]
listSchedule config deps = instss
    where
        depMap = Map.fromList deps
        latency = scLatency config
        leftInst = Set.fromList $ map fst deps
        accumLatency = calcAccumLatency latency deps
        inites = ExecState {
            esConfig = config,
            esDepMap = depMap,
            esAccumLatency = accumLatency,
            esClk = 0,
            esExecClk = Map.empty,
            esLeftInst = leftInst
        }
        lastes = until finishedSchduling nextExecState inites
        instss = getInstsList lastes

finishedSchduling :: ExecState -> Bool
finishedSchduling es = null $ esLeftInst es

getInstsList :: ExecState -> [[InstId]]
getInstsList (ExecState { esExecClk = execClk }) = instss
    where
        instClks = sortOn snd $ Map.toList execClk
        instClkss = groupBy (\ x y -> snd x == snd y) instClks
        instss = map (map fst) instClkss

nextExecState :: ExecState -> ExecState
nextExecState es = es'
    where
        ExecState { esClk = clk } = es
        ready = getReady es
        insts = takeInsts es ready
        es' = (execInsts insts es) { esClk = clk + 1 }

getReady :: ExecState -> Ready
getReady es = ready
    where
        ExecState { esClk = clk, esExecClk = execClk,
            esDepMap = depMap, esLeftInst = leftInst } = es
        latency = scLatency $ esConfig es
        notFinishedInst = Set.union leftInst $ Set.fromList $ map fst
            $ flip filter (Map.toList execClk) $ \ (i, c) ->
                clk < c + Map.lookupDefault 1 i latency
        updateCanExec can inst =
            let deps = fromJust $ Map.lookup inst depMap
                leftDeps = filter (flip Set.member notFinishedInst . snd) deps
                (weakDeps, strongDeps) = partition ((== 0) . fst) leftDeps
                weakInsts = map snd weakDeps
            in
                if null strongDeps && all (flip Map.member can) weakInsts then
                    Map.insert inst weakInsts can
                else
                    can
        ready = foldl' updateCanExec Map.empty $ sort $ Set.toList leftInst

takeInsts :: ExecState -> Ready -> [InstId]
takeInsts es ready = takenInsts
    where
        config = esConfig es
        paraNum = scParaNum config
        paraMax = scParaMax config
        instDeps = Map.toList ready
        notSortedInsts = map fst instDeps
        accls = map (fromJust . flip Map.lookup (esAccumLatency es)) notSortedInsts
        sortedInsts = map fst $ sortOn (\ (inst, accl) -> (-accl, inst))
            $ zip notSortedInsts accls
        satisfyDeps insts (inst, dep) =
            let depIndexMax = maximum $ (-1) : map (fromJust . flip elemIndex insts) dep
                instIndex = fromJust $ elemIndex inst insts
            in if depIndexMax > instIndex then
                let (left, right) = splitAt (depIndexMax + 1) insts
                    (lleft, i : lright) = splitAt instIndex left
                in lleft ++ lright ++ [i] ++ right
            else insts
        trueSortedInsts = foldl' satisfyDeps sortedInsts $ sortOn fst instDeps
        satisfyParaMax (taken, left) (constrInsts, maxNum) =
            let isCinsts = map (flip Set.member constrInsts) taken
                ti1 = length $ flip takeWhile (tail $ inits isCinsts) $ \ cs ->
                    maxNum >= (length $ filter id cs)
                (takenl, takenr) = splitAt ti1 taken
                takenr' = filter (not . flip Set.member constrInsts) takenr
                left' = filter (not . flip Set.member constrInsts) left
                newTaken = take (paraNum - length takenl - length takenr') left'
            in (takenl ++ takenr' ++ newTaken, left')
        (takenInsts, _) = foldl' satisfyParaMax (splitAt paraNum trueSortedInsts) paraMax

execInsts :: [InstId] -> ExecState -> ExecState
execInsts insts es = es'
    where
        ExecState { esClk = clk, esExecClk = execClk, esLeftInst = leftInst } = es
        leftInst' = foldl' (flip Set.delete) leftInst insts
        execClk' = foldl' (flip $ flip Map.insert clk) execClk insts
        es' = es { esExecClk = execClk', esLeftInst = leftInst' }

calcAccumLatency :: Map InstId Latency -> [DepEdge] -> AccumLatency
calcAccumLatency latency deps = accls
    where
        sortedDeps = reverse $ sortOn fst deps
        updateAccl acclm (nowI, depLis) =
            let nowAccl = fromJust $ Map.lookup nowI acclm
                acclm' = foldl' (\ m (l, i) ->
                    Map.adjust (max (nowAccl + l)) i m) acclm depLis
            in acclm'
        accls = foldl' updateAccl latency sortedDeps
