module AsmConv.Schedule.Schedule (
    schedule
) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable
import Data.Maybe
import AsmConv.Schedule.AbstInst
import AsmConv.Schedule.Config
import AsmConv.Schedule.Dep
import AsmConv.Schedule.DepOn
import AsmConv.Schedule.ListSchedule
import AsmConv.Util
import AsmConv.Schedule.Type
-- import Debug.Trace

schedule :: (Eq i, Eq r, Eq k, Hashable i, Hashable r, Hashable k) =>
    Config i r k -> [(i, [r])] -> Either (String, Int) [[Int]]
schedule config insts = do
    let instIds = map InstId [0 ..]
        idInsts = zip instIds insts
    idAbstInsts <- mapLeft (mapSnd unInstId) $ makeAbstInsts config idInsts
    let depsOn = makeDepsOn $ curry (fromJust . flip Map.lookup (cfgKDepsOn config))
        depEdges = makeDepEdges depsOn idAbstInsts
        paraMax = makeParaMax (cfgKParaMax config) idAbstInsts
        lsConfig = defaultScheduleConfig {
            scParaNum = cfgParaNum config,
            scParaMax = paraMax,
            scLatency = Map.fromList $ map (mapSnd aiLatency) idAbstInsts
        }
        scheduledIds = listSchedule lsConfig depEdges
    return $ map (map unInstId) scheduledIds

unInstId :: InstId -> Int
unInstId (InstId i) = i

makeAbstInsts :: (Eq i, Eq r, Hashable i, Hashable r) =>
    Config i r k -> [(InstId, (i, [r]))]
    -> Either (String, InstId) [(InstId, AbstInst k r)]
makeAbstInsts config idInsts = do
    let eitherMapWithId f l = mapLeft (mapSnd fst) $ eitherMap (mapSndM f) l
        toAbstInst = uncurry $ makeToAbstInst config
    idAbstInsts <- eitherMapWithId toAbstInst idInsts
    return idAbstInsts

makeParaMax :: (Eq k, Hashable k) =>
    [(Set k, Int)] -> [(InstId, AbstInst k r)] -> [(Set InstId, Int)]
makeParaMax kParaMax idAbstInsts = zip constrIds paraMaxs
    where
        (kindss, paraMaxs) = unzip kParaMax
        constrIds = flip map kindss $ \ ks ->
            Set.fromList $ map fst
                $ filter (flip Set.member ks . aiInstKind . snd) idAbstInsts
