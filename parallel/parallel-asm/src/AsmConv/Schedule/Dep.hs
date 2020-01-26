module AsmConv.Schedule.Dep (
    DepsOn, makeDepEdges
) where

import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.List
import AsmConv.Schedule.Type
import AsmConv.Util

type DepsOn i = i -> i -> Maybe Latency

makeDepEdges :: DepsOn i -> [(InstId, i)] -> [DepEdge]
makeDepEdges depsOn insts = depEdges
    where
        instMap = Map.fromList insts
        depEdges = makeDepEdges' depsOn instMap $ sort $ map fst insts

makeDepEdges' :: DepsOn i -> Map InstId i -> [InstId] -> [DepEdge]
makeDepEdges' depsOn instMap instIds = depEdges
    where
        makeDepEdge (_, priorIds) instId =
            (makeDep depsOn instMap priorIds instId, instId : priorIds)
        dummy = error "dummy"
        deps = map fst $ tail $ scanl' makeDepEdge (dummy, []) instIds
        depEdges = zip instIds deps

makeDep :: DepsOn i -> Map InstId i -> [InstId] -> InstId -> Dep
makeDep depsOn instMap priorIds instId = dep
    where
        inst = fromJust $ Map.lookup instId instMap
        priorInsts = map (fromJust . flip Map.lookup instMap) priorIds
        maybeDeps = map (depsOn inst) priorInsts
        dep = mapMaybe (mapFstM id) $ zip maybeDeps priorIds
