module AsmConv.Schedule.Config where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable
import AsmConv.Schedule.Type

data ReadOrWrite = RwRead | RwWrite | RwIgnore

data Config i r k = Config {
    cfgParaNum :: Int,
    cfgIgnoreRegs :: Set r,
    cfgArgClassify :: Map i [ReadOrWrite],
    cfgImplicitReadArg :: Map i (Set r),
    cfgImplicitWriteArg :: Map i (Set r),
    cfgLatency :: Map i Latency,
    cfgDefaultLatency :: Latency,
    cfgKind :: Map i k,
    cfgDefaultKind :: k,
    cfgKDepsOn :: Map (k, k) (Maybe Latency),
    cfgKParaMax :: [(Set k, Int)]
}

defaultConfig :: Config i r k
defaultConfig = Config {
    cfgParaNum = 1,
    cfgIgnoreRegs = Set.empty,
    cfgArgClassify = Map.empty,
    cfgImplicitReadArg = Map.empty,
    cfgImplicitWriteArg = Map.empty,
    cfgLatency = Map.empty,
    cfgDefaultLatency = 1,
    cfgKind = Map.empty,
    cfgDefaultKind = error "default kind is not given",
    cfgKDepsOn = Map.empty,
    cfgKParaMax = []
}

validateConfig :: (Eq k, Hashable k) => Config i r k -> Either String ()
validateConfig c =
    if cfgParaNum c <= 0 then
        Left "paraNum must be positive"
    else if not (Set.null unknownKinds && Set.member (cfgDefaultKind c) kinds) then
        Left "Unknown kind exists"
    else if any (<= 0) $ map snd $ cfgKParaMax c then
        Left "kParaNum must be positive"
    else
        return ()
    where
        kinds = Set.fromList $ map fst $ Map.keys $ cfgKDepsOn c
        unknownKinds = Set.difference (Set.fromList $ Map.elems $ cfgKind c) kinds
