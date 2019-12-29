module AsmConv.Schedule.Config where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
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
