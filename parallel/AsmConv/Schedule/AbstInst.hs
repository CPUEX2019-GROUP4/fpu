module AsmConv.Schedule.AbstInst (
    AbstInst(..), makeToAbstInst
) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable
import AsmConv.Util
import AsmConv.Schedule.Type
import AsmConv.Schedule.Config

data AbstInst k r = AbstInst {
    aiInstKind :: k,
    aiLatency :: Latency,
    aiWriteRegs :: [r],
    aiReadRegs :: [r]
} deriving (Show)

makeToAbstInst :: (Eq i, Eq r, Hashable i, Hashable r) =>
    Config i r k -> i -> [r] -> Either String (AbstInst k r)
makeToAbstInst config inst args = do
    let Config {
            cfgIgnoreRegs = ignoreRegs,
            cfgArgClassify = argClassify,
            cfgImplicitReadArg = implicitReadArg,
            cfgImplicitWriteArg = implicitWriteArg,
            cfgKind = kind,
            cfgDefaultKind = defaultKind,
            cfgLatency = latency,
            cfgDefaultLatency = defaultLatency
        } = config
        unknownInstErr = "Unknown instruction"
        argsNumErr = "Invalid arguments number"
    classify <- maybeToEither unknownInstErr $ Map.lookup inst argClassify
    (wRegs, rRegs) <- maybeToEither argsNumErr $ partitionRegs args classify
    let lat = Map.lookupDefault defaultLatency inst latency
        ki = Map.lookupDefault defaultKind inst kind
        irRegs = Map.lookupDefault Set.empty inst implicitReadArg
        iwRegs = Map.lookupDefault Set.empty inst implicitWriteArg
        wRegs' = Set.toList $ Set.difference (Set.union iwRegs $ Set.fromList wRegs) ignoreRegs
        rRegs' = Set.toList $ Set.difference (Set.union irRegs $ Set.fromList rRegs) ignoreRegs
    return AbstInst {
        aiInstKind = ki,
        aiLatency = lat,
        aiWriteRegs = wRegs',
        aiReadRegs = rRegs'
    }

-- returns: (write regs, read regs)
partitionRegs :: [r] -> [ReadOrWrite] -> Maybe ([r], [r])
partitionRegs [] [] = Just ([], [])
partitionRegs (reg : regs) (clf : clfs) = do
    (w, r) <- partitionRegs regs clfs
    return $ case clf of
        RwRead -> (w, reg : r)
        RwWrite -> (reg : w, r)
        RwIgnore -> (w, r)
partitionRegs _ _ = Nothing
