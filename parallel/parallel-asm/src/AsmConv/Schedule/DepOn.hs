module AsmConv.Schedule.DepOn (
    DepsOn, makeDepsOn
) where

import Data.List
import AsmConv.Schedule.AbstInst
import AsmConv.Schedule.Type

type DepsOn i = i -> i -> Maybe Latency

makeDepsOn :: (Eq r) => (k -> k -> Maybe Latency) -> DepsOn (AbstInst k r)
makeDepsOn kDepsOn i1 i2 = max kDep regDep
    where
        AbstInst {
            aiInstKind = kind1,
            aiWriteRegs = writeRegs1,
            aiReadRegs = readRegs1
        } = i1
        AbstInst {
            aiInstKind = kind2,
            aiLatency = latency2,
            aiWriteRegs = writeRegs2,
            aiReadRegs = readRegs2
        } = i2
        kDep = kDepsOn kind1 kind2
        regDep = regDepsOn writeRegs1 readRegs1 writeRegs2 readRegs2 latency2

regDepsOn :: (Eq r) => [r] -> [r] -> [r] -> [r] -> Latency -> Maybe Latency
regDepsOn w1 r1 w2 r2 l =
    if not $ null $ intersect w1 w2 then
        Just 1
    else if not $ null $ intersect r1 w2 then
        Just l
    else if not $ null $ intersect w1 r2 then
        Just 0
    else
        Nothing
