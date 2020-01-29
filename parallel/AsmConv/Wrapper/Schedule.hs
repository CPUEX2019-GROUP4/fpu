{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AsmConv.Wrapper.Schedule (
    schedule
) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vec
import GHC.Generics (Generic)

import qualified Back.Asm as Asm
import Front.Syntax (
    Unary_operator(..), Arith_unary(..), Arith_binary(..), Float_unary(..),
    Float_binary(..)
    )
import qualified RunRun.Type as Type
import RunRun.RunRun (
    RunRun, genid, eputstrln
    -- , throw, Error(..)
    )

import AsmConv.Schedule.Config
import AsmConv.Schedule.Schedule (scheduleNpt)
import AsmConv.Util

data Kind = KDefault | KMread | KMwrite | KIn | KOut | KCall | KIf
    deriving (Show, Eq, Enum, Bounded, Generic)
instance Hashable Kind

deriving instance Generic Unary_operator
instance Hashable Unary_operator
deriving instance Generic Arith_unary
instance Hashable Arith_unary
deriving instance Generic Arith_binary
instance Hashable Arith_binary
deriving instance Generic Float_unary
instance Hashable Float_unary
deriving instance Generic Float_binary
instance Hashable Float_binary

data InstName =
    Nop
    | Li
    | FLi
    | SetL
    | Mv
    | Out
    | In
    | Unary_op Unary_operator
    | Arith1 Arith_unary
    | Arith2 Arith_binary
    | Float1 Float_unary
    | Float2 Float_binary
    | Slw
    | Lw
    | Sw
    | FMv
    | Cmp
    | If
    | IfCmp
    | FIfCmp
    | Lf
    | Sf
    | CallDir
    | Save
    | SaveFloat
    | Restore
    | Makearray
    deriving (Show, Eq, Generic)
instance Hashable InstName

-- (instKind, writeRegs, readRegs)
type Inst = (InstName, [String], [String])

config :: Config InstName String Kind
config = defaultConfig {
    cfgParaNum = 1,
    cfgIgnoreRegs = Set.fromList ["r0"],
    cfgLatency = Map.fromList [
        (Mv, 0),
        (FMv, 0),
        (Float2 FAdd, 2),
        (Float2 FSub, 2),
        (Float2 FMul, 2)
    ],
    cfgDefaultLatency = 1,
    cfgKind = Map.fromList [
        (Out, KOut),
        (In, KIn),
        (Lw, KMread),
        (Sw, KMwrite),
        (If, KIf),
        (IfCmp, KIf),
        (FIfCmp, KIf),
        (Lf, KMread),
        (Sf, KMwrite),
        (CallDir, KCall),
        (Save, KMwrite),
        (Restore, KMread),
        (Makearray, KCall)
    ],
    cfgDefaultKind = KDefault,
    cfgKDepsOn =
        let ks = enumFrom minBound :: [Kind]
            ___ = Nothing
            d = Just
        in Map.fromList $ zip (cartesianProduct ks ks) [
            -- KDefault KMread KMwrite KIn KOut KCall KIf
            -- KCall, KIf は場所を動かさない
            ___, ___, ___, ___, ___, d 1, d 1,
            ___, ___, d 1, ___, ___, d 1, d 1,
            ___, d 0, d 1, ___, ___, d 1, d 1,
            ___, ___, ___, d 1, ___, d 1, d 1,
            ___, ___, ___, ___, d 1, d 1, d 1,
            d 0, d 0, d 0, d 0, d 0, d 1, d 1,
            d 0, d 0, d 0, d 0, d 0, d 1, d 1
        ]
}

schedule :: Asm.Aprog -> RunRun Asm.Aprog
schedule (Asm.Aprog afundefs theT) = do
    eputstrln $ "schedule ..."
    theT' <- tSchedule Type.Unit theT
    afundefs' <- flip mapM afundefs $ \ f -> do
        t <- tSchedule (Asm.a_ret f) (Asm.a_body f)
        return $ f { Asm.a_body = t }
    return $ Asm.Aprog afundefs' theT'

tSchedule :: Type.Type -> Asm.T -> RunRun Asm.T
tSchedule theTy theT = do
    theVar <- genid "AsmConv.Wrapper.Schedule.ans"
    (varexps, insts) <- unzip <$> toInsts (theVar, theTy) theT

    let scheduled = concat $ scheduleNpt config insts
        varexpVec = Vec.fromList varexps
        scheduledVarexps = map (varexpVec Vec.!) scheduled
    -- もともと Ans のところにあった Exp が別のところに行っているかもしれないので、
    -- 最後に Mv を入れる。
    lastExp <- case theTy of
        Type.Float -> return $ Asm.FMv theVar
        -- Type.Int -> return $ Asm.Mv theVar
        Type.Unit -> return $ Asm.Nop -- 本来の意味の nop (プログラムの終了ではない)
        _ -> return $ Asm.Mv theVar
        -- _ -> throw $ Fail $ "Fatal: AsmConv.Wrapper.Schedule.tSchedule: "
        --     ++ "Move inst is undefined for type " ++ show theTy
    let scheduledT = foldr (\ (varty, e) t -> Asm.Let varty e t)
            (Asm.Ans lastExp) scheduledVarexps

    return scheduledT

toInsts :: (String, Type.Type) -> Asm.T
    -> RunRun [(((String, Type.Type), Asm.Exp), Inst)]
toInsts theVarty@(_, theTy) (Asm.Ans e) = do
    e' <- expSchedule theTy e
    let inst = toInst e'
    return [((theVarty, e'), inst)]
toInsts theVarty (Asm.Let varty@(var, ty) e t) = do
    e' <- expSchedule ty e
    let (instName, wRegs, rRegs) = toInst e'
    insts <- toInsts theVarty t
    return $ ((varty, e'), (instName, var : wRegs, rRegs)) : insts

expSchedule :: Type.Type -> Asm.Exp -> RunRun Asm.Exp
expSchedule ty (Asm.If r1 t1 t2) = do
    t1' <- tSchedule ty t1
    t2' <- tSchedule ty t2
    return $ Asm.If r1 t1' t2'
expSchedule ty (Asm.IfCmp c r1 r2 t1 t2) = do
    t1' <- tSchedule ty t1
    t2' <- tSchedule ty t2
    return $ Asm.IfCmp c r1 r2 t1' t2'
expSchedule ty (Asm.FIfCmp c r1 r2 t1 t2) = do
    t1' <- tSchedule ty t1
    t2' <- tSchedule ty t2
    return $ Asm.FIfCmp c r1 r2 t1' t2'
expSchedule _ e = return e

toInst :: Asm.Exp -> Inst
toInst Asm.Nop                          = (Nop, [], [])
toInst (Asm.Li _)                       = (Li, [], [])
toInst (Asm.FLi _)                      = (FLi, [], [])
toInst (Asm.SetL _)                     = (SetL, [], [])
toInst (Asm.Mv r1)                      = (Mv, [], [r1])
toInst (Asm.Out _ r1)                   = (Out, [], [r1])
toInst (Asm.In _)                       = (In, [], [])
toInst (Asm.Unary_op op _ _ r1)         = (Unary_op op, [], [r1])
toInst (Asm.Arith1 op r1)               = (Arith1 op, [], [r1])
toInst (Asm.Arith2 op r1 (Asm.V r2))    = (Arith2 op, [], [r1, r2])
toInst (Asm.Arith2 op r1 (Asm.C _))     = (Arith2 op, [], [r1])
toInst (Asm.Float1 op r1)               = (Float1 op, [], [r1])
toInst (Asm.Float2 op r1 r2)            = (Float2 op, [], [r1, r2])
toInst (Asm.Slw r1 (Asm.V r2))          = (Slw, [], [r1, r2])
toInst (Asm.Slw r1 (Asm.C _))           = (Slw, [], [r1])
toInst (Asm.Lw r1 (Asm.V r2))           = (Lw, [], [r1, r2])
toInst (Asm.Lw r1 (Asm.C _))            = (Lw, [], [r1])
toInst (Asm.Sw r1 r2 (Asm.V r3))        = (Sw, [], [r1, r2, r3])
toInst (Asm.Sw r1 r2 (Asm.C _))         = (Sw, [], [r1, r2])
toInst (Asm.FMv r1)                     = (FMv, [], [r1])
toInst (Asm.Cmp _ r1 (Asm.V r2))        = (Cmp, [], [r1, r2])
toInst (Asm.Cmp _ r1 (Asm.C _))         = (Cmp, [], [r1])
------- とりあえず ここから -------
toInst (Asm.If r1 _ _)                  = (If, [], [r1])
toInst (Asm.IfCmp _ r1 r2 _ _)          = (IfCmp, [], [r1, r2])
toInst (Asm.FIfCmp _ r1 r2 _ _)         = (FIfCmp, [], [r1, r2])
------- とりあえず ここまで -------
toInst (Asm.Lf r1 (Asm.V r2))           = (Lf, [], [r1, r2])
toInst (Asm.Lf r1 (Asm.C _))            = (Lf, [], [r1])
toInst (Asm.Sf r1 r2 (Asm.V r3))        = (Sf, [], [r1, r2, r3])
toInst (Asm.Sf r1 r2 (Asm.C _))         = (Sf, [], [r1, r2])
toInst (Asm.CallDir _ rs1 rs2)          = (CallDir, [], rs1 ++ rs2)
toInst (Asm.Save r1 r2)                 = (Save, [], [r1, r2])
toInst (Asm.SaveFloat r1 r2)            = (SaveFloat, [], [r1, r2])
toInst (Asm.Restore r1)                 = (Restore, [], [r1])
toInst (Asm.Makearray _ (Asm.V r1) r2)  = (Makearray, [], [r1, r2])
toInst (Asm.Makearray _ (Asm.C _) r2)   = (Makearray, [], [r2])
