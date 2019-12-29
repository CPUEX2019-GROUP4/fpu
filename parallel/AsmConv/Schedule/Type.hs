{-# LANGUAGE DeriveGeneric #-}

module AsmConv.Schedule.Type where

import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable

type Map = Map.HashMap
type Set = Set.HashSet

newtype InstId = InstId Int deriving (Show, Eq, Ord, Generic)
instance Hashable InstId

type Latency = Int

type Dep = [(Latency, InstId)]
type DepEdge = (InstId, Dep)
type DepMap = Map InstId Dep
