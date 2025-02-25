module Unison.Codebase.Causal exposing (..)

import HashingContainers.HashSet as HashSet exposing (HashSet)
import Unison.Codebase.Branch exposing (..)
import Unison.Hash exposing (Hash32)


{-| Haskell type: Unison.Codebase.Causal.Raw
-}
type RawCausal
    = RawOne RawBranch
    | RawCons RawBranch Hash32
    | RawMerge RawBranch (HashSet Hash32)


rawCausalHead :
    RawCausal
    -> RawBranch
rawCausalHead causal =
    case causal of
        RawOne branch ->
            branch

        RawCons branch _ ->
            branch

        RawMerge branch _ ->
            branch


{-| Predecessors in the causal chain.
-}
rawCausalPredecessors :
    RawCausal
    -> List Hash32
rawCausalPredecessors causal =
    case causal of
        RawOne _ ->
            []

        RawCons _ hash ->
            [ hash ]

        RawMerge _ hashes ->
            HashSet.toList hashes
