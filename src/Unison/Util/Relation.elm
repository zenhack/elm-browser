module Unison.Util.Relation exposing (..)

import HashingContainers.HashDict exposing (HashDict)
import HashingContainers.HashSet exposing (HashSet)
import Misc exposing (hashDictFromListWith, hashSetSingleton, hashSetUnion)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)


{-| Haskell type: Unison.Util.Relation.Relation
-}
type alias Relation a b =
    { domain : HashDict a (HashSet b)
    , range : HashDict b (HashSet a)
    }


relationFromList :
    Equality a
    -> Hashing a
    -> Equality b
    -> Hashing b
    -> List ( a, b )
    -> Relation a b
relationFromList equalityA hashingA equalityB hashingB elements =
    { domain =
        hashDictFromListWith
            equalityA
            hashingA
            (hashSetUnion equalityB hashingB)
            (List.map
                (\( x, y ) -> ( x, hashSetSingleton equalityB hashingB y ))
                elements
            )
    , range =
        hashDictFromListWith
            equalityB
            hashingB
            (hashSetUnion equalityA hashingA)
            (List.map
                (\( x, y ) -> ( y, hashSetSingleton equalityA hashingA x ))
                elements
            )
    }
