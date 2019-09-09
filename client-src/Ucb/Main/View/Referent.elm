module Ucb.Main.View.Referent exposing (viewReferent)

import Element exposing (..)
import Element.Font exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Unison.Referent exposing (..)
import Yaks.PrettyPrint as PP


viewReferent :
    { showBuiltin : Bool
    , take : Maybe Int
    }
    -> Referent
    -> PP.Doc msg
viewReferent opts referent =
    case referent of
        Ref reference ->
            viewReference opts reference

        Con reference _ _ ->
            viewReference opts reference
