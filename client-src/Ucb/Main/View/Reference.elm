module Ucb.Main.View.Reference exposing (viewId, viewReference)

import Misc exposing (maybe)
import Unison.Reference exposing (..)
import Yaks.PrettyPrint as PP
import Html.Styled exposing (span, toUnstyled, fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Css exposing (color, rgb)


viewReference :
    { showBuiltin : Bool
    , take : Maybe Int
    }
    -> Reference
    -> PP.Doc msg
viewReference { showBuiltin, take } reference =
    case reference of
        Builtin name ->
            if showBuiltin then
                PP.text name

            else
                PP.empty

        Derived id ->
            viewId take id


viewId : Maybe Int -> Id -> PP.Doc msg
viewId take { hash, pos, size } =
    let textColor = rgb 127 127 127
        highlight =
            PP.html
                (List.map fromUnstyled
                    >> span [ css [ color textColor ] ]
                    >> toUnstyled
                    )
    in
    PP.concat
        [ highlight
            (PP.text (maybe identity String.left take hash))
        , if size > 1 then
            highlight
                (PP.text (String.cons '#' (String.fromInt pos)))

          else
              PP.empty
        ]
