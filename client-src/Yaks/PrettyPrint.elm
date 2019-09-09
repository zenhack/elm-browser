module Yaks.PrettyPrint
    exposing
        ( Doc
        , append
        , concat
        , empty
        , html
        , indent
        , join
        , lines
        , parensIf
        , text
        , toHtml
        , toString
        , words
        )

import Html exposing (Html, span, pre)

-- PRIMITIVES

type Doc msg
    = Lines (List String)
    | Indent Int (Doc msg)
    | Append (Doc msg) (Doc msg)
    | Html (Html msg -> Html msg) (Doc msg)

text : String -> Doc msg
text s =
    case s of
        "" ->
            Lines []

        _ ->
            Lines (String.split "\n" s)

append : Doc msg -> Doc msg -> Doc msg
append = Append

indent : Int -> Doc msg -> Doc msg
indent = Indent

html
    : (List (Html msg) -> Html msg)
    -> Doc msg
    -> Doc msg
html f doc =
    Html (\kid -> f [kid]) doc

toString : Doc msg -> String
toString = toStringLevel 0

toHtml : Doc msg -> Html msg
toHtml doc =
    pre [] [ toHtmlLevel 0 doc ]

toHtmlLevel : Int -> Doc msg -> Html msg
toHtmlLevel indentLevel doc =
    case doc of
        Append x y ->
            Html.span
                []
                [ toHtmlLevel indentLevel x
                , toHtmlLevel indentLevel y
                ]

        Indent n innerDoc ->
            toHtmlLevel (indentLevel + n) innerDoc

        Lines ls ->
            let indentation = String.repeat indentLevel " " in
            ls
            |> List.map (\l -> Html.text (indentation ++ l))
            |> Html.span []

        Html mkElement innerDoc ->
            mkElement (toHtmlLevel indentLevel innerDoc)

toStringLevel : Int -> Doc msg -> String
toStringLevel indentLevel doc =
    -- TODO: this has poor asymtotic performance, because (++) is O(n). We could improve
    -- perf by using `Bytes.Encoder`, and then convert back to a string at the end.
    case doc of
        Append x y -> toStringLevel indentLevel x ++ toStringLevel indentLevel y

        Indent n innerDoc ->
            toStringLevel (indentLevel + n) innerDoc

        Lines ls ->
            let indentation = String.repeat indentLevel " " in
            ls
            |> List.map (\l -> indentation ++ l)
            |> String.join "\n"

        Html _ innerDoc ->
            toStringLevel indentLevel innerDoc

-- Everything below here could concievably be in another module; they don't depend on non-exported things:

empty : Doc msg
empty =
    text ""

concat : List (Doc msg) -> Doc msg
concat =
    List.foldl append empty

join : Doc msg -> List (Doc msg) -> Doc msg
join sep =
    List.intersperse sep >> concat

parensIf : Bool -> Doc msg -> Doc msg
parensIf b doc =
    if b then
        concat [ text "(", doc, text ")" ]
    else
        doc

lines : List (Doc msg) -> Doc msg
lines = join (text "\n")

words : List (Doc msg) -> Doc msg
words = join (text " ")
