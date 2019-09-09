module Ucb.Main.View.Term exposing (viewTerm)

import Array exposing (Array)
import Element exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Int64 exposing (..)
import Misc exposing (..)
import Ucb.Main.View.Referent exposing (viewReferent)
import Ucb.Unison.Name exposing (..)
import Ucb.Unison.NameDict exposing (..)
import Ucb.Unison.ReferentSet exposing (..)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Name exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Word64 exposing (..)
import Yaks.PrettyPrint as PP


type alias Env =
    { precedence : Int
    , blockContext : BlockContext
    , infixContext : InfixContext
    }


type BlockContext
    = Block
    | Normal


type InfixContext
    = Infix
    | NonInfix


viewTerm :
    { r | head : Branch }
    -> Term Symbol
    -> PP.Doc msg
viewTerm view =
    viewTerm2
        view
        { precedence = -1
        , blockContext = Normal
        , infixContext = NonInfix
        }


viewTerm2 :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> PP.Doc msg
viewTerm2 view env { out } =
    case out of
        TermAbs var term ->
            PP.concat
                [ PP.text "("
                , viewTermVar var
                , PP.text " -> "
                , viewTerm view term
                , PP.text ")"
                ]

        TermCycle term ->
            viewTerm2 view env term

        TermVar var ->
            viewTermVar var

        TermTm (TermApp t1 t2) ->
            viewTermApp view env t1 t2

        TermTm (TermAnd t1 t2) ->
            viewTermAnd view env t1 t2

        TermTm (TermAnn term type_) ->
            viewTermAnn view env term type_

        TermTm (TermBlank blank) ->
            PP.text "(not implemented: TermBlank)"

        TermTm (TermBoolean b) ->
            viewTermBoolean b

        TermTm (TermChar c) ->
            viewTermChar c

        TermTm (TermConstructor reference n) ->
            viewTermConstructor view reference n

        TermTm (TermFloat n) ->
            viewTermFloat n

        TermTm (TermHandle t1 t2) ->
            viewTermHandle view env t1 t2

        TermTm (TermIf t1 t2 t3) ->
            viewTermIf view env t1 t2 t3

        TermTm (TermInt n) ->
            viewTermInt n

        TermTm (TermLam term) ->
            viewTermLam view env term

        TermTm (TermLet _ _ _) ->
            PP.text "(not implemented: TermLet)"

        TermTm (TermLetRec _ _ _) ->
            PP.text "(not implemented: TermLetRec)"

        TermTm (TermMatch _ _) ->
            PP.text "(not implemented: TermMatch)"

        TermTm (TermNat n) ->
            viewTermNat n

        TermTm (TermOr t1 t2) ->
            viewTermOr view env t1 t2

        TermTm (TermRef reference) ->
            viewTermRef view reference

        TermTm (TermRequest reference n) ->
            viewTermRequest view reference n

        TermTm (TermSequence terms) ->
            viewTermSequence view env terms

        TermTm (TermText s) ->
            PP.text "(not implemented: TermText)"


{-| Should be the same as viewTermOr
-}
viewTermAnd :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> PP.Doc msg
viewTermAnd view env t1 t2 =
    let
        env2 : Env
        env2 =
            { precedence = 10
            , blockContext = Normal
            , infixContext = NonInfix
            }
    in
    PP.parensIf (env.precedence >= 10)
        (PP.lines
            [ PP.text "and"
            , PP.indent 2 <| PP.lines
                [ viewTerm2 view env2 t1
                , viewTerm2 view env2 t2
                ]
            ]
        )


viewTermAnn :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Type Symbol
    -> PP.Doc msg
viewTermAnn view env term _ =
    viewTerm2 view env term


viewTermApp :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> PP.Doc msg
viewTermApp view env t1 t2 =
    case termUnApps t1 t2 of
        ( f, xs ) ->
            PP.parensIf
                (env.precedence >= 10)
                (PP.words
                    ((f :: xs)
                        |> List.map
                            (viewTerm2
                                view
                                { precedence = 10
                                , blockContext = Normal
                                , infixContext = NonInfix
                                }
                            )
                    )
                )


viewTermBoolean :
    Bool
    -> PP.Doc msg
viewTermBoolean b =
    PP.text
        (if b then
            "true"

         else
            "false"
        )


viewTermChar :
    Char
    -> PP.Doc msg
viewTermChar c =
    PP.text ("'" ++ String.fromChar c ++ "'")


viewTermConstructor :
    { r | head : Branch }
    -> Reference
    -> Int
    -> PP.Doc msg
viewTermConstructor view reference n =
    viewReferent_ view (Con reference n Data)


viewTermFloat :
    Float
    -> PP.Doc msg
viewTermFloat n =
    PP.text (String.fromFloat n)


viewTermHandle :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> PP.Doc msg
viewTermHandle view env t1 t2 =
    PP.parensIf (env.precedence >= 2)
        (PP.lines
            [ PP.words
                [ PP.text "handle"
                , viewTerm2
                    view
                    { precedence = 2
                    , blockContext = Normal
                    , infixContext = NonInfix
                    }
                    t1
                , PP.text "in"
                ]
            , PP.indent 2 <|
                viewTerm2
                    view
                    { precedence = 2
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t2
            ]
        )


viewTermIf :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> Term Symbol
    -> PP.Doc msg
viewTermIf view env t1 t2 t3 =
    PP.parensIf (env.precedence >= 2)
        (PP.lines
            [ PP.words
                [ PP.text "if"
                , viewTerm2
                    view
                    { precedence = 2
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t1
                , PP.text "then"
                ]
            , PP.indent 2 <|
                viewTerm2
                    view
                    { precedence = 0
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t2
            , PP.text "else"
            , PP.indent 2 <|
                viewTerm2
                    view
                    { precedence = 0
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t3
            ]
        )


viewTermInt :
    Int64
    -> PP.Doc msg
viewTermInt n =
    -- FIXME: this will do the wrong thing if the integer is out of the
    -- safe range.
    let
        n2 : Int
        n2 =
            unsafeInt64ToInt53 n
    in
    if n2 >= 0 then
        PP.text (String.cons '+' (String.fromInt n2))

    else
        PP.text (String.fromInt n2)


viewTermLam :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> PP.Doc msg
viewTermLam view env term =
    case termUnLams term of
        ( vars, body ) ->
            PP.parensIf (env.precedence >= 3)
                (PP.lines
                    [ PP.words
                        [ vars
                            |> List.map (symbolToString >> PP.text)
                            |> PP.words
                        , PP.text "->"
                        ]
                    , PP.indent 2 <|
                        viewTerm2
                            view
                            { precedence = 2
                            , blockContext = Block
                            , infixContext = NonInfix
                            }
                            body
                    ]
                )


viewTermNat :
    Word64
    -> PP.Doc msg
viewTermNat n =
    -- FIXME: deal with >53 bit integers correctly.
    PP.text (String.fromInt (unsafeWord64ToInt53 n))


{-| Should be the same as viewTermAnd
-}
viewTermOr :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> PP.Doc msg
viewTermOr view env t1 t2 =
    let
        env2 : Env
        env2 =
            { precedence = 10
            , blockContext = Normal
            , infixContext = NonInfix
            }
    in
    PP.parensIf (env.precedence >= 10)
        (PP.lines
            [ PP.text "or"
            , PP.indent 2 <|
                PP.lines
                    [ viewTerm2 view env2 t1
                    , viewTerm2 view env2 t2
                    ]
            ]
        )


viewTermRef :
    { r | head : Branch }
    -> Reference
    -> PP.Doc msg
viewTermRef view reference =
    viewReferent_ view (Ref reference)


viewTermRequest :
    { r | head : Branch }
    -> Reference
    -> Int
    -> PP.Doc msg
viewTermRequest view reference n =
    viewReferent_ view (Con reference n Effect)


viewTermSequence :
    { r | head : Branch }
    -> Env
    -> Array (Term Symbol)
    -> PP.Doc msg
viewTermSequence view env terms =
    PP.words
        [ PP.text "["
        , terms
            |> Array.map
                (viewTerm2
                    view
                    { precedence = 0
                    , blockContext = Normal
                    , infixContext = NonInfix
                    }
                )
            |> Array.toList
            |> PP.join (PP.text ", ")
        , PP.text "]"
        ]


viewTermVar :
    Symbol
    -> PP.Doc msg
viewTermVar var =
    PP.text (symbolToString var)


viewReferent_ :
    { r | head : Branch }
    -> Referent
    -> PP.Doc msg
viewReferent_ view referent =
    let
        fallback : PP.Doc msg
        fallback =
            viewReferent
                { showBuiltin = True
                , take = Just 7
                }
                referent

        head : Branch0
        head =
            branchHead view.head
    in
    case HashDict.get referent head.cache.termToName of
        Nothing ->
            fallback

        Just names ->
            case HashSet.toList names of
                [] ->
                    impossible "viewReferent: empty names"

                -- TODO, we should handle aliases better. this
                -- just takes the first name
                name :: _ ->
                    viewReferent2 head.cache.nameToTerm name


viewReferent2 :
    NameDict ReferentSet
    -> Name
    -> PP.Doc msg
viewReferent2 nameToTerm fullName =
    PP.text (nameToString (shortenName nameToTerm fullName))
