module Ucb.Main.View exposing (..)

import Bytes
import Element exposing (..)
import Element.Events exposing (..)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Html exposing (Html)
import Misc exposing (hashSetSize)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Hash exposing (..)
import Unison.Util.Relation exposing (..)


view : Model -> Html Message
view model =
    layout
        []
        (view2 model)


view2 : Model -> Element Message
view2 model =
    column
        []
        (List.filterMap identity
            [ model.head
                |> Maybe.andThen
                    (\head ->
                        Maybe.map
                            (viewRawCausal
                                model.codebase.branches
                                model.ui.branches
                                head
                            )
                            (HashDict.get head model.codebase.branches)
                    )
            , if List.isEmpty model.errors then
                Nothing

              else
                Just
                    (column []
                        (text "Errors:"
                            :: List.map viewError (List.reverse model.errors)
                        )
                    )
            , Maybe.map
                (\limit -> text ("GitHub rate limit: " ++ limit))
                model.rateLimit
            ]
        )


viewError :
    Error
    -> Element message
viewError error =
    text (Debug.toString error)


viewRawBranch :
    HashDict Hash32 RawCausal
    -> HashDict Hash32 Bool
    -> RawBranch
    -> Element Message
viewRawBranch branches visible branch =
    let
        terms : List NameSegment
        terms =
            relationRange branch.terms.d1

        types : List NameSegment
        types =
            relationRange branch.types.d1

        children : List ( NameSegment, Hash32 )
        children =
            HashDict.toList branch.children

        edits : List NameSegment
        edits =
            List.map Tuple.first (HashDict.toList branch.edits)
    in
    column []
        [ column [] (List.map text terms)
        , column [] (List.map text types)
        , column
            []
            (List.map
                (\( name, hash ) ->
                    column
                        []
                        [ el
                            [ onClick (User_GetBranch { hash = hash, focus = False })
                            , pointer
                            ]
                            (text name)
                        , el
                            [ paddingEach
                                { bottom = 0
                                , left = 10
                                , right = 0
                                , top = 0
                                }
                            ]
                            (viewMaybe
                                (\causal ->
                                    case HashDict.get hash visible of
                                        Just True ->
                                            viewRawCausal branches visible hash causal

                                        _ ->
                                            none
                                )
                                (HashDict.get hash branches)
                            )
                        ]
                )
                children
            )
        , column [] (List.map text edits)
        ]


viewRawCausal :
    HashDict Hash32 RawCausal
    -> HashDict Hash32 Bool
    -> Hash32
    -> RawCausal
    -> Element Message
viewRawCausal branches visible hash causal =
    let
        viewPrevBranch : Hash32 -> Element Message
        viewPrevBranch hash_ =
            el
                [ onClick (User_GetBranch { hash = hash_, focus = True })
                , pointer
                ]
                (text hash_)
    in
    el [ padding 10 ]
        (column
            []
            [ text hash
            , case causal of
                RawOne branch ->
                    viewRawBranch branches visible branch

                RawCons branch hash_ ->
                    column
                        [ spacing 5 ]
                        [ row
                            [ spacing 10 ]
                            [ text "Prev"
                            , viewPrevBranch hash_
                            ]
                        , viewRawBranch
                            branches
                            visible
                            branch
                        ]

                RawMerge branch hashes ->
                    column
                        [ spacing 5 ]
                        [ row
                            [ spacing 10 ]
                            [ text "Prev"
                            , row [] (List.map viewPrevBranch (HashSet.toList hashes))
                            ]
                        , viewRawBranch
                            branches
                            visible
                            branch
                        ]
            ]
        )


viewMaybe :
    (a -> Element message)
    -> Maybe a
    -> Element message
viewMaybe f mx =
    case mx of
        Nothing ->
            none

        Just x ->
            f x
