module Unison.Term exposing (..)

import Array exposing (Array)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Int64 exposing (Int64)
import Misc exposing (..)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Unison.ABT exposing (..)
import Unison.Blank exposing (Blank)
import Unison.Pattern exposing (Pattern)
import Unison.Reference exposing (Reference)
import Unison.Type exposing (Type)
import Word64 exposing (Word64)


{-| Haskell type: Unison.Term.Term
-}
type alias Term var =
    AbtTerm var (TermAbt var)


type TermAbt var
    = TermVar var
    | TermCycle (Term var)
    | TermAbs var (Term var)
    | TermTm (TermF var)


type TermF var
    = TermInt Int64
    | TermNat Word64
    | TermFloat Float
    | TermBoolean Bool
    | TermText String
    | TermChar Char
    | TermBlank Blank
    | TermRef Reference
    | TermConstructor Reference Int
    | TermRequest Reference Int
    | TermHandle (Term var) (Term var)
    | TermApp (Term var) (Term var)
    | TermAnn (Term var) (Type var)
    | TermSequence (Array (Term var))
    | TermIf (Term var) (Term var) (Term var)
    | TermAnd (Term var) (Term var)
    | TermOr (Term var) (Term var)
    | TermLam (Term var)
    | TermLetRec Bool (List (Term var)) (Term var)
    | TermLet Bool (Term var) (Term var)
    | TermMatch (Term var) (List (MatchCase (Term var)))


termFFreeVars :
    Equality var
    -> Hashing var
    -> TermF var
    -> HashSet var
termFFreeVars varEquality varHashing term =
    case term of
        TermInt _ ->
            HashSet.empty varEquality varHashing

        TermNat _ ->
            HashSet.empty varEquality varHashing

        TermFloat _ ->
            HashSet.empty varEquality varHashing

        TermBoolean _ ->
            HashSet.empty varEquality varHashing

        TermText _ ->
            HashSet.empty varEquality varHashing

        TermChar _ ->
            HashSet.empty varEquality varHashing

        TermBlank _ ->
            HashSet.empty varEquality varHashing

        TermRef _ ->
            HashSet.empty varEquality varHashing

        TermConstructor _ _ ->
            HashSet.empty varEquality varHashing

        TermRequest _ _ ->
            HashSet.empty varEquality varHashing

        TermHandle t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TermApp t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TermAnn t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TermSequence ts ->
            ts
                |> Array.map .freeVars
                |> Array.toList
                |> hashSetUnions varEquality varHashing

        TermIf t1 t2 t3 ->
            hashSetUnions
                varEquality
                varHashing
                [ t1.freeVars, t2.freeVars, t3.freeVars ]

        TermAnd t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TermOr t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TermLam t ->
            t.freeVars

        TermLetRec _ ts t ->
            hashSetUnion
                (hashSetUnions
                    varEquality
                    varHashing
                    (List.map .freeVars ts)
                )
                t.freeVars

        TermLet _ t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TermMatch t1 cases ->
            hashSetUnion
                t1.freeVars
                (hashSetUnions
                    varEquality
                    varHashing
                    (List.map matchCaseFreeVars cases)
                )


{-| Haskell type: Unison.Term.MatchCase
-}
type MatchCase var
    = MatchCase Pattern (Maybe var) var


matchCaseFreeVars :
    MatchCase (Term var)
    -> HashSet var
matchCaseFreeVars matchCase =
    case matchCase of
        MatchCase _ Nothing term ->
            term.freeVars

        MatchCase _ (Just t1) t2 ->
            hashSetUnion t1.freeVars t2.freeVars


termVar :
    Equality var
    -> Hashing var
    -> var
    -> Term var
termVar varEquality varHashing =
    abtVar varEquality varHashing TermVar


termAbs :
    var
    -> Term var
    -> Term var
termAbs =
    abtAbs TermAbs


termCycle :
    Term var
    -> Term var
termCycle =
    abtCycle TermCycle


termTerm :
    Equality var
    -> Hashing var
    -> TermF var
    -> Term var
termTerm varEquality varHashing term =
    { freeVars = termFFreeVars varEquality varHashing term
    , out = TermTm term
    }
