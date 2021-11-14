module Logic.BooleanExpression exposing (BooleanExpression(..), PropositionTruth, evaluate, fromPrositionTruthConjunctions, propositionNames, toPropositionTruthConjunctions)

import Dict exposing (Dict)
import Logic.Evaluation exposing (Evaluation(..))
import Maybe exposing (Maybe(..))
import Maybe.Extra as ME
import Set exposing (Set)


type alias PropositionTruth =
    ( String, Bool )


type BooleanExpression
    = Proposition String
    | And (List BooleanExpression)
    | Not BooleanExpression
    | Or (List BooleanExpression)


fromPrositionTruthConjunctions : List PropositionTruth -> BooleanExpression
fromPrositionTruthConjunctions propTruths =
    propTruths
        |> List.map
            (\( p, t ) ->
                Proposition p
                    |> (if t then
                            identity

                        else
                            Not
                       )
            )
        |> (\props ->
                case props of
                    [ single ] ->
                        single

                    _ ->
                        And props
           )


toPropositionTruthConjunctions : BooleanExpression -> Maybe (List PropositionTruth)
toPropositionTruthConjunctions be =
    case be of
        Proposition p ->
            Just [ ( p, True ) ]

        Not (Not innerBE) ->
            toPropositionTruthConjunctions innerBE

        Not (Proposition p) ->
            Just [ ( p, False ) ]

        -- DeMorgans Theorem
        Not (Or innerBEs) ->
            innerBEs
                |> List.map Not
                |> List.map toPropositionTruthConjunctions
                |> ME.combine
                |> Maybe.map List.concat

        Or [ single ] ->
            toPropositionTruthConjunctions single

        And [ single ] ->
            toPropositionTruthConjunctions single

        And innerBEs ->
            innerBEs
                |> List.map toPropositionTruthConjunctions
                |> ME.combine
                |> Maybe.map List.concat

        _ ->
            Nothing


evaluate : Dict String Evaluation -> BooleanExpression -> Evaluation
evaluate table term =
    let
        anyEquals booleanExpressions =
            let
                evaluations =
                    booleanExpressions |> List.map (evaluate table)
            in
            \eval -> List.any ((==) eval) evaluations

        tests : List BooleanExpression -> List Evaluation -> Evaluation -> Evaluation
        tests booleanExpressions cases defaultCase =
            case cases |> List.filter (anyEquals booleanExpressions) of
                firstMatch :: _ ->
                    firstMatch

                _ ->
                    defaultCase
    in
    case term of
        Proposition prop ->
            Dict.get prop table |> Maybe.withDefault EUnknown

        And booleanExpressions ->
            tests booleanExpressions [ EContradiction, EFalse, EUnknown ] ETrue

        Not innerTerm ->
            case evaluate table innerTerm of
                ETrue ->
                    EFalse

                EFalse ->
                    ETrue

                x ->
                    x

        Or booleanExpressions ->
            tests booleanExpressions [ EContradiction, ETrue, EUnknown ] EFalse


propositionNames : BooleanExpression -> Set String
propositionNames be =
    case be of
        Proposition p ->
            Set.singleton p

        And innerBEs ->
            innerBEs |> List.map propositionNames |> List.foldl Set.union Set.empty

        Not inner ->
            propositionNames inner

        Or innerBEs ->
            innerBEs |> List.map propositionNames |> List.foldl Set.union Set.empty
