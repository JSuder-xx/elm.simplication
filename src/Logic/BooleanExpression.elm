module Logic.BooleanExpression exposing (BooleanExpression(..), evaluate)

import Dict exposing (Dict)
import Logic.Evaluation exposing (Evaluation(..))


type BooleanExpression
    = Proposition String
    | And (List BooleanExpression)
    | Not BooleanExpression
    | Or (List BooleanExpression)


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
            tests booleanExpressions [ EContradiction, EUnknown, EFalse ] ETrue

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
