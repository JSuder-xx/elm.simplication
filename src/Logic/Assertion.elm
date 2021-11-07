module Logic.Assertion exposing (Assertion(..), applyAssertions)

import Dict exposing (Dict)
import Logic.BooleanExpression as BE exposing (BooleanExpression)
import Logic.Evaluation as E exposing (Evaluation(..))


type Assertion
    = TruthOfProposition String Bool
    | Implication BooleanExpression (List ( String, Bool ))


type alias SymbolTable =
    Dict String Evaluation


applyProposition : SymbolTable -> ( String, Bool ) -> Maybe SymbolTable
applyProposition symbolTable ( prop, truth ) =
    case Dict.get prop symbolTable of
        Nothing ->
            Just (Dict.insert prop (E.fromBool truth) symbolTable)

        Just existingEvaluation ->
            case existingEvaluation of
                ETrue ->
                    if truth then
                        Nothing

                    else
                        Just <| Dict.insert prop EContradiction symbolTable

                EFalse ->
                    if not truth then
                        Nothing

                    else
                        Just <| Dict.insert prop EContradiction symbolTable

                EUnknown ->
                    Just (Dict.insert prop (E.fromBool truth) symbolTable)

                EContradiction ->
                    Nothing


applyAssertion : SymbolTable -> Assertion -> Maybe SymbolTable
applyAssertion symbolTable assertion =
    case assertion of
        TruthOfProposition prop truth ->
            applyProposition symbolTable ( prop, truth )

        Implication antecedent consequents ->
            case BE.evaluate symbolTable antecedent of
                ETrue ->
                    applyList symbolTable applyProposition consequents

                _ ->
                    Nothing


applyList : SymbolTable -> (SymbolTable -> a -> Maybe SymbolTable) -> List a -> Maybe SymbolTable
applyList originalTable assert assertions =
    let
        fold assertion accum =
            assert (Tuple.first accum) assertion
                |> Maybe.map (\t -> ( t, True ))
                |> Maybe.withDefault accum

        ( resultTable, anyChanges ) =
            assertions |> List.foldl fold ( originalTable, False )
    in
    if anyChanges then
        Just resultTable

    else
        Nothing


applyAssertions : SymbolTable -> List Assertion -> SymbolTable
applyAssertions original assertions =
    let
        recurse accum lastTableMaybe =
            case lastTableMaybe of
                Nothing ->
                    accum

                Just lastTable ->
                    recurse lastTable (applyList lastTable applyAssertion assertions)
    in
    recurse original (Just original)
