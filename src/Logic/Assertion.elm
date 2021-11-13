module Logic.Assertion exposing (Assertion(..), applyAssertions, expandWithModusTollens)

import Dict exposing (Dict)
import Logic.BooleanExpression as BE exposing (BooleanExpression, PropositionTruth, toPropositionTruthConjunctions)
import Logic.Evaluation as E exposing (Evaluation(..))
import Maybe exposing (Maybe(..))
import Maybe.Extra


type Assertion
    = AssertProposition String Bool
    | Implication BooleanExpression (List PropositionTruth)


type alias SymbolTable =
    Dict String Evaluation


{-| Modus Tollens is not always possible because the consequent of Implication is a conjunction list of propositions (i.e. it is not a BooleanExpresion).
-}
modusTollens : Assertion -> Maybe Assertion
modusTollens a =
    case a of
        Implication be consequents ->
            toPropositionTruthConjunctions (BE.Not be)
                |> Maybe.map
                    (\newConsequents ->
                        Implication (BE.Not <| BE.fromPrositionTruthConjunctions consequents) newConsequents
                    )

        _ ->
            Nothing


{-| Expand the assertions with their Modus Tollens counterpart if possible.

In other words `if a then b` would expand to

  - `if a then b`
  - `if not b then not a`

-}
expandWithModusTollens : List Assertion -> List Assertion
expandWithModusTollens =
    List.concatMap
        (\a ->
            Maybe.Extra.cons
                (modusTollens a)
                [ a ]
        )


applyProposition : SymbolTable -> PropositionTruth -> Maybe SymbolTable
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
        AssertProposition prop truth ->
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
