module Logic.Assertion exposing (Assertion(..), evaluateAssertions, expandWithModusTollens, propositionNames)

import Dict exposing (Dict)
import Logic.BooleanExpression as BE exposing (BooleanExpression, PropositionTruth, toPropositionTruthConjunctions)
import Logic.Evaluation as E exposing (Evaluation(..))
import Maybe exposing (Maybe(..))
import Maybe.Extra
import Set exposing (Set)


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


evaluateProposition : SymbolTable -> PropositionTruth -> Maybe SymbolTable
evaluateProposition symbolTable ( prop, truth ) =
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


evaluateAssertion : SymbolTable -> Assertion -> Maybe SymbolTable
evaluateAssertion symbolTable assertion =
    case assertion of
        AssertProposition prop truth ->
            evaluateProposition symbolTable ( prop, truth )

        Implication antecedent consequents ->
            case BE.evaluate symbolTable antecedent of
                ETrue ->
                    evaluateList symbolTable evaluateProposition consequents

                _ ->
                    Nothing


evaluateList : SymbolTable -> (SymbolTable -> a -> Maybe SymbolTable) -> List a -> Maybe SymbolTable
evaluateList originalTable eval items =
    let
        fold assertion accum =
            eval (Tuple.first accum) assertion
                |> Maybe.map (\t -> ( t, True ))
                |> Maybe.withDefault accum

        ( resultTable, anyChanges ) =
            items |> List.foldl fold ( originalTable, False )
    in
    if anyChanges then
        Just resultTable

    else
        Nothing


evaluateAssertions : List Assertion -> SymbolTable
evaluateAssertions assertions =
    let
        recurse accum lastTableMaybe =
            case lastTableMaybe of
                Nothing ->
                    accum

                Just lastTable ->
                    recurse lastTable (evaluateList lastTable evaluateAssertion assertions)
    in
    recurse Dict.empty (Just Dict.empty)


assertionPropositionNames : Assertion -> Set String
assertionPropositionNames a =
    case a of
        AssertProposition p _ ->
            Set.singleton p

        Implication booleanExpression consequents ->
            Set.union
                (BE.propositionNames booleanExpression)
                (consequents |> List.foldl (Tuple.first >> Set.insert) Set.empty)


propositionNames : List Assertion -> Set String
propositionNames =
    List.map assertionPropositionNames >> List.foldl Set.union Set.empty
