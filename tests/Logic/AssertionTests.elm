module Logic.AssertionTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (..)
import Logic.Assertion exposing (Assertion(..), applyAssertions)
import Logic.BooleanExpression exposing (BooleanExpression(..))
import Logic.Evaluation exposing (Evaluation(..))
import Test exposing (..)


xyzSymbolTable : Dict.Dict String Evaluation
xyzSymbolTable =
    Dict.insert "x" ETrue Dict.empty
        |> Dict.insert "y" EFalse
        |> Dict.insert "z" EContradiction


table : List ( String, Evaluation ) -> Dict String Evaluation
table propValues =
    propValues
        |> List.foldl (\( p, e ) -> \d -> Dict.insert p e d) Dict.empty


suite : Test
suite =
    describe "Assertion.applyAssertions"
        [ describe "with empty symbol table" <|
            let
                expect propValues assertions _ =
                    Expect.equal (table propValues) <| applyAssertions Dict.empty <| assertions
            in
            [ test "empty array of assertions returns empty symbol table" <|
                expect [] []
            , test "returns x is ETrue with proposition x = true" <|
                expect [ ( "x", ETrue ) ] [ TruthOfProposition "x" True ]
            , test "returns x is EFalse with proposition x = false" <|
                expect [ ( "x", EFalse ) ] [ TruthOfProposition "x" False ]
            , test "returns empty with an untriggered implication" <|
                expect [] [ Implication (Proposition "x") [] ]
            , test "returns expected with triggered implication" <|
                expect [ ( "x", ETrue ), ( "y", ETrue ) ] [ TruthOfProposition "x" True, Implication (Proposition "x") [ ( "y", True ) ] ]
            , test "returns expected with triggered implication resulting in setting x to itself" <|
                expect [ ( "x", ETrue ) ] [ TruthOfProposition "x" True, Implication (Proposition "x") [ ( "x", True ) ] ]
            , test "returns expected with triggered implication resulting in contradiction" <|
                expect [ ( "x", EContradiction ) ] [ TruthOfProposition "x" True, Implication (Proposition "x") [ ( "x", False ) ] ]
            , test "returns expected with syllogism" <|
                expect
                    [ ( "x", ETrue )
                    , ( "y", ETrue )
                    , ( "z", ETrue )
                    ]
                    [ TruthOfProposition "x" True
                    , Implication (Proposition "x") [ ( "y", True ) ]
                    , Implication (Proposition "y") [ ( "z", True ) ]
                    ]
            , test "returns expected with long syllogism resulting in contradiction" <|
                expect
                    [ ( "a", EContradiction )
                    , ( "b", ETrue )
                    , ( "c", ETrue )
                    , ( "d", ETrue )
                    , ( "e", ETrue )
                    ]
                    [ TruthOfProposition "a" True
                    , Implication (Proposition "a") [ ( "b", True ) ]
                    , Implication (Proposition "b") [ ( "c", True ) ]
                    , Implication (Proposition "c") [ ( "d", True ) ]
                    , Implication (Proposition "d") [ ( "e", True ) ]
                    , Implication (Proposition "e") [ ( "a", False ) ]
                    ]
            ]
        ]
