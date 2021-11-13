module Logic.BooleanExpressionTests exposing (evaluateTest, fromPrositionTruthConjunctionsTest, toPropositionTruthConjunctionsTest)

import Dict
import Expect exposing (..)
import Logic.BooleanExpression exposing (BooleanExpression(..), evaluate, fromPrositionTruthConjunctions, toPropositionTruthConjunctions)
import Logic.Evaluation exposing (Evaluation(..))
import Test exposing (..)


xyzSymbolTable : Dict.Dict String Evaluation
xyzSymbolTable =
    Dict.insert "x" ETrue Dict.empty
        |> Dict.insert "y" EFalse
        |> Dict.insert "z" EContradiction


propX : BooleanExpression
propX =
    Proposition "x"


propY : BooleanExpression
propY =
    Proposition "y"


toPropositionTruthConjunctionsTest : Test
toPropositionTruthConjunctionsTest =
    describe "BooleanExpression.toPropositionTruthConjunctionsTest" <|
        let
            expectFailure be _ =
                Expect.equal (toPropositionTruthConjunctions be) Nothing

            expectSuccess be consequents _ =
                Expect.equal (toPropositionTruthConjunctions be) (Just consequents)
        in
        [ test "x or y fails" <|
            expectFailure (Or [ propX, propY ])
        , test "not (x or y) succeeds (DeMorgans Theorem)" <|
            expectSuccess (Not <| Or [ propX, propY ]) [ ( "x", False ), ( "y", False ) ]
        , test "not (not x or not y) succeeds (DeMorgans Theorem)" <|
            expectSuccess (Not <| Or [ Not propX, Not propY ]) [ ( "x", True ), ( "y", True ) ]
        , test "x succeeds" <|
            expectSuccess propX [ ( "x", True ) ]
        , test "not y succeeds" <|
            expectSuccess (Not propY) [ ( "y", False ) ]
        , test "x and y succeeds" <|
            expectSuccess (And [ propX, propY ]) [ ( "x", True ), ( "y", True ) ]
        , test "not x and not y succeeds" <|
            expectSuccess (And [ Not propX, Not propY ]) [ ( "x", False ), ( "y", False ) ]
        ]


fromPrositionTruthConjunctionsTest : Test
fromPrositionTruthConjunctionsTest =
    describe "BooleanExpression.fromPrositionTruthConjunctions" <|
        let
            expect givenProps expectedBE _ =
                Expect.equal (fromPrositionTruthConjunctions givenProps) expectedBE
        in
        [ test "x is True = Proposition X" <|
            expect [ ( "x", True ) ] propX
        , test "x is False = Not Proposition X" <|
            expect [ ( "x", False ) ] (Not propX)
        , test "x is False, y is True = And [Not Proposition X, Proposition Y]" <|
            expect [ ( "x", False ), ( "y", True ) ] (And [ Not propX, propY ])
        ]


evaluateTest : Test
evaluateTest =
    describe "BooleanExpression.evaluate"
        [ describe "with empty symbol table" <|
            let
                expect expected term _ =
                    Expect.equal expected <| evaluate Dict.empty term
            in
            [ test "Proposition returns EUnknown" <|
                expect EUnknown propX
            , test "Not Proposition returns EUnknown" <|
                expect EUnknown (Not propX)
            , test "And [] returns ETrue" <|
                expect ETrue (And [])
            , test "And [Proposition] returns EUnknown" <|
                expect EUnknown (And [ propX ])
            ]
        , describe "with symbol table having x=ETrue, y=EFalse, z=EContradiction" <|
            let
                expect expected term _ =
                    Expect.equal expected <| evaluate xyzSymbolTable term
            in
            [ test "Proposition x returns ETrue" <|
                expect ETrue propX
            , test "Not Proposition x returns EFalse" <|
                expect EFalse <|
                    Not <|
                        propX
            , test "Proposition y returns EFalse" <|
                expect EFalse propY
            , test "Not Proposition y returns ETrue" <|
                expect ETrue <|
                    Not <|
                        propY
            , test "And [Proposition x, Proposition y] returns EFalse" <|
                expect EFalse <|
                    And [ propX, propY ]
            , test "And [Proposition x, Not <| Proposition y] returns ETrue" <|
                expect ETrue <|
                    Or [ propX, Not <| propY ]
            , test "And [Proposition x, Proposition z] returns EContradiction" <|
                expect EContradiction <|
                    And [ propX, Proposition "z" ]
            , test "Or [Proposition x, Proposition y] returns ETrue" <|
                expect ETrue <|
                    Or [ propX, propY ]
            ]
        ]
