module Parser.AssertionTests exposing (..)

import Expect exposing (..)
import Logic.Assertion exposing (Assertion(..))
import Logic.BooleanExpression exposing (BooleanExpression(..))
import Parser.Assertion exposing (assertion, assertions)
import Parser.Expect as E
import Test exposing (..)


suite : Test
suite =
    describe "Parsing.Assertion"
        [ describe "assertion" <|
            let
                expectFailure =
                    E.anyFailure assertion

                expectSuccess =
                    E.successWith assertion

                parsesCorrectly expressionString expectedAssertion =
                    test ("parses correctly with '" ++ expressionString ++ "'") <| expectSuccess expressionString expectedAssertion
            in
            [ test "fails on an empty string" <|
                expectFailure ""
            , parsesCorrectly "x is True" <|
                TruthOfProposition "x" True
            , parsesCorrectly "# This is a comment\nx is True" <|
                TruthOfProposition "x" True
            , parsesCorrectly "x is True\n# This is a comment" <|
                TruthOfProposition "x" True
            , parsesCorrectly "y is False" <|
                TruthOfProposition "y" False
            , parsesCorrectly "if x then y" <|
                Implication (Proposition "x") [ ( "y", True ) ]
            , parsesCorrectly "if x then y and z" <|
                Implication (Proposition "x") [ ( "y", True ), ( "z", True ) ]
            , parsesCorrectly "if x then q and r and s" <|
                Implication (Proposition "x") [ ( "q", True ), ( "r", True ), ( "s", True ) ]
            , parsesCorrectly "if x then q and not r and s" <|
                Implication (Proposition "x") [ ( "q", True ), ( "r", False ), ( "s", True ) ]
            , parsesCorrectly "if x or y then q and not r" <|
                Implication (Or [ Proposition "x", Proposition "y" ]) [ ( "q", True ), ( "r", False ) ]
            , parsesCorrectly "if not x and not y then q and not r" <|
                Implication (And [ Not <| Proposition "x", Not <| Proposition "y" ]) [ ( "q", True ), ( "r", False ) ]
            , parsesCorrectly "if (not x and not y) or (a and b) then not q and not r" <|
                Implication
                    (Or
                        [ And [ Not <| Proposition "x", Not <| Proposition "y" ]
                        , And [ Proposition "a", Proposition "b" ]
                        ]
                    )
                    [ ( "q", False ), ( "r", False ) ]
            ]
        , describe "assertionS" <|
            let
                expectFailure =
                    E.anyFailure assertion

                expectSuccess =
                    E.successWith assertions

                parsesCorrectly expressionString expectedAssertion =
                    test ("parses correctly with '" ++ expressionString ++ "'") <| expectSuccess expressionString expectedAssertion
            in
            [ test "fails on an empty string" <|
                expectFailure ""
            , parsesCorrectly "x is True" <|
                [ TruthOfProposition "x" True ]
            , parsesCorrectly "x is True y is False" <|
                [ TruthOfProposition "x" True, TruthOfProposition "y" False ]
            , parsesCorrectly "x is True\ny is False" <|
                [ TruthOfProposition "x" True, TruthOfProposition "y" False ]
            ]
        ]
