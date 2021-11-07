module Logic.BooleanExpressionTests exposing (..)

import Dict
import Expect exposing (..)
import Logic.BooleanExpression exposing (BooleanExpression(..), evaluate)
import Logic.Evaluation exposing (Evaluation(..))
import Test exposing (..)


xyzSymbolTable : Dict.Dict String Evaluation
xyzSymbolTable =
    Dict.insert "x" ETrue Dict.empty
        |> Dict.insert "y" EFalse
        |> Dict.insert "z" EContradiction


suite : Test
suite =
    describe "BooleanExpression.evaluate"
        [ describe "with empty symbol table" <|
            let
                expect expected term _ =
                    Expect.equal expected <| evaluate Dict.empty term
            in
            [ test "Proposition returns EUnknown" <|
                expect EUnknown (Proposition "x")
            , test "Not Proposition returns EUnknown" <|
                expect EUnknown (Not (Proposition "x"))
            , test "And [] returns ETrue" <|
                expect ETrue (And [])
            , test "And [Proposition] returns EUnknown" <|
                expect EUnknown (And [ Proposition "X" ])
            ]
        , describe "with symbol table having x=ETrue, y=EFalse, z=EContradiction" <|
            let
                expect expected term _ =
                    Expect.equal expected <| evaluate xyzSymbolTable term
            in
            [ test "Proposition x returns ETrue" <|
                expect ETrue (Proposition "x")
            , test "Not Proposition x returns EFalse" <|
                expect EFalse <|
                    Not <|
                        Proposition "x"
            , test "Proposition y returns EFalse" <|
                expect EFalse (Proposition "y")
            , test "Not Proposition y returns ETrue" <|
                expect ETrue <|
                    Not <|
                        Proposition "y"
            , test "And [Proposition x, Proposition y] returns EFalse" <|
                expect EFalse <|
                    And [ Proposition "x", Proposition "y" ]
            , test "And [Proposition x, Not <| Proposition y] returns ETrue" <|
                expect ETrue <|
                    Or [ Proposition "x", Not <| Proposition "y" ]
            , test "And [Proposition x, Proposition z] returns EContradiction" <|
                expect EContradiction <|
                    And [ Proposition "x", Proposition "z" ]
            , test "Or [Proposition x, Proposition y] returns ETrue" <|
                expect ETrue <|
                    Or [ Proposition "x", Proposition "y" ]
            ]
        ]
