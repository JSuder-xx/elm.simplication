module Logic.EvaluationTests exposing (..)

import Expect exposing (..)
import Logic.Evaluation as E exposing (Evaluation(..))
import Test exposing (..)


suite : Test
suite =
    describe "Evaluation"
        [ describe "boolToEvaluation"
            [ test "True returns ETrue" <|
                \_ -> Expect.equal ETrue <| E.fromBool True
            , test "False returns EFalse" <|
                \_ -> Expect.equal EFalse <| E.fromBool False
            ]
        , describe "evaluationToBool"
            [ test "EUnknown returns Nothing" <|
                \_ -> Expect.equal Nothing <| E.toBool EUnknown
            , test "EContradiction returns Nothing" <|
                \_ -> Expect.equal Nothing <| E.toBool EContradiction
            , test "ETrue returns Just True" <|
                \_ -> Expect.equal (Just True) <| E.toBool ETrue
            , test "EFalse returns Just False" <|
                \_ -> Expect.equal (Just False) <| E.toBool EFalse
            ]
        ]
