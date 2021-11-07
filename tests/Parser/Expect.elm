module Parser.Expect exposing (anyFailure, successWith)

import Expect exposing (Expectation, equal, fail, pass)
import Logic.Assertion exposing (Assertion(..))
import Parser exposing (Parser)
import Parser.Runner exposing (run)
import Test exposing (..)


anyFailure : Parser a -> String -> () -> Expectation
anyFailure parser input _ =
    case run parser input of
        Result.Ok _ ->
            fail "Expecting to fail"

        Result.Err _ ->
            pass


successWith : Parser a -> String -> a -> () -> Expectation
successWith parser input expected _ =
    case run parser input of
        Result.Err err ->
            fail <| "Expecting success but got" ++ (err |> List.map Tuple.first |> String.join ", ")

        Ok actual ->
            equal actual expected
