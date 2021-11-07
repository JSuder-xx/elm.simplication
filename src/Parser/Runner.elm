module Parser.Runner exposing (run)

import List.Extra
import Parser
    exposing
        ( DeadEnd
        , Parser
        , Problem(..)
        , problem
        )


problemToString : Problem -> String
problemToString problem =
    case problem of
        Expecting str ->
            "Expecting '" ++ str ++ "'"

        ExpectingVariable ->
            "Expecting a Proposition name"

        ExpectingEnd ->
            "Expecting end of input."

        Problem str ->
            "Problem: '" ++ str ++ "'"

        ExpectingKeyword k ->
            "Expecting keyword '" ++ k ++ "'"

        ExpectingSymbol s ->
            "Expecting " ++ s

        _ ->
            "?"


deadEndsToStrings =
    let
        toInt { col, row } =
            row * 1000 + col

        equalPosition l r =
            l.col == r.col && l.row == r.row

        nonEmptyGroupingToString : ( DeadEnd, List DeadEnd ) -> ( String, List String )
        nonEmptyGroupingToString ( first, remainingList ) =
            ( [ "Line: "
              , String.fromInt first.row
              , ", Col: "
              , String.fromInt first.col
              , ": "
              , first |> .problem |> problemToString

              -- (first :: remainingList) |> List.map (.problem >> problemToString) |> String.join " or "
              ]
                |> String.concat
            , remainingList |> List.map (.problem >> problemToString)
            )
    in
    List.sortBy toInt >> List.Extra.groupWhile equalPosition >> List.map nonEmptyGroupingToString


run : Parser a -> String -> Result (List ( String, List String )) a
run parser =
    Parser.run parser >> Result.mapError deadEndsToStrings
