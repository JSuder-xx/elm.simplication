module Interop.URL exposing (fromString, toString)

import Base64
import Regex
import UrlBase64


fromString : String -> Maybe String
fromString =
    UrlBase64.decode Base64.decode >> Result.toMaybe


replaceForUrl : Regex.Regex
replaceForUrl =
    Regex.fromString "[\\+/=]" |> Maybe.withDefault Regex.never


clean : String -> String
clean s =
    let
        replaceChar rematch =
            case rematch.match of
                "+" ->
                    "-"

                "/" ->
                    "_"

                _ ->
                    ""
    in
    s |> Regex.replace replaceForUrl replaceChar


toString : String -> String
toString =
    Base64.encode >> clean
