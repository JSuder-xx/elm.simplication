module Logic.Evaluation exposing (Evaluation(..), fromBool, toBool, toString)


type Evaluation
    = EUnknown
    | EFalse
    | ETrue
    | EContradiction


fromBool : Bool -> Evaluation
fromBool b =
    if b then
        ETrue

    else
        EFalse


toBool : Evaluation -> Maybe Bool
toBool ev =
    case ev of
        ETrue ->
            Just True

        EFalse ->
            Just False

        _ ->
            Nothing


toString : Evaluation -> String
toString e =
    case e of
        EUnknown ->
            "Unknown"

        EFalse ->
            "False"

        ETrue ->
            "True"

        EContradiction ->
            "Contradiction"
