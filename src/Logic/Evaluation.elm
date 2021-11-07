module Logic.Evaluation exposing (Evaluation(..), fromBool, toBool)


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
