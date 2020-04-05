module Extras.String exposing
    ( ellipsis
    , replaceIfEmpty
    )


ellipsis : Int -> String -> String
ellipsis maxLength str =
    case (str |> String.length) > maxLength of
        True ->
            (str
                |> String.left
                    maxLength
            )
                ++ "..."

        False ->
            str


replaceIfEmpty : String -> String -> String
replaceIfEmpty replacement original =
    case original of
        "" ->
            replacement

        _ ->
            original
