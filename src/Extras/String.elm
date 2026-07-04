module Extras.String exposing
    ( ellipsis
    , replaceIfEmpty
    )


ellipsis : Int -> String -> String
ellipsis maxLength str =
    if String.length str > maxLength then
        String.left maxLength str
            ++ "..."

    else
        str


replaceIfEmpty : String -> String -> String
replaceIfEmpty replacement original =
    case original of
        "" ->
            replacement

        _ ->
            original
