module Extras.List exposing (unzip3)

{-| -}


{-| Take a List of three Tuples, and mutate it into a three Tuple of Lists.
-}
unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 triples =
    let
        step : ( x, y, z ) -> ( List x, List y, List z ) -> ( List x, List y, List z )
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    triples
        |> List.foldr step ( [], [], [] )
