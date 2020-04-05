module SpaceInvaders.Assets.Ground exposing
    ( grass
    , soil
    )

{-| The ground is a combination of [grass](#grass) and [soil](#soil).

The [grass](#grass) is where the [Players](Shared.Players)
[Ship](SpaceInvaders.Assets.Ship) sits.

The [soil](#soil) absorbs enemy [Lasers](SpaceInvaders.Assets.Lasers) and hides
them as they sink below the [grass](#grass).

@docs grass

@docs soil

-}

import SpaceInvaders.Configs as Configs
import Svg exposing (Svg, line, rect)
import Svg.Attributes exposing (fill, height, stroke, width, x, x1, x2, y, y1, y2)



-- VIEW


{-| -}
grass : Float -> Svg msg
grass boundaryRight =
    let
        y =
            config
                |> .y
                |> String.fromFloat
    in
    line
        [ stroke "green"
        , y1 y
        , y2 y
        , x1
            (config
                |> .boundaryLeft
                |> String.fromFloat
            )
        , x2
            (boundaryRight
                - 1
                |> String.fromFloat
            )
        ]
        []


{-| -}
soil : Float -> Svg msg
soil boundaryRight =
    let
        width_ =
            config
    in
    rect
        [ fill "black"
        , x
            (config
                |> .boundaryLeft
                |> String.fromFloat
            )
        , y
            (config
                |> .y
                |> String.fromFloat
            )
        , height
            (config
                |> .y
                |> (+) 10
                |> String.fromFloat
            )
        , width
            (boundaryRight
                |> String.fromFloat
            )
        ]
        []


config : Configs.Ground
config =
    Configs.ground
