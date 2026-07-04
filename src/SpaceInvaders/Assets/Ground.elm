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
            String.fromFloat groundLevel.y
    in
    line
        [ stroke "green"
        , y1 y
        , y2 y
        , x1 (String.fromFloat groundLevel.boundaryLeft)
        , x2 (String.fromFloat (boundaryRight - 1))
        ]
        []


{-| -}
soil : Float -> Svg msg
soil boundaryRight =
    rect
        [ fill "black"
        , x (String.fromFloat groundLevel.boundaryLeft)
        , y (String.fromFloat groundLevel.y)
        , height (String.fromFloat (groundLevel.y + 10))
        , width (String.fromFloat boundaryRight)
        ]
        []


groundLevel : Configs.Ground
groundLevel =
    Configs.ground
