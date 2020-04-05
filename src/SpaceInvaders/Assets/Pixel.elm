module SpaceInvaders.Assets.Pixel exposing
    ( Pixel
    , init
    , view
    )

{-| Create and view a [Pixel](#Pixel).

@docs Pixel


# CREATING

@docs init


# VIEWING

@docs view

-}

import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Point as Point exposing (Point)
import Svg exposing (Svg, line)
import Svg.Attributes exposing (stroke, x1, x2, y1, y2)


{-| -}
type alias Pixel =
    { color : String
    , boundingBox : BoundingBox
    , point : Point
    }


{-| Initialize a [Pixel](#Pixel) with the desired color and at the desired [Point](Shared.Point#Point)
on screen.

    import Shared.Point as Point

    point =
        Point.init
            { x = 10
            , y = 10
            }

    point
        |> init "yellow"

-}
init : String -> Point -> Pixel
init color point =
    { color = color
    , boundingBox =
        point
            |> BoundingBox.fromPoint
    , point = point
    }


{-| -}
view : Pixel -> Svg msg
view pixel =
    line
        [ stroke pixel.color
        , x1 (pixel.point |> Point.xToString)
        , y1 (pixel.point |> Point.yToString)
        , x2 (pixel.point |> Point.xToString)
        , y2 (pixel.point |> Point.moveDown 1 |> Point.yToString)
        ]
        []
