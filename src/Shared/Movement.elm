module Shared.Movement exposing
    ( Direction(..)
    , move, movePoint, moveBox
    )

{-| A simple module for moving game assets around the screen.

Uses a 2D [SVG](https://package.elm-lang.org/packages/elm/svg/latest/) coordinate system where (0,0) is top left.

@docs Direction


# MOVING

@docs move, movePoint, moveBox

-}

import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Point as Point exposing (Point)


{-| A union type representing the [Direction](#Direction) and amount to move.
-}
type Direction
    = Left Float
    | Right Float
    | Up Float
    | Down Float
    | Stop


{-| Move a game asset in the specified [Direction](#Direction) and by the specified amount.

Any asset with both a `point` field [Type](Shared.Point#Point) **and** a
`boundingBox` field [Type](Shared.BoundingBox#BoundingBox) can be moved.

    import Shared.BoundingBox as BoundingBox
    import Shared.Point as Point

    { point = Point.zero
    , boundingBox =
        BoundingBox.zero
            |> BoundingBox.incrementRight 10
            |> BoundingBox.incrementBottom 10
    }
    |> move (Right 10)
    |> move (Down 5)
    --> { point = Point.init { x = 10, y = 5 }, boundingBox = BoundingBox.init { left = 10, top = 5, right = 20, bottom = 15 } }

Moving an asset by a negative amount will cause the `move` to be in the opposite [Direction](#Direction).

    import Shared.BoundingBox as BoundingBox
    import Shared.Point as Point

    { point = Point.zero
    , boundingBox =
        BoundingBox.zero
            |> BoundingBox.incrementRight 10
            |> BoundingBox.incrementBottom 10
    }
    |> move (Right -10)
    |> move (Down -5)
    --> { point = Point.init { x = -10, y = -5 }, boundingBox = BoundingBox.init { left = -10, top = -5, right = 0, bottom = 5 } }

-}
move : Direction -> { asset | point : Point, boundingBox : BoundingBox } -> { asset | point : Point, boundingBox : BoundingBox }
move direction asset =
    let
        ( newPoint, newBoundingBox ) =
            case direction of
                Stop ->
                    ( asset.point
                    , asset.boundingBox
                    )

                Left amount ->
                    ( asset.point
                        |> Point.moveLeft
                            amount
                    , asset.boundingBox
                        |> BoundingBox.moveLeft
                            amount
                    )

                Right amount ->
                    ( asset.point
                        |> Point.moveRight
                            amount
                    , asset.boundingBox
                        |> BoundingBox.moveRight
                            amount
                    )

                Up amount ->
                    ( asset.point
                        |> Point.moveUp
                            amount
                    , asset.boundingBox
                        |> BoundingBox.moveUp
                            amount
                    )

                Down amount ->
                    ( asset.point
                        |> Point.moveDown
                            amount
                    , asset.boundingBox
                        |> BoundingBox.moveDown
                            amount
                    )
    in
    { asset
        | point = newPoint
        , boundingBox = newBoundingBox
    }


{-| Move a [Point](Shared.Point#Point) in the specified [Direction](#Direction)
and by the specified amount.

    import Shared.Point as Point

    Point.zero
    |> movePoint (Right 10)
    |> movePoint (Down 5)
    --> Point.init { x = 10, y = 5 }

-}
movePoint : Direction -> Point -> Point
movePoint direction point =
    case direction of
        Stop ->
            point

        Left amount ->
            point
                |> Point.moveLeft
                    amount

        Right amount ->
            point
                |> Point.moveRight
                    amount

        Up amount ->
            point
                |> Point.moveUp
                    amount

        Down amount ->
            point
                |> Point.moveDown
                    amount


{-| Move a [BoundingBox](Shared.BoundingBox#BoundingBox) in the specified
[Direction](#Direction) and by the specified amount.

    import Shared.BoundingBox as BoundingBox

    BoundingBox.zero
        |> moveBox (Right 10)
        |> moveBox (Down 5)
        --> BoundingBox.init { left = 10, top = 5, right = 10, bottom = 5 }

-}
moveBox : Direction -> BoundingBox -> BoundingBox
moveBox direction boundingBox =
    case direction of
        Stop ->
            boundingBox

        Left amount ->
            boundingBox
                |> BoundingBox.moveLeft
                    amount

        Right amount ->
            boundingBox
                |> BoundingBox.moveRight
                    amount

        Up amount ->
            boundingBox
                |> BoundingBox.moveUp
                    amount

        Down amount ->
            boundingBox
                |> BoundingBox.moveDown
                    amount
