module Shared.Point exposing
    ( Point
    , init, zero
    , move, moveLeft, moveRight, moveUp, moveDown, moveToX, moveToY
    , x, y
    , xToString, yToString
    )

{-| A point represents a 2D point in space and follows the [SVG](https://package.elm-lang.org/packages/elm/svg/latest/)
coordinate system where (0,0) is top left.

@docs Point


# CREATING

@docs init, zero


# MOVING

@docs move, moveLeft, moveRight, moveUp, moveDown, moveToX, moveToY


# QUERYING

@docs x, y


# CONVERTING

@docs xToString, yToString

-}


{-| A type representing a [Point](#Point) on the screen.

This is an opaque type, use the exposed API to interact with it.

-}
type Point
    = Point
        { x : Float
        , y : Float
        }



-- CREATING


{-| Initialize a [Point](#Point), setting the position of the `x` and `y` values.
-}
init : { x : Float, y : Float } -> Point
init point =
    Point
        point


{-| Create a [Point](#Point) at coordinate (0,0).

    zero --> init { x = 0, y = 0 }

-}
zero : Point
zero =
    init
        { x = 0
        , y = 0
        }



-- MOVING


{-| Move the [Point](#Point) by the specified delta [Point](#Point).

    init
        { x = 5
        , y = 10
        }
        |> move
            (init
                { x = 10
                , y = 20
                }
            )
        --> init { x = 15, y = 30 }

-}
move : Point -> Point -> Point
move delta original =
    original
        |> updateX
            (original
                |> x
                |> (+) (delta |> x)
            )
        |> updateY
            (original
                |> y
                |> (+) (delta |> y)
            )


{-| Move the `x` value left.

    init
        { x = 10
        , y = 5
        }
        |> moveLeft 5
        --> init { x = 5, y = 5 }

-}
moveLeft : Float -> Point -> Point
moveLeft num point =
    point
        |> updateX
            ((point |> x) - num)


{-| Move the `x` value right.

    init
        { x = 5
        , y = 10
        }
        |> moveRight 5
        --> init { x = 10, y = 10 }

-}
moveRight : Float -> Point -> Point
moveRight num point =
    point
        |> updateX
            ((point |> x) + num)


{-| Move the `y` value up.

    init
        { x = 5
        , y = 10
        }
        |> moveUp 5
        --> init { x = 5, y = 5 }

-}
moveUp : Float -> Point -> Point
moveUp num point =
    point
        |> updateY
            ((point |> y) - num)


{-| Move the `y` value down.

    init
        { x = 5
        , y = 10
        }
        |> moveDown 5
        --> init { x = 5, y = 15 }

-}
moveDown : Float -> Point -> Point
moveDown num point =
    point
        |> updateY
            ((point |> y) + num)


{-| Move to the new `x` value.

    init
        { x = 5
        , y = 10
        }
        |> moveToX 15
        --> init { x = 15, y = 10 }

-}
moveToX : Float -> Point -> Point
moveToX num point =
    point
        |> updateX
            num


{-| Move to the new `y` value.

    init
        { x = 5
        , y = 10
        }
        |> moveToY 5
        --> init { x = 5, y = 5 }

-}
moveToY : Float -> Point -> Point
moveToY num point =
    point
        |> updateY
            num



-- QUERYING


{-| The `x` position of the [Point](#Point).

    init
        { x = 5
        , y = 10
        }
        |> x
        --> 5

-}
x : Point -> Float
x (Point point) =
    point.x


{-| The `y` position of the [Point](#Point).

    init
        { x = 5
        , y = 10
        }
        |> y
        --> 10

-}
y : Point -> Float
y (Point point) =
    point.y



-- UPDATING


updateX : Float -> Point -> Point
updateX num (Point point) =
    Point
        { point
            | x = num
        }


updateY : Float -> Point -> Point
updateY num (Point point) =
    Point
        { point
            | y = num
        }



-- CONVERTING


{-| The `x` value as a String.

    init
        { x = 5
        , y = 10
        }
        |> xToString
        --> "5"

-}
xToString : Point -> String
xToString point =
    point
        |> x
        |> String.fromFloat


{-| The `y` value as a String.

    init
        { x = 5
        , y = 10
        }
        |> yToString
        --> "10"

-}
yToString : Point -> String
yToString point =
    point
        |> y
        |> String.fromFloat
