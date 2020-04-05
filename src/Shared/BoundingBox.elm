module Shared.BoundingBox exposing
    ( BoundingBox
    , init, zero, fromList, fromPoint
    , move, moveTo, moveLeft, moveUp, moveRight, moveDown
    , growAroundCenter, shrinkAroundCenter
    , incrementLeft, decrementLeft, incrementTop, decrementTop, incrementRight, decrementRight, incrementBottom, decrementBottom
    , calculate, calculateLeft, calculateTop, calculateRight, calculateBottom
    , left, top, right, bottom, height, width, center
    , leftToString, topToString, rightToString, bottomToString
    , toPoint
    )

{-| A [BoundingBox](#BoundingBox) represents the on screen space taken by a
game asset. It follows the 2D
[SVG](https://package.elm-lang.org/packages/elm/svg/latest/) coordinate system
where (0,0) is top left.

The purpose of this module is to enable the easy creation and manipulation of
[BoundingBox](#BoundingBox)es.

With this module, you can create, move and resize [BoundingBox](#BoundingBox)es.
For hit detection between two [BoundingBox](#BoundingBox)es, you can use the
[HitTest](Shared.HitTest#Hit) module.

**Note:** This module does not allow you to create a back-to-front/inside-out
[BoundingBox](#BoundingBox). That is to say, that the `x` coordinate of the
[left](#left) edge cannot be greater than the `x` coordinate of the
[right](#right) edge. The same goes for the [top](#top) and [bottom](#bottom)
edges - the `y` coordinate of the [top](#top) edge cannot be greater than the
`y` coordinate of the [bottom](#bottom) edge.

This means that if you provide a value to a function that changes the shape
of a [BoundingBox](#BoundingBox) in one of those ways, the respective edges
will come together and be the same value.

Functions where this can be important have notes and examples in their docs.

@docs BoundingBox


# CREATING

@docs init, zero, fromList, fromPoint


# MOVING

@docs move, moveTo, moveLeft, moveUp, moveRight, moveDown


# SIZING

@docs growAroundCenter, shrinkAroundCenter

@docs incrementLeft, decrementLeft, incrementTop, decrementTop, incrementRight, decrementRight, incrementBottom, decrementBottom


# CALCULATING

@docs calculate, calculateLeft, calculateTop, calculateRight, calculateBottom


# QUERYING

@docs left, top, right, bottom, height, width, center


# CONVERTING

@docs leftToString, topToString, rightToString, bottomToString

@docs toPoint

-}

import Shared.Point as Point exposing (Point)


{-| A type representing the [BoundingBox](#BoundingBox) of an on screen game
asset.

This is an opaque type, use the exposed API to interact with it.

-}
type BoundingBox
    = BoundingBox
        { left : Float
        , top : Float
        , right : Float
        , bottom : Float
        }



-- CREATING


{-| Initialize a [BoundingBox](#BoundingBox), setting the position of each of its edges.

    init
        { left = 5
        , top = 5
        , right = 10
        , bottom = 10
        }

**Note** you cannot create an inside-out [BoundingBox](#BoundingBox).

    init
        { left = 5
        , top = 10
        , right = 0
        , bottom = 20
        }
        |> right
        --> 5

    init
        { left = 5
        , top = 30
        , right = 10
        , bottom = 20
        }
        |> bottom
        --> 30

-}
init : { left : Float, top : Float, right : Float, bottom : Float } -> BoundingBox
init boundingBox =
    let
        ( left_, right_ ) =
            case boundingBox.left <= boundingBox.right of
                True ->
                    ( boundingBox.left
                    , boundingBox.right
                    )

                False ->
                    ( boundingBox.left
                    , boundingBox.left
                    )

        ( top_, bottom_ ) =
            case boundingBox.top <= boundingBox.bottom of
                True ->
                    ( boundingBox.top
                    , boundingBox.bottom
                    )

                False ->
                    ( boundingBox.top
                    , boundingBox.top
                    )
    in
    zero
        |> updateLeft
            left_
        |> updateTop
            top_
        |> updateRight
            right_
        |> updateBottom
            bottom_


{-| Create a [BoundingBox](#BoundingBox) with all edges positioned at (0,0).

    zero --> init { left = 0, top = 0, right = 0, bottom = 0 }

-}
zero : BoundingBox
zero =
    BoundingBox
        { left = 0
        , top = 0
        , right = 0
        , bottom = 0
        }


{-| Create a [BoundingBox](#BoundingBox) from a [Point](Shared.Point#Point).

The `left` and `right` edges will be equal to the `x` value of the [Point](Shared.Point#Point) and the
`top` and `bottom` edges will be equal to the `y` value of the [Point](Shared.Point#Point).

From there, you can use the [sizing](#sizing) functions to set the required size.

    import Shared.Point as Point

    fromPoint
        (Point.init
            { x = 5
            , y = 10
            }
        )
        --> init { left = 5, top = 10, right = 5, bottom = 10 }

-}
fromPoint : Point -> BoundingBox
fromPoint point =
    let
        x =
            point
                |> Point.x

        y =
            point
                |> Point.y
    in
    zero
        |> updateLeft
            x
        |> updateTop
            y
        |> updateRight
            x
        |> updateBottom
            y



-- MOVING


{-| Move a [BoundingBox](#BoundingBox) by the specified delta
[Point](Shared.Point#Point).

    import Shared.Point as Point

    move
        (Point.init
            { x = 5
            , y = 10
            }
        )
        (init
            { left = 10
            , top = 10
            , right = 20
            , bottom = 20
            }
        )
        --> init { left = 15, top = 20, right = 25, bottom = 30 }

-}
move : Point -> BoundingBox -> BoundingBox
move delta boundingBox =
    let
        deltaX =
            delta
                |> Point.x

        deltaY =
            delta
                |> Point.y
    in
    boundingBox
        |> updateLeft
            ((boundingBox |> left) + deltaX)
        |> updateTop
            ((boundingBox |> top) + deltaY)
        |> updateRight
            ((boundingBox |> right) + deltaX)
        |> updateBottom
            ((boundingBox |> bottom) + deltaY)


{-| Move a [BoundingBox](#BoundingBox) to the specified [Point](Shared.Point#Point).

    import Shared.Point as Point

    moveTo
        (Point.init
            { x = 5
            , y = 7.5
            }
        )
        (init
            { left = 10
            , top = 10
            , right = 20
            , bottom = 20
            }
        )
        --> init { left = 5, top = 7.5, right = 15, bottom = 17.5 }

-}
moveTo : Point -> BoundingBox -> BoundingBox
moveTo point boundingBox =
    let
        x =
            point
                |> Point.x

        y =
            point
                |> Point.y
    in
    boundingBox
        |> updateLeft
            x
        |> updateTop
            y
        |> updateRight
            ((boundingBox |> width) + x)
        |> updateBottom
            ((boundingBox |> height) + y)


{-| Move a [BoundingBox](#BoundingBox) left by the specified amount.

    moveLeft
        5
        (init
            { left = 10
            , top = 10
            , right = 20
            , bottom = 20
            }
        )
        --> init { left = 5, top = 10, right = 15, bottom = 20 }

-}
moveLeft : Float -> BoundingBox -> BoundingBox
moveLeft amount boundingBox =
    boundingBox
        |> updateLeft
            ((boundingBox |> left) - amount)
        |> updateRight
            ((boundingBox |> right) - amount)


{-| Move a [BoundingBox](#BoundingBox) up by the specified amount.

    moveUp
        5
        (init
            { left = 10
            , top = 10
            , right = 20
            , bottom = 20
            }
        )
        --> init { left = 10, top = 5, right = 20, bottom = 15 }

-}
moveUp : Float -> BoundingBox -> BoundingBox
moveUp amount boundingBox =
    boundingBox
        |> updateTop
            ((boundingBox |> top) - amount)
        |> updateBottom
            ((boundingBox |> bottom) - amount)


{-| Move a [BoundingBox](#BoundingBox) right by the specified amount.

    moveRight
        5
        (init
            { left = 10
            , top = 10
            , right = 20
            , bottom = 20
            }
        )
        --> init { left = 15, top = 10, right = 25, bottom = 20 }

-}
moveRight : Float -> BoundingBox -> BoundingBox
moveRight amount boundingBox =
    boundingBox
        |> updateLeft
            ((boundingBox |> left) + amount)
        |> updateRight
            ((boundingBox |> right) + amount)


{-| Move a [BoundingBox](#BoundingBox) down by the specified amount.

    moveDown
        5
        (init
            { left = 10
            , top = 10
            , right = 20
            , bottom = 20
            }
        )
        --> init { left = 10, top = 15, right = 20, bottom = 25 }

-}
moveDown : Float -> BoundingBox -> BoundingBox
moveDown amount boundingBox =
    boundingBox
        |> updateTop
            ((boundingBox |> top) + amount)
        |> updateBottom
            ((boundingBox |> bottom) + amount)



-- SIZING


{-| Grow a [BoundingBox](#BoundingBox) by the specified amount. The [BoundingBox](#BoundingBox) will increase in size equally
around the center point.

    growAroundCenter
        5
        { boundingBox =
            init
                { left = 10
                , top = 10
                , right = 20
                , bottom = 20
                }
        }
        --> { boundingBox = init { left = 7.5, top = 7.5, right = 22.5, bottom = 22.5 }}

-}
growAroundCenter : Float -> { a | boundingBox : BoundingBox } -> { a | boundingBox : BoundingBox }
growAroundCenter amount asset =
    let
        delta =
            amount / 2
    in
    { asset
        | boundingBox =
            asset.boundingBox
                |> decrementLeft
                    delta
                |> decrementTop
                    delta
                |> incrementRight
                    delta
                |> incrementBottom
                    delta
    }


{-| Shrink a [BoundingBox](#BoundingBox) by the specified amount. The [BoundingBox](#BoundingBox) will decrease in size equally
around the center point.

    shrinkAroundCenter
        4
        { boundingBox =
            init
                { left = 0
                , top = 0
                , right = 10
                , bottom = 10
                }
        }
        --> { boundingBox = init { left = 2, top = 2, right = 8, bottom = 8 }}

**Note** you cannot turn a [BoundingBox](#BoundingBox) inside-out.

    shrinkAroundCenter
        12
        { boundingBox =
            init
                { left = 0
                , top = 0
                , right = 10
                , bottom = 10
                }
        }
        --> { boundingBox = init { left = 5, top = 5, right = 5, bottom = 5 }}

-}
shrinkAroundCenter : Float -> { a | boundingBox : BoundingBox } -> { a | boundingBox : BoundingBox }
shrinkAroundCenter amount asset =
    let
        width_ =
            asset.boundingBox
                |> width

        height_ =
            asset.boundingBox
                |> height

        deltaX =
            case amount < width_ of
                True ->
                    amount / 2

                False ->
                    width_ / 2

        deltaY =
            case amount < height_ of
                True ->
                    amount / 2

                False ->
                    height_ / 2
    in
    { asset
        | boundingBox =
            asset.boundingBox
                |> incrementLeft
                    deltaX
                |> incrementTop
                    deltaY
                |> decrementRight
                    deltaX
                |> decrementBottom
                    deltaY
    }


{-| Increment the left edge by the specified amount.

    incrementLeft
        5
        (init
            { left = 0
            , top = 0
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 5, top = 0, right = 10, bottom = 10 }

**Note** you cannot turn a [BoundingBox](#BoundingBox) inside-out.

    incrementLeft
        15
        (init
            { left = 0
            , top = 0
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 10, top = 0, right = 10, bottom = 10 }

-}
incrementLeft : Float -> BoundingBox -> BoundingBox
incrementLeft num boundingBox =
    let
        left_ =
            boundingBox
                |> left

        right_ =
            boundingBox
                |> right

        newLeft =
            case left_ + num > right_ of
                True ->
                    right_

                False ->
                    left_ + num
    in
    boundingBox
        |> updateLeft
            newLeft


{-| Decrement the left edge by the specified amount.

    decrementLeft
        5
        (init
            { left = 5
            , top = 0
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 0, top = 0, right = 10, bottom = 10 }

-}
decrementLeft : Float -> BoundingBox -> BoundingBox
decrementLeft amount boundingBox =
    boundingBox
        |> updateLeft
            ((boundingBox |> left) - amount)


{-| Increment the right edge by the specified amount.

    incrementRight
        5
        (init
            { left = 0
            , top = 0
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 0, top = 0, right = 15, bottom = 10 }

-}
incrementRight : Float -> BoundingBox -> BoundingBox
incrementRight amount boundingBox =
    boundingBox
        |> updateRight
            ((boundingBox |> right) + amount)


{-| Decrement the right edge by the specified amount.

    decrementRight
        5
        (init
            { left = 5
            , top = 0
            , right = 15
            , bottom = 10
            }
        )
        --> init { left = 5, top = 0, right = 10, bottom = 10 }

**Note** you cannot turn a [BoundingBox](#BoundingBox) inside-out.

    decrementRight
        15
        (init
            { left = 5
            , top = 0
            , right = 15
            , bottom = 10
            }
        )
        --> init { left = 5, top = 0, right = 5, bottom = 10 }

-}
decrementRight : Float -> BoundingBox -> BoundingBox
decrementRight num boundingBox =
    let
        left_ =
            boundingBox
                |> left

        right_ =
            boundingBox
                |> right

        newRight =
            case right_ - num < left_ of
                True ->
                    left_

                False ->
                    right_ - num
    in
    boundingBox
        |> updateRight
            newRight


{-| Increment the top edge by the specified amount.

    incrementTop
        5
        (init
            { left = 0
            , top = 0
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 0, top = 5, right = 10, bottom = 10 }

**Note** you cannot turn a [BoundingBox](#BoundingBox) inside-out.

    incrementTop
        15
        (init
            { left = 0
            , top = 5
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 0, top = 10, right = 10, bottom = 10 }

-}
incrementTop : Float -> BoundingBox -> BoundingBox
incrementTop num boundingBox =
    let
        top_ =
            boundingBox
                |> top

        bottom_ =
            boundingBox
                |> bottom

        newTop =
            case top_ + num > bottom_ of
                True ->
                    bottom_

                False ->
                    top_ + num
    in
    boundingBox
        |> updateTop
            newTop


{-| Decrement the top edge by the specified amount.

    decrementTop
        5
        (init
            { left = 0
            , top = 5
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 0, top = 0, right = 10, bottom = 10 }

-}
decrementTop : Float -> BoundingBox -> BoundingBox
decrementTop num boundingBox =
    boundingBox
        |> updateTop
            ((boundingBox |> top) - num)


{-| Increment the bottom edge by the specified amount.

    incrementBottom
        5
        (init
            { left = 0
            , top = 0
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 0, top = 0, right = 10, bottom = 15 }

-}
incrementBottom : Float -> BoundingBox -> BoundingBox
incrementBottom num boundingBox =
    boundingBox
        |> updateBottom
            ((boundingBox |> bottom) + num)


{-| Decrement the bottom edge by the specified amount.

    decrementBottom
        5
        (init
            { left = 0
            , top = 0
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 0, top = 0, right = 10, bottom = 5 }

**Note** you cannot turn a [BoundingBox](#BoundingBox) inside-out.

    decrementBottom
        15
        (init
            { left = 0
            , top = 5
            , right = 10
            , bottom = 10
            }
        )
        --> init { left = 0, top = 5, right = 10, bottom = 5 }

-}
decrementBottom : Float -> BoundingBox -> BoundingBox
decrementBottom num boundingBox =
    let
        top_ =
            boundingBox
                |> top

        bottom_ =
            boundingBox
                |> bottom

        newBottom =
            case bottom_ - num < top_ of
                True ->
                    top_

                False ->
                    bottom_ - num
    in
    boundingBox
        |> updateBottom
            newBottom



-- CALCULATING


{-| Calculate the edges of a [BoundingBox](#BoundingBox) for a game asset from
a list of assets with [BoundingBox](#BoundingBox)es.

    calculate
        [ { boundingBox =
                init
                    { left = 10
                    , top = 10
                    , right = 20
                    , bottom = 20
                    }
          }
        , { boundingBox =
                init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
          }
        ]
        { boundingBox = zero }
        --> { boundingBox = init { left = 5, top = 5, right = 20, bottom = 20 } }

-}
calculate :
    List { a | boundingBox : BoundingBox }
    -> { b | boundingBox : BoundingBox }
    -> { b | boundingBox : BoundingBox }
calculate list asset =
    case list of
        [] ->
            { asset
                | boundingBox = zero
            }

        _ ->
            { asset
                | boundingBox =
                    zero
                        |> updateLeft
                            (list
                                |> calculateLeft
                            )
                        |> updateTop
                            (list
                                |> calculateTop
                            )
                        |> updateRight
                            (list
                                |> calculateRight
                            )
                        |> updateBottom
                            (list
                                |> calculateBottom
                            )
            }


{-| Create a [BoundingBox](#BoundingBox) from a list of
[BoundingBox](#BoundingBox)es.

    fromList
        [ init
            { left = 10
            , top = 10
            , right = 20
            , bottom = 20
            }
        , init
            { left = 5
            , top = 5
            , right = 15
            , bottom = 15
            }
        ]
        --> init { left = 5, top = 5, right = 20, bottom = 20 }

-}
fromList : List BoundingBox -> BoundingBox
fromList list =
    let
        list_ =
            list
                |> List.map
                    (\box ->
                        { boundingBox = box }
                    )
    in
    zero
        |> updateLeft
            (list_
                |> calculateLeft
            )
        |> updateTop
            (list_
                |> calculateTop
            )
        |> updateRight
            (list_
                |> calculateRight
            )
        |> updateBottom
            (list_
                |> calculateBottom
            )


{-| Calculate the furthest [left](#left) edge from a list of game assets, each
with a `boundingBox` field [Type](#BoundingBox).

    calculateLeft
        [ { boundingBox =
                init
                    { left = 10
                    , top = 10
                    , right = 20
                    , bottom = 20
                    }
          }
        , { boundingBox =
                init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
          }
        ]
    --> 5

-}
calculateLeft : List { b | boundingBox : BoundingBox } -> Float
calculateLeft list =
    case list |> List.sortWith (ascending left) of
        [] ->
            0

        first :: _ ->
            first.boundingBox
                |> left


{-| Calculate the highest [top](#top) edge from a list of game assets, each
with a `boundingBox` field [Type](#BoundingBox).

    calculateTop
        [ { boundingBox =
                init
                    { left = 10
                    , top = 10
                    , right = 20
                    , bottom = 20
                    }
          }
        , { boundingBox =
                init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
          }
        ]
    --> 5

-}
calculateTop : List { b | boundingBox : BoundingBox } -> Float
calculateTop list =
    case list |> List.sortWith (ascending top) of
        [] ->
            0

        first :: _ ->
            first.boundingBox
                |> top


{-| Calculate the furthest [right](#right) edge from a list of game assets,
each with a `boundingBox` field [Type](#BoundingBox).

    calculateRight
        [ { boundingBox =
                init
                    { left = 10
                    , top = 10
                    , right = 20
                    , bottom = 20
                    }
          }
        , { boundingBox =
                init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
          }
        ]
    --> 20

-}
calculateRight : List { b | boundingBox : BoundingBox } -> Float
calculateRight list =
    case list |> List.sortWith (descending right) of
        [] ->
            0

        first :: _ ->
            first.boundingBox
                |> right


{-| Calculate the lowest [bottom](#bottom) edge from a list of game assets,
each with a `boundingBox` field [Type](#BoundingBox).

    calculateBottom
        [ { boundingBox =
                init
                    { left = 10
                    , top = 10
                    , right = 20
                    , bottom = 20
                    }
          }
        , { boundingBox =
                init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
          }
        ]
    --> 20

-}
calculateBottom : List { b | boundingBox : BoundingBox } -> Float
calculateBottom list =
    case list |> List.sortWith (descending bottom) of
        [] ->
            0

        first :: _ ->
            first.boundingBox
                |> bottom



-- SORTING


ascending : (BoundingBox -> Float) -> { b | boundingBox : BoundingBox } -> { b | boundingBox : BoundingBox } -> Order
ascending edge aggressor target =
    case compare (aggressor.boundingBox |> edge) (target.boundingBox |> edge) of
        LT ->
            LT

        EQ ->
            EQ

        GT ->
            GT


descending : (BoundingBox -> Float) -> { b | boundingBox : BoundingBox } -> { b | boundingBox : BoundingBox } -> Order
descending edge aggressor target =
    case compare (aggressor.boundingBox |> edge) (target.boundingBox |> edge) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT



-- QUERYING


{-| The position of the [left](#left) edge on the x-axis.

    left
        (init
            { left = 5, top = 10, right = 15, bottom = 20 }
        )
        --> 5

-}
left : BoundingBox -> Float
left (BoundingBox boundingBox) =
    boundingBox.left


{-| The position of the [top](#top) edge on the y-axis.

    top
        (init
            { left = 5, top = 10, right = 15, bottom = 20 }
        )
        --> 10

-}
top : BoundingBox -> Float
top (BoundingBox boundingBox) =
    boundingBox.top


{-| The position of the [right](#right) edge on the x-axis.

    right
        (init
            { left = 5, top = 10, right = 15, bottom = 20 }
        )
        --> 15

-}
right : BoundingBox -> Float
right (BoundingBox boundingBox) =
    boundingBox.right


{-| The position of the [bottom](#bottom) edge on the y-axis.

    bottom
        (init
            { left = 5, top = 10, right = 15, bottom = 20 }
        )
        --> 20

-}
bottom : BoundingBox -> Float
bottom (BoundingBox boundingBox) =
    boundingBox.bottom


{-| The [height](#height) of the [BoundingBox](#BoundingBox).

    height
        (init
            { left = 5, top = 10, right = 15, bottom = 30 }
        )
        --> 20

-}
height : BoundingBox -> Float
height boundingBox =
    (boundingBox |> bottom) - (boundingBox |> top)


{-| The [width](#width) of the [BoundingBox](#BoundingBox).

    width
        (init
            { left = 5, top = 10, right = 17, bottom = 20 }
        )
        --> 12

-}
width : BoundingBox -> Float
width boundingBox =
    (boundingBox |> right) - (boundingBox |> left)


{-| The [center](#center) of the [BoundingBox](#BoundingBox) returned as a
[Point](Shared.Point#Point).

    import Shared.Point as Point

    center
        (init
            { left = 10
            , top = 10
            , right = 20
            , bottom = 20
            }
        )
        --> Point.init { x = 15, y = 15 }

-}
center : BoundingBox -> Point
center boundingBox =
    let
        left_ =
            boundingBox
                |> left

        top_ =
            boundingBox
                |> top

        right_ =
            boundingBox
                |> right

        bottom_ =
            boundingBox
                |> bottom
    in
    Point.zero
        |> Point.moveToX
            (((right_ - left_) / 2) + left_)
        |> Point.moveToY
            (((bottom_ - top_) / 2) + top_)



-- CONVERTING


{-| The position of the bottom edge as a String.

    bottomToString
        (init
            { left = 5
            , top = 10
            , right = 15
            , bottom = 20
            }
        )
        --> "20"

-}
bottomToString : BoundingBox -> String
bottomToString boundingBox =
    boundingBox
        |> bottom
        |> String.fromFloat


{-| The position of the left edge as a String.

    leftToString
        (init
            { left = 5
            , top = 10
            , right = 15
            , bottom = 20
            }
        )
        --> "5"

-}
leftToString : BoundingBox -> String
leftToString boundingBox =
    boundingBox
        |> left
        |> String.fromFloat


{-| The position of the right edge as a String.

    rightToString
        (init
            { left = 5
            , top = 10
            , right = 15
            , bottom = 20
            }
        )
        --> "15"

-}
rightToString : BoundingBox -> String
rightToString boundingBox =
    boundingBox
        |> right
        |> String.fromFloat


{-| The position of the top edge as a String.

    topToString
        (init
            { left = 5
            , top = 10
            , right = 15
            , bottom = 20
            }
        )
        --> "10"

-}
topToString : BoundingBox -> String
topToString boundingBox =
    boundingBox
        |> top
        |> String.fromFloat


{-| Create a [Point](Shared.Point#Point) from a [BoundingBox](#BoundingBox).

    import Shared.Point as Point

    toPoint
        (init
            { left = 5
            , top = 10
            , right = 15
            , bottom = 20
            }
        )
        --> Point.init { x = 5, y = 10 }

-}
toPoint : BoundingBox -> Point
toPoint boundingBox =
    Point.zero
        |> Point.moveToX
            (boundingBox
                |> left
            )
        |> Point.moveToY
            (boundingBox
                |> top
            )



-- UPDATING


updateLeft : Float -> BoundingBox -> BoundingBox
updateLeft amount (BoundingBox boundingBox) =
    BoundingBox
        { boundingBox
            | left = amount
        }


updateTop : Float -> BoundingBox -> BoundingBox
updateTop amount (BoundingBox boundingBox) =
    BoundingBox
        { boundingBox
            | top = amount
        }


updateRight : Float -> BoundingBox -> BoundingBox
updateRight amount (BoundingBox boundingBox) =
    BoundingBox
        { boundingBox
            | right = amount
        }


updateBottom : Float -> BoundingBox -> BoundingBox
updateBottom amount (BoundingBox boundingBox) =
    BoundingBox
        { boundingBox
            | bottom = amount
        }
