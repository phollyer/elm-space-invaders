module Shared.BoundingBoxTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (float, floatRange, tuple)
import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Point as Point exposing (Point)
import Test exposing (Test, describe, fuzz, fuzz2, only, test)


boundingBox : BoundingBox
boundingBox =
    BoundingBox.init
        { left = 10
        , top = 10
        , right = 20
        , bottom = 20
        }


point : Point
point =
    Point.init
        { x = 10
        , y = 20
        }


suite : Test
suite =
    describe "The Shared.BoundingBox module"
        [ test "zero" <|
            \_ ->
                BoundingBox.zero
                    |> Expect.equal
                        (BoundingBox.init
                            { left = 0
                            , top = 0
                            , right = 0
                            , bottom = 0
                            }
                        )
        , test "fromList" <|
            \_ ->
                [ boundingBox
                    |> BoundingBox.moveRight 10
                , boundingBox
                    |> BoundingBox.moveDown 10
                ]
                    |> BoundingBox.fromList
                    |> Expect.equal
                        (BoundingBox.init
                            { left = 10
                            , top = 10
                            , right = 30
                            , bottom = 30
                            }
                        )
        , test "fromPoint" <|
            \_ ->
                point
                    |> BoundingBox.fromPoint
                    |> Expect.equal
                        (BoundingBox.init
                            { left =
                                point
                                    |> Point.x
                            , top =
                                point
                                    |> Point.y
                            , right =
                                point
                                    |> Point.x
                            , bottom =
                                point
                                    |> Point.y
                            }
                        )
        , test "left" <|
            \_ ->
                point
                    |> BoundingBox.fromPoint
                    |> BoundingBox.left
                    |> Expect.equal
                        (point
                            |> Point.x
                        )
        , test "top" <|
            \_ ->
                point
                    |> BoundingBox.fromPoint
                    |> BoundingBox.top
                    |> Expect.equal
                        (point
                            |> Point.y
                        )
        , test "right" <|
            \_ ->
                point
                    |> BoundingBox.fromPoint
                    |> BoundingBox.right
                    |> Expect.equal
                        (point
                            |> Point.x
                        )
        , test "bottom" <|
            \_ ->
                point
                    |> BoundingBox.fromPoint
                    |> BoundingBox.bottom
                    |> Expect.equal
                        (point
                            |> Point.y
                        )
        , test "height" <|
            \_ ->
                boundingBox
                    |> BoundingBox.height
                    |> Expect.equal
                        ((boundingBox |> BoundingBox.bottom) - (boundingBox |> BoundingBox.top))
        , test "width" <|
            \_ ->
                boundingBox
                    |> BoundingBox.height
                    |> Expect.equal
                        ((boundingBox |> BoundingBox.right) - (boundingBox |> BoundingBox.left))
        , test "center" <|
            \_ ->
                boundingBox
                    |> BoundingBox.center
                    |> Expect.equal
                        (Point.init
                            { x =
                                (boundingBox |> BoundingBox.left) + ((boundingBox |> BoundingBox.width) / 2)
                            , y =
                                (boundingBox |> BoundingBox.top) + ((boundingBox |> BoundingBox.height) / 2)
                            }
                        )
        , test "growAroundCenter" <|
            \_ ->
                { boundingBox = boundingBox }
                    |> BoundingBox.growAroundCenter 10
                    |> Expect.equal
                        { boundingBox =
                            boundingBox
                                |> BoundingBox.decrementLeft 5
                                |> BoundingBox.decrementTop 5
                                |> BoundingBox.incrementRight 5
                                |> BoundingBox.incrementBottom 5
                        }
        , describe "shrinkAroundCenter"
            [ test "shrinks evenly around the center" <|
                \_ ->
                    { boundingBox = boundingBox }
                        |> BoundingBox.shrinkAroundCenter 6
                        |> Expect.equal
                            { boundingBox =
                                boundingBox
                                    |> BoundingBox.incrementLeft 3
                                    |> BoundingBox.incrementTop 3
                                    |> BoundingBox.decrementRight 3
                                    |> BoundingBox.decrementBottom 3
                            }
            , test "will not turn inside out" <|
                \_ ->
                    { boundingBox = boundingBox }
                        |> BoundingBox.shrinkAroundCenter 20
                        |> Expect.equal
                            { boundingBox =
                                boundingBox
                                    |> BoundingBox.incrementLeft 5
                                    |> BoundingBox.incrementTop 5
                                    |> BoundingBox.decrementRight 5
                                    |> BoundingBox.decrementBottom 5
                            }
            ]
        , describe "decrementLeft"
            [ test "moves the left edge left" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.decrementLeft 5
                        |> BoundingBox.left
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.left) - 5)
            ]
        , describe "incrementLeft"
            [ test "moves the left edge right" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.incrementLeft 5
                        |> BoundingBox.left
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.left) + 5)
            , test "will not turn inside out" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.incrementLeft 20
                        |> BoundingBox.left
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.left) + (boundingBox |> BoundingBox.width))
            ]
        , describe "decrementRight"
            [ test "moves the right edge left" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.decrementRight 5
                        |> BoundingBox.right
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.right) - 5)
            , test "will not turn inside out" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.decrementRight 20
                        |> BoundingBox.right
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.right) - (boundingBox |> BoundingBox.width))
            ]
        , describe "incrementRight"
            [ test "moves the right edge right" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.incrementRight 5
                        |> BoundingBox.right
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.right) + 5)
            ]
        , describe "decrementTop"
            [ test "moves the top edge up" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.decrementTop 5
                        |> BoundingBox.top
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.top) - 5)
            ]
        , describe "incrementTop"
            [ test "moves the top edge down" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.incrementTop 5
                        |> BoundingBox.top
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.top) + 5)
            , test "will not turn inside out" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.incrementTop 20
                        |> BoundingBox.top
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.top) + (boundingBox |> BoundingBox.height))
            ]
        , describe "decrementBottom"
            [ test "moves the bottom edge left" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.decrementBottom 5
                        |> BoundingBox.bottom
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.bottom) - 5)
            , test "will not turn inside out" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.decrementBottom 20
                        |> BoundingBox.bottom
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.bottom) - (boundingBox |> BoundingBox.height))
            ]
        , describe "incrementBottom"
            [ test "moves the bottom edge right" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.incrementBottom 5
                        |> BoundingBox.bottom
                        |> Expect.equal
                            ((boundingBox |> BoundingBox.bottom) + 5)
            ]
        , describe "move"
            [ fuzz2 float float "moves the boundingBox by the delta point" <|
                \x y ->
                    boundingBox
                        |> BoundingBox.move
                            (Point.init
                                { x = x
                                , y = y
                                }
                            )
                        |> Expect.equal
                            (boundingBox
                                |> BoundingBox.moveRight x
                                |> BoundingBox.moveDown y
                            )
            ]
        , describe "moveTo"
            [ fuzz2 float float "moves the boundingBox to the point" <|
                \x y ->
                    boundingBox
                        |> BoundingBox.moveTo
                            (Point.init
                                { x = x
                                , y = y
                                }
                            )
                        |> Expect.all
                            [ \box ->
                                box
                                    |> BoundingBox.left
                                    |> Expect.within (Absolute 0) x
                            , \box ->
                                box
                                    |> BoundingBox.top
                                    |> Expect.within (Absolute 0) y
                            ]
            ]
        , describe "moveLeft"
            [ test "moves the boundingBox left" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.moveLeft 5
                        |> Expect.equal
                            (BoundingBox.init
                                { left =
                                    (boundingBox |> BoundingBox.left) - 5
                                , right =
                                    (boundingBox |> BoundingBox.right) - 5
                                , top =
                                    boundingBox
                                        |> BoundingBox.top
                                , bottom =
                                    boundingBox
                                        |> BoundingBox.bottom
                                }
                            )
            ]
        , describe "moveRight"
            [ test "moves the boundingBox right" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.moveRight 5
                        |> Expect.equal
                            (BoundingBox.init
                                { left =
                                    (boundingBox |> BoundingBox.left) + 5
                                , right =
                                    (boundingBox |> BoundingBox.right) + 5
                                , top =
                                    boundingBox
                                        |> BoundingBox.top
                                , bottom =
                                    boundingBox
                                        |> BoundingBox.bottom
                                }
                            )
            ]
        , describe "moveUp"
            [ test "moves the boundingBox up" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.moveUp 5
                        |> Expect.equal
                            (BoundingBox.init
                                { left =
                                    boundingBox |> BoundingBox.left
                                , right =
                                    boundingBox |> BoundingBox.right
                                , top =
                                    (boundingBox |> BoundingBox.top) - 5
                                , bottom =
                                    (boundingBox |> BoundingBox.bottom) - 5
                                }
                            )
            ]
        , describe "moveDown"
            [ test "moves the boundingBox down" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.moveDown 5
                        |> Expect.equal
                            (BoundingBox.init
                                { left =
                                    boundingBox |> BoundingBox.left
                                , right =
                                    boundingBox |> BoundingBox.right
                                , top =
                                    (boundingBox |> BoundingBox.top) + 5
                                , bottom =
                                    (boundingBox |> BoundingBox.bottom) + 5
                                }
                            )
            ]
        , describe "calculate"
            [ fuzz2 (tuple ( floatRange 0 100, floatRange 0 100 )) (tuple ( floatRange 0 100, floatRange 0 100 )) "calculates a bounding box from a list of bounding boxes" <|
                \( left, right ) ( top, bottom ) ->
                    { boundingBox = BoundingBox.zero }
                        |> BoundingBox.calculate
                            [ { boundingBox =
                                    boundingBox
                                        |> BoundingBox.moveLeft left
                              }
                            , { boundingBox =
                                    boundingBox
                                        |> BoundingBox.moveRight right
                              }
                            , { boundingBox =
                                    boundingBox
                                        |> BoundingBox.moveUp top
                              }
                            , { boundingBox =
                                    boundingBox
                                        |> BoundingBox.moveDown bottom
                              }
                            ]
                        |> Expect.equal
                            { boundingBox =
                                BoundingBox.init
                                    { left =
                                        (boundingBox |> BoundingBox.left) - left
                                    , top =
                                        (boundingBox |> BoundingBox.top) - top
                                    , right =
                                        (boundingBox |> BoundingBox.right) + right
                                    , bottom =
                                        (boundingBox |> BoundingBox.bottom) + bottom
                                    }
                            }
            ]
        , describe "calculateLeft"
            [ test "calculates the left edge from a list of bounding boxes" <|
                \_ ->
                    [ { boundingBox = boundingBox }
                    , { boundingBox =
                            boundingBox
                                |> BoundingBox.moveLeft 5
                      }
                    ]
                        |> BoundingBox.calculateLeft
                        |> Expect.equal 5
            ]
        , describe "calculateTop"
            [ test "calculates the top edge from a list of bounding boxes" <|
                \_ ->
                    [ { boundingBox = boundingBox }
                    , { boundingBox =
                            boundingBox
                                |> BoundingBox.moveUp 5
                      }
                    ]
                        |> BoundingBox.calculateTop
                        |> Expect.equal 5
            ]
        , describe "calculateRight"
            [ test "calculates the right edge from a list of bounding boxes" <|
                \_ ->
                    [ { boundingBox = boundingBox }
                    , { boundingBox =
                            boundingBox
                                |> BoundingBox.moveRight 5
                      }
                    ]
                        |> BoundingBox.calculateRight
                        |> Expect.equal 25
            ]
        , describe "calculateBottom"
            [ test "calculates the bottom edge from a list of bounding boxes" <|
                \_ ->
                    [ { boundingBox = boundingBox }
                    , { boundingBox =
                            boundingBox
                                |> BoundingBox.moveDown 5
                      }
                    ]
                        |> BoundingBox.calculateBottom
                        |> Expect.equal 25
            ]
        , describe "leftToString"
            [ test "converts the left edge position to a string" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.leftToString
                        |> Expect.equal
                            (boundingBox
                                |> BoundingBox.left
                                |> String.fromFloat
                            )
            ]
        , describe "rightToString"
            [ test "converts the right edge position to a string" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.rightToString
                        |> Expect.equal
                            (boundingBox
                                |> BoundingBox.right
                                |> String.fromFloat
                            )
            ]
        , describe "topToString"
            [ test "converts the top edge position to a string" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.topToString
                        |> Expect.equal
                            (boundingBox
                                |> BoundingBox.top
                                |> String.fromFloat
                            )
            ]
        , describe "bottomToString"
            [ test "converts the bottom edge position to a string" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.bottomToString
                        |> Expect.equal
                            (boundingBox
                                |> BoundingBox.bottom
                                |> String.fromFloat
                            )
            ]
        , describe "toPoint"
            [ test "creates a Point type from the boundingBox" <|
                \_ ->
                    boundingBox
                        |> BoundingBox.toPoint
                        |> Expect.equal
                            (Point.init
                                { x =
                                    boundingBox
                                        |> BoundingBox.left
                                , y =
                                    boundingBox
                                        |> BoundingBox.top
                                }
                            )
            ]
        ]
