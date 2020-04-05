module Shared.PointTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (float)
import Shared.Point as Point
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "The Point Module"
        [ -- CREATING
          describe "Point.init"
            [ fuzz2 float float "initializes a Point at the provided coordinates" <|
                \x y ->
                    Point.init
                        { x = x
                        , y = y
                        }
                        |> Expect.all
                            [ \point -> point |> Point.x |> Expect.within (Absolute 0) x
                            , \point -> point |> Point.y |> Expect.within (Absolute 0) y
                            ]
            ]
        , describe "Point.zero"
            [ test "creates a Point at coordinates X = 0, and Y = 0" <|
                \_ ->
                    Point.zero
                        |> Expect.all
                            [ \point -> point |> Point.x |> Expect.equal 0
                            , \point -> point |> Point.y |> Expect.equal 0
                            ]
            ]

        -- X COORDINATE
        , describe "Point.x"
            [ fuzz2 float float "returns the X coordinate" <|
                \x y ->
                    Point.init
                        { x = x
                        , y = y
                        }
                        |> Point.x
                        |> Expect.within (Absolute 0) x
            ]
        , describe "Point.xToString"
            [ fuzz2 float float "returns the X Coordinate as a String" <|
                \x y ->
                    Point.init
                        { x = x
                        , y = y
                        }
                        |> Point.xToString
                        |> Expect.equal (x |> String.fromFloat)
            ]
        , describe "Point.moveLeft"
            [ fuzz2 float float "decrements the X Coordinate by the specified amount" <|
                \x y ->
                    Point.init
                        { x = x
                        , y = y
                        }
                        |> Point.moveLeft x
                        |> Point.x
                        |> Expect.equal 0
            ]
        , describe "Point.moveRight"
            [ fuzz float "increments the X Coordinate by the specified amount" <|
                \x ->
                    Point.zero
                        |> Point.moveRight x
                        |> Point.x
                        |> Expect.within (Absolute 0) x
            ]
        , describe "Point.moveToX"
            [ fuzz3 float float float "replaces the X Coordinate with the specified Coordinate" <|
                \initX moveToX y ->
                    Point.init
                        { x = initX
                        , y = y
                        }
                        |> Point.moveToX moveToX
                        |> Point.x
                        |> Expect.within (Absolute 0) moveToX
            ]

        -- Y COORDINATE
        , describe "Point.y"
            [ fuzz2 float float "returns the Y coordinate" <|
                \x y ->
                    Point.init
                        { x = x
                        , y = y
                        }
                        |> Point.y
                        |> Expect.within (Absolute 0) y
            ]
        , describe "Point.yToString"
            [ fuzz2 float float "returns the Y Coordinate as a String" <|
                \x y ->
                    Point.init
                        { x = x
                        , y = y
                        }
                        |> Point.yToString
                        |> Expect.equal (y |> String.fromFloat)
            ]
        , describe "Point.moveUp"
            [ fuzz2 float float "decrements the Y Coordinate by the specified amount" <|
                \x y ->
                    Point.init
                        { x = x
                        , y = y
                        }
                        |> Point.moveUp y
                        |> Point.y
                        |> Expect.equal 0
            ]
        , describe "Point.moveDown"
            [ fuzz2 float float "increments the Y Coordinate by the specified amount" <|
                \x y ->
                    Point.zero
                        |> Point.moveDown y
                        |> Point.y
                        |> Expect.within (Absolute 0) y
            ]
        , describe "Point.moveToY"
            [ fuzz3 float float float "replaces the Y Coordinate with the specified Coordinate" <|
                \x initY moveToY ->
                    Point.init
                        { x = x
                        , y = initY
                        }
                        |> Point.moveToY moveToY
                        |> Point.y
                        |> Expect.within (Absolute 0) moveToY
            ]

        -- MOVEMENT
        , describe "Point.move"
            [ fuzz2 float float "moves the Point by the specified delta Point" <|
                \x y ->
                    let
                        deltaPoint =
                            Point.init
                                { x = x
                                , y = y
                                }
                    in
                    Point.zero
                        |> Point.move deltaPoint
                        |> Expect.equal deltaPoint
            ]
        ]
