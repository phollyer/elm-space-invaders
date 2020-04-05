module Shared.MovementTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, float)
import Random
import Shared.BoundingBox as BoundingBox
import Shared.Movement as Movement exposing (Direction(..))
import Shared.Point as Point
import Shrink
import Test exposing (Test, describe, fuzz2)


move : Fuzzer Direction
move =
    let
        generator =
            Random.int 0 4
                |> Random.andThen
                    (\i ->
                        case i of
                            0 ->
                                Random.constant Stop

                            1 ->
                                Random.constant (Left 0)

                            2 ->
                                Random.constant (Up 0)

                            3 ->
                                Random.constant (Right 0)

                            _ ->
                                Random.constant (Down 0)
                    )

        shrinker direction =
            case direction of
                Left _ ->
                    Shrink.noShrink (Left 0)

                Up _ ->
                    Shrink.noShrink (Up 0)

                Right _ ->
                    Shrink.noShrink (Right 0)

                Down _ ->
                    Shrink.noShrink (Down 0)

                Stop ->
                    Shrink.noShrink Stop
    in
    Fuzz.custom generator shrinker


suite : Test
suite =
    describe "The Movement Module"
        [ describe "Movement.move"
            [ fuzz2 move float "moves the asset in the specified direction and by the specified amount" <|
                \direction amount ->
                    let
                        asset =
                            { boundingBox = BoundingBox.zero
                            , point = Point.zero
                            }
                    in
                    case direction of
                        Left _ ->
                            asset
                                |> Movement.move (Left amount)
                                |> Expect.equal
                                    { boundingBox =
                                        asset.boundingBox
                                            |> BoundingBox.decrementLeft amount
                                            |> BoundingBox.decrementRight amount
                                    , point =
                                        asset.point
                                            |> Point.moveLeft amount
                                    }

                        Right _ ->
                            asset
                                |> Movement.move (Right amount)
                                |> Expect.equal
                                    { boundingBox =
                                        asset.boundingBox
                                            |> BoundingBox.incrementRight amount
                                            |> BoundingBox.incrementLeft amount
                                    , point =
                                        asset.point
                                            |> Point.moveRight amount
                                    }

                        Up _ ->
                            asset
                                |> Movement.move (Up amount)
                                |> Expect.equal
                                    { boundingBox =
                                        asset.boundingBox
                                            |> BoundingBox.decrementTop amount
                                            |> BoundingBox.decrementBottom amount
                                    , point =
                                        asset.point
                                            |> Point.moveUp amount
                                    }

                        Down _ ->
                            asset
                                |> Movement.move (Down amount)
                                |> Expect.equal
                                    { boundingBox =
                                        asset.boundingBox
                                            |> BoundingBox.incrementBottom amount
                                            |> BoundingBox.incrementTop amount
                                    , point =
                                        asset.point
                                            |> Point.moveDown amount
                                    }

                        Stop ->
                            asset
                                |> Movement.move Stop
                                |> Expect.equal asset
            ]
        , describe "Movement.movePoint"
            [ fuzz2 move float "moves the point in the specified direction and by the specified amount" <|
                \direction amount ->
                    let
                        point =
                            Point.zero
                    in
                    case direction of
                        Left _ ->
                            point
                                |> Movement.movePoint (Left amount)
                                |> Expect.equal
                                    (point
                                        |> Point.moveLeft amount
                                    )

                        Right _ ->
                            point
                                |> Movement.movePoint (Right amount)
                                |> Expect.equal
                                    (point
                                        |> Point.moveRight amount
                                    )

                        Up _ ->
                            point
                                |> Movement.movePoint (Up amount)
                                |> Expect.equal
                                    (point
                                        |> Point.moveUp amount
                                    )

                        Down _ ->
                            point
                                |> Movement.movePoint (Down amount)
                                |> Expect.equal
                                    (point
                                        |> Point.moveDown amount
                                    )

                        Stop ->
                            point
                                |> Movement.movePoint Stop
                                |> Expect.equal point
            ]
        , describe "Movement.moveBox"
            [ fuzz2 move float "moves the boundingBox in the specified direction and by the specified amount" <|
                \direction amount ->
                    let
                        boundingBox =
                            BoundingBox.zero
                    in
                    case direction of
                        Left _ ->
                            boundingBox
                                |> Movement.moveBox (Left amount)
                                |> Expect.equal
                                    (boundingBox
                                        |> BoundingBox.decrementLeft amount
                                        |> BoundingBox.decrementRight amount
                                    )

                        Right _ ->
                            boundingBox
                                |> Movement.moveBox (Right amount)
                                |> Expect.equal
                                    (boundingBox
                                        |> BoundingBox.incrementRight amount
                                        |> BoundingBox.incrementLeft amount
                                    )

                        Up _ ->
                            boundingBox
                                |> Movement.moveBox (Up amount)
                                |> Expect.equal
                                    (boundingBox
                                        |> BoundingBox.decrementTop amount
                                        |> BoundingBox.decrementBottom amount
                                    )

                        Down _ ->
                            boundingBox
                                |> Movement.moveBox (Down amount)
                                |> Expect.equal
                                    (boundingBox
                                        |> BoundingBox.incrementBottom amount
                                        |> BoundingBox.incrementTop amount
                                    )

                        Stop ->
                            boundingBox
                                |> Movement.moveBox Stop
                                |> Expect.equal boundingBox
            ]
        ]
