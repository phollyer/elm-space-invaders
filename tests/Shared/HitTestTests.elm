module Shared.HitTestTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float)
import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.HitTest as HitTest exposing (Edge(..), Hit(..), Overlap(..), Primary(..), Speared(..), Target(..))
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, only, test)


boundingBox : BoundingBox
boundingBox =
    BoundingBox.init
        { left = 10
        , right = 20
        , top = 10
        , bottom = 20
        }


primary : BoundingBox
primary =
    boundingBox


target : BoundingBox
target =
    boundingBox


suite : Test
suite =
    describe "The Shared.HitTest module"
        [ describe "Miss"
            [ describe "Miss on the left"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveLeft 20
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Miss
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveLeft 20
                            |> Primary
                            |> HitTest.detectOnly Miss (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveLeft 20
                            |> Primary
                            |> HitTest.detectOneOf [ Miss ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveLeft 20
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal False
                ]
            , describe "Miss on the right"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveRight 20
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Miss
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveRight 20
                            |> Primary
                            |> HitTest.detectOnly Miss (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveRight 20
                            |> Primary
                            |> HitTest.detectOneOf [ Miss ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveRight 20
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal False
                ]
            , describe "Miss above"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveUp 20
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Miss
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveUp 20
                            |> Primary
                            |> HitTest.detectOnly Miss (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveUp 20
                            |> Primary
                            |> HitTest.detectOneOf [ Miss ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveUp 20
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal False
                ]
            , describe "Miss below"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveDown 20
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Miss
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveDown 20
                            |> Primary
                            |> HitTest.detectOnly Miss (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveDown 20
                            |> Primary
                            |> HitTest.detectOneOf [ Miss ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveDown 20
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal False
                ]
            ]
        , describe "Exact"
            [ test "detect" <|
                \_ ->
                    Primary primary
                        |> HitTest.detect (Target target)
                        |> Expect.equal Exact
            , test "detectOnly" <|
                \_ ->
                    Primary primary
                        |> HitTest.detectOnly Exact (Target target)
                        |> Expect.equal True
            , test "detectOneOf" <|
                \_ ->
                    Primary primary
                        |> HitTest.detectOneOf [ Exact ] (Target target)
                        |> Expect.equal True
            , test "detectAny" <|
                \_ ->
                    Primary primary
                        |> HitTest.detectAny (Target target)
                        |> Expect.equal True
            ]
        , describe "Inside"
            [ test "detect" <|
                \_ ->
                    { boundingBox = primary }
                        |> BoundingBox.shrinkAroundCenter 1
                        |> .boundingBox
                        |> Primary
                        |> HitTest.detect (Target target)
                        |> Expect.equal Inside
            , test "detectOnly" <|
                \_ ->
                    { boundingBox = primary }
                        |> BoundingBox.shrinkAroundCenter 1
                        |> .boundingBox
                        |> Primary
                        |> HitTest.detectOnly Inside (Target target)
                        |> Expect.equal True
            , test "detectOneOf" <|
                \_ ->
                    { boundingBox = primary }
                        |> BoundingBox.shrinkAroundCenter 1
                        |> .boundingBox
                        |> Primary
                        |> HitTest.detectOneOf [ Inside ] (Target target)
                        |> Expect.equal True
            , test "detectAny" <|
                \_ ->
                    { boundingBox = primary }
                        |> BoundingBox.shrinkAroundCenter 1
                        |> .boundingBox
                        |> Primary
                        |> HitTest.detectAny (Target target)
                        |> Expect.equal True
            ]
        , describe "Resting"
            [ describe "left edge"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveLeft 10
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal (Resting Left)
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveLeft 10
                            |> Primary
                            |> HitTest.detectOnly (Resting Left) (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveLeft 10
                            |> Primary
                            |> HitTest.detectOneOf [ Resting Left ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveLeft 10
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "top edge"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveUp 10
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal (Resting Top)
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveUp 10
                            |> Primary
                            |> HitTest.detectOnly (Resting Top) (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveUp 10
                            |> Primary
                            |> HitTest.detectOneOf [ Resting Top ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveUp 10
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "right edge"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveRight 10
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal (Resting Right)
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveRight 10
                            |> Primary
                            |> HitTest.detectOnly (Resting Right) (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveRight 10
                            |> Primary
                            |> HitTest.detectOneOf [ Resting Right ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveRight 10
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "bottom edge"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveDown 10
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal (Resting Bottom)
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveDown 10
                            |> Primary
                            |> HitTest.detectOnly (Resting Bottom) (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveDown 10
                            |> Primary
                            |> HitTest.detectOneOf [ Resting Bottom ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.moveDown 10
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            ]
        , describe "Speared"
            [ describe "TopAndBottomOutside"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementBottom 1
                            |> BoundingBox.incrementLeft 1
                            |> BoundingBox.decrementRight 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal (Speared TopAndBottomOutside)
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementBottom 1
                            |> BoundingBox.incrementLeft 1
                            |> BoundingBox.decrementRight 1
                            |> Primary
                            |> HitTest.detectOnly (Speared TopAndBottomOutside) (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementBottom 1
                            |> BoundingBox.incrementLeft 1
                            |> BoundingBox.decrementRight 1
                            |> Primary
                            |> HitTest.detectOneOf [ Speared TopAndBottomOutside ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementBottom 1
                            |> BoundingBox.incrementLeft 1
                            |> BoundingBox.decrementRight 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "TopAndBottomMatch"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementLeft 1
                            |> BoundingBox.decrementRight 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal (Speared TopAndBottomMatch)
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementLeft 1
                            |> BoundingBox.decrementRight 1
                            |> Primary
                            |> HitTest.detectOnly (Speared TopAndBottomMatch) (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementLeft 1
                            |> BoundingBox.decrementRight 1
                            |> Primary
                            |> HitTest.detectOneOf [ Speared TopAndBottomMatch ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementLeft 1
                            |> BoundingBox.decrementRight 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "TopOrBottom Top"
                [ describe "top edge outside, bottom edge matches"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (TopOrBottom Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (TopOrBottom Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (TopOrBottom Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "top edge outside, bottom edge inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (TopOrBottom Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (TopOrBottom Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (TopOrBottom Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "top edge matches, bottom edge inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (TopOrBottom Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (TopOrBottom Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (TopOrBottom Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "TopOrBottom Bottom"
                [ describe "bottom edge outside, top edge matches"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementBottom 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (TopOrBottom Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementBottom 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (TopOrBottom Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementBottom 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (TopOrBottom Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementBottom 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "bottom edge outside, top edge inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (TopOrBottom Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (TopOrBottom Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (TopOrBottom Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "bottom edge matches, top edge inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (TopOrBottom Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (TopOrBottom Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (TopOrBottom Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "LeftAndRightOutside"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementTop 1
                            |> BoundingBox.decrementBottom 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal (Speared LeftAndRightOutside)
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementTop 1
                            |> BoundingBox.decrementBottom 1
                            |> Primary
                            |> HitTest.detectOnly (Speared LeftAndRightOutside) (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementTop 1
                            |> BoundingBox.decrementBottom 1
                            |> Primary
                            |> HitTest.detectOneOf [ Speared LeftAndRightOutside ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementTop 1
                            |> BoundingBox.decrementBottom 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "LeftAndRightMatch"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementTop 1
                            |> BoundingBox.decrementBottom 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal (Speared LeftAndRightMatch)
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementTop 1
                            |> BoundingBox.decrementBottom 1
                            |> Primary
                            |> HitTest.detectOnly (Speared LeftAndRightMatch) (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementTop 1
                            |> BoundingBox.decrementBottom 1
                            |> Primary
                            |> HitTest.detectOneOf [ Speared LeftAndRightMatch ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementTop 1
                            |> BoundingBox.decrementBottom 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "LeftOrRight Left"
                [ describe "left edge outside, right edge matches"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (LeftOrRight Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (LeftOrRight Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (LeftOrRight Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge outside, right edge inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (LeftOrRight Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (LeftOrRight Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (LeftOrRight Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge matches, right edge inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (LeftOrRight Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (LeftOrRight Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (LeftOrRight Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "LeftOrRight Right"
                [ describe "left edge matches, right edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (LeftOrRight Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (LeftOrRight Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (LeftOrRight Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge inside, right edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (LeftOrRight Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (LeftOrRight Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (LeftOrRight Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge inside, right edge matches"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Speared (LeftOrRight Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Speared (LeftOrRight Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Speared (LeftOrRight Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            ]
        , describe "Covers"
            [ describe "all"
                [ test "detect" <|
                    \_ ->
                        { boundingBox = primary }
                            |> BoundingBox.growAroundCenter 1
                            |> .boundingBox
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        { boundingBox = primary }
                            |> BoundingBox.growAroundCenter 1
                            |> .boundingBox
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        { boundingBox = primary }
                            |> BoundingBox.growAroundCenter 1
                            |> .boundingBox
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        { boundingBox = primary }
                            |> BoundingBox.growAroundCenter 1
                            |> .boundingBox
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "top edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "left edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "right edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "bottom edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "right and bottom edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.decrementTop 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "bottom and left edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "left and top edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementRight 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "top and right edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "right, bottom and left edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementTop 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "left, top and bottom edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementRight 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "left, top and right edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.incrementBottom 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            , describe "top, right and bottom edges match"
                [ test "detect" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> Primary
                            |> HitTest.detect (Target target)
                            |> Expect.equal Covers
                , test "detectOnly" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> Primary
                            |> HitTest.detectOnly Covers (Target target)
                            |> Expect.equal True
                , test "detectOneOf" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> Primary
                            |> HitTest.detectOneOf [ Covers ] (Target target)
                            |> Expect.equal True
                , test "detectAny" <|
                    \_ ->
                        primary
                            |> BoundingBox.decrementLeft 1
                            |> Primary
                            |> HitTest.detectAny (Target target)
                            |> Expect.equal True
                ]
            ]
        , describe "Overlap overlap"
            [ describe "left edge"
                [ describe "left, top and bottom edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and bottom edges outside, top edge matches"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and top edges outside, bottom edge matches"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge outside, top and bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edges match, top and bottom edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and top edges match, bottom edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and bottom edges match, top edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left, top and bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Left))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Left)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Left) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "top edge"
                [ describe "left, right and top edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and top edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edges match, right and top edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and right edges match, top edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and right edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left, right and top edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Top))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Top)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Top) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "right edge"
                [ describe "right, top and bottom edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right and bottom edges outside, top edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right and top edges outside, bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right edge outside, top and bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right edges match, top and bottom edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementTop 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right and top edges match, bottom edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right and bottom edges match, top edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right, top and bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Right))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Right)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Right) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "bottom edge"
                [ describe "left, right and bottom edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and bottom edges outside, right edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edges match, right and bottom edges outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and right edges match, bottom edge outside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and right edges outside, bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementLeft 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge outside, right and bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.decrementLeft 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right edge outside, left and bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> BoundingBox.incrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left, right and bottom edges match"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap (Edge Bottom))
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap (Edge Bottom)) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap (Edge Bottom) ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "TopLeft"
                [ describe "left and top edges outside, right and bottom edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap TopLeft)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap TopLeft) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap TopLeft ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "top edge outside, left edges match, right and bottom edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap TopLeft)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap TopLeft) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap TopLeft ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge outside, top edges match, right and bottom edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap TopLeft)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap TopLeft) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap TopLeft ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and top edges match, right and bottom edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap TopLeft)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap TopLeft) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap TopLeft ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "TopRight"
                [ describe "right and top edges outside, left and bottom edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap TopRight)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap TopRight) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap TopRight ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.moveUp 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "top edge outside, right edges match, left and bottom edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap TopRight)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap TopRight) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap TopRight ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveUp 1
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right edge outside, top edges match, left and bottom edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap TopRight)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap TopRight) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap TopRight ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right and top edges match, left and bottom edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap TopRight)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap TopRight) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap TopRight ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.decrementBottom 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "BottomRight"
                [ describe "right and bottom edges outside, left and top edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap BottomRight)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap BottomRight) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap BottomRight ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "bottom edge outside, right edges match, left and top edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap BottomRight)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap BottomRight) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap BottomRight ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.incrementLeft 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right edge outside, bottom edges match, left and top edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap BottomRight)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap BottomRight) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap BottomRight ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveRight 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "right and bottom edges match, left and top edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap BottomRight)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap BottomRight) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap BottomRight ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.incrementLeft 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            , describe "BottomLeft"
                [ describe "left and bottom edges outside, right and top edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap BottomLeft)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap BottomLeft) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap BottomLeft ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.moveDown 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "bottom edge outside, left edges match, right and top edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap BottomLeft)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap BottomLeft) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap BottomLeft ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveDown 1
                                |> BoundingBox.decrementRight 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left edge outside, bottom edges match, right and top edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap BottomLeft)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap BottomLeft) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap BottomLeft ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.moveLeft 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                , describe "left and bottom edges match, right and top edges inside"
                    [ test "detect" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detect (Target target)
                                |> Expect.equal (Overlap BottomLeft)
                    , test "detectOnly" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOnly (Overlap BottomLeft) (Target target)
                                |> Expect.equal True
                    , test "detectOneOf" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectOneOf [ Overlap BottomLeft ] (Target target)
                                |> Expect.equal True
                    , test "detectAny" <|
                        \_ ->
                            primary
                                |> BoundingBox.decrementRight 1
                                |> BoundingBox.incrementTop 1
                                |> Primary
                                |> HitTest.detectAny (Target target)
                                |> Expect.equal True
                    ]
                ]
            ]
        ]
