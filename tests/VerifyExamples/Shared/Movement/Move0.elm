module VerifyExamples.Shared.Movement.Move0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Movement exposing (..)
import Shared.Point as Point
import Shared.BoundingBox as BoundingBox
import Shared.Point as Point
import Shared.BoundingBox as BoundingBox







spec0 : Test.Test
spec0 =
    Test.test "#move: \n\n    { point = Point.zero\n    , boundingBox =\n        BoundingBox.zero\n            |> BoundingBox.incrementRight 10\n            |> BoundingBox.incrementBottom 10\n    }\n    |> move (Right -10)\n    |> move (Down -5)\n    --> { point = Point.init { x = -10, y = -5 }, boundingBox = BoundingBox.init { left = -10, top = -5, right = 0, bottom = 5 } }" <|
        \() ->
            Expect.equal
                (
                { point = Point.zero
                , boundingBox =
                    BoundingBox.zero
                        |> BoundingBox.incrementRight 10
                        |> BoundingBox.incrementBottom 10
                }
                |> move (Right -10)
                |> move (Down -5)
                )
                (
                { point = Point.init { x = -10, y = -5 }, boundingBox = BoundingBox.init { left = -10, top = -5, right = 0, bottom = 5 } }
                )