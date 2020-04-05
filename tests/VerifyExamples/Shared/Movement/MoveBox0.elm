module VerifyExamples.Shared.Movement.MoveBox0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Movement exposing (..)
import Shared.BoundingBox as BoundingBox







spec0 : Test.Test
spec0 =
    Test.test "#moveBox: \n\n    BoundingBox.zero\n        |> moveBox (Right 10)\n        |> moveBox (Down 5)\n    --> BoundingBox.init { left = 10, top = 5, right = 10, bottom = 5 }" <|
        \() ->
            Expect.equal
                (
                BoundingBox.zero
                    |> moveBox (Right 10)
                    |> moveBox (Down 5)
                )
                (
                BoundingBox.init { left = 10, top = 5, right = 10, bottom = 5 }
                )