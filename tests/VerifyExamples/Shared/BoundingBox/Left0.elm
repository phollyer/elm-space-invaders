module VerifyExamples.Shared.BoundingBox.Left0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.BoundingBox exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#left: \n\n    left\n        (init\n            { left = 5, top = 10, right = 15, bottom = 20 }\n        )\n    --> 5" <|
        \() ->
            Expect.equal
                (
                left
                    (init
                        { left = 5, top = 10, right = 15, bottom = 20 }
                    )
                )
                (
                5
                )