module VerifyExamples.Shared.BoundingBox.Height0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.BoundingBox exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#height: \n\n    height\n        (init\n            { left = 5, top = 10, right = 15, bottom = 30 }\n        )\n    --> 20" <|
        \() ->
            Expect.equal
                (
                height
                    (init
                        { left = 5, top = 10, right = 15, bottom = 30 }
                    )
                )
                (
                20
                )