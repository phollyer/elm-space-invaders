module VerifyExamples.Shared.BoundingBox.IncrementLeft1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.BoundingBox exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#incrementLeft: \n\n    incrementLeft\n        5\n        (init\n            { left = 0\n            , top = 0\n            , right = 10\n            , bottom = 10\n            }\n        )\n    --> init { left = 5, top = 0, right = 10, bottom = 10 }" <|
        \() ->
            Expect.equal
                (
                incrementLeft
                    5
                    (init
                        { left = 0
                        , top = 0
                        , right = 10
                        , bottom = 10
                        }
                    )
                )
                (
                init { left = 5, top = 0, right = 10, bottom = 10 }
                )