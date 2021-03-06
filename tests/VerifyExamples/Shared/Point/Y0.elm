module VerifyExamples.Shared.Point.Y0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Point exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#y: \n\n    init\n        { x = 5\n        , y = 10\n        }\n        |> y\n    --> 10" <|
        \() ->
            Expect.equal
                (
                init
                    { x = 5
                    , y = 10
                    }
                    |> y
                )
                (
                10
                )