module VerifyExamples.Shared.Level.Goto0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Level exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#goto: \n\n    init\n        |> goto -5\n        |> number\n    --> 1" <|
        \() ->
            Expect.equal
                (
                init
                    |> goto -5
                    |> number
                )
                (
                1
                )