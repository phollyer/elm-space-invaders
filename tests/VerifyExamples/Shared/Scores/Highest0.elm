module VerifyExamples.Shared.Scores.Highest0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Scores exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#highest: \n\n    init\n        2\n        [ { name = \"Paul\"\n          , points = 1000\n          }\n        , { name = \"Steve\"\n          , points = 900\n          }\n        ]\n        |> highest\n        |> points\n    --> 1000" <|
        \() ->
            Expect.equal
                (
                init
                    2
                    [ { name = "Paul"
                      , points = 1000
                      }
                    , { name = "Steve"
                      , points = 900
                      }
                    ]
                    |> highest
                    |> points
                )
                (
                1000
                )