module VerifyExamples.Shared.Players.Score0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)
import Shared.Scores as Scores







spec0 : Test.Test
spec0 =
    Test.test "#score: \n\n    init\n        (Lives 3)\n        (Highscores\n            { list = []\n            , max = 10\n            }\n        )\n        |> register \"Paul\"\n        |> register \"Steve\"\n        |> current\n        |> score\n        |> Scores.points\n    --> 0" <|
        \() ->
            Expect.equal
                (
                init
                    (Lives 3)
                    (Highscores
                        { list = []
                        , max = 10
                        }
                    )
                    |> register "Paul"
                    |> register "Steve"
                    |> current
                    |> score
                    |> Scores.points
                )
                (
                0
                )