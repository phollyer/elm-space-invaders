module VerifyExamples.Shared.Players.RemovePlayer0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#removePlayer: \n\n    init\n        (Lives 3)\n        (Highscores\n            { list = []\n            , max = 10\n            }\n        )\n        |> register \"Paul\"\n        |> register \"Steve\"\n        |> register \"Jeremy\"\n        |> next\n        |> removePlayer 2\n        |> nameForCurrentPlayer\n    --> \"Jeremy\"" <|
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
                    |> register "Jeremy"
                    |> next
                    |> removePlayer 2
                    |> nameForCurrentPlayer
                )
                (
                "Jeremy"
                )