module VerifyExamples.Shared.Players.RemovePlayer2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)







spec2 : Test.Test
spec2 =
    Test.test "#removePlayer: \n\n    init\n        (Lives 3)\n        (Highscores\n            { list = []\n            , max = 10\n            }\n        )\n        |> register \"Paul\"\n        |> register \"Steve\"\n        |> removePlayer 1\n        |> nameForCurrentPlayer\n    --> \"Steve\"" <|
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
                    |> removePlayer 1
                    |> nameForCurrentPlayer
                )
                (
                "Steve"
                )