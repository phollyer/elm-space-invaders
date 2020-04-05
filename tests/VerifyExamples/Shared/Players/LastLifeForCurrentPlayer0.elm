module VerifyExamples.Shared.Players.LastLifeForCurrentPlayer0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#lastLifeForCurrentPlayer: \n\n    init\n        (Lives 3)\n        (Highscores\n            { list = []\n            , max = 10\n            }\n        )\n        |> register \"Paul\"\n        |> killCurrentPlayer\n        |> killCurrentPlayer\n        |> lastLifeForCurrentPlayer\n    --> True" <|
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
                    |> killCurrentPlayer
                    |> killCurrentPlayer
                    |> lastLifeForCurrentPlayer
                )
                (
                True
                )