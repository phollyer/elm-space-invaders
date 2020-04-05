module VerifyExamples.Shared.Players.LastLifeForPlayer1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#lastLifeForPlayer: \n\n    init\n        (Lives 3)\n        (Highscores\n            { list = []\n            , max = 10\n            }\n        )\n        |> register \"Paul\"\n        |> killPlayer 1\n        |> killPlayer 1\n        |> lastLifeForPlayer 1\n    --> Just True" <|
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
                    |> killPlayer 1
                    |> killPlayer 1
                    |> lastLifeForPlayer 1
                )
                (
                Just True
                )