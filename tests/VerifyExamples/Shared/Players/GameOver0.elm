module VerifyExamples.Shared.Players.GameOver0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#gameOver: \n\n    init\n        (Lives 3)\n        (Highscores\n            { list = []\n            , max = 0\n            }\n        )\n        |> register \"Paul\"\n        |> current\n        |> gameOver\n        |> lives\n    --> 0" <|
        \() ->
            Expect.equal
                (
                init
                    (Lives 3)
                    (Highscores
                        { list = []
                        , max = 0
                        }
                    )
                    |> register "Paul"
                    |> current
                    |> gameOver
                    |> lives
                )
                (
                0
                )