module VerifyExamples.Shared.Players.HighscoreToString0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#highscoreToString: \n\n    init\n        (Lives 3)\n        (Highscores\n            { list = [{name = \"Paul\", points = 100}]\n            , max = 10\n            }\n        )\n        |> highscoreToString\n    --> \"100\"" <|
        \() ->
            Expect.equal
                (
                init
                    (Lives 3)
                    (Highscores
                        { list = [{name = "Paul", points = 100}]
                        , max = 10
                        }
                    )
                    |> highscoreToString
                )
                (
                "100"
                )