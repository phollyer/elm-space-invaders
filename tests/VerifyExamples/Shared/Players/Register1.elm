module VerifyExamples.Shared.Players.Register1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#register: \n\n    init\n        (Lives 3)\n        (Highscores\n            { list = []\n            ,  max = 0\n            }\n        )\n        |> register \"Paul\"\n        |> register \"Steve\"\n        |> nameForPlayer 1\n    --> Just \"Paul\"" <|
        \() ->
            Expect.equal
                (
                init
                    (Lives 3)
                    (Highscores
                        { list = []
                        ,  max = 0
                        }
                    )
                    |> register "Paul"
                    |> register "Steve"
                    |> nameForPlayer 1
                )
                (
                Just "Paul"
                )