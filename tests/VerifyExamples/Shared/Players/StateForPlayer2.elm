module VerifyExamples.Shared.Players.StateForPlayer2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.Players exposing (..)







spec2 : Test.Test
spec2 =
    Test.test "#stateForPlayer: \n\n    init\n        (Lives 3)\n        (Highscores { list = []\n        , max = 0\n        })\n        |> register \"Paul\"\n        |> storeStateForPlayer 1\n            { enemies = 5 }\n        |> stateForPlayer 1\n    --> Just (State { enemies = 5 })" <|
        \() ->
            Expect.equal
                (
                init
                    (Lives 3)
                    (Highscores { list = []
                    , max = 0
                    })
                    |> register "Paul"
                    |> storeStateForPlayer 1
                        { enemies = 5 }
                    |> stateForPlayer 1
                )
                (
                Just (State { enemies = 5 })
                )