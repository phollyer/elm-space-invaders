module VerifyExamples.Shared.HitTest.Detect0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.HitTest exposing (..)
import Shared.BoundingBox as BoundingBox







spec0 : Test.Test
spec0 =
    Test.test "#detect: \n\n    Primary\n        (BoundingBox.init\n            { left = 10\n            , top = 10\n            , right = 10\n            , bottom = 20\n            }\n        )\n        |> detect\n            (Target\n                (BoundingBox.init\n                    { left = 5\n                    , top = 5\n                    , right = 15\n                    , bottom = 15\n                    }\n                )\n            )\n    --> (Speared (TopOrBottom Bottom))" <|
        \() ->
            Expect.equal
                (
                Primary
                    (BoundingBox.init
                        { left = 10
                        , top = 10
                        , right = 10
                        , bottom = 20
                        }
                    )
                    |> detect
                        (Target
                            (BoundingBox.init
                                { left = 5
                                , top = 5
                                , right = 15
                                , bottom = 15
                                }
                            )
                        )
                )
                (
                (Speared (TopOrBottom Bottom))
                )