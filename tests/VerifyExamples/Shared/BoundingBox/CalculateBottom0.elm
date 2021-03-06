module VerifyExamples.Shared.BoundingBox.CalculateBottom0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Shared.BoundingBox exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#calculateBottom: \n\n    calculateBottom\n        [ { boundingBox =\n                init\n                    { left = 10\n                    , top = 10\n                    , right = 20\n                    , bottom = 20\n                    }\n          }\n        , { boundingBox =\n                init\n                    { left = 5\n                    , top = 5\n                    , right = 15\n                    , bottom = 15\n                    }\n          }\n        ]\n    --> 20" <|
        \() ->
            Expect.equal
                (
                calculateBottom
                    [ { boundingBox =
                            init
                                { left = 10
                                , top = 10
                                , right = 20
                                , bottom = 20
                                }
                      }
                    , { boundingBox =
                            init
                                { left = 5
                                , top = 5
                                , right = 15
                                , bottom = 15
                                }
                      }
                    ]
                )
                (
                20
                )