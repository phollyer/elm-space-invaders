module Shared.ScoresTests exposing (suite)

import Expect
import Fuzz exposing (intRange)
import Shared.Scores as Scores exposing (Score, Scores)
import Test exposing (Test, describe, fuzz, fuzz2, test)


init : Scores
init =
    Scores.init
        10
        []


suite : Test
suite =
    describe "The Scores Module"
        [ describe "init"
            [ fuzz (intRange 0 100) "with no highscores" <|
                \total ->
                    Scores.init
                        total
                        []
                        |> Scores.total
                        |> Expect.equal total
            , fuzz2 (intRange 1 10) (intRange 10 100) "with too many highscores" <|
                \total range ->
                    Scores.init
                        total
                        (range
                            |> List.range 10
                            |> List.map
                                (\index ->
                                    { name = ""
                                    , points = 0
                                    }
                                )
                        )
                        |> Scores.total
                        |> Expect.equal total
            ]
        , test "register" <|
            \_ ->
                Scores.register "Paul"
                    |> Expect.all
                        [ \score ->
                            score
                                |> Scores.name
                                |> Expect.equal "Paul"
                        , \score ->
                            score
                                |> Scores.points
                                |> Expect.equal 0
                        ]
        , test "incrementScore" <|
            \_ ->
                Scores.register "Paul"
                    |> Scores.incrementScore 10
                    |> Scores.points
                    |> Expect.equal 10
        , test "decrementScore" <|
            \_ ->
                Scores.register "Paul"
                    |> Scores.decrementScore 10
                    |> Scores.points
                    |> Expect.equal -10
        , describe "scoreChange"
            [ test "updates the current highest" <|
                \_ ->
                    init
                        |> Scores.scoreChange
                            ("Paul"
                                |> Scores.register
                                |> Scores.incrementScore 10
                            )
                        |> Scores.highestToString
                        |> Expect.equal "10"
            , test "does not update the list" <|
                \_ ->
                    init
                        |> Scores.scoreChange
                            ("Paul"
                                |> Scores.register
                                |> Scores.incrementScore 10
                            )
                        |> Scores.highest
                        |> Scores.points
                        |> Expect.equal 0
            ]
        , describe "gameOver"
            [ test "updates the current highest" <|
                \_ ->
                    init
                        |> Scores.gameOver
                            ("Paul"
                                |> Scores.register
                                |> Scores.incrementScore 10
                            )
                        |> Scores.highestToString
                        |> Expect.equal "10"
            , test "updates the list" <|
                \_ ->
                    init
                        |> Scores.gameOver
                            ("Paul"
                                |> Scores.register
                                |> Scores.incrementScore 10
                            )
                        |> Scores.highest
                        |> Scores.points
                        |> Expect.equal 10
            ]
        , test "prepareForSave" <|
            \_ ->
                init
                    |> Scores.gameOver
                        ("Paul"
                            |> Scores.register
                            |> Scores.incrementScore 10
                        )
                    |> Scores.prepareForSave
                    |> Expect.equal
                        [ { name = "Paul", points = 10 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        ]
        , test "highest" <|
            \_ ->
                init
                    |> Scores.gameOver
                        ("Paul"
                            |> Scores.register
                            |> Scores.incrementScore 10
                        )
                    |> Scores.highest
                    |> Scores.points
                    |> Expect.equal 10
        , test "lowest" <|
            \_ ->
                init
                    |> Scores.gameOver
                        ("Paul"
                            |> Scores.register
                            |> Scores.incrementScore 10
                        )
                    |> Scores.lowest
                    |> Scores.points
                    |> Expect.equal 0
        , test "list" <|
            \_ ->
                init
                    |> Scores.gameOver
                        ("Paul"
                            |> Scores.register
                            |> Scores.incrementScore 10
                        )
                    |> Scores.list
                    |> List.map
                        (\score ->
                            { name =
                                score
                                    |> Scores.name
                            , points =
                                score
                                    |> Scores.points
                            }
                        )
                    |> Expect.equal
                        [ { name = "Paul", points = 10 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        , { name = "---", points = 0 }
                        ]
        , test "highestToString" <|
            \_ ->
                init
                    |> Scores.scoreChange
                        ("Paul"
                            |> Scores.register
                            |> Scores.incrementScore 10
                        )
                    |> Scores.highestToString
                    |> Expect.equal "10"
        , test "name" <|
            \_ ->
                "Paul"
                    |> Scores.register
                    |> Scores.name
                    |> Expect.equal "Paul"
        , test "points" <|
            \_ ->
                "Paul"
                    |> Scores.register
                    |> Scores.incrementScore 10
                    |> Scores.points
                    |> Expect.equal 10
        , test "total" <|
            \_ ->
                init
                    |> Scores.total
                    |> Expect.equal 10
        , test "updateName" <|
            \_ ->
                "Paul"
                    |> Scores.register
                    |> Scores.updateName "Steve"
                    |> Scores.name
                    |> Expect.equal "Steve"
        ]
