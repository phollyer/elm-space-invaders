module Shared.PlayersTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, floatRange, intRange, string, tuple)
import Shared.Players as Players exposing (Highscores(..), Lives(..), Players)
import Shared.Scores as Scores
import Test exposing (Test, describe, fuzz, fuzz2, only, test)


type alias State =
    { enemies : Int }


state : State
state =
    { enemies = 5 }


init : Players State
init =
    Players.init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )


onePlayer : Players State
onePlayer =
    init
        |> Players.register "Player 1"


twoPlayers : Players State
twoPlayers =
    init
        |> Players.register "Player 1"
        |> Players.register "Player 2"


randomPlayersFuzzer : String -> (Int -> Expect.Expectation) -> Test
randomPlayersFuzzer =
    fuzz (intRange 1 100)


suite : Test
suite =
    describe "The Shared.Players module"
        [ describe "init"
            [ test "initializes the Players type" <|
                \_ ->
                    Players.init
                        (Lives 3)
                        (Highscores
                            { list = []
                            , max = 10
                            }
                        )
                        |> Players.total
                        |> Expect.equal 0
            ]
        , describe "register"
            [ randomPlayersFuzzer "registers a player" <|
                \total ->
                    total
                        |> List.range 1
                        |> List.foldl
                            (\index players ->
                                players
                                    |> Players.register
                                        ("Player " ++ (index |> String.fromInt))
                            )
                            init
                        |> Players.total
                        |> Expect.equal total
            ]
        , describe "Storing and Retrieving State"
            [ test "stateForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.stateForCurrentPlayer
                        |> Expect.equal
                            Players.NoState
            , test "storeStateForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.storeStateForCurrentPlayer state
                        |> Players.stateForCurrentPlayer
                        |> Expect.equal
                            (Players.State state)
            , test "stateForPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.stateForPlayer 1
                        |> Expect.equal
                            (Just Players.NoState)
            , test "storeStateForPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.storeStateForPlayer 1 state
                        |> Players.stateForPlayer 1
                        |> Expect.equal
                            (Just (Players.State state))
            , test "state" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.state
                        |> Expect.equal
                            Players.NoState
            , test "storeState" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.storeState state
                        |> Players.state
                        |> Expect.equal
                            (Players.State state)
            ]
        , describe "Removing Players"
            [ test "removeCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.removeCurrentPlayer
                        |> Players.total
                        |> Expect.equal 0
            , test "removePlayer" <|
                \_ ->
                    twoPlayers
                        |> Players.removePlayer 2
                        |> Players.total
                        |> Expect.equal 1
            , test "removeAll" <|
                \_ ->
                    twoPlayers
                        |> Players.removeAll
                        |> Players.total
                        |> Expect.equal 0
            ]
        , describe "Replacing Players"
            [ test "replaceCurrentPlayer" <|
                \_ ->
                    let
                        player2 =
                            twoPlayers
                                |> Players.next
                                |> Players.current
                    in
                    onePlayer
                        |> Players.replaceCurrentPlayer player2
                        |> Players.current
                        |> Expect.all
                            [ \player ->
                                player
                                    |> Players.name
                                    |> Expect.equal
                                        (player2
                                            |> Players.name
                                        )
                            , \player ->
                                player
                                    |> Players.number
                                    |> Expect.equal 1
                            ]
            , test "replacePlayer" <|
                \_ ->
                    let
                        player2 =
                            twoPlayers
                                |> Players.next
                                |> Players.current
                    in
                    onePlayer
                        |> Players.replacePlayer 1 player2
                        |> Players.current
                        |> Expect.all
                            [ \player ->
                                player
                                    |> Players.name
                                    |> Expect.equal
                                        (player2
                                            |> Players.name
                                        )
                            , \player ->
                                player
                                    |> Players.number
                                    |> Expect.equal 1
                            ]
            ]
        , describe "Changing Players"
            [ describe "next"
                [ test "selects the next player" <|
                    \_ ->
                        twoPlayers
                            |> Players.next
                            |> Players.numberForCurrentPlayer
                            |> Expect.equal 2
                , test "cycles back to the first player" <|
                    \_ ->
                        twoPlayers
                            |> Players.next
                            |> Players.next
                            |> Players.numberForCurrentPlayer
                            |> Expect.equal 1
                ]
            , describe "previous"
                [ test "selects the previous player" <|
                    \_ ->
                        twoPlayers
                            |> Players.next
                            |> Players.previous
                            |> Players.numberForCurrentPlayer
                            |> Expect.equal 1
                , test "cycles back to the last player" <|
                    \_ ->
                        twoPlayers
                            |> Players.previous
                            |> Players.numberForCurrentPlayer
                            |> Expect.equal 2
                ]
            , describe "goto"
                [ test "goto the specified player" <|
                    \_ ->
                        twoPlayers
                            |> Players.goto 2
                            |> Players.numberForCurrentPlayer
                            |> Expect.equal 2
                ]
            ]
        , describe "Accessing Player Numbers"
            [ test "numberForCurrentPlayer" <|
                \_ ->
                    twoPlayers
                        |> Players.next
                        |> Players.numberForCurrentPlayer
                        |> Expect.equal 2
            , test "number" <|
                \_ ->
                    twoPlayers
                        |> Players.next
                        |> Players.current
                        |> Players.number
                        |> Expect.equal 2
            ]
        , describe "Storing and Retrieving Player Names"
            [ test "nameForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.nameForCurrentPlayer
                        |> Expect.equal "Player 1"
            , test "updateNameForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.updateNameForCurrentPlayer "Paul"
                        |> Players.nameForCurrentPlayer
                        |> Expect.equal "Paul"
            , test "nameForPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.nameForPlayer 1
                        |> Expect.equal (Just "Player 1")
            , test "updateNameForPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.updateNameForPlayer 1 "Paul"
                        |> Players.nameForPlayer 1
                        |> Expect.equal (Just "Paul")
            , test "name" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.name
                        |> Expect.equal "Player 1"
            , test "updateName" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.updateName "Paul"
                        |> Players.name
                        |> Expect.equal "Paul"
            ]
        , describe "Players Lives"
            [ test "livesForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.livesForCurrentPlayer
                        |> Expect.equal 3
            , test "killCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.killCurrentPlayer
                        |> Players.livesForCurrentPlayer
                        |> Expect.equal 2
            , test "extraLifeForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.extraLifeForCurrentPlayer
                        |> Players.livesForCurrentPlayer
                        |> Expect.equal 4
            , describe "lastLifeForCurrentPlayer"
                [ test "with lives left" <|
                    \_ ->
                        onePlayer
                            |> Players.lastLifeForCurrentPlayer
                            |> Expect.equal False
                , test "with 1 life left" <|
                    \_ ->
                        onePlayer
                            |> Players.killCurrentPlayer
                            |> Players.killCurrentPlayer
                            |> Players.lastLifeForCurrentPlayer
                            |> Expect.equal True
                ]
            , describe "anyLivesLeftForCurrentPlayer"
                [ test "with lives left" <|
                    \_ ->
                        onePlayer
                            |> Players.anyLivesLeftForCurrentPlayer
                            |> Expect.equal True
                , test "with no lives left" <|
                    \_ ->
                        onePlayer
                            |> Players.killCurrentPlayer
                            |> Players.killCurrentPlayer
                            |> Players.killCurrentPlayer
                            |> Players.anyLivesLeftForCurrentPlayer
                            |> Expect.equal False
                ]
            , test "livesForPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.livesForPlayer 1
                        |> Expect.equal (Just 3)
            , test "killPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.killPlayer 1
                        |> Players.livesForPlayer 1
                        |> Expect.equal (Just 2)
            , test "extraLifeForPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.extraLifeForPlayer 1
                        |> Players.livesForPlayer 1
                        |> Expect.equal (Just 4)
            , describe "lastLifeForPlayer"
                [ test "with lives left" <|
                    \_ ->
                        onePlayer
                            |> Players.lastLifeForPlayer 1
                            |> Expect.equal (Just False)
                , test "with 1 life left" <|
                    \_ ->
                        onePlayer
                            |> Players.killPlayer 1
                            |> Players.killPlayer 1
                            |> Players.lastLifeForPlayer 1
                            |> Expect.equal (Just True)
                ]
            , describe "anyLivesLeftForPlayer"
                [ test "with lives left" <|
                    \_ ->
                        onePlayer
                            |> Players.anyLivesLeftForPlayer 1
                            |> Expect.equal (Just True)
                , test "with 1 life left" <|
                    \_ ->
                        onePlayer
                            |> Players.killPlayer 1
                            |> Players.killPlayer 1
                            |> Players.killPlayer 1
                            |> Players.anyLivesLeftForPlayer 1
                            |> Expect.equal (Just False)
                ]
            , test "lives" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.lives
                        |> Expect.equal 3
            , test "kill" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.kill
                        |> Players.lives
                        |> Expect.equal 2
            , test "extraLife" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.extraLife
                        |> Players.lives
                        |> Expect.equal 4
            , describe "lastLife"
                [ test "with lives left" <|
                    \_ ->
                        onePlayer
                            |> Players.current
                            |> Players.lastLife
                            |> Expect.equal False
                , test "with 1 life left" <|
                    \_ ->
                        onePlayer
                            |> Players.current
                            |> Players.kill
                            |> Players.kill
                            |> Players.lastLife
                            |> Expect.equal True
                ]
            , describe "anyLivesLeft"
                [ test "with lives left" <|
                    \_ ->
                        onePlayer
                            |> Players.current
                            |> Players.anyLivesLeft
                            |> Expect.equal True
                , test "with no lives left" <|
                    \_ ->
                        onePlayer
                            |> Players.current
                            |> Players.kill
                            |> Players.kill
                            |> Players.kill
                            |> Players.anyLivesLeft
                            |> Expect.equal False
                ]
            , describe "anyAlive"
                [ describe "one player"
                    [ test "is alive" <|
                        \_ ->
                            onePlayer
                                |> Players.killCurrentPlayer
                                |> Players.anyAlive
                                |> Expect.equal True
                    , test "is not alive" <|
                        \_ ->
                            onePlayer
                                |> Players.killCurrentPlayer
                                |> Players.killCurrentPlayer
                                |> Players.killCurrentPlayer
                                |> Players.anyAlive
                                |> Expect.equal False
                    ]
                , describe "two players"
                    [ test "both players alive" <|
                        \_ ->
                            twoPlayers
                                |> Players.anyAlive
                                |> Expect.equal True
                    , test "one player alive" <|
                        \_ ->
                            twoPlayers
                                |> Players.killCurrentPlayer
                                |> Players.killCurrentPlayer
                                |> Players.killCurrentPlayer
                                |> Players.anyAlive
                                |> Expect.equal True
                    , test "no players alive" <|
                        \_ ->
                            twoPlayers
                                |> Players.killPlayer 1
                                |> Players.killPlayer 1
                                |> Players.killPlayer 1
                                |> Players.killPlayer 2
                                |> Players.killPlayer 2
                                |> Players.killPlayer 2
                                |> Players.anyAlive
                                |> Expect.equal False
                    ]
                ]
            ]
        , describe "Players Scores"
            [ test "scoreForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.scoreForCurrentPlayer
                        |> Scores.points
                        |> Expect.equal 0
            , test "pointsForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.pointsForCurrentPlayer
                        |> Expect.equal 0
            , test "incrementScoreForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.incrementScoreForCurrentPlayer 10
                        |> Players.pointsForCurrentPlayer
                        |> Expect.equal 10
            , test "decrementScoreForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.decrementScoreForCurrentPlayer 10
                        |> Players.pointsForCurrentPlayer
                        |> Expect.equal -10
            , test "scoreForPlayer 1" <|
                \_ ->
                    onePlayer
                        |> Players.scoreForPlayer 1
                        |> Maybe.withDefault (Scores.register "")
                        |> Scores.points
                        |> Expect.equal 0
            , test "pointsForPlayer 1" <|
                \_ ->
                    onePlayer
                        |> Players.pointsForPlayer 1
                        |> Expect.equal (Just 0)
            , test "incrementScoreForPlayer 1" <|
                \_ ->
                    onePlayer
                        |> Players.incrementScoreForPlayer 1 10
                        |> Players.pointsForPlayer 1
                        |> Expect.equal (Just 10)
            , test "decrementScoreForPlayer 1" <|
                \_ ->
                    onePlayer
                        |> Players.decrementScoreForPlayer 1 10
                        |> Players.pointsForPlayer 1
                        |> Expect.equal (Just -10)
            , test "score" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.score
                        |> Scores.points
                        |> Expect.equal 0
            , test "points" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.points
                        |> Expect.equal 0
            , test "incrementScore" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.incrementScore 10
                        |> Players.points
                        |> Expect.equal 10
            , test "decrementScore" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.decrementScore 10
                        |> Players.points
                        |> Expect.equal -10
            , describe "highscores"
                [ test "empty list" <|
                    \_ ->
                        Players.init
                            (Lives 3)
                            (Highscores
                                { list = []
                                , max = 0
                                }
                            )
                            |> Players.highscores
                            |> Scores.list
                            |> Expect.equal []
                , test "with scores" <|
                    \_ ->
                        Players.init
                            (Lives 3)
                            (Highscores
                                { list =
                                    [ { name = "Paul"
                                      , points = 100
                                      }
                                    , { name = "Steve"
                                      , points = 90
                                      }
                                    ]
                                , max = 2
                                }
                            )
                            |> Players.highscores
                            |> Scores.list
                            |> List.map
                                (\score ->
                                    { name = score |> Scores.name
                                    , points = score |> Scores.points
                                    }
                                )
                            |> Expect.equal
                                [ { name = "Paul"
                                  , points = 100
                                  }
                                , { name = "Steve"
                                  , points = 90
                                  }
                                ]
                ]
            , test "highscoreToString" <|
                \_ ->
                    onePlayer
                        |> Players.incrementScoreForCurrentPlayer 10
                        |> Players.highscoreToString
                        |> Expect.equal "10"
            ]
        , describe "Game Over"
            [ test "gameOverForCurrentPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.gameOverForCurrentPlayer
                        |> Players.anyLivesLeftForCurrentPlayer
                        |> Expect.equal False
            , test "gameOverForPlayer" <|
                \_ ->
                    onePlayer
                        |> Players.gameOverForPlayer 1
                        |> Players.anyLivesLeftForPlayer 1
                        |> Expect.equal (Just False)
            , test "gameOver" <|
                \_ ->
                    onePlayer
                        |> Players.current
                        |> Players.gameOver
                        |> Players.anyLivesLeft
                        |> Expect.equal False
            ]
        , randomPlayersFuzzer "total" <|
            \total ->
                total
                    |> List.range 1
                    |> List.foldl
                        (\index players ->
                            players
                                |> Players.register ""
                        )
                        init
                    |> Players.total
                    |> Expect.equal total
        , describe "multiple"
            [ test "with one player" <|
                \_ ->
                    onePlayer
                        |> Players.multiple
                        |> Expect.equal False
            , test "with more than one player" <|
                \_ ->
                    twoPlayers
                        |> Players.multiple
                        |> Expect.equal True
            ]
        , test "current" <|
            \_ ->
                onePlayer
                    |> Players.current
                    |> Expect.all
                        [ \player ->
                            player
                                |> Players.name
                                |> Expect.equal "Player 1"
                        , \player ->
                            player
                                |> Players.number
                                |> Expect.equal 1
                        ]
        , test "playerNumber" <|
            \_ ->
                twoPlayers
                    |> Players.playerNumber 2
                    |> Expect.equal
                        (twoPlayers
                            |> Players.next
                            |> Players.current
                            |> Just
                        )
        , describe "all"
            [ test "with no players" <|
                \_ ->
                    init
                        |> Players.all
                        |> Expect.equal []
            , test "with players" <|
                \_ ->
                    twoPlayers
                        |> Players.all
                        |> Expect.all
                            [ \players ->
                                players
                                    |> List.map Players.name
                                    |> Expect.equal
                                        [ "Player 1", "Player 2" ]
                            , \players ->
                                players
                                    |> List.map Players.number
                                    |> Expect.equal
                                        [ 1, 2 ]
                            ]
            ]
        ]
