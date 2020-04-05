module Shared.Players exposing
    ( Players, Player
    , Lives(..), Highscores(..), init
    , register
    , removeCurrentPlayer, removePlayer, removeAll
    , replaceCurrentPlayer, replacePlayer
    , next, previous, goto
    , numberForCurrentPlayer, number
    , nameForCurrentPlayer, updateNameForCurrentPlayer, nameForPlayer, updateNameForPlayer, name, updateName
    , killCurrentPlayer, extraLifeForCurrentPlayer, livesForCurrentPlayer, anyLivesLeftForCurrentPlayer, lastLifeForCurrentPlayer
    , killPlayer, extraLifeForPlayer, livesForPlayer, anyLivesLeftForPlayer, lastLifeForPlayer
    , kill, extraLife, lives, anyLivesLeft, lastLife
    , anyAlive
    , incrementScoreForCurrentPlayer, decrementScoreForCurrentPlayer, scoreForCurrentPlayer, pointsForCurrentPlayer
    , incrementScoreForPlayer, decrementScoreForPlayer, scoreForPlayer, pointsForPlayer
    , incrementScore, decrementScore, score, points
    , highscores, highscoreToString
    , gameOverForCurrentPlayer, gameOverForPlayer, gameOver
    , total, multiple, all, current, playerNumber
    , NoStoredState
    , GameState(..), storeStateForCurrentPlayer, stateForCurrentPlayer, storeStateForPlayer, stateForPlayer, storeState, state
    )

{-| A module to manage one or more players in a game.

The purpose of this module is to enable easy management of [Players](#Players)
[turns](#changing), [scores](#scores), [highscores](#highscores),
[lives](#lives) and in-game [Memory](#memory).

@docs Players, Player


# INITIALIZING

@docs Lives, Highscores, init


# REGISTERING

@docs register


# REMOVING

@docs removeCurrentPlayer, removePlayer, removeAll


# REPLACING

@docs replaceCurrentPlayer, replacePlayer


# CHANGING

@docs next, previous, goto


# NUMBER

@docs numberForCurrentPlayer, number


# NAME

@docs nameForCurrentPlayer, updateNameForCurrentPlayer, nameForPlayer, updateNameForPlayer, name, updateName


# LIVES

@docs killCurrentPlayer, extraLifeForCurrentPlayer, livesForCurrentPlayer, anyLivesLeftForCurrentPlayer, lastLifeForCurrentPlayer
@docs killPlayer, extraLifeForPlayer, livesForPlayer, anyLivesLeftForPlayer, lastLifeForPlayer
@docs kill, extraLife, lives, anyLivesLeft, lastLife
@docs anyAlive


# SCORE

@docs incrementScoreForCurrentPlayer, decrementScoreForCurrentPlayer, scoreForCurrentPlayer, pointsForCurrentPlayer
@docs incrementScoreForPlayer, decrementScoreForPlayer, scoreForPlayer, pointsForPlayer
@docs incrementScore, decrementScore, score, points


## Highscores

@docs highscores, highscoreToString


# GAME OVER

@docs gameOverForCurrentPlayer, gameOverForPlayer, gameOver


# GENERAL

@docs total, multiple, all, current, playerNumber


# MEMORY

Store and retrieve the game state for a player.

This is primarily aimed at multi-player turn based games, where each player's
game state needs to be:

1.  Stored while the next player plays, and

2.  Resumed when it is that players turn again

In order to be able to store **any** game state for **any** game, and still be
able to access the fields later on, it is necessary to give the compiler a
little help. This is done by providing a `type` signature to
[Players](#Players).

    -- 1. The record type that will hold a players
    -- game state between turns
    type alias PlayerState =
        { playerShip : Ship
        , enemy : List Enemy
        }

    type alias Game =
        { playerShip : Ship
        , enemy : List Enemy
        -- 2. Provide the type that represents a players
        -- game state
        , players : Players PlayerState
        }

    -- 3. A Player is killed by the Enemy
    killPlayer : Game -> Game
    killPlayer game =
    { game
        | players =
            game.players
                |> Players.killCurrentPlayer
                -- 4. So store the game state
                |> Players.storeStateForCurrentPlayer
                    { playerShip = game.playerShip
                    , enemy = game.enemy
                    }
    }

    -- 5. Then it's the next Players turn
    nextPlayer : Game -> Game
    nextPlayer game =
        let
            players =
                game.players
                    |> Players.next

             playerState =
                -- 6. So retrieve any game state stored
                 case players |> Players.stateForCurrentPlayer of
                    State state ->
                        state -- 7. This is a PlayerState type
                        -- which the compiler is aware of thanks
                        -- to 2, therefore, it's fields are now
                        -- accessible

                    NoState ->
                        { playerShip = Ship.init
                        , enemy = Enemy.init
                        }
         in
         { game
             | playerShip = playerState.playerShip
             , enemy = playerState.enemy
             , players = players
         }

If it is not necessary to store the game state, you can tag your
[Players](#Players) type with [NoStoredState](#NoStoredState), or whatever
makes sense for you.

@docs NoStoredState

@docs GameState, storeStateForCurrentPlayer, stateForCurrentPlayer, storeStateForPlayer, stateForPlayer, storeState, state

-}

import Array
import Dict exposing (Dict)
import Shared.Scores as Scores exposing (Score)


{-| A type representing all the [Players](#Players) in a game.

This is an opaque type, use the exposed API to interact with it.

The unbound variable, `a`, is for storing game state. It allows you to define
the game state `type` so that when you save the current state, and need to
access it later, the compiler knows the type and will allow you to access its'
fields. See [MEMORY](#memory) for more info on this.

-}
type Players a
    = Players
        { list : Dict Int (Player a)
        , current : Player a
        , highscores : Scores.Scores
        , defaultLives : Int
        }


{-| A type representing a [Player](#Player) in a game.

This is an opaque type, use the exposed API to interact with it.

As with [Players](#Players), the unbound variable, `a`, is for storing game
state. See [MEMORY](#memory) for more info on this.

-}
type Player a
    = Player
        { name : String
        , lives : Int
        , score : Score
        , number : Int
        , state : GameState a
        }



-- INITIALIZING


{-| A type representing the default number of lives for each player at the
start of a game.
-}
type Lives
    = Lives Int


{-| A type representing highscores that have been stored and recalled, and the
maximum number of scores to store.

This module uses the [Scores](Shared.Scores#Scores) module to manage
[Player](#Player)s scores, see [Scores.init](Shared.Scores#init) for more info
on `max`.

-}
type Highscores
    = Highscores
        { list :
            List
                { name : String
                , points : Int
                }
        , max : Int
        }


{-| Initialize the default number of [Lives](#Lives) and any
[Highscores](Shared.Scores#Highscores).

This function should only be called once - on first start up in order to set
the default number of [lives](#lives) for each [Player](#Player) along with any
[Highscores](Shared.Scores#Highscores) retrieved from whatever source you
choose.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )

-}
init : Lives -> Highscores -> Players a
init (Lives lives_) (Highscores highscores_) =
    Players
        { list = Dict.empty
        , current = player
        , defaultLives = lives_
        , highscores =
            highscores_.list
                |> Scores.init
                    highscores_.max
        }


player : Player a
player =
    Player
        { name = ""
        , lives = 0
        , score = Scores.register ""
        , number = 0
        , state = NoState
        }



-- REGISTERING


{-| Register [Player](#Player)s to play a game.

The order of registration is important. The first [Player](#Player) to be
registered will be player one, the second will be player two, and so on.

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
        --> Just "Paul"

    init
        (Lives 3)
        (Highscores
            { list = []
            ,  max = 0
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> nameForPlayer 2
        --> Just "Steve"

-}
register : String -> Players a -> Players a
register name_ players =
    let
        key =
            players
                |> highestKey
                |> (+) 1

        newPlayer =
            player
                |> updateName
                    name_
                |> updateNumber
                    key
                |> updateLives
                    (players
                        |> defaultLives
                    )
                |> updateScore
                    (name_
                        |> Scores.register
                    )

        currentPlayer =
            case key of
                1 ->
                    newPlayer

                _ ->
                    players
                        |> current
    in
    players
        |> updateCurrent
            currentPlayer
        |> updateListItem
            key
            newPlayer


highestKey : Players a -> Int
highestKey players =
    players
        |> list
        |> Dict.keys
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0


updateListItem : Int -> Player a -> Players a -> Players a
updateListItem key player_ players =
    players
        |> updateList
            (players
                |> list
                |> Dict.update
                    key
                    (\_ -> Just player_)
            )



-- REMOVING


{-| Remove the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> removeCurrentPlayer
        |> total
        --> 1

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> removeCurrentPlayer
        |> nameForCurrentPlayer
        --> "Steve"

-}
removeCurrentPlayer : Players a -> Players a
removeCurrentPlayer players =
    players
        |> removePlayer
            (players
                |> current
                |> number
            )


{-| Remove a [Player](#Player), identifying them by their number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> removePlayer 1
        |> nameForCurrentPlayer
        --> "Steve"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> register "Jeremy"
        |> removePlayer 2
        |> nameForCurrentPlayer
        --> "Paul"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> register "Jeremy"
        |> next
        |> removePlayer 2
        |> nameForCurrentPlayer
        --> "Jeremy"

-}
removePlayer : Int -> Players a -> Players a
removePlayer key players =
    let
        players_ =
            players
                |> updateList
                    (players
                        |> list
                        |> Dict.remove
                            key
                    )
    in
    case players_ |> isCurrent key of
        True ->
            players_
                |> replaceCurrent
                    key

        False ->
            players_


isCurrent : Int -> Players a -> Bool
isCurrent key players =
    players
        |> current
        |> number
        |> (==) key


replaceCurrent : Int -> Players a -> Players a
replaceCurrent key players =
    let
        ( lowerKeys, higherKeys ) =
            players
                |> list
                |> partitionKeys
                    (<)
                    key
    in
    case ( lowerKeys, higherKeys ) of
        ( [], [] ) ->
            players
                |> updateCurrent
                    player

        ( first :: _, [] ) ->
            players
                |> goto
                    first

        ( _, first :: _ ) ->
            players
                |> goto
                    first


partitionKeys : (Int -> Int -> Bool) -> Int -> Dict Int (Player a) -> ( List Int, List Int )
partitionKeys compareFunc key_ dict =
    dict
        |> Dict.keys
        |> List.partition
            (\key ->
                compareFunc key key_
            )


{-| Remove all [Player](#Player)s.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> removeAll
        |> total
        --> 0

-}
removeAll : Players a -> Players a
removeAll players =
    players
        |> updateList
            Dict.empty
        |> updateCurrent
            player



-- REPLACING


{-| Replace the [current](#current) [Player](#Player) with the new
[Player](#Player).
-}
replaceCurrentPlayer : Player a -> Players a -> Players a
replaceCurrentPlayer player_ players =
    players
        |> replacePlayer
            (players
                |> current
                |> number
            )
            player_


{-| Replace a [Player](#Player) with the new [Player](#Player), identifying the
replaced [Player](#Player) by their number.
-}
replacePlayer : Int -> Player a -> Players a -> Players a
replacePlayer key player_ players =
    players
        |> updateListItem
            key
            (player_
                |> updateNumber
                    key
            )
        |> goto
            key



-- CHANGING


{-| Set the next [Player](#Player) as the [current](#current)
[Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> next
        |> nameForCurrentPlayer
        --> "Steve"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> register "Jeremy"
        |> removePlayer 2
        |> next
        |> nameForCurrentPlayer
        --> "Jeremy"

When the current [Player](#Player) is the last [Player](#Player), calling
[next](#next) will cycle back to the first [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> next
        |> next
        |> nameForCurrentPlayer
        --> "Paul"

When the current [Player](#Player) is the only [Player](#Player), calling
[next](#next) has no effect.

-}
next : Players a -> Players a
next players =
    let
        ( lowerKeys, higherKeys ) =
            players
                |> list
                |> partitionKeys
                    (<=)
                    (players
                        |> current
                        |> number
                    )
    in
    case ( lowerKeys, higherKeys ) of
        ( [], [] ) ->
            players

        ( first :: _, [] ) ->
            players
                |> goto
                    first

        ( _, first :: _ ) ->
            players
                |> goto
                    first


{-| Set the previous [Player](#Player) as the [current](#current)
[Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> next
        |> previous
        |> nameForCurrentPlayer
        --> "Paul"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> register "Jeremy"
        |> next
        |> next
        |> removePlayer 2
        |> previous
        |> nameForCurrentPlayer
        --> "Paul"

When the current [Player](#Player) is the first [Player](#Player), calling
[previous](#previous) will cycle back to the last [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> previous
        |> nameForCurrentPlayer
        --> "Steve"

When the current [Player](#Player) is the only [Player](#Player), calling
[previous](#previous) has no effect.

-}
previous : Players a -> Players a
previous players =
    let
        ( lowerKeys, higherKeys ) =
            players
                |> list
                |> partitionKeys
                    (<)
                    (players
                        |> current
                        |> number
                    )
    in
    case ( lowerKeys, higherKeys ) of
        ( [], [] ) ->
            players

        ( [], first :: rest ) ->
            players
                |> goto
                    (rest
                        |> lastKeyOr
                            first
                    )

        ( first :: rest, _ ) ->
            players
                |> goto
                    (rest
                        |> lastKeyOr
                            first
                    )


lastKeyOr : Int -> List Int -> Int
lastKeyOr default keys =
    keys
        |> List.reverse
        |> List.head
        |> Maybe.withDefault
            default


{-| Goto the specified [Player](#Player) number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> goto 2
        |> nameForCurrentPlayer
        --> "Steve"

If the [Player](#Player) does not exist, no changes are made.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> goto 5
        |> nameForCurrentPlayer
        --> "Paul"

-}
goto : Int -> Players a -> Players a
goto key players =
    players
        |> updateCurrent
            (players
                |> list
                |> Dict.get
                    key
                |> Maybe.withDefault
                    (players
                        |> current
                    )
            )



-- NUMBER


{-| Get the current [Player](#Player)'s number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> numberForCurrentPlayer
        --> 1

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> next
        |> numberForCurrentPlayer
        --> 2

-}
numberForCurrentPlayer : Players a -> Int
numberForCurrentPlayer players =
    players
        |> current
        |> number


{-| Get a [Player](#Player)'s number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> number
        --> 1

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> next
        |> current
        |> number
        --> 2

-}
number : Player a -> Int
number (Player player_) =
    player_.number



-- NAME


{-| Get the name of the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> nameForCurrentPlayer
        --> "Paul"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> next
        |> nameForCurrentPlayer
        --> "Steve"

-}
nameForCurrentPlayer : Players a -> String
nameForCurrentPlayer players =
    players
        |> current
        |> name


{-| Get the name of a [Player](#Player), identifiying them by their number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> nameForPlayer 1
        --> Just "Paul"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> nameForPlayer 2
        --> Just "Steve"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> nameForPlayer 3
        --> Nothing

-}
nameForPlayer : Int -> Players a -> Maybe String
nameForPlayer key players =
    players
        |> maybeAccessAndCall
            key
            name


{-| Get the name of a [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> name
        --> "Paul"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> next
        |> current
        |> name
        --> "Steve"

-}
name : Player a -> String
name (Player player_) =
    player_.name


{-| Update the name for the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> updateNameForCurrentPlayer "James"
        |> nameForCurrentPlayer
        --> "James"

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> updateNameForCurrentPlayer "James"
        |> nameForPlayer 1
        --> Just "James"

-}
updateNameForCurrentPlayer : String -> Players a -> Players a
updateNameForCurrentPlayer name_ players =
    players
        |> sync
            (players
                |> current
                |> updateName
                    name_
            )


{-| Update the name of a [Player](#Player), identifying them by their number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> updateNameForPlayer 1 "James"
        |> nameForPlayer 1
        --> Just "James"

If the [Player](#Player) does not exist, no changes are made.

-}
updateNameForPlayer : Int -> String -> Players a -> Players a
updateNameForPlayer key name_ players =
    players
        |> maybeAccessAndSync
            key
            (updateName name_)


{-| Update the name of a [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> updateName "James"
        |> name
        --> "James"

-}
updateName : String -> Player a -> Player a
updateName name_ (Player player_) =
    Player
        { player_
            | name = name_
        }
        |> updateScore
            (Player player_
                |> score
                |> Scores.updateName
                    name_
            )



-- LIVES


{-| Get the number of lives left for the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> killCurrentPlayer
        |> livesForCurrentPlayer
        --> 2

-}
livesForCurrentPlayer : Players a -> Int
livesForCurrentPlayer players =
    players
        |> current
        |> lives


{-| Get the number of lives left for a [Player](#Player), identifying them by
their number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> killPlayer 1
        |> livesForPlayer 1
        --> Just 2

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> killCurrentPlayer
        |> livesForPlayer 2
        --> Just 3

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> killCurrentPlayer
        |> livesForPlayer 3
        --> Nothing

-}
livesForPlayer : Int -> Players a -> Maybe Int
livesForPlayer key players =
    players
        |> maybeAccessAndCall
            key
            lives


{-| Get the number of lives a [Player](#Player) has left.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> current
        |> lives
        --> 3

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> killCurrentPlayer
        |> current
        |> lives
        --> 2

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
        |> current
        |> lives
        --> 1

-}
lives : Player a -> Int
lives (Player player_) =
    player_.lives


{-| Determine if any [Players](#Players) are still alive.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> anyAlive
        --> True

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
        |> killCurrentPlayer
        |> anyAlive
        --> False

-}
anyAlive : Players a -> Bool
anyAlive players =
    players
        |> list
        |> Dict.values
        |> List.any
            anyLivesLeft


{-| Determine if the current [Player](#Player) has any lives left.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> anyLivesLeftForCurrentPlayer
        --> True

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
        |> killCurrentPlayer
        |> anyLivesLeftForCurrentPlayer
        --> False

-}
anyLivesLeftForCurrentPlayer : Players a -> Bool
anyLivesLeftForCurrentPlayer players =
    players
        |> current
        |> anyLivesLeft


{-| Determine if a [Player](#Player) has any lives left, identiftying them by
their number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> anyLivesLeftForPlayer 1
        --> Just True

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
        |> killPlayer 1
        |> anyLivesLeftForPlayer 1
        --> Just False

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> anyLivesLeftForPlayer 2
        --> Nothing

-}
anyLivesLeftForPlayer : Int -> Players a -> Maybe Bool
anyLivesLeftForPlayer key players =
    players
        |> maybeAccessAndCall
            key
            anyLivesLeft


{-| Determine if a [Player](#Player) has any lives left.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> current
        |> anyLivesLeft
        --> True

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
        |> killCurrentPlayer
        |> current
        |> anyLivesLeft
        --> False

-}
anyLivesLeft : Player a -> Bool
anyLivesLeft player_ =
    (player_ |> lives) > 0


{-| Determine if the current [Player](#Player) is on their last life.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> lastLifeForCurrentPlayer
        --> False

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
        --> True

-}
lastLifeForCurrentPlayer : Players a -> Bool
lastLifeForCurrentPlayer players =
    players
        |> current
        |> lastLife


{-| Determine if a [Player](#Player) is on their last life, identifying them by
their number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> lastLifeForPlayer 1
        --> Just False

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
        --> Just True

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> lastLifeForPlayer 2
        --> Nothing

-}
lastLifeForPlayer : Int -> Players a -> Maybe Bool
lastLifeForPlayer key players =
    players
        |> maybeAccessAndCall
            key
            lastLife


{-| Determine if a [Player](#Player) is on their last life.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> current
        |> lastLife
        --> False

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
        |> current
        |> lastLife
        --> True

-}
lastLife : Player a -> Bool
lastLife player_ =
    (player_ |> lives) == 1



-- Killing


{-| Kill the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> killCurrentPlayer
        |> current
        |> lives
        --> 2

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
        |> current
        |> lives
        --> 1

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
        |> killCurrentPlayer
        |> current
        |> lives
        --> 0

-}
killCurrentPlayer : Players a -> Players a
killCurrentPlayer players =
    players
        |> sync
            (players
                |> current
                |> kill
            )


{-| Kill a [Player](#Player), identifiyng them by their number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> killPlayer 1
        |> livesForPlayer 1
        --> Just 2

If the [Player](#Player) does not exist, no changes are made.

-}
killPlayer : Int -> Players a -> Players a
killPlayer key players =
    players
        |> maybeAccessAndSync
            key
            kill


{-| Kill a [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> kill
        |> lives
        --> 2

-}
kill : Player a -> Player a
kill player_ =
    player_
        |> updateLives
            ((player_ |> lives) - 1)



-- Extra Lives


{-| Give the current [Player](#Player) an extra life.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> extraLifeForCurrentPlayer
        |> livesForCurrentPlayer
        --> 4

-}
extraLifeForCurrentPlayer : Players a -> Players a
extraLifeForCurrentPlayer players =
    players
        |> sync
            (players
                |> current
                |> extraLife
            )


{-| Give a [Player](#Player) an extra life, identifying them by their number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> extraLifeForPlayer 1
        |> livesForPlayer 1
        --> Just 4

If the [Player](#Player) does not exist, no changes are made.

-}
extraLifeForPlayer : Int -> Players a -> Players a
extraLifeForPlayer key players =
    players
        |> maybeAccessAndSync
            key
            extraLife


{-| Give a [Player](#Player) an extra life.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> extraLife
        |> lives
        --> 4

-}
extraLife : Player a -> Player a
extraLife player_ =
    player_
        |> updateLives
            (player_
                |> lives
                |> (+) 1
            )



-- SCORE


{-| Get the current [Highscores](Shared.Highscores#Highscores).

    import Shared.Scores as Scores

    init
        (Lives 3)
        (Highscores
            { list = [{name = "James", points = 100}]
            , max = 10
            }
        )
        |> highscores
        |> Scores.highest
        |> Scores.points
        --> 100

-}
highscores : Players a -> Scores.Scores
highscores (Players players) =
    players.highscores


{-| Get the [Score](Shared.Scores#Score) for the current [Player](#Player).

    import Shared.Scores as Scores

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> scoreForCurrentPlayer
        |> Scores.points
        --> 0

-}
scoreForCurrentPlayer : Players a -> Score
scoreForCurrentPlayer players =
    players
        |> current
        |> score


{-| Get the [Score](Shared.Scores#Score) for a [Player](#Player), identifying
them by their number.

    import Shared.Scores as Scores

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> incrementScoreForPlayer 1 10
        |> scoreForPlayer 1
        |> Maybe.withDefault (Scores.register "")
        |> Scores.points
        --> 10

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> scoreForPlayer 3
        --> Nothing

-}
scoreForPlayer : Int -> Players a -> Maybe Score
scoreForPlayer key players =
    players
        |> maybeAccessAndCall
            key
            score


{-| Get the [Score](Shared.Scores#Score) for a [Player](#Player).

    import Shared.Scores as Scores

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> score
        |> Scores.points
        --> 0

-}
score : Player a -> Score
score (Player player_) =
    player_.score


{-| Increase the score for the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> incrementScoreForCurrentPlayer 10
        |> pointsForCurrentPlayer
        --> 10

-}
incrementScoreForCurrentPlayer : Int -> Players a -> Players a
incrementScoreForCurrentPlayer score_ players =
    let
        updatedPlayer =
            players
                |> current
                |> incrementScore
                    score_
    in
    players
        |> sync
            updatedPlayer
        |> scoreChange
            (Scores.scoreChange
                (updatedPlayer
                    |> score
                )
            )


scoreChange : (Scores.Scores -> Scores.Scores) -> Players a -> Players a
scoreChange func players =
    players
        |> updateHighscores
            (players
                |> highscores
                |> func
            )


{-| Increase the score for a [Player](#Player), identifying them by their
number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> incrementScoreForPlayer 1 10
        |> pointsForPlayer 1
        --> Just 10

If the [Player](#Player) does not exist, no changes are made.

-}
incrementScoreForPlayer : Int -> Int -> Players a -> Players a
incrementScoreForPlayer key score_ players =
    case players |> list |> Dict.get key of
        Just player_ ->
            let
                updatedPlayer =
                    player_
                        |> incrementScore
                            score_
            in
            players
                |> syncIfCurrent
                    key
                    updatedPlayer
                |> scoreChange
                    (Scores.scoreChange
                        (updatedPlayer
                            |> score
                        )
                    )

        Nothing ->
            players


{-| Increase the score for a [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> incrementScore 10
        |> points
        --> 10

-}
incrementScore : Int -> Player a -> Player a
incrementScore points_ player_ =
    player_
        |> updateScore
            (player_
                |> score
                |> Scores.incrementScore
                    points_
            )


{-| Decrease the score for the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> decrementScoreForCurrentPlayer 10
        |> current
        |> points
        --> -10

-}
decrementScoreForCurrentPlayer : Int -> Players a -> Players a
decrementScoreForCurrentPlayer score_ players =
    let
        updatedPlayer =
            players
                |> current
                |> decrementScore
                    score_
    in
    players
        |> sync
            updatedPlayer
        |> scoreChange
            (Scores.scoreChange
                (updatedPlayer
                    |> score
                )
            )


{-| Decrease the score for a [Player](#Player), identifying them by their
number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> decrementScoreForPlayer 1 10
        |> pointsForPlayer 1
        --> Just -10

If the [Player](#Player) does not exist, no changes are made.

-}
decrementScoreForPlayer : Int -> Int -> Players a -> Players a
decrementScoreForPlayer key score_ players =
    case players |> list |> Dict.get key of
        Just player_ ->
            let
                updatedPlayer =
                    player_
                        |> decrementScore
                            score_
            in
            players
                |> syncIfCurrent
                    key
                    updatedPlayer
                |> scoreChange
                    (Scores.scoreChange
                        (updatedPlayer
                            |> score
                        )
                    )

        Nothing ->
            players


{-| Decrease the score for a [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> decrementScore 10
        |> points
        --> -10

-}
decrementScore : Int -> Player a -> Player a
decrementScore points_ player_ =
    player_
        |> updateScore
            (player_
                |> score
                |> Scores.decrementScore
                    points_
            )


{-| Get the points scored for the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> incrementScoreForCurrentPlayer 10
        |> pointsForCurrentPlayer
        --> 10

-}
pointsForCurrentPlayer : Players a -> Int
pointsForCurrentPlayer players =
    players
        |> current
        |> points


{-| Get the points scored for a [Player](#Player), indentifying them by their
number.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> incrementScoreForPlayer 1 10
        |> pointsForPlayer 1
        --> Just 10

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> incrementScoreForPlayer 1 10
        |> incrementScoreForPlayer 2 20
        |> pointsForPlayer 2
        --> Just 20

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> pointsForPlayer 3
        --> Nothing

-}
pointsForPlayer : Int -> Players a -> Maybe Int
pointsForPlayer key players =
    players
        |> maybeAccessAndCall
            key
            points


{-| Get the points scored for a [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> incrementScoreForCurrentPlayer 10
        |> current
        |> points
        --> 10

-}
points : Player a -> Int
points player_ =
    player_
        |> score
        |> Scores.points



-- GAME OVER


{-| Call this function when the current [Player](#Player) has finished their
game.

It will set the [Player](#Player)'s lives to zero and submit their
[Score](Shared.Scores#Score) to the [Highscores](Shared.Scores#Highscores) list.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 0
            }
        )
        |> register "Paul"
        |> gameOverForCurrentPlayer
        |> livesForCurrentPlayer
        --> 0

    import Shared.Scores as Scores

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 5
            }
        )
        |> register "Paul"
        |> incrementScoreForCurrentPlayer 100
        |> gameOverForCurrentPlayer
        |> highscores
        |> Scores.highest
        |> Scores.points
        --> 100

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 5
            }
        )
        |> register "Paul"
        |> incrementScoreForCurrentPlayer 100
        |> gameOverForCurrentPlayer
        |> highscores
        |> Scores.highest
        |> Scores.name
        --> "Paul"

-}
gameOverForCurrentPlayer : Players a -> Players a
gameOverForCurrentPlayer players =
    let
        updatedPlayer =
            players
                |> current
                |> gameOver
    in
    players
        |> sync
            updatedPlayer
        |> scoreChange
            (Scores.gameOver
                (updatedPlayer
                    |> score
                )
            )


{-| Call this function when a [Player](#Player) has finished their game,
identifying them by their number.

It will set the [Player](#Player)'s lives to zero and submit their
[Score](Shared.Scores#Score) to the [Highscores](Shared.Scores#Highscores) list.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 0
            }
        )
        |> register "Paul"
        |> gameOverForPlayer 1
        |> livesForPlayer 1
        --> Just 0

    import Shared.Scores as Scores

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 5
            }
        )
        |> register "Paul"
        |> incrementScoreForPlayer 1 100
        |> gameOverForPlayer 1
        |> highscores
        |> Scores.highest
        |> Scores.points
        --> 100

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 5
            }
        )
        |> register "Paul"
        |> incrementScoreForPlayer 1 100
        |> gameOverForPlayer 1
        |> highscores
        |> Scores.highest
        |> Scores.name
        --> "Paul"

If the [Player](#Player) does not exist, no changes are made.

-}
gameOverForPlayer : Int -> Players a -> Players a
gameOverForPlayer key players =
    case players |> list |> Dict.get key of
        Just player_ ->
            let
                updatedPlayer =
                    player_
                        |> gameOver
            in
            players
                |> syncIfCurrent
                    key
                    updatedPlayer
                |> scoreChange
                    (Scores.gameOver
                        (updatedPlayer
                            |> score
                        )
                    )

        Nothing ->
            players


{-| Call this function when a [Player](#Player) has finished their game.

It will set the [Player](#Player)'s lives to zero - but will not submit their
[Score](Shared.Scores#Score) to the [Highscores](Shared.Scores#Highscores) list.

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
        --> 0

-}
gameOver : Player a -> Player a
gameOver (Player player_) =
    Player
        { player_
            | lives = 0
        }



-- STRING CONVERSION


{-| Convert the current highest [Score](Shared.Scores#Score) to a `String`.

    init
        (Lives 3)
        (Highscores
            { list = [{name = "Paul", points = 100}]
            , max = 10
            }
        )
        |> highscoreToString
        --> "100"

-}
highscoreToString : Players a -> String
highscoreToString players =
    players
        |> highscores
        |> Scores.highestToString



-- GENERAL


{-| Get a list of all the players.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> all
        |> List.map (\ player -> player |> name)
        --> ["Paul", "Steve"]

-}
all : Players a -> List (Player a)
all players =
    players
        |> list
        |> Dict.values


{-| Get the current [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> current
        |> name
        --> "Paul"

**Note:** If you call `current` before any [Player](#Player)s have been
[register](#register)ed, you will receive back a default [Player](#Player).

The default [Player](#Player) will have no name, no points, no lives etc.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> current
        |> name
        --> ""

-}
current : Players a -> Player a
current (Players players) =
    players.current


defaultLives : Players a -> Int
defaultLives (Players players) =
    players.defaultLives


list : Players a -> Dict Int (Player a)
list (Players players) =
    players.list


{-| Determine if there is more than one [Player](#Player).

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> multiple
        --> True

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> multiple
        --> False

-}
multiple : Players a -> Bool
multiple players =
    (players |> total) > 1


{-| Get a [Player](#Player), identifying them by their number.
-}
playerNumber : Int -> Players a -> Maybe (Player a)
playerNumber num players =
    players
        |> list
        |> Dict.get
            num


{-| The total number of players.

    init
        (Lives 3)
        (Highscores
            { list = []
            , max = 10
            }
        )
        |> register "Paul"
        |> register "Steve"
        |> total
        --> 2

-}
total : Players a -> Int
total players =
    players
        |> list
        |> Dict.size



-- UPDATING


updateCurrent : Player a -> Players a -> Players a
updateCurrent player_ (Players players) =
    Players
        { players
            | current = player_
        }


updateHighscores : Scores.Scores -> Players a -> Players a
updateHighscores highscores_ (Players players) =
    Players
        { players
            | highscores = highscores_
        }


updateList : Dict Int (Player a) -> Players a -> Players a
updateList list_ (Players players) =
    Players
        { players
            | list = list_
        }


updateLives : Int -> Player a -> Player a
updateLives lives_ (Player player_) =
    Player
        { player_
            | lives = lives_
        }


updateNumber : Int -> Player a -> Player a
updateNumber number_ (Player player_) =
    Player
        { player_
            | number = number_
        }


updateScore : Score -> Player a -> Player a
updateScore score_ (Player player_) =
    Player
        { player_
            | score = score_
        }



-- MAYBE ACCESS


maybeAccessAndCall : Int -> (Player a -> b) -> Players a -> Maybe b
maybeAccessAndCall key func players =
    -- If the player can be accessed, call the func on it
    case players |> list |> Dict.get key of
        Just player_ ->
            player_
                |> func
                |> Just

        Nothing ->
            Nothing


maybeAccessAndSync : Int -> (Player a -> Player a) -> Players a -> Players a
maybeAccessAndSync key func players =
    -- If the player can be accessed, syncIfCurrent
    case players |> list |> Dict.get key of
        Just player_ ->
            players
                |> syncIfCurrent
                    key
                    (player_
                        |> func
                    )

        Nothing ->
            players



-- SYNCING


sync : Player a -> Players a -> Players a
sync player_ players =
    -- Sync the current player with the dict version
    players
        |> updateListItem
            (player_
                |> number
            )
            player_
        |> updateCurrent
            player_


syncIfCurrent : Int -> Player a -> Players a -> Players a
syncIfCurrent key player_ players =
    -- Only sync if the player is the current player
    case (player_ |> number) == (players |> current |> number) of
        True ->
            players
                |> sync
                    player_

        False ->
            players
                |> updateListItem
                    key
                    player_



-- MEMORY


{-| A union type representing the game [State](#GameState) for a
[Player](#Player).

This is the type that stores game state internally, and it is what you get back
when retrieving a [Player](#Player)s previous game state.

-}
type GameState a
    = State a
    | NoState


{-| Convenience type intended to declare that the game state is not being stored.

    type alias Game =
        { players : Players NoStoredState }

-}
type NoStoredState
    = NoStoredState


{-| Store the game state for the current [Player](#Player) so that it can be
resumed when it is that [Player](#Player)'s turn again.
-}
storeStateForCurrentPlayer : a -> Players a -> Players a
storeStateForCurrentPlayer state_ players =
    players
        |> sync
            (players
                |> current
                |> storeState
                    state_
            )


{-| Store the game state for a [Player](#Player), identifying them by their
number.

If the [Player](#Player) does not exist, no changes are made.

-}
storeStateForPlayer : Int -> a -> Players a -> Players a
storeStateForPlayer key state_ players =
    players
        |> maybeAccessAndSync
            key
            (storeState state_)


{-| Store the game state for a [Player](#Player).
-}
storeState : a -> Player a -> Player a
storeState state_ (Player player_) =
    Player
        { player_
            | state = State state_
        }


{-| Retrieve the state for the current [Player](#Player).

    init
        (Lives 3)
        (Highscores { list = []
        , max = 0
        })
        |> register "Paul"
        |> storeStateForCurrentPlayer
            { enemies = 5 }
        |> stateForCurrentPlayer
        --> State { enemies = 5 }

If no state has been stored, you will receive back [NoState](#GameState).

    init
        (Lives 3)
        (Highscores { list = []
        , max = 0
        })
        |> register "Paul"
        |> stateForCurrentPlayer
        --> NoState

-}
stateForCurrentPlayer : Players a -> GameState a
stateForCurrentPlayer players =
    players
        |> current
        |> state


{-| Retrieve the state for a [Player](#Player), identifying them by their number.

    init
        (Lives 3)
        (Highscores { list = []
        , max = 0
        })
        |> register "Paul"
        |> storeStateForPlayer 1
            { enemies = 5 }
        |> stateForPlayer 1
        --> Just (State { enemies = 5 })

If no state has been stored, you will receive back [NoState](#GameState).

    init
        (Lives 3)
        (Highscores { list = []
        , max = 0
        })
        |> register "Paul"
        |> stateForPlayer 1
        --> Just NoState

If you try and access the state of a non existent player, you will receive back
`Nothing`.

    init
        (Lives 3)
        (Highscores { list = []
        , max = 0
        })
        |> register "Paul"
        |> storeStateForPlayer 1
            { enemies = 5 }
        |> stateForPlayer 2
        --> Nothing

-}
stateForPlayer : Int -> Players a -> Maybe (GameState a)
stateForPlayer key players =
    players
        |> maybeAccessAndCall
            key
            state


{-| Retrieve the state for a [Player](#Player).

    init
        (Lives 3)
        (Highscores { list = []
        , max = 0
        })
        |> register "Paul"
        |> current
        |> storeState
            { enemies = 5 }
        |> state
        --> State { enemies = 5 }

If no state has been stored, you will receive back [NoState](#GameState).

    init
        (Lives 3)
        (Highscores { list = []
        , max = 0
        })
        |> register "Paul"
        |> current
        |> state
        --> NoState

-}
state : Player a -> GameState a
state (Player player_) =
    player_.state
