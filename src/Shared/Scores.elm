module Shared.Scores exposing
    ( Scores, Score
    , Max, init
    , register
    , incrementScore, decrementScore, scoreChange, highest, lowest, list
    , gameOver
    , prepareForSave
    , highestToString
    , name, points, total
    , updateName
    )

{-| A module for managing game [Score](#Score)s and highscores.

This module is used by the [Players](Shared.Players) module to manage players
scores, so if you're using that module, you probably won't need many of the
functions in this module.

@docs Scores, Score


# CREATING

@docs Max, init


# REGISTERING

@docs register


# SCORING

@docs incrementScore, decrementScore, scoreChange, highest, lowest, list


# GAME OVER

@docs gameOver


# SAVING

@docs prepareForSave


# CONVERTING

@docs highestToString


# QUERYING

@docs name, points, total


# UPDATING

@docs updateName

-}


{-| A type representing a list of [Scores](#Scores).

This is an opaque type, use the exposed API to interact with it.

-}
type Scores
    = Scores
        { currentHighest : Score
        , list : List Score
        , total : Int
        }


{-| A type representing a [Player](Shared.Players#Player)'s [Score](#Score).

This is an opaque type, use the exposed API to interact with it.

-}
type Score
    = Score
        { points : Int
        , name : String
        }



-- CREATING


{-| A type alias representing the maximum number of [Score](#Score)s to keep
track of.
-}
type alias Max =
    Int


{-| Initialize a list of [Scores](#Scores).

**Note**, if you are using the [Players](Players#Players) module this is taken
care of for you.

If you want to record a total of 10 [Score](#Score)s, but the list contains
only 2, then an additional 8 zero [Score](#score)s will be created.

    init
        10
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        ]
        |> list
        |> List.length
        --> 10

If you want to record a total of 2 [Score](#Score)s, but the list contains 3,
then the lowest [Score](#Score) will be dropped.

    init
        2
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        , { name = "Jeremy"
          , points = 800
          }
        ]
        |> list
        |> List.length
        --> 2

    init
        2
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        , { name = "Jeremy"
          , points = 800
          }
        ]
        |> lowest
        |> name
        --> "Steve"

-}
init : Max -> List { name : String, points : Int } -> Scores
init max list_ =
    let
        newList =
            list_
                |> initScores
                    max
    in
    Scores
        { currentHighest =
            newList
                |> List.head
                |> Maybe.withDefault
                    initScore
        , list = newList
        , total = max
        }


initScores : Max -> List { name : String, points : Int } -> List Score
initScores max scores_ =
    let
        list_ =
            scores_
                |> List.map
                    Score

        extras =
            max - (list_ |> List.length)
    in
    case extras < 0 of
        True ->
            list_
                |> List.sortWith
                    highToLow
                |> List.take
                    max

        False ->
            list_
                |> List.append
                    (initScore
                        |> List.repeat
                            extras
                    )
                |> List.sortWith
                    highToLow


initScore : Score
initScore =
    Score
        { name = "---"
        , points = 0
        }



-- REGISTERING


{-| Register a new player.

**Note**, if you are using the [Players](Players#Players) module this is taken care of
for you.

    register "Paul"
        |> name
        --> "Paul"

    register "Paul"
        |> points
        --> 0

-}
register : String -> Score
register name_ =
    initScore
        |> updateName
            name_



-- SCORING


{-| Increase a [Score](#Score) by the specified amount.

**Note**, if you are using the [Players](Players#Players) module this is taken care of
for you.

    register "Paul"
        |> incrementScore 10
        |> incrementScore 5
        |> points
        --> 15

-}
incrementScore : Int -> Score -> Score
incrementScore points_ score =
    score
        |> updatePoints
            (score
                |> points
                |> (+) points_
            )


{-| Decrease a [Score](#Score) by the specified amount.

**Note**, if you are using the [Players](Players#Players) module this is taken care of
for you.

    register "Paul"
        |> incrementScore 10
        |> decrementScore 3
        |> points
        --> 7

-}
decrementScore : Int -> Score -> Score
decrementScore points_ score =
    score
        |> updatePoints
            ((score |> points) - points_)


{-| A players score has changed, so check to see if it replaces the current
`highest`.

**Note**, if you are using the [Players](Players#Players) module this is taken care of
for you.

This does not affect the whole list, only the `highest`
[Score](#Score). This is really intended for in-game use where you
just want to compare the current score to the highest, and display accordingly.

The whole list will be updated when [gameOver](#gameOver) is called.

-}
scoreChange : Score -> Scores -> Scores
scoreChange score scores =
    case (score |> points) > (scores |> currentHighest |> points) of
        True ->
            scores
                |> updateHighest
                    score

        False ->
            scores


{-| Get the highest score.

    init
        2
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        ]
        |> highest
        |> points
        --> 1000

-}
highest : Scores -> Score
highest scores =
    scores
        |> list
        |> first
            highToLow


{-| Get the lowest [Score](#Score).

    init
        2
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        ]
        |> lowest
        |> points
        --> 900

-}
lowest : Scores -> Score
lowest scores =
    scores
        |> list
        |> first
            lowToHigh



-- SORTING


first : (Score -> Score -> Order) -> List Score -> Score
first sorter list_ =
    list_
        |> List.sortWith
            sorter
        |> List.head
        |> Maybe.withDefault
            initScore


highToLow : Score -> Score -> Order
highToLow a b =
    case compare (a |> points) (b |> points) of
        GT ->
            LT

        EQ ->
            EQ

        LT ->
            GT


lowToHigh : Score -> Score -> Order
lowToHigh a b =
    case compare (a |> points) (b |> points) of
        GT ->
            GT

        EQ ->
            EQ

        LT ->
            LT



-- GAME OVER


{-| A player has finished their game so let's see if they make the highscores
list.

**Note**, if you are using the [Players](Players#Players) module this is taken care of
for you.

-}
gameOver : Score -> Scores -> Scores
gameOver score scores =
    case scores |> list |> qualifies score of
        True ->
            let
                newList =
                    scores
                        |> list
                        |> List.append
                            [ score ]
                        |> List.sortWith
                            highToLow
                        |> List.take
                            (scores
                                |> total
                            )
            in
            scores
                |> updateList
                    newList
                |> updateHighest
                    (newList
                        |> first
                            highToLow
                    )

        False ->
            scores


qualifies : Score -> List Score -> Bool
qualifies potential list_ =
    let
        filteredList =
            list_
                |> List.filter
                    (\score ->
                        (score |> points) > (potential |> points)
                    )
    in
    (filteredList |> List.length) /= (list_ |> List.length)



-- SAVING


{-| Prepare the list of [Scores](#Scores) for saving, either to
`localStorage` or a back end database.

    init
        3
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        ]
        |> prepareForSave
        --> [ {name = "Paul", points = 1000}, {name = "Steve", points = 900}, {name = "---", points = 0}]

-}
prepareForSave : Scores -> List { name : String, points : Int }
prepareForSave scores =
    scores
        |> list
        |> List.map
            (\(Score score) -> score)



-- CONVERTING


{-| Convert the highest score to a `String` for displaying in a `view`.

    init
        3
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        ]
        |> highestToString
        --> "1000"

-}
highestToString : Scores -> String
highestToString scores =
    scores
        |> currentHighest
        |> points
        |> String.fromInt



-- QUERYING


currentHighest : Scores -> Score
currentHighest (Scores scores) =
    scores.currentHighest


{-| The list of [Score](#Score)s.
-}
list : Scores -> List Score
list (Scores scores) =
    scores.list


{-| The name associated with a [Score](#Score).

    init
        3
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        ]
        |> highest
        |> name
        --> "Paul"

-}
name : Score -> String
name (Score score) =
    score.name


{-| The points associated with a [Score](#Score).

    init
        3
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        ]
        |> highest
        |> points
        --> 1000

-}
points : Score -> Int
points (Score score) =
    score.points


{-| The total number of [Score](#Score)s.

    init
        3
        [ { name = "Paul"
          , points = 1000
          }
        , { name = "Steve"
          , points = 900
          }
        ]
        |> total
        --> 3

-}
total : Scores -> Int
total (Scores scores) =
    scores.total



-- UPDATING


updateHighest : Score -> Scores -> Scores
updateHighest score (Scores scores) =
    Scores
        { scores
            | currentHighest = score
        }


updateList : List Score -> Scores -> Scores
updateList list_ (Scores scores) =
    Scores
        { scores
            | list = list_
        }


{-| Update the name associated with a [Score](#Score).
-}
updateName : String -> Score -> Score
updateName name_ (Score score) =
    Score
        { score
            | name = name_
        }


updatePoints : Int -> Score -> Score
updatePoints points_ (Score score) =
    Score
        { score
            | points = points_
        }
