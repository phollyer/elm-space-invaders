module Shared.Level exposing
    ( Level
    , init
    , number
    , goto, next, previous
    , toString
    , Difficulty(..)
    )

{-| A simple module to manage game [Level](#Level)s.

@docs Level


# INITIALIZING

@docs init


# QUERYING

@docs number


# CHANGING

@docs goto, next, previous


# CONVERTING

@docs toString


# EASTER EGG

@docs Difficulty

-}


{-| A type representing a game [Level](#Level).

This is an opaque type, use the exposed API to interact with it.

-}
type Level
    = Level Int


{-| A union type representing different game [Difficulty](#Difficulty)'s.
-}
type Difficulty
    = Easy
    | Medium
    | Hard



-- CREATING


{-| Initialize at [Level](#Level) 1.

    init
        |> number
        --> 1

-}
init : Level
init =
    Level 1



-- ACCESSING


{-| Access the [Level](#Level) number.

    init
        |> number
        --> 1

-}
number : Level -> Int
number (Level num) =
    num



-- CHANGING


{-| Go to the [next](#next) [Level](#Level).

Increments the [Level](#Level) by 1.

    init
        |> next
        |> number
        --> 2

-}
next : Level -> Level
next level =
    level
        |> updateNumber
            ((level |> number) + 1)


{-| Go to the [previous](#previous) [Level](#Level).

Decrements the [Level](#Level) by 1.

    init
        |> goto 5
        |> previous
        |> number
        --> 4

Will not go below 1.

    init
        |> previous
        |> previous
        |> number
        --> 1

-}
previous : Level -> Level
previous level =
    level
        |> updateNumber
            ((level |> number) - 1)


{-| Go to the specified [Level](#Level) number.

    init
        |> goto 5
        |> number
        --> 5

Will not go below 1.

    init
        |> goto -5
        |> number
        --> 1

-}
goto : Int -> Level -> Level
goto num level =
    level
        |> updateNumber
            num



-- STRING CONVERSION


{-| Return the [Level](#Level) number as a String

    init
        |> toString
        --> "1"

-}
toString : Level -> String
toString (Level level) =
    level
        |> String.fromInt



-- UPDATING


updateNumber : Int -> Level -> Level
updateNumber num (Level level) =
    case num < 1 of
        True ->
            Level 1

        False ->
            Level num
