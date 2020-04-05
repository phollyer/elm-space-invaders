port module SpaceInvaders.Cache exposing (saveHighscores)

{-| A cache for saving game data to local storage. Currently only saves game
highscores.

@docs saveHighscores

-}


{-| -}
port saveHighscores : List { name : String, points : Int } -> Cmd msg
