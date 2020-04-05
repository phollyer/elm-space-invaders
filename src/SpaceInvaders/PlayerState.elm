module SpaceInvaders.PlayerState exposing
    ( State
    , init
    , store
    , restore
    )

{-| This module is for storing [Players](Shared.Players) Game [State](#State)
between turns.

It is mainly used by the [Players](Shared.Players) module.

@docs State


# INITIALIZING

@docs init


# STORING

@docs store


# RETRIEVING

@docs restore

-}

import Shared.Level as Level exposing (Difficulty(..), Level(..))
import SpaceInvaders.Assets.Aliens as Aliens exposing (Aliens)
import SpaceInvaders.Assets.Bunkers as Bunkers exposing (Bunkers)
import SpaceInvaders.Assets.Laser as Laser exposing (Laser)
import SpaceInvaders.Assets.Lasers as Lasers exposing (Lasers)
import SpaceInvaders.Assets.Mothership as Mothership exposing (Mothership)



-- Model


{-| A type alias representing the Game [State](#State) for a
[Player](Shared.Players#Player).
-}
type alias State =
    { aliens : Aliens
    , alienLasers : Lasers
    , mothership : Mothership
    , mothershipLasers : Lasers
    , bunkers : Bunkers
    , laser : Laser
    , level : Level
    }


{-| Initialize the starting state.
-}
init : Difficulty -> State
init difficulty =
    { bunkers = Bunkers.init
    , aliens = Aliens.init Level.init difficulty
    , alienLasers = Lasers.init
    , mothership = Mothership.init
    , mothershipLasers = Lasers.init
    , laser = Laser.init "yellow" 10
    , level = Level.init
    }


{-| Store the [Player](Shared.Players#Player)s [State](#State) between turns.
-}
store :
    { a
        | aliens : Aliens
        , alienLasers : Lasers
        , mothership : Mothership
        , mothershipLasers : Lasers
        , bunkers : Bunkers
        , laser : Laser
        , level : Level
    }
    -> State
store game =
    { bunkers = game.bunkers
    , aliens = game.aliens
    , alienLasers = game.alienLasers
    , mothership = game.mothership
    , mothershipLasers = game.mothershipLasers
    , laser = game.laser
    , level = game.level
    }


{-| Retrieve the [Player](Shared.Players#Player)s [State](#State).
-}
restore :
    State
    ->
        { a
            | aliens : Aliens
            , alienLasers : Lasers
            , mothership : Mothership
            , mothershipLasers : Lasers
            , bunkers : Bunkers
            , laser : Laser
            , level : Level
        }
    ->
        { a
            | aliens : Aliens
            , alienLasers : Lasers
            , mothership : Mothership
            , mothershipLasers : Lasers
            , bunkers : Bunkers
            , laser : Laser
            , level : Level
        }
restore state gameData =
    { gameData
        | bunkers = state.bunkers
        , aliens = state.aliens
        , alienLasers = state.alienLasers
        , mothership = state.mothership
        , mothershipLasers = state.mothershipLasers
        , laser = state.laser
        , level = state.level
    }
