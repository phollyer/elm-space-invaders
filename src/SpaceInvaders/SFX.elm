port module SpaceInvaders.SFX exposing
    ( SFX
    , init
    , Sound(..), Status(..), add
    , execute
    )

{-| The sound effects for the game use [HowlerJS](https://howlerjs.com) via
ports.

@docs SFX


# INITIALIZING

@docs init


# ADDING

@docs Sound, Status, add


# PLAYING

@docs execute

-}

-- MODEL


{-| A type representing all the sound effects to play.

This is an opaque type, use the exposed API to interact with it.

-}
type SFX
    = SFX { list : List Sound }



-- INITIALIZING


{-| Initialize an empty list of sounds.
-}
init : SFX
init =
    SFX
        { list = [] }



-- ADDING


{-| A type representing all the possible sound effects that can be played.
-}
type Sound
    = NoSound
    | Fire
    | HitAlien
    | HitBunker
    | HitMothership
    | HitPlayer
    | Alien String Int
    | Mothership Status


{-| A type representing whether to play or stop a sound effect.

Currently, this only applies to the `Mothership` [Sound](#Sound) which loops
continuously once started, so we need a way to stop it when required.

-}
type Status
    = Play
    | Stop


{-| Add a new sound to be played, or, in the case of `Mothership`, stopped.
-}
add : Sound -> SFX -> SFX
add sound sfx =
    sfx
        |> updateList
            (sound
                :: (sfx
                        |> list
                   )
            )



-- PLAYING


{-| Play all the [add](#add)ed sound effects.

This sends out the list of sounds through the port to `JS` where
[HowlerJS](https://howlerjs.com) can take over and play them.

-}
execute : SFX -> Cmd msg
execute sfx =
    sfx
        |> list
        |> List.map
            selectCmd
        |> Cmd.batch


selectCmd : Sound -> Cmd msg
selectCmd sound =
    case sound of
        NoSound ->
            Cmd.none

        Mothership Stop ->
            sound
                |> toString
                |> stopSFX

        _ ->
            sound
                |> toString
                |> playSFX


port playSFX : String -> Cmd msg


port stopSFX : String -> Cmd msg


toString : Sound -> String
toString sound =
    case sound of
        NoSound ->
            ""

        Fire ->
            "fire"

        HitAlien ->
            "hitAlien"

        HitBunker ->
            "hitBunker"

        HitMothership ->
            "hitMothership"

        HitPlayer ->
            "hitPlayer"

        Mothership _ ->
            "mothership"

        Alien step num ->
            "step"
                ++ step
                ++ (num
                        |> String.fromInt
                   )



-- QUERYING


list : SFX -> List Sound
list (SFX sfx) =
    sfx.list



-- UPDATING


updateList : List Sound -> SFX -> SFX
updateList list_ (SFX sfx) =
    SFX
        { sfx
            | list = list_
        }
