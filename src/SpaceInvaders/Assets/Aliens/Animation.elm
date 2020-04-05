module SpaceInvaders.Assets.Aliens.Animation exposing
    ( Animation
    , Remaining, init
    , move, TimeDelta, timeToMove, addTimeDelta, needToChangeSpeed, changeSpeed
    , selectHorizontal, selectLeftOrDown, selectRightOrDown
    , createSFX
    )

{-| Animate the [Aliens](SpaceInvaders.Assets.Aliens) left, right and down the
screen.

This module does not perform any rendering. It simply handles the logic
neccessary to determine if/when the [Aliens](SpaceInvaders.Assets.Aliens)
should move, and by how much.

@docs Animation


# INITIALIZING

@docs Remaining, init


# MOVING

@docs move, TimeDelta, timeToMove, addTimeDelta, needToChangeSpeed, changeSpeed


# DIRECTION

@docs selectHorizontal, selectLeftOrDown, selectRightOrDown


# SOUND EFFECTS

@docs createSFX

-}

import Dict exposing (Dict)
import Shared.Level as Level exposing (Difficulty(..), Level(..))
import Shared.Movement as Movement exposing (Direction)
import SpaceInvaders.Configs as Configs
import SpaceInvaders.SFX as SFX exposing (Sound)



-- MODEL


{-| A type representing the data required to move the
[Aliens](SpaceInvaders.Assets.Aliens#Aliens) across and down the screen.

This is an opaque type, use the exposed API to interact with it.

-}
type Animation
    = Animation
        { timeSinceLastFrame : Float
        , velocity : Speed
        , config : Dict Int Speed
        , nextSpeedChange : Int
        , currentDirection : Direction
        , previousDirection : Direction
        , sfx : SFX
        , sfxIndex : Int
        }


type alias Speed =
    { distance : Float
    , time : Float
    }


{-| Type to portray which sound effect to play when the Aliens [move](#move).

Refers to the "Left, Right, Left, Right" of a march of soldiers, not the
direction of travel.

-}
type SFX
    = Left
    | Right



-- INITIALIZING


{-| A type alias representing the number of
[Aliens](SpaceInvaders.Assets.Aliens#Aliens) remaining.
-}
type alias Remaining =
    Int


{-| Initialize the [Animation](#animation) based on the
[Level](Shared.Level#Level), the [Difficulty](Shared.Level#Difficulty) and the
number of [Aliens](SpaceInvaders.Assets.Aliens#Aliens) [Remaining](#Remaining).
-}
init : Level -> Difficulty -> Remaining -> Animation
init level difficulty total =
    let
        config_ =
            difficulty
                |> initConfig
                    level
    in
    Animation
        { config = config_
        , timeSinceLastFrame = 0
        , currentDirection = Movement.Down configs.stepDown
        , previousDirection = Movement.Left 10
        , sfx = Left
        , sfxIndex = 1
        , velocity =
            config_
                |> initSpeed
                    total
        , nextSpeedChange =
            total
                |> getNextSpeedChange
        }


configs : Configs.Aliens
configs =
    Configs.aliens


initSpeed : Int -> Dict Int Speed -> Speed
initSpeed total config_ =
    config_
        |> Dict.get
            total
        |> Maybe.withDefault
            { distance = 0
            , time = 0
            }


getNextSpeedChange : Int -> Int
getNextSpeedChange current =
    [ 40, 30, 20, 10, 5, 3, 1 ]
        |> List.filter
            (\i -> i < current)
        |> List.head
        |> Maybe.withDefault
            40


initConfig : Level -> Difficulty -> Dict Int Speed
initConfig level difficulty =
    let
        levelNumber =
            level
                |> Level.number
                |> toFloat

        ( time_, divisor ) =
            case difficulty of
                Easy ->
                    ( 10, 1 )

                Medium ->
                    ( 50, 1.25 )

                Hard ->
                    ( 100, 1.5 )

        pauseTime =
            1000 - (levelNumber * time_)

        time =
            case pauseTime < 10 of
                True ->
                    10

                False ->
                    pauseTime
    in
    Dict.empty
        |> Dict.insert
            50
            { distance = 10
            , time = time / divisor
            }
        |> Dict.insert
            40
            { distance = 8
            , time = time / (divisor * 1.25)
            }
        |> Dict.insert
            30
            { distance = 7
            , time = time / (divisor * 2.5)
            }
        |> Dict.insert
            20
            { distance = 6
            , time = time / (divisor * 5)
            }
        |> Dict.insert
            10
            { distance = 5
            , time = time / (divisor * 10)
            }
        |> Dict.insert
            5
            { distance = 6
            , time = time / (divisor * 20)
            }
        |> Dict.insert
            3
            { distance = 6
            , time = time / (divisor * 40)
            }
        |> Dict.insert
            1
            { distance = 7
            , time = time / (divisor * 40)
            }



-- MOVING


{-| Update the internal state to represent a [move](#move) in the specified
[Direction](Shared.Movement#Direction).
-}
move : Direction -> Animation -> Animation
move direction animation =
    animation
        |> updateCurrentDirection
            direction
        |> updatePreviousDirection
            (animation
                |> currentDirection
            )
        |> updateSFXStep
            (animation
                |> toggleSFX
            )
        |> updateTimeSinceLastFrame
            0


{-| A type alias representing the amount of time passed since the last
animation frame.

This should be obtained from
[onAnimationFrameDelta](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrameDelta).

-}
type alias TimeDelta =
    Float


{-| Determine if it is time for the
[Aliens](SpaceInvaders.Assets.Aliens#Aliens) to move yet.
-}
timeToMove : TimeDelta -> Animation -> Bool
timeToMove delta animation =
    ((animation |> timeSinceLastFrame) + delta) >= (animation |> velocity |> .time)


{-| It's not time for the [Aliens](SpaceInvaders.Assets.Aliens#Aliens) to move
yet, so add the [TimeDelta](#TimeDelta) to the current running total.
-}
addTimeDelta : TimeDelta -> Animation -> Animation
addTimeDelta delta animation =
    animation
        |> updateTimeSinceLastFrame
            ((animation |> timeSinceLastFrame) + delta)


{-| Determine it there is a need to change the current [Speed](#Speed).
-}
needToChangeSpeed : Remaining -> Animation -> Bool
needToChangeSpeed remaining animation =
    (animation |> nextSpeedChange) == remaining


{-| Change the current [Speed](#Speed) based on the number of
[Aliens](SpaceInvaders.Assets.Aliens#Aliens) [Remaining](#Remaining).
-}
changeSpeed : Remaining -> Animation -> Animation
changeSpeed remaining animation =
    animation
        |> updateSpeed
            (animation
                |> getSpeed
                    remaining
            )
        |> updateNextSpeedChange
            (animation
                |> nextSpeedChange
                |> getNextSpeedChange
            )
        |> updateSFXIndex
            (animation
                |> sfxIndex
                |> (+) 1
            )


getSpeed : Int -> Animation -> Speed
getSpeed remaining_ animation =
    animation
        |> config
        |> Dict.get
            remaining_
        |> Maybe.withDefault
            { distance = 0
            , time = 0
            }



-- DIRECTION


{-| This function will decide whether the
[Aliens](SpaceInvaders.Assets.Aliens#Aliens) will move
[Left](Shared.Movement#Direction) or [Right](Shared.Movement#Direction).

If they have just moved [Down](Shared.Movement#Direction) due to reaching a
boundary, then we need to determine the previous
[Direction](Shared.Movement#Direction) of travel and then [move](#move) in the
opposite [Direction](Shared.Movement#Direction).

-}
selectHorizontal : Animation -> Direction
selectHorizontal animation =
    case animation |> currentDirection of
        Movement.Down _ ->
            case animation |> previousDirection of
                Movement.Left distance ->
                    Movement.Right distance

                Movement.Right distance ->
                    Movement.Left distance

                _ ->
                    Movement.Stop

        _ ->
            animation
                |> currentDirection


{-| Select a [Direction](Shared.Movement#Direction) of travel.

This function will decide whether the
[Aliens](SpaceInvaders.Assets.Aliens#Aliens) will move
[Down](Shared.Movement#Direction) because they have just reached the right hand
boundary, or [Left](Shared.Movement#Direction) after moving
[Down](Shared.Movement#Direction).

-}
selectLeftOrDown : Animation -> Direction
selectLeftOrDown animation =
    case animation |> currentDirection of
        Movement.Down _ ->
            Movement.Left
                (animation
                    |> velocity
                    |> .distance
                )

        _ ->
            Movement.Down configs.stepDown


{-| Select a [Direction](Shared.Movement#Direction) of travel.

This function will decide whether the
[Aliens](SpaceInvaders.Assets.Aliens#Aliens) will move
[Down](Shared.Movement#Direction) because they have just reached the left hand
boundary, or [Right](Shared.Movement#Direction) after moving
[Down](Shared.Movement#Direction).

-}
selectRightOrDown : Animation -> Direction
selectRightOrDown animation =
    case animation |> currentDirection of
        Movement.Down _ ->
            Movement.Right
                (animation
                    |> velocity
                    |> .distance
                )

        _ ->
            Movement.Down configs.stepDown



-- SFX


{-| -}
createSFX : Animation -> Sound
createSFX animation =
    SFX.Alien
        (animation
            |> sfxToString
        )
        (animation
            |> sfxIndex
        )


toggleSFX : Animation -> SFX
toggleSFX animation =
    case animation |> sfx of
        Left ->
            Right

        Right ->
            Left


sfxToString : Animation -> String
sfxToString animation =
    case animation |> sfx of
        Left ->
            "Left"

        Right ->
            "Right"



-- QUERYING


config : Animation -> Dict Int Speed
config (Animation animation) =
    animation.config


currentDirection : Animation -> Direction
currentDirection (Animation animation) =
    animation.currentDirection


nextSpeedChange : Animation -> Int
nextSpeedChange (Animation animation) =
    animation.nextSpeedChange


previousDirection : Animation -> Direction
previousDirection (Animation animation) =
    animation.previousDirection


sfx : Animation -> SFX
sfx (Animation animation) =
    animation.sfx


sfxIndex : Animation -> Int
sfxIndex (Animation animation) =
    animation.sfxIndex


timeSinceLastFrame : Animation -> Float
timeSinceLastFrame (Animation animation) =
    animation.timeSinceLastFrame


velocity : Animation -> Speed
velocity (Animation animation) =
    animation.velocity



-- UPDATING


updateTimeSinceLastFrame : Float -> Animation -> Animation
updateTimeSinceLastFrame time (Animation animation) =
    Animation
        { animation
            | timeSinceLastFrame = time
        }


updateCurrentDirection : Direction -> Animation -> Animation
updateCurrentDirection direction (Animation animation) =
    Animation
        { animation
            | currentDirection = direction
        }


updatePreviousDirection : Direction -> Animation -> Animation
updatePreviousDirection direction (Animation animation) =
    Animation
        { animation
            | previousDirection = direction
        }


updateSFXStep : SFX -> Animation -> Animation
updateSFXStep sfx_ (Animation animation) =
    Animation
        { animation
            | sfx = sfx_
        }


updateSFXIndex : Int -> Animation -> Animation
updateSFXIndex index (Animation animation) =
    Animation
        { animation
            | sfxIndex =
                case index > 4 of
                    True ->
                        4

                    False ->
                        index
        }


updateSpeed : Speed -> Animation -> Animation
updateSpeed velocity_ (Animation animation) =
    Animation
        { animation
            | velocity = velocity_
        }


updateNextSpeedChange : Int -> Animation -> Animation
updateNextSpeedChange num (Animation animation) =
    Animation
        { animation
            | nextSpeedChange = num
        }
