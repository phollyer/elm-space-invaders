module SpaceInvaders.Assets.Aliens exposing
    ( Aliens
    , init
    , TimeDelta, maybeMove, maybeChangeSpeed
    , Index, maybeNewLasers, loadLasers, maybeFire
    , aboveGround, anyLeft, boundingBox, remaining, Alien, list, Remaining, Row, Rows, rows
    , updateRows
    , playSFX, sfx
    , view
    )

{-| The [Aliens](#Aliens) attacking Earth.

They move left and right across the screen in unison, gradually getting closer
to the ground.

They drop intermittent [Lasers](SpaceInvaders.Assets.Lasers) on the
[Players](Shared.Players) [Ship](SpaceInvaders.Assets.Ship).

Their movement speeds up as their numbers reduce.

Their [Lasers](SpaceInvaders.Assets.Lasers) do damage to
[Bunkers](SpaceInvaders.Assets.Bunkers) and the
[Players](Shared.Players) ship.

They destroy [Bunkers](SpaceInvaders.Assets.Bunkers) as they collide with them.

They destroy the [Players](Shared.Players) ship as they collide with it.

The [Level](Shared.Level) is restarted, and a life is lost, if they reach the
ground.

@docs Aliens


# INITIALIZING

@docs init


# MOVING

@docs TimeDelta, maybeMove, maybeChangeSpeed


# FIRING

@docs Index, maybeNewLasers, loadLasers, maybeFire


# QUERYING

@docs aboveGround, anyLeft, boundingBox, remaining, Alien, list, Remaining, Row, Rows, rows


# UPDATING

@docs updateRows


# SOUND EFFECTS

@docs playSFX, sfx


# VIEWING

@docs view

-}

import Array exposing (Array)
import Random
import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Level as Level exposing (Difficulty(..), Level)
import Shared.Movement as Movement exposing (Direction)
import Shared.Point as Point exposing (Point)
import SpaceInvaders.Assets.Aliens.Animation as Animation exposing (Animation)
import SpaceInvaders.Configs as Configs
import SpaceInvaders.SFX exposing (Sound)
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, fill, height, transform, viewBox, width, x, y)



-- MODEL


{-| A type representing the [Aliens](#Aliens) attacking Earth.

This is an opaque type, use the exposed API to interact with it.

-}
type Aliens
    = Aliens
        { rows : Rows
        , state : State
        , laserIndices : List Int
        , maxLasers : Int
        , animation : Animation
        }


{-| A type alias representing an [Alien](#Alien).
-}
type alias Alien =
    { point : Point
    , boundingBox : BoundingBox
    , score : Int
    }


{-| A type alias representing the number of [Aliens](#Aliens) remaining.
-}
type alias Remaining =
    Int


{-| A type alias
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
representing the number of [Alien](#Alien)s [Remaining](#Remaining) in a row,
the containing [BoundingBox](Shared.BoundingBox#BoundingBox), and the list of
[Alien](#Alien)s that make up the [Row](#Row).
-}
type alias Row =
    ( ( Remaining, BoundingBox ), List Alien )


{-| A type alias
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
representing the total number of [Aliens](#Aliens) [Remaining](#Remaining), the
containing [BoundingBox](Shared.BoundingBox#BoundingBox), and the [Row](#Row)s
of [Aliens](#Aliens).
-}
type alias Rows =
    ( ( Remaining, BoundingBox ), List Row )


type State
    = Moved
    | WaitingToMove



-- INITIALIZING


{-| Initialize the [Aliens](#Aliens).

We need to know the [Level](Shared.Level#Level) and the
[Difficulty](Shared.Level#Difficulty) in order to set up the correct starting
velocity for the [Aliens](#Aliens).

-}
init : Level -> Difficulty -> Aliens
init level difficulty =
    let
        rows_ =
            initRows
    in
    Aliens
        { rows = rows_
        , state = WaitingToMove
        , laserIndices = []
        , maxLasers =
            difficulty
                |> calculateMaxLasers
                    level
        , animation =
            rows_
                |> Tuple.first
                |> Tuple.first
                |> Animation.init
                    level
                    difficulty
        }


config : Configs.Aliens
config =
    Configs.aliens


initRows : Rows
initRows =
    let
        rows_ =
            List.range 0 4
                |> List.map
                    initRow
                |> List.reverse
    in
    ( ( rows_
            |> List.foldl
                (\row acc ->
                    row
                        |> Tuple.first
                        |> Tuple.first
                        |> (+) acc
                )
                0
      , rows_
            |> List.map
                Tuple.first
            |> List.map
                Tuple.second
            |> BoundingBox.fromList
      )
    , rows_
    )


initRow : Int -> Row
initRow rowIndex =
    let
        aliens =
            List.range 0 9
                |> List.map
                    (initAlien rowIndex)
    in
    ( ( aliens
            |> List.length
      , aliens
            |> List.map
                .boundingBox
            |> BoundingBox.fromList
      )
    , aliens
    )


initAlien : Int -> Int -> Alien
initAlien y x =
    let
        point =
            Point.zero
                |> Point.moveToX
                    ((x |> toFloat) * config.spacing * 2)
                |> Point.moveToY
                    (((y |> toFloat) * config.spacing * 2)
                        + config.height
                        + Configs.mothership.height
                    )
    in
    { point = point
    , boundingBox =
        point
            |> BoundingBox.fromPoint
            |> BoundingBox.incrementRight
                config.width
            |> BoundingBox.incrementBottom
                config.height
    , score = 10
    }


calculateMaxLasers : Level -> Difficulty -> Int
calculateMaxLasers level difficulty =
    let
        maximum =
            case difficulty of
                Easy ->
                    level
                        |> Level.number

                Medium ->
                    level
                        |> Level.number
                        |> toFloat
                        |> (*) 1.25
                        |> round

                Hard ->
                    level
                        |> Level.number
                        |> toFloat
                        |> (*) 1.75
                        |> round
    in
    case maximum > 10 of
        True ->
            10

        False ->
            maximum



-- MOVING


{-| A type alias representing the amount of time passed since the last
animation frame.

This should be obtained from
[onAnimationFrameDelta](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrameDelta).

-}
type alias TimeDelta =
    Float


{-| Move the [Aliens](#Aliens) if it's time to move.

If more than the required time has passed since the last move, then the
[Aliens](#Aliens) will move. If not, they won't.

-}
maybeMove : TimeDelta -> Aliens -> Aliens
maybeMove timeDelta aliens =
    case aliens |> animation |> Animation.timeToMove timeDelta of
        True ->
            let
                direction_ =
                    aliens
                        |> selectDirection
            in
            aliens
                |> updateState
                    Moved
                |> updateRows
                    (aliens
                        |> rows
                        |> moveRows
                            direction_
                    )
                |> updateAnimation
                    (aliens
                        |> animation
                        |> Animation.move
                            direction_
                    )

        False ->
            aliens
                |> updateState
                    WaitingToMove
                |> updateAnimation
                    (aliens
                        |> animation
                        |> Animation.addTimeDelta
                            timeDelta
                    )


selectDirection : Aliens -> Direction
selectDirection aliens =
    case aliens |> atABoundary of
        True ->
            aliens
                |> selectDirectionByBoundary

        False ->
            aliens
                |> animation
                |> Animation.selectHorizontal


atABoundary : Aliens -> Bool
atABoundary aliens =
    (aliens |> reachedLeftBoundary) || (aliens |> reachedRightBoundary)


type Boundary
    = LeftSide
    | RightSide


selectDirectionByBoundary : Aliens -> Direction
selectDirectionByBoundary aliens =
    case aliens |> whichBoundary of
        LeftSide ->
            aliens
                |> animation
                |> Animation.selectRightOrDown

        RightSide ->
            aliens
                |> animation
                |> Animation.selectLeftOrDown


whichBoundary : Aliens -> Boundary
whichBoundary aliens =
    case aliens |> reachedRightBoundary of
        True ->
            RightSide

        False ->
            LeftSide


reachedLeftBoundary : Aliens -> Bool
reachedLeftBoundary aliens =
    (aliens |> boundingBox |> BoundingBox.left) <= Configs.view.boundaryLeft


reachedRightBoundary : Aliens -> Bool
reachedRightBoundary aliens =
    (aliens |> boundingBox |> BoundingBox.right) >= Configs.view.boundaryRight


moveRows : Direction -> Rows -> Rows
moveRows direction_ rows_ =
    ( ( rows_
            |> Tuple.first
            |> Tuple.first
      , rows_
            |> Tuple.first
            |> Tuple.second
            |> Movement.moveBox
                direction_
      )
    , rows_
        |> Tuple.second
        |> List.map
            (moveRow direction_)
    )


moveRow : Direction -> Row -> Row
moveRow direction_ row =
    ( ( row
            |> Tuple.first
            |> Tuple.first
      , row
            |> Tuple.first
            |> Tuple.second
            |> Movement.moveBox
                direction_
      )
    , row
        |> Tuple.second
        |> List.map
            (Movement.move direction_)
    )


{-| Maybe change the speed of the [Aliens](#Aliens).

As their numbers decrease, they need to speed up their movement. This function
checks to see if there is a need to increase the speed, and does so if
required.

_(Fun Fact: In the original arcade machine, the reason the Aliens speed
increased as their numbers reduced, was due to the limitations of the hardware.
It would run slower when rendering more aliens, and faster as their numbers
fell. Or so I was told by a friend in the pub ;-) )_

-}
maybeChangeSpeed : Aliens -> Aliens
maybeChangeSpeed aliens =
    case aliens |> needToChangeSpeed of
        True ->
            aliens
                |> changeSpeed

        False ->
            aliens


needToChangeSpeed : Aliens -> Bool
needToChangeSpeed aliens =
    aliens
        |> animation
        |> Animation.needToChangeSpeed
            (aliens
                |> remaining
            )


changeSpeed : Aliens -> Aliens
changeSpeed aliens =
    aliens
        |> updateAnimation
            (aliens
                |> animation
                |> Animation.changeSpeed
                    (aliens
                        |> remaining
                    )
            )



-- FIRING


{-| A type alias representing the index for an [Alien](#Alien).
-}
type alias Index =
    Int


{-| Maybe select a
[Random](https://package.elm-lang.org/packages/elm/random/latest/) number of
[Alien](#Alien)s to fire [Laser](Shared.Lasers#Laser)s from.

The number of new [Laser](SpaceInvaders.Assets.Laser#Laser)s, if any, is
determined by the number of [Laser](SpaceInvaders.Assets.Laser#Laser)s
currently active, the current [Level](Shared.Level#Level) and the selected
[Difficulty](Shared.Level#Difficulty).

The [Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
will produce a [Msg](SpaceInvaders.Logic#Msg) with the [indices](#Index) of the
selected [Alien](#Alien)s, or `Cmd.none` if no new
[Laser](SpaceInvaders.Assets.Laser#Laser)s were needed.

-}
maybeNewLasers : (List Index -> msg) -> List a -> Aliens -> Cmd msg
maybeNewLasers msg currentLasers aliens =
    let
        currentTotal =
            currentLasers
                |> List.length
    in
    case aliens |> newLasersRequired currentTotal of
        True ->
            Random.generate
                msg
                (indicesGenerator
                    ((aliens |> maxLasers) - currentTotal)
                    (aliens
                        |> remaining
                    )
                )

        False ->
            Cmd.none


indicesGenerator : Int -> Int -> Random.Generator (List Int)
indicesGenerator max remaining_ =
    Random.int
        0
        max
        |> Random.andThen
            (remaining_
                |> selectIndices
            )


selectIndices : Int -> Int -> Random.Generator (List Int)
selectIndices remaining_ total =
    remaining_
        |> Random.int
            0
        |> Random.list
            total


newLasersRequired : Int -> Aliens -> Bool
newLasersRequired currentTotal aliens =
    currentTotal < (aliens |> maxLasers)


{-| Store the [indices](#Index) of the [Aliens](#Aliens) that were selected by
[maybeNewLasers](#maybeNewLasers).
-}
loadLasers : List Index -> Aliens -> Aliens
loadLasers indices aliens =
    aliens
        |> updateLaserIndices
            indices


{-| [Aliens](#Aliens) are only allowed to fire
[Lasers](SpaceInvaders.Assets.Lasers#Lasers) as they [move](#move) so that the
[Lasers](SpaceInvaders.Assets.Lasers#Lasers) are released from the
[Aliens](#Aliens) new position.

This ensures the [Lasers](SpaceInvaders.Assets.Lasers#Lasers) are synched to
the position of the [Aliens](#Aliens), rather than, intermittently, falling
from an [Alien](#Alien) that has just moved i.e. empty space.

If the [Aliens](#Aliens) are allowed to fire, return a tuple where the
first element is a list of [Point](Shared.Point#Point)s selected to fire from,
and the second element is the [Aliens](#Aliens) updated to reflect they have
fired.

If the [Aliens](#Aliens) are not allowed to fire, return a tuple where the
first element is an empty list, and the second element is the [Aliens](#Aliens)
unchanged.

-}
maybeFire : Aliens -> ( List Point, Aliens )
maybeFire aliens =
    case aliens |> state of
        Moved ->
            ( aliens
                |> list
                |> positionLasers
                    (aliens
                        |> laserIndices
                    )
            , aliens
                |> emptyLasers
            )

        WaitingToMove ->
            ( []
            , aliens
            )


positionLasers : List Int -> List Alien -> List Point
positionLasers list_ aliens =
    case list_ of
        [] ->
            []

        _ ->
            let
                aliensArray =
                    aliens
                        |> Array.fromList
            in
            list_
                |> List.filterMap
                    (positionLaser aliensArray)


positionLaser : Array Alien -> Int -> Maybe Point
positionLaser aliens index =
    case aliens |> Array.get index of
        Just alien ->
            alien.boundingBox
                |> BoundingBox.center
                |> Just

        Nothing ->
            Nothing


emptyLasers : Aliens -> Aliens
emptyLasers aliens =
    aliens
        |> updateLaserIndices
            []



-- QUERYING


{-| -}
anyLeft : Aliens -> Bool
anyLeft aliens =
    (aliens |> remaining) > 0


{-| -}
aboveGround : Aliens -> Bool
aboveGround aliens =
    (aliens |> boundingBox |> BoundingBox.bottom) < Configs.ground.y


{-| -}
list : Aliens -> List Alien
list aliens =
    aliens
        |> rows
        |> Tuple.second
        |> List.map
            Tuple.second
        |> List.concat


{-| -}
boundingBox : Aliens -> BoundingBox
boundingBox aliens =
    aliens
        |> rows
        |> Tuple.first
        |> Tuple.second


{-| -}
remaining : Aliens -> Int
remaining aliens =
    aliens
        |> rows
        |> Tuple.first
        |> Tuple.first


{-| -}
rows : Aliens -> Rows
rows (Aliens aliens) =
    aliens.rows


animation : Aliens -> Animation
animation (Aliens aliens) =
    aliens.animation


laserIndices : Aliens -> List Int
laserIndices (Aliens aliens) =
    aliens.laserIndices


maxLasers : Aliens -> Int
maxLasers (Aliens aliens) =
    aliens.maxLasers


state : Aliens -> State
state (Aliens aliens) =
    aliens.state



-- UPDATING


{-| -}
updateRows : Rows -> Aliens -> Aliens
updateRows rows_ (Aliens aliens) =
    Aliens
        { aliens
            | rows = rows_
        }


updateAnimation : Animation -> Aliens -> Aliens
updateAnimation animation_ (Aliens aliens) =
    Aliens
        { aliens
            | animation = animation_
        }


updateLaserIndices : List Int -> Aliens -> Aliens
updateLaserIndices indices (Aliens aliens) =
    Aliens
        { aliens
            | laserIndices = indices
        }


updateState : State -> Aliens -> Aliens
updateState state_ (Aliens aliens) =
    Aliens
        { aliens
            | state = state_
        }



-- SOUND EFFECTS


{-| Flag to determine whether to play a sound effect as the [Aliens](#Aliens)
[move](#move).
-}
playSFX : Aliens -> Bool
playSFX aliens =
    (aliens |> state) == Moved


{-| The sound effect to play.
-}
sfx : Aliens -> Sound
sfx aliens =
    aliens
        |> animation
        |> Animation.createSFX



-- VIEWING


{-| Credit to:
<https://github.com/gege251/space_invaders> for the
[SVG](https://package.elm-lang.org/packages/elm/svg/latest/) data.
-}
view : Aliens -> List (Svg msg)
view aliens =
    aliens
        |> list
        |> List.map
            viewAlien


viewAlien : Alien -> Svg msg
viewAlien alien =
    svg
        [ viewBox config.viewBox
        , fill "red"
        , width
            (config.width
                |> String.fromFloat
            )
        , height
            (config.height
                |> String.fromFloat
            )
        , x
            (alien.point
                |> Point.xToString
            )
        , y
            (alien.point
                |> Point.yToString
            )
        ]
        [ g
            [ transform "translate(0,511) scale(0.1,-0.1)"
            ]
            [ path [ d "M2055.4,4285.3v-483.1h483.1h483.1v483.1v483.1h-483.1h-483.1V4285.3z" ] []
            , path [ d "M6978.4,4285.3v-483.1h483.1h483.1v483.1v483.1h-483.1h-483.1V4285.3z" ] []
            , path [ d "M3044.6,3284.6V2790H2550h-494.6v-483.1v-483.1h-483.1h-483.1v-494.6V834.6H594.6H100v-2196.9v-2196.9h483.1h483.1v977.7v977.7h253.1h253.1v-977.7v-977.7h483.1h483.1v483.1v483.1H5000h2461.5v-483.1v-483.1h483.1h483.1v977.7v977.7h253.1h253v-977.7v-977.7h483.1H9900v2196.9V834.6h-494.6h-494.6v494.6v494.6h-483.1h-483.1v483.1V2790H7450h-494.6v494.6v494.6h-483.1h-483.1v-494.6V2790H5000h-989.2v494.6v494.6h-483.1h-483.1V3284.6z M4033.8-143.1v-494.6h-506.1h-506.1v494.6v494.6h506.1h506.1V-143.1z M6978.4-143.1v-494.6h-506.1h-506.1v494.6v494.6h506.1h506.1V-143.1z" ] []
            , path [ d "M2561.5-4065.4v-483.1h966.2h966.2v483.1v483.1h-966.2h-966.2V-4065.4z" ] []
            , path [ d "M5506.1-4065.4v-483.1h966.2h966.2v483.1v483.1h-966.2h-966.2V-4065.4z" ] []
            ]
        ]
