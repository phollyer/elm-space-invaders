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
        , maxLasers = calculateMaxLasers level difficulty
        , animation =
            rows_
                |> Tuple.first
                |> Tuple.first
                |> Animation.init level difficulty
        }


config : Configs.Aliens
config =
    Configs.aliens


initRows : Rows
initRows =
    let
        rows_ =
            List.range 0 4
                |> List.map initRow
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
            |> List.map Tuple.first
            |> List.map Tuple.second
            |> BoundingBox.fromList
      )
    , rows_
    )


initRow : Int -> Row
initRow rowIndex =
    let
        aliens =
            List.range 0 9
                |> List.map (initAlien rowIndex)
    in
    ( ( List.length aliens
      , aliens
            |> List.map .boundingBox
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
                    (toFloat x * config.spacing * 2)
                |> Point.moveToY
                    ((toFloat y * config.spacing * 2)
                        + config.height
                        + Configs.mothership.height
                    )
    in
    { point = point
    , boundingBox =
        point
            |> BoundingBox.fromPoint
            |> BoundingBox.incrementRight config.width
            |> BoundingBox.incrementBottom config.height
    , score = 10
    }


calculateMaxLasers : Level -> Difficulty -> Int
calculateMaxLasers level difficulty =
    let
        maximum =
            case difficulty of
                Easy ->
                    Level.number level

                Medium ->
                    Level.number level
                        |> toFloat
                        |> (*) 1.25
                        |> round

                Hard ->
                    Level.number level
                        |> toFloat
                        |> (*) 1.75
                        |> round
    in
    if maximum > 10 then
        10

    else
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
    if Animation.timeToMove timeDelta (animation aliens) then
        let
            direction_ =
                selectDirection aliens
        in
        aliens
            |> updateState Moved
            |> updateRows
                (rows aliens
                    |> moveRows direction_
                )
            |> updateAnimation
                (animation aliens
                    |> Animation.move direction_
                )

    else
        aliens
            |> updateState WaitingToMove
            |> updateAnimation
                (animation aliens
                    |> Animation.addTimeDelta timeDelta
                )


selectDirection : Aliens -> Direction
selectDirection aliens =
    if atABoundary aliens then
        selectDirectionByBoundary aliens

    else
        animation aliens
            |> Animation.selectHorizontal


atABoundary : Aliens -> Bool
atABoundary aliens =
    reachedLeftBoundary aliens || reachedRightBoundary aliens


type Boundary
    = LeftSide
    | RightSide


selectDirectionByBoundary : Aliens -> Direction
selectDirectionByBoundary aliens =
    case whichBoundary aliens of
        LeftSide ->
            animation aliens
                |> Animation.selectRightOrDown

        RightSide ->
            animation aliens
                |> Animation.selectLeftOrDown


whichBoundary : Aliens -> Boundary
whichBoundary aliens =
    if reachedRightBoundary aliens then
        RightSide

    else
        LeftSide


reachedLeftBoundary : Aliens -> Bool
reachedLeftBoundary aliens =
    (boundingBox aliens |> BoundingBox.left) <= Configs.view.boundaryLeft


reachedRightBoundary : Aliens -> Bool
reachedRightBoundary aliens =
    (boundingBox aliens |> BoundingBox.right) >= Configs.view.boundaryRight


moveRows : Direction -> Rows -> Rows
moveRows direction_ rows_ =
    ( ( rows_
            |> Tuple.first
            |> Tuple.first
      , rows_
            |> Tuple.first
            |> Tuple.second
            |> Movement.moveBox direction_
      )
    , rows_
        |> Tuple.second
        |> List.map (moveRow direction_)
    )


moveRow : Direction -> Row -> Row
moveRow direction_ row =
    ( ( row
            |> Tuple.first
            |> Tuple.first
      , row
            |> Tuple.first
            |> Tuple.second
            |> Movement.moveBox direction_
      )
    , row
        |> Tuple.second
        |> List.map (Movement.move direction_)
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
    if needToChangeSpeed aliens then
        changeSpeed aliens

    else
        aliens


needToChangeSpeed : Aliens -> Bool
needToChangeSpeed aliens =
    aliens
        |> animation
        |> Animation.needToChangeSpeed (remaining aliens)


changeSpeed : Aliens -> Aliens
changeSpeed aliens =
    updateAnimation
        (animation aliens
            |> Animation.changeSpeed (remaining aliens)
        )
        aliens



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
            List.length currentLasers
    in
    if newLasersRequired currentTotal aliens then
        Random.generate
            msg
            (indicesGenerator
                (maxLasers aliens - currentTotal)
                (remaining aliens)
            )

    else
        Cmd.none


indicesGenerator : Int -> Int -> Random.Generator (List Int)
indicesGenerator max remaining_ =
    Random.int 0 max
        |> Random.andThen (selectIndices remaining_)


selectIndices : Int -> Int -> Random.Generator (List Int)
selectIndices remaining_ total =
    Random.int 0 remaining_
        |> Random.list total


newLasersRequired : Int -> Aliens -> Bool
newLasersRequired currentTotal aliens =
    currentTotal < maxLasers aliens


{-| Store the [indices](#Index) of the [Aliens](#Aliens) that were selected by
[maybeNewLasers](#maybeNewLasers).
-}
loadLasers : List Index -> Aliens -> Aliens
loadLasers =
    updateLaserIndices


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
    case state aliens of
        Moved ->
            ( positionLasers (laserIndices aliens) (list aliens)
            , updateLaserIndices [] aliens
            )

        WaitingToMove ->
            ( []
            , aliens
            )


positionLasers : List Int -> List Alien -> List Point
positionLasers list_ aliens =
    List.filterMap (positionLaser (Array.fromList aliens)) list_


positionLaser : Array Alien -> Int -> Maybe Point
positionLaser aliens index =
    Array.get index aliens
        |> Maybe.map (\alien -> BoundingBox.center alien.boundingBox)



-- QUERYING


{-| -}
anyLeft : Aliens -> Bool
anyLeft aliens =
    remaining aliens > 0


{-| -}
aboveGround : Aliens -> Bool
aboveGround aliens =
    BoundingBox.bottom (boundingBox aliens) < Configs.ground.y


{-| -}
list : Aliens -> List Alien
list =
    rows
        >> Tuple.second
        >> List.map Tuple.second
        >> List.concat


{-| -}
boundingBox : Aliens -> BoundingBox
boundingBox =
    rows
        >> Tuple.first
        >> Tuple.second


{-| -}
remaining : Aliens -> Int
remaining =
    rows
        >> Tuple.first
        >> Tuple.first


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
        { aliens | rows = rows_ }


updateAnimation : Animation -> Aliens -> Aliens
updateAnimation animation_ (Aliens aliens) =
    Aliens
        { aliens | animation = animation_ }


updateLaserIndices : List Int -> Aliens -> Aliens
updateLaserIndices indices (Aliens aliens) =
    Aliens
        { aliens | laserIndices = indices }


updateState : State -> Aliens -> Aliens
updateState state_ (Aliens aliens) =
    Aliens
        { aliens | state = state_ }



-- SOUND EFFECTS


{-| Flag to determine whether to play a sound effect as the [Aliens](#Aliens)
[move](#move).
-}
playSFX : Aliens -> Bool
playSFX aliens =
    state aliens == Moved


{-| The sound effect to play.
-}
sfx : Aliens -> Sound
sfx =
    animation >> Animation.createSFX



-- VIEWING


{-| Credit to:
<https://github.com/gege251/space_invaders> for the
[SVG](https://package.elm-lang.org/packages/elm/svg/latest/) data.
-}
view : Aliens -> List (Svg msg)
view =
    list >> List.map viewAlien


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
