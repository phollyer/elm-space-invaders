module SpaceInvaders.Assets.Mothership exposing
    ( Mothership
    , init
    , AliensRemaining, NextDueIn, maybeOrder, nextDueIn, send
    , maybeMove, isMoving
    , LaserXPosition, newLasers, loadLasers, maybeFire
    , maybePlaySFX
    , subscriptions
    , boundingBox
    , view
    )

{-| A [Mothership](#Mothership) travels across the top of the screen,
intermittently. Dropping one or more [Laser](Shared.Laser#Laser)s as it goes.

@docs Mothership


# INITIALIZING

@docs init


# ORDERING

@docs AliensRemaining, NextDueIn, maybeOrder, nextDueIn, send


# MOVING

@docs maybeMove, isMoving


# FIRING

@docs LaserXPosition, newLasers, loadLasers, maybeFire


# SOUND EFFECTS

@docs maybePlaySFX


# SUBSCRIBING

@docs subscriptions


# QUERYING

@docs boundingBox


# VIEWING

@docs view

-}

import Random
import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Level as Level exposing (Difficulty(..), Level)
import Shared.Movement as Movement exposing (Direction(..))
import Shared.Point as Point exposing (Point)
import SpaceInvaders.Configs as Configs
import SpaceInvaders.SFX as SFX exposing (Sound(..), Status(..))
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, enableBackground, fill, height, transform, viewBox, width, x, y)
import Time



-- MODEL


{-| A type representing an alien [Mothership](#Mothership).

This is an opaque type, use the exposed API to interact with it.

-}
type Mothership
    = Mothership
        { boundingBox : BoundingBox
        , laserPoints : List Point
        , state : State
        }


type State
    = Docked
    | Ordering
    | Ready Int
    | Moving



-- INITIALIZING


{-| -}
init : Mothership
init =
    Mothership
        { state = Docked
        , laserPoints = []
        , boundingBox =
            startingPosition
                |> BoundingBox.fromPoint
                |> BoundingBox.incrementRight
                    config.width
                |> BoundingBox.incrementBottom
                    config.height
        }


startingPosition : Point
startingPosition =
    Point.init
        { x = rightBoundary
        , y = topBoundary
        }


config : Configs.Mothership
config =
    Configs.mothership


rightBoundary : Float
rightBoundary =
    Configs.view.boundaryRight + config.width + Configs.view.paddingX


leftBoundary : Float
leftBoundary =
    Configs.view.boundaryLeft - config.width - Configs.view.paddingX


topBoundary : Float
topBoundary =
    Configs.view.boundaryTop



-- ORDERING


{-| A type alias representing the total number of
[Alien](SpaceInvaders.Aliens#Alien)s alive.

This is used when scheduling the next [Mothership](#Mothership) - as the total
number of [Alien](SpaceInvaders.Aliens#Alien)s reduces, the frequency of the
[Mothership](#Mothership) should increase.

-}
type alias AliensRemaining =
    Int


{-| A type alias representing the number of seconds until the next
[Mothership](#Mothership) is due.
-}
type alias NextDueIn =
    Int


{-| Maybe order a [Mothership](#Mothership).

The `Cmd` returned in the
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple) will be
either:

1.  `Cmd (msg NextDueIn)` if the [Mothership](#Mothership) has been ordered
    successfully, or
2.  `Cmd.none` if the [Mothership](#Mothership) has already been ordered, or if
    the [Mothership](#Mothership) is already travelling across screen.

The [Mothership](#Mothership) element of the
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple) will be
updated to reflect either scenario.

-}
maybeOrder : (NextDueIn -> msg) -> Level -> AliensRemaining -> Mothership -> ( Mothership, Cmd msg )
maybeOrder msg level numAliensLeft mothership =
    case mothership |> state of
        Docked ->
            let
                ( fromInt, toInt ) =
                    case (level |> Level.number) > numAliensLeft of
                        True ->
                            ( 0, 10 )

                        False ->
                            ( level
                                |> Level.number
                            , numAliensLeft
                            )
            in
            ( mothership
                |> updateState
                    Ordering
            , Random.generate
                msg
                (Random.int
                    fromInt
                    toInt
                )
            )

        _ ->
            ( mothership
            , Cmd.none
            )


{-| Set the number of _seconds_ until the next [Mothership](#Mothership) is due.
-}
nextDueIn : NextDueIn -> Mothership -> Mothership
nextDueIn time mothership =
    mothership
        |> updateState
            (Ready
                (time * 1000)
            )


{-| Send the [Mothership](#Mothership) across the screen.
-}
send : Mothership -> Mothership
send mothership =
    mothership
        |> updateState
            Moving



-- MOVING


{-| Move the [Mothership](#Mothership) across the screen if it is meant to
move.
-}
maybeMove : Mothership -> Mothership
maybeMove mothership =
    case mothership |> state of
        Moving ->
            case (mothership |> boundingBox |> BoundingBox.right) > leftBoundary of
                True ->
                    mothership
                        |> updateBoundingBox
                            (mothership
                                |> boundingBox
                                |> Movement.moveBox
                                    (Left 1)
                            )

                False ->
                    init

        _ ->
            mothership


{-| Determine if the [Mothership](#Mothership) is moving across the screen.
-}
isMoving : Mothership -> Bool
isMoving mothership =
    (mothership |> state) == Moving



-- FIRING


{-| A type alias representing the x-position on screen that a
[Laser](SpaceInvaders.Assets.Laser) should be dropped from.
-}
type alias LaserXPosition =
    Int


{-| Create the [LaserXPosition](#LaserXPosition)(s) that
[Lasers](SpaceInvaders.Assets.Lasers#Lasers) are to be dropped from while
the [Mothership](#Mothership) travels across the screen.
-}
newLasers : (List LaserXPosition -> msg) -> Level -> Difficulty -> Cmd msg
newLasers msg level difficulty =
    let
        maxBombs =
            case difficulty of
                Easy ->
                    level
                        |> Level.number

                Medium ->
                    level
                        |> Level.number
                        |> toFloat
                        |> (*) 1.25
                        |> floor

                Hard ->
                    level
                        |> Level.number
                        |> toFloat
                        |> (*) 1.75
                        |> floor

        numBombs =
            case maxBombs > 10 of
                True ->
                    10

                False ->
                    maxBombs
    in
    Random.generate
        msg
        (Random.list
            numBombs
            (Random.int
                (Configs.view.boundaryLeft
                    |> round
                )
                (Configs.view.boundaryRight
                    |> round
                )
            )
        )


{-| Sets up the [Point](Shared.Point#Point)(s) that
[Laser](SpaceInvaders.Assets.Laser)(s) should be dropped from.
-}
loadLasers : List LaserXPosition -> Mothership -> Mothership
loadLasers xPoints mothership =
    let
        point =
            Point.zero
                |> Point.moveToY
                    Configs.mothership.y
    in
    mothership
        |> updateLaserPoints
            (xPoints
                |> List.map
                    (\x ->
                        point
                            |> Point.moveToX
                                (x
                                    |> toFloat
                                )
                    )
            )


{-| Maybe fire a [Laser](Shared.Laser#Laser).

Return a `Maybe` [Point](Shared.Point#Point) representing the position a
[Laser](SpaceInvaders.Assets.Laser) should be fired from if the
[Mothership](#Mothership) has reached a firing position.

The [Mothership](#Mothership) element of the
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple) will be
updated to reflect that it has fired - if that is the case - otherwise it will
be unchanged.

-}
maybeFire : Mothership -> ( Maybe Point, Mothership )
maybeFire mothership =
    case mothership |> state of
        Moving ->
            let
                ( points, remaining ) =
                    mothership
                        |> laserPoints
                        |> List.partition
                            (\point_ ->
                                (point_ |> Point.x) == (mothership |> boundingBox |> BoundingBox.left)
                            )
            in
            case points of
                point :: _ ->
                    ( Just point
                    , mothership
                        |> updateLaserPoints
                            remaining
                    )

                [] ->
                    ( Nothing
                    , mothership
                    )

        _ ->
            ( Nothing
            , mothership
            )



-- SOUND EFFECTS


{-| Return the [Sound](SpaceInvaders.SFX#Sound) effect to play when moving, or if
not moving, return [NoSound](SpaceInvaders.SFX#Sound).
-}
maybePlaySFX : Mothership -> Sound
maybePlaySFX mothership =
    case mothership |> state of
        Moving ->
            SFX.Mothership Play

        _ ->
            SFX.NoSound



-- SUBSCRIBING


{-| The only [subscription](#subscriptions) is
[Time.every](https://package.elm-lang.org/packages/elm/time/latest/Time#every).

This is used to schedule the next [Mothership](#Mothership) due.

You need to provide a `Msg` type that accepts a
[Time.Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)
type. When the [Mothership](#Mothership) is due, your `update` function will be
called with your `Msg` type so that you can [send](#send) out the
[Mothership](#Mothership).

-}
subscriptions : (Time.Posix -> msg) -> Mothership -> Sub msg
subscriptions msg mothership =
    case mothership |> state of
        Ready timeTillNext ->
            Time.every
                (timeTillNext
                    |> toFloat
                )
                msg

        _ ->
            Sub.none



-- QUERYING


{-| -}
boundingBox : Mothership -> BoundingBox
boundingBox (Mothership mothership) =
    mothership.boundingBox


laserPoints : Mothership -> List Point
laserPoints (Mothership mothership) =
    mothership.laserPoints


state : Mothership -> State
state (Mothership mothership) =
    mothership.state



-- UPDATING


updateBoundingBox : BoundingBox -> Mothership -> Mothership
updateBoundingBox boundingBox_ (Mothership mothership) =
    Mothership
        { mothership
            | boundingBox = boundingBox_
        }


updateState : State -> Mothership -> Mothership
updateState state_ (Mothership mothership) =
    Mothership
        { mothership
            | state = state_
        }


updateLaserPoints : List Point -> Mothership -> Mothership
updateLaserPoints points (Mothership mothership) =
    Mothership
        { mothership
            | laserPoints = points
        }



-- VIEWING


{-| Credit to:
<https://github.com/gege251/space_invaders>
-}
view : Mothership -> Svg msg
view mothership =
    case mothership |> state of
        Moving ->
            let
                point =
                    mothership
                        |> boundingBox
                        |> BoundingBox.toPoint
            in
            svg
                [ viewBox Configs.aliens.viewBox
                , fill "blue"
                , width
                    (config.width
                        |> String.fromFloat
                    )
                , height
                    (config.height
                        |> String.fromFloat
                    )
                , x
                    (point
                        |> Point.xToString
                    )
                , y
                    (point
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

        _ ->
            svg [] []
