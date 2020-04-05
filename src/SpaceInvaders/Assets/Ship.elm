module SpaceInvaders.Assets.Ship exposing
    ( Ship
    , init
    , move, moveLeft, moveRight, stop
    , fire
    , kill, PlayerNumber, LifeNumber, spareLife
    , alive, boundingBox
    , Color(..), updateColor
    , view
    )

{-| The [Players](Shared.Players) [Ship](#Ship).

It moves left and right, and fires one [Laser](SpaceInvaders.Assets.Laser) at a
time.

It is [kill](#kill)ed when hit by an [Alien](SpaceInvaders.Assets.Aliens#Alien)
[Laser](SpaceInvaders.Assets.Laser#Laser), a
[Mothership](SpaceInvaders.Assets.Mothership#Mothership)
[Laser](SpaceInvaders.Assets.Laser#Laser), or when hit by an
[Alien](SpaceInvaders.Assets.Aliens#Alien) as they approach the ground.

@docs Ship


# INITIALISING

@docs init


# MOVING

The [Ship](#Ship) only moves on each animation frame for smoother movement, so
pressing the keys for left or right sets a flag which is read and actioned each
time [move](#move) is called.

@docs move, moveLeft, moveRight, stop


# FIRING

@docs fire


# LIVES

@docs kill, PlayerNumber, LifeNumber, spareLife


# QUERYING

@docs alive, boundingBox


# UPDATING

@docs Color, updateColor


# VIEWING

@docs view

-}

import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Movement as Movement exposing (Direction)
import Shared.Point as Point exposing (Point)
import SpaceInvaders.Configs as Configs
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes as SA exposing (d, fill, viewBox, x, y)



-- MODEL


{-| A type representing a [Players](Shared.Players) [Ship](#Ship).

This is an opaque type, use the exposed API to interact with it.

-}
type Ship
    = Ship
        { alive : Bool
        , boundingBox : BoundingBox
        , color : Color
        , movement : Movement
        }


type Movement
    = Left
    | Right
    | Stop



-- INIT


{-| Initialize a [Ship](#Ship) at its starting position on screen.
-}
init : Ship
init =
    Ship
        { alive = True
        , color = White
        , movement = Stop
        , boundingBox =
            Point.zero
                |> Point.moveToX
                    leftBoundary
                |> Point.moveToY
                    yPos
                |> BoundingBox.fromPoint
                |> BoundingBox.incrementRight
                    config.width
                |> BoundingBox.incrementBottom
                    config.height
        }


config : Configs.Ship
config =
    Configs.ship


leftBoundary : Float
leftBoundary =
    Configs.view.boundaryLeft


rightBoundary : Float
rightBoundary =
    Configs.view.boundaryRight


yPos : Float
yPos =
    Configs.ground.y - config.height



-- MOVING


{-| Move the [Ship](#Ship).

This should be called continuously inside an
[animation loop](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#animation).

-}
move : Ship -> Ship
move ship =
    case ship |> movement of
        Left ->
            ship
                |> maybeMoveLeft

        Right ->
            ship
                |> maybeMoveRight

        Stop ->
            ship


maybeMoveLeft : Ship -> Ship
maybeMoveLeft ship =
    let
        movedBoundingBox =
            ship
                |> moveBoundingBox
                    Movement.Left
    in
    case (movedBoundingBox |> BoundingBox.left) >= leftBoundary of
        True ->
            ship
                |> updateBoundingBox
                    movedBoundingBox

        False ->
            ship


maybeMoveRight : Ship -> Ship
maybeMoveRight ship =
    let
        movedBoundingBox =
            ship
                |> moveBoundingBox
                    Movement.Right
    in
    case (movedBoundingBox |> BoundingBox.right) <= rightBoundary of
        True ->
            ship
                |> updateBoundingBox
                    movedBoundingBox

        False ->
            ship


moveBoundingBox : (Float -> Direction) -> Ship -> BoundingBox
moveBoundingBox direction ship =
    ship
        |> boundingBox
        |> Movement.moveBox
            (direction config.velocity)


{-| Set the movement flag to `Left`, ready to be actioned by [move](#move).
-}
moveLeft : Ship -> Ship
moveLeft ship =
    ship
        |> updateMovement
            Left


{-| Set the movement flag to `Right`, ready to be actioned by [move](#move).
-}
moveRight : Ship -> Ship
moveRight ship =
    ship
        |> updateMovement
            Right


{-| Set the movement flag to `Stop`. This should be called whenever a keyboard
control is released so that the [Ship](#Ship) does not continue aimlessly in
the same direction.
-}
stop : Ship -> Ship
stop ship =
    ship
        |> updateMovement
            Stop



-- FIRING


{-| Return the [Point](shared.Point#Point) that a
[Laser](SpaceInvaders.Assets.Laser#Laser) should be fired from.
-}
fire : Ship -> Point
fire ship =
    ship
        |> boundingBox
        |> BoundingBox.center



-- LIVES


{-| -}
kill : Ship -> Ship
kill ship =
    ship
        |> updateAlive
            False
        |> updateColor
            Red


{-| A type alias representing a [Player](Shared.Players#Player)'s number.
-}
type alias PlayerNumber =
    Int


{-| A type alias representing a [Player](Shared.Players#Player)'s life number.
-}
type alias LifeNumber =
    Int


{-| Position a [Ship](#Ship) that represents a spare life. This will be below
the [Players](Shared.Players#Players) [Ship](#Ship) and on the left side of the
screen for [Player](#Shared.Players#Player) 1, and the right of the screen for
[Player](#Shared.Players#Player) 2.
-}
spareLife : Color -> PlayerNumber -> LifeNumber -> Ship
spareLife color_ playerNumber lifeNumber =
    let
        defaultXPosition =
            ((lifeNumber |> toFloat) - 1) * config.width

        xPosition =
            case playerNumber of
                1 ->
                    defaultXPosition

                _ ->
                    rightBoundary - defaultXPosition - config.width
    in
    init
        |> updateColor color_
        |> updateBoundingBox
            (BoundingBox.zero
                |> Movement.moveBox (Movement.Right xPosition)
                |> Movement.moveBox (Movement.Down Configs.ground.y)
            )



-- QUERYING


{-| -}
boundingBox : Ship -> BoundingBox
boundingBox (Ship ship) =
    ship.boundingBox


{-| Determine if the [Ship](#Ship) is still alive or not.
-}
alive : Ship -> Bool
alive (Ship ship) =
    ship.alive


movement : Ship -> Movement
movement (Ship ship) =
    ship.movement


color : Ship -> Color
color (Ship ship) =
    ship.color



-- UPDATING


{-| A union type to represent the color of the [Ship](#Ship)
-}
type Color
    = White
    | Yellow
    | Red


{-| -}
updateColor : Color -> Ship -> Ship
updateColor color_ (Ship ship) =
    Ship
        { ship
            | color = color_
        }


updateAlive : Bool -> Ship -> Ship
updateAlive isAlive_ (Ship ship) =
    Ship
        { ship
            | alive = isAlive_
        }


updateBoundingBox : BoundingBox -> Ship -> Ship
updateBoundingBox boundingBox_ (Ship ship) =
    Ship
        { ship
            | boundingBox = boundingBox_
        }


updateMovement : Movement -> Ship -> Ship
updateMovement movement_ (Ship ship) =
    Ship
        { ship
            | movement = movement_
        }



-- VIEWING


{-| Credit to:
<https://github.com/gege251/space_invaders>
-}
view : Ship -> Svg msg
view ship =
    let
        point =
            ship
                |> boundingBox
                |> BoundingBox.toPoint
    in
    svg
        [ viewBox config.viewBox
        , fill
            (ship
                |> color
                |> colorToString
            )
        , x
            (point
                |> Point.xToString
            )
        , y
            (point
                |> Point.yToString
            )
        , SA.width
            (config
                |> .width
                |> String.fromFloat
            )
        , SA.height
            (config
                |> .height
                |> String.fromFloat
            )
        ]
        [ g []
            [ path [ d "M0 127h176v65H0z" ] []
            , path [ d "M31 96h115v66H31z" ] []
            , path [ d "M63 64h48v65h-48z" ] []
            ]
        ]


colorToString : Color -> String
colorToString color_ =
    case color_ of
        White ->
            "white"

        Yellow ->
            "yellow"

        Red ->
            "red"
