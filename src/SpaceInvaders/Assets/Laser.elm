module SpaceInvaders.Assets.Laser exposing
    ( Laser
    , Color, Height, init, fromPoint
    , DidMove, maybeMove
    , load, loadAll, fire, fireAll, didHit
    , detectBunkerHits
    , isActive, boundingBox
    , view
    )

{-| A [Laser](#Laser) is shot upwards by the
[Player](Shared.Players)s [Ship](SpaceInvaders.Assets.Ship), and shot downwards
by the [Aliens](SpaceInvaders.Assets.Aliens) and the
[Mothership](SpaceInvaders.Assets.Mothership).

A [Laser](#Laser) will destroy the [Player](Shared.Players)s ship when shot
from an [Alien](SpaceInvaders.Assets.Aliens) or from a
[Mothership](SpaceInvaders.Assets.Mothership), and will destroy an
[Alien](SpaceInvaders.Assets.Aliens) and a
[Mothership](SpaceInvaders.Assets.Mothership) when shot by the
[Player](Shared.Players).

A [Laser](#Laser) will damage a [Bunker](SpaceInvaders.Assets.Bunker) no matter
who shot it.

@docs Laser


# INITIALIZING

@docs Color, Height, init, fromPoint


# MOVING

@docs DidMove, maybeMove


# FIRING

@docs load, loadAll, fire, fireAll, didHit


# HIT DETECTION

@docs detectBunkerHits


# QUERYING

@docs isActive, boundingBox


# VIEWING

@docs view

-}

import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Movement as Movement exposing (Direction(..))
import Shared.Point as Point exposing (Point)
import SpaceInvaders.Assets.Bunkers exposing (Bunkers)
import SpaceInvaders.Configs as Configs
import Svg exposing (Svg, line)
import Svg.Attributes as SA exposing (stroke, x1, x2, y1, y2)



-- MODEL


{-| A type representing a [Laser](#Laser).

This is an opaque type, use the exposed API to interact with it.

-}
type Laser
    = Laser
        { boundingBox : BoundingBox
        , isActive : Bool
        , color : String
        }



-- INITIALIZING


{-| A type alias representing the color of the [Laser](#Laser).
-}
type alias Color =
    String


{-| A type alias representing the height of a [Laser](#Laser).
-}
type alias Height =
    Float


{-| -}
init : Color -> Height -> Laser
init color_ height_ =
    Laser
        { boundingBox =
            BoundingBox.zero
                |> BoundingBox.incrementBottom
                    height_
                |> BoundingBox.incrementRight
                    1
        , isActive = False
        , color = color_
        }


{-| Create a [Laser](#Laser) from a [Point](Shared.Point#Point).
-}
fromPoint : Color -> Height -> Point -> Laser
fromPoint color_ height point =
    Laser
        { boundingBox =
            point
                |> BoundingBox.fromPoint
                |> BoundingBox.incrementBottom
                    height
                |> BoundingBox.incrementRight
                    1
        , isActive = False
        , color = color_
        }



-- MOVING


{-| A type alias representing whether or not the [Laser](#Laser) moved.
-}
type alias DidMove =
    Bool


{-| Maybe move a [Laser](#Laser) in the specified
[Direction](Shared.Movement#Direction).

If the [Laser](#Laser) [isActive](#isActive) it will move, if it isn't, it
won't.

A [Laser](#Laser) is made active when it is [fire](#fire)d.

If a [Laser](#Laser) moves outside of an upper or lower boundary it is
de-activated.

-}
maybeMove : Direction -> Laser -> ( Laser, DidMove )
maybeMove direction laser =
    case laser |> isActive of
        True ->
            ( laser
                |> move
                    direction
            , True
            )

        False ->
            ( laser
            , False
            )


move : Direction -> Laser -> Laser
move direction laser =
    case direction of
        Up amount ->
            laser
                |> moveWithinBoundary
                    direction
                    BoundingBox.bottom
                    (<)
                    (Configs.view.boundaryTop + amount)

        Down amount ->
            laser
                |> moveWithinBoundary
                    direction
                    BoundingBox.top
                    (>)
                    (Configs.view.boundaryBottom - amount)

        _ ->
            laser


moveWithinBoundary : Direction -> (BoundingBox -> Float) -> (Float -> Float -> Bool) -> Float -> Laser -> Laser
moveWithinBoundary direction edge compare boundary laser =
    case compare (laser |> boundingBox |> edge) boundary of
        True ->
            -- outside the boundary
            laser
                |> updateIsActive
                    False

        False ->
            -- inside the boundary
            laser
                |> updateBoundingBox
                    (laser
                        |> boundingBox
                        |> Movement.moveBox
                            direction
                    )



-- FIRING


{-| Load a [Laser](#Laser) ready for firing.

The [Laser](#Laser) will be positioned at the [Point](Shared.Point#Point).

-}
load : Point -> Laser -> Laser
load point_ laser =
    case laser |> isActive of
        True ->
            laser

        False ->
            laser
                |> updateBoundingBox
                    (laser
                        |> boundingBox
                        |> BoundingBox.moveTo
                            point_
                    )


{-| Load a list of [Laser](#Laser)s ready for firing.

Each [Laser](#Laser) will be positioned at it's relevant
[Point](Shared.Point#Point).

-}
loadAll : Color -> Height -> List Point -> List Laser
loadAll color_ height points =
    points
        |> List.map
            (\point_ ->
                height
                    |> init
                        color_
                    |> load
                        point_
            )


{-| -}
fire : Laser -> Laser
fire laser =
    laser
        |> updateIsActive
            True


{-| -}
fireAll : List Laser -> List Laser
fireAll lasers =
    lasers
        |> List.map
            fire


{-| The [Laser](#Laser) hit something, so de-activate it.
-}
didHit : Laser -> Laser
didHit laser =
    laser
        |> updateIsActive
            False



-- HIT DETECTION


{-| Detect if any [Laser](#Laser)s hit a
[Bunker](SpaceInvaders.Assets.Bunkers#Bunker).

The three [Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
output contains:

1.  The [List](https://package.elm-lang.org/packages/elm/core/latest/List) of
    [Laser](#Laser)s with any [Laser](#Laser)s that hit removed.

2.  The [Bunkers](SpaceInvaders.Assets.Bunkers#Bunkers) updated to reflect any
    [Hit](Shared.HitTest#Hit)s.

3.  Whether there was a [Hit](Shared.HitTest#Hit).

-}
detectBunkerHits :
    (List { boundingBox : BoundingBox, color : String, isActive : Bool }
     -> ( List { boundingBox : BoundingBox, color : String, isActive : Bool }, Bunkers, Bool )
    )
    -> List Laser
    -> ( List Laser, Bunkers, Bool )
detectBunkerHits func lasers =
    let
        ( lasers_, bunkers, hit ) =
            lasers
                |> List.map
                    unwrap
                |> func
    in
    case hit of
        True ->
            ( lasers_
                |> List.map
                    wrap
            , bunkers
            , True
            )

        False ->
            ( lasers
            , bunkers
            , False
            )


unwrap :
    Laser
    ->
        { boundingBox : BoundingBox
        , isActive : Bool
        , color : String
        }
unwrap (Laser laser) =
    laser


wrap :
    { boundingBox : BoundingBox
    , isActive : Bool
    , color : String
    }
    -> Laser
wrap laser =
    Laser laser



-- QUERYING


{-| -}
boundingBox : Laser -> BoundingBox
boundingBox (Laser laser) =
    laser.boundingBox


color : Laser -> String
color (Laser laser) =
    laser.color


{-| -}
isActive : Laser -> Bool
isActive (Laser laser) =
    laser.isActive



-- UPDATING


updateColor : String -> Laser -> Laser
updateColor color_ (Laser laser) =
    Laser
        { laser
            | color = color_
        }


updateBoundingBox : BoundingBox -> Laser -> Laser
updateBoundingBox boundingBox_ (Laser laser) =
    Laser
        { laser
            | boundingBox = boundingBox_
        }


updateIsActive : Bool -> Laser -> Laser
updateIsActive isActive_ (Laser laser) =
    Laser
        { laser
            | isActive = isActive_
        }



-- VIEWING


{-| Credit to:
<https://github.com/gege251/space_invaders>
-}
view : Laser -> Svg msg
view laser =
    case laser |> isActive of
        True ->
            line
                [ stroke
                    (laser
                        |> color
                    )
                , x1 (laser |> boundingBox |> BoundingBox.leftToString)
                , x2 (laser |> boundingBox |> BoundingBox.leftToString)
                , y1 (laser |> boundingBox |> BoundingBox.topToString)
                , y2 (laser |> boundingBox |> BoundingBox.bottomToString)
                ]
                []

        False ->
            line [] []
