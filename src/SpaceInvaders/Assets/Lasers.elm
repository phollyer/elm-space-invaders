module SpaceInvaders.Assets.Lasers exposing
    ( Lasers
    , init
    , maybeMove
    , detectBunkerHits
    , add
    , calculateBoundingBox
    , boundingBox, list
    , updateList
    , view
    )

{-|

@docs Lasers


# INITIALIZING

@docs init


# MOVING

@docs maybeMove

# HIT DETECTION

@docs detectBunkerHits


# ADDING

@docs add


# CALCULATING

@docs calculateBoundingBox


# QUERYING

@docs boundingBox, list


# UPDATING

@docs updateList


# VIEWING

@docs view

-}

import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Movement as Movement exposing (Direction)
import SpaceInvaders.Assets.Bunkers exposing (Bunkers)
import SpaceInvaders.Assets.Laser as Laser exposing (Laser)
import SpaceInvaders.Configs as Configs
import Svg exposing (Svg)



-- MODEL


{-| A type representing a group of [Laser](SpaceInvaders.Assets.Laser#Laser)s.

This is an opaque type, use the exposed API to interact with it.

-}
type Lasers
    = Lasers
        { list : List Laser
        , boundingBox : BoundingBox
        }



-- INITIALIZING


{-| -}
init : Lasers
init =
    Lasers
        { list = []
        , boundingBox = BoundingBox.zero
        }



-- MOVING


{-| Maybe move a group of [Laser](SpaceInvaders.Assets.Laser#Laser)s in the
specified [Direction](Shared.Movement#Direction).
-}
maybeMove : Direction -> Lasers -> Lasers
maybeMove direction lasers =
    case lasers |> list of
        [] ->
            init

        _ ->
            lasers
                |> move
                    direction


move : Direction -> Lasers -> Lasers
move direction lasers =
    let
        lasers_ =
            lasers
                |> list
                |> List.map
                    (direction
                        |> Laser.maybeMove
                    )
                |> List.unzip
                |> Tuple.first
                |> List.filter
                    Laser.isActive
    in
    lasers
        |> updateList
            lasers_
        |> maybeReCalculateBoundingBox
            (lasers
                |> list
            )
        |> updateBoundingBox
            (lasers
                |> boundingBox
                |> Movement.moveBox
                    direction
            )


maybeReCalculateBoundingBox : List Laser -> Lasers -> Lasers
maybeReCalculateBoundingBox oldList lasers =
    case (oldList |> List.length) == (lasers |> list |> List.length) of
        True ->
            lasers

        False ->
            lasers
                |> calculateBoundingBox



-- HIT DETECTION


{-| Detect if any [Lasers](#Lasers) hit a
[Bunker](SpaceInvaders.Assets.Bunkers#Bunker).

The three [Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
output contains:

1.  The group of [Lasers](#Lasers) with any [Laser](#Laser)s that hit removed.

2.  The [Bunkers](SpaceInvaders.Assets.Bunkers#Bunkers) updated to reflect any
    [Hit](Shared.HitTest#Hit)s.

3.  Whether there was a [Hit](Shared.HitTest#Hit).

-}
detectBunkerHits :
    (List { boundingBox : BoundingBox, color : String, isActive : Bool }
     -> ( List { boundingBox : BoundingBox, color : String, isActive : Bool }, Bunkers, Bool )
    )
    -> Lasers
    -> ( Lasers, Bunkers, Bool )
detectBunkerHits func lasers =
    let
        ( lasers_, bunkers, hit ) =
            lasers
                |> list
                |> Laser.detectBunkerHits
                    func
    in
    case hit of
        True ->
            ( lasers
                |> updateList
                    lasers_
                |> calculateBoundingBox
            , bunkers
            , True
            )

        False ->
            ( lasers
            , bunkers
            , False
            )



-- ADDING


{-| -}
add : List Laser -> Lasers -> Lasers
add list_ lasers =
    lasers
        |> updateList
            (lasers
                |> list
                |> List.append
                    list_
            )
        |> calculateBoundingBox



-- CALCULATING


{-| -}
calculateBoundingBox : Lasers -> Lasers
calculateBoundingBox lasers =
    lasers
        |> updateBoundingBox
            (lasers
                |> list
                |> List.map
                    Laser.boundingBox
                |> BoundingBox.fromList
            )



-- QUERYING


{-| -}
list : Lasers -> List Laser
list (Lasers lasers) =
    lasers.list


{-| -}
boundingBox : Lasers -> BoundingBox
boundingBox (Lasers lasers) =
    lasers.boundingBox



-- UPDATING


updateBoundingBox : BoundingBox -> Lasers -> Lasers
updateBoundingBox boundingBox_ (Lasers lasers) =
    Lasers
        { lasers
            | boundingBox = boundingBox_
        }


{-| -}
updateList : List Laser -> Lasers -> Lasers
updateList list_ (Lasers lasers) =
    Lasers
        { lasers
            | list = list_
        }



-- VIEWING


{-| -}
view : Lasers -> List (Svg msg)
view lasers =
    lasers
        |> list
        |> List.map
            Laser.view
