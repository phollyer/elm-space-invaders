module SpaceInvaders.Assets.Bunkers exposing
    ( Bunkers
    , init
    , boundingBox, Remaining, Row, Rows, Bunker, left, middle, right
    , update, updateLeft, updateMiddle, updateRight
    , view
    )

{-| [Bunkers](#Bunkers) are there to protect the
[Players](Shared.Players) [Ship](SpaceInvaders.Assets.ship).

They are built from individual [Pixel](SpaceInvaders.Assets.Pixel)s so that
damage from [Lasers](SpaceInvaders.Assets.Lasers) and collisions by
[Aliens](SpaceInvaders.Assets.Aliens) can be easily rendered.

I may choose to do something a bit fancier in the future.

@docs Bunkers


# INITIALIZING

@docs init


# QUERYING

@docs boundingBox, Remaining, Row, Rows, Bunker, left, middle, right


# UPDATING

@docs update, updateLeft, updateMiddle, updateRight


# VIEWING

@docs view

-}

import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Point as Point exposing (Point)
import SpaceInvaders.Assets.Pixel as Pixel exposing (Pixel)
import SpaceInvaders.Configs as Configs
import Svg exposing (Svg)



-- MODEL


{-| A type representing all 3 [Bunker](#Bunker)s.

This is an opaque type, use the exposed API to interact with it.

-}
type Bunkers
    = Bunkers
        { left : Bunker
        , middle : Bunker
        , right : Bunker
        , boundingBox : BoundingBox
        }



-- INITIALIZING


{-| -}
init : Bunkers
init =
    let
        left_ =
            config
                |> .leftX
                |> initBunker

        middle_ =
            config
                |> .middleX
                |> initBunker

        right_ =
            config
                |> .rightX
                |> initBunker
    in
    Bunkers
        { left = left_
        , middle = middle_
        , right = right_
        , boundingBox =
            [ left_
                |> bunkerBoundingBox
            , middle_
                |> bunkerBoundingBox
            , right_
                |> bunkerBoundingBox
            ]
                |> BoundingBox.fromList
        }


config : Configs.Bunkers
config =
    Configs.bunkers


initBunker : Float -> Bunker
initBunker xPos =
    let
        rows =
            xPos
                |> initRows
    in
    { rows =
        ( ( rows
                |> calcRemaining
          , rows
                |> calcBoundingBox
          )
        , rows
        )
    }


initRows : Float -> List Row
initRows xPos =
    let
        point =
            Point.zero
                |> Point.moveToX
                    xPos
                |> Point.moveToY
                    config.y
    in
    config.definition
        |> List.indexedMap
            (initRow point)


initRow : Point -> Int -> List Int -> Row
initRow point rowIndex row =
    let
        newPoint =
            point
                |> Point.moveDown
                    (rowIndex
                        |> toFloat
                    )

        list =
            row
                |> List.indexedMap
                    (initPixel newPoint)
                |> List.filterMap
                    (\maybe -> maybe)
    in
    ( ( list
            |> List.length
      , list
            |> List.map
                .boundingBox
            |> BoundingBox.fromList
      )
    , list
    )


initPixel : Point -> Int -> Int -> Maybe Pixel
initPixel point columnIndex flag =
    case flag of
        1 ->
            point
                |> Point.moveRight
                    (columnIndex
                        |> toFloat
                    )
                |> Pixel.init
                    config.color
                |> BoundingBox.growAroundCenter
                    1
                |> Just

        _ ->
            Nothing


calcRemaining : List Row -> Int
calcRemaining rows =
    rows
        |> List.foldl
            (\row acc ->
                (row
                    |> Tuple.first
                    |> Tuple.first
                )
                    + acc
            )
            0


calcBoundingBox : List Row -> BoundingBox
calcBoundingBox rows =
    rows
        |> List.map
            Tuple.first
        |> List.map
            Tuple.second
        |> BoundingBox.fromList


bunkerBoundingBox : Bunker -> BoundingBox
bunkerBoundingBox bunker =
    bunker
        |> .rows
        |> Tuple.first
        |> Tuple.second



-- QUERYING


{-| -}
boundingBox : Bunkers -> BoundingBox
boundingBox (Bunkers bunkers) =
    bunkers.boundingBox


{-| A type alias representing the number of
[Pixel](SpaceInvaders.Assets.Pixel#Pixel)s remaining. Each bunker is made up of
[Row](#Row)s of [Pixel](SpaceInvaders.Assets.Pixel#Pixel)s.
-}
type alias Remaining =
    Int


{-| A type alias
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
representing the number of [Pixel](SpaceInvaders.Assets.Pixel#Pixel)s
[Remaining](#Remaining) in the [Row](#Row), the containing
[BoundingBox](Shared.BoundingBox#BoundingBox), and the list of
[Pixel](SpaceInvaders.Assets.Pixel#Pixel)s that make up the [Row](#Row).
-}
type alias Row =
    ( ( Remaining, BoundingBox ), List Pixel )


{-| A type alias
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
representing the total number of [Pixel](SpaceInvaders.Assets.Pixel#Pixel)s
[Remaining](#Remaining) in the [Bunker](#Bunker), the containing
[BoundingBox](Shared.BoundingBox#BoundingBox), and the [Row](#Row)s of
[Pixel](SpaceInvaders.Assets.Pixel#Pixel)s.
-}
type alias Rows =
    ( ( Remaining, BoundingBox ), List Row )


{-| A type alias representing a [Bunker](#Bunker).
-}
type alias Bunker =
    { rows : Rows
    }


{-| -}
left : Bunkers -> Bunker
left (Bunkers bunkers) =
    bunkers.left


{-| -}
middle : Bunkers -> Bunker
middle (Bunkers bunkers) =
    bunkers.middle


{-| -}
right : Bunkers -> Bunker
right (Bunkers bunkers) =
    bunkers.right



-- UPDATING


{-| Update all 3 [Bunker](#Bunker)s.
-}
update : Rows -> Rows -> Rows -> Bunkers -> Bunkers
update left_ middle_ right_ bunkers =
    bunkers
        |> updateLeft
            left_
        |> updateMiddle
            middle_
        |> updateRight
            right_


{-| -}
updateLeft : Rows -> Bunkers -> Bunkers
updateLeft rows (Bunkers bunkers) =
    Bunkers
        { bunkers
            | left =
                { rows = rows }
        }


{-| -}
updateMiddle : Rows -> Bunkers -> Bunkers
updateMiddle rows (Bunkers bunkers) =
    Bunkers
        { bunkers
            | middle =
                { rows = rows }
        }


{-| -}
updateRight : Rows -> Bunkers -> Bunkers
updateRight rows (Bunkers bunkers) =
    Bunkers
        { bunkers
            | right =
                { rows = rows }
        }



-- VIEWING


{-| -}
view : Bunkers -> List (Svg msg)
view bunkers =
    [ bunkers
        |> left
        |> .rows
        |> Tuple.second
        |> viewBunker
    , bunkers
        |> middle
        |> .rows
        |> Tuple.second
        |> viewBunker
    , bunkers
        |> right
        |> .rows
        |> Tuple.second
        |> viewBunker
    ]
        |> List.concat


viewBunker : List Row -> List (Svg msg)
viewBunker list =
    list
        |> List.map
            Tuple.second
        |> List.concat
        |> List.map
            Pixel.view
