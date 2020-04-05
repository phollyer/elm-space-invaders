module ResponsiveUI.Element exposing
    ( Multiplier
    , height, heightWithOptions, width, widthWithOptions
    , spacing, spacingWithOptions
    , padding, paddingWithOptions, PaddingX, PaddingY, paddingXY, MultiplierX, MultiplierY, paddingXYWithOptions, Edges, paddingEach, EdgeMultiplier, paddingEachWithOptions
    , moveUp, moveUpWithOptions, moveDown, moveDownWithOptions, moveRight, moveRightWithOptions, moveLeft, moveLeftWithOptions
    )

{-| These functions map to their counterparts in
[Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest) and
they all return the same type as their
[Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest)
equivalent. Therefore they can be inter-mingled with
[Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest)
functions.

The only difference being:

1.  They require the [Viewport](ResponsiveUI.Viewport) as a parameter in order
    to be able to calculate the size.
2.  The `...WithOptions` versions also require a list of
    [Option](ResponsiveUI.Option#Option)s.


## Height and Width

@docs Multiplier

@docs height, heightWithOptions, width, widthWithOptions

## Spacing

@docs spacing, spacingWithOptions

## Padding

@docs padding, paddingWithOptions, PaddingX, PaddingY, paddingXY, MultiplierX, MultiplierY, paddingXYWithOptions, Edges, paddingEach, EdgeMultiplier, paddingEachWithOptions


## Adjustment

@docs moveUp, moveUpWithOptions, moveDown, moveDownWithOptions, moveRight, moveRightWithOptions, moveLeft, moveLeftWithOptions

-}

import Element exposing (Attr, Attribute)
import ResponsiveUI as RUI exposing (Option, Viewport)



-- SIZE


{-| A type alias representing the `Float` to be used to adjust the size
calculated for an [Option](ResponsiveUI.Option#Option). This is simply a
convenience type for readabilty.
-}
type alias Multiplier =
    Float


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#height>
-}
height : Float -> Viewport -> Attribute msg
height size viewport =
    viewport
        |> RUI.scale
            size
        |> round
        |> Element.px
        |> Element.height


{-| -}
heightWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attribute msg
heightWithOptions size options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> height
                    size

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    size
                    multiplier
                |> round
                |> Element.px
                |> Element.height


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#width>
-}
width : Float -> Viewport -> Attribute msg
width size viewport =
    viewport
        |> RUI.scale
            size
        |> round
        |> Element.px
        |> Element.width


{-| -}
widthWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attribute msg
widthWithOptions size options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> width
                    size

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    size
                    multiplier
                |> round
                |> Element.px
                |> Element.width



-- PADDING


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#padding>
-}
padding : Float -> Viewport -> Attribute msg
padding size viewport =
    viewport
        |> RUI.scale
            size
        |> round
        |> Element.padding


{-| -}
paddingWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attribute msg
paddingWithOptions size options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> padding
                    size

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    size
                    multiplier
                |> round
                |> Element.padding


{-| A type alias representing a `Float`. This is simply a convenience type for
readabilty.
-}
type alias PaddingX =
    Float


{-| A type alias representing a `Float`. This is simply a convenience type for
readabilty.
-}
type alias PaddingY =
    Float


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#paddingXY>
-}
paddingXY : PaddingX -> PaddingY -> Viewport -> Attribute msg
paddingXY x y viewport =
    let
        scaledX =
            viewport
                |> RUI.scale
                    x
                |> round

        scaledY =
            viewport
                |> RUI.scale
                    y
                |> round
    in
    Element.paddingXY
        scaledX
        scaledY


{-| A type alias representing the `Float` to be used to adjust the size
calculated for an [Option](ResponsiveUI.Option#Option). This is simply a
convenience type for readabilty.
-}
type alias MultiplierX =
    Float


{-| A type alias representing the `Float` to be used to adjust the size
calculated for an [Option](ResponsiveUI.Option#Option). This is simply a
convenience type for readabilty.
-}
type alias MultiplierY =
    Float


{-| -}
paddingXYWithOptions : PaddingX -> PaddingY -> List (Option ( MultiplierX, MultiplierY )) -> Viewport -> Attribute msg
paddingXYWithOptions x y options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> paddingXY
                    x
                    y

        Just ( xMultiplier, yMultiplier ) ->
            Element.paddingXY
                (viewport
                    |> RUI.useMultiplier
                        x
                        xMultiplier
                    |> round
                )
                (viewport
                    |> RUI.useMultiplier
                        y
                        yMultiplier
                    |> round
                )


{-| -}
type alias Edges =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#paddingEach>
-}
paddingEach : Edges -> Viewport -> Attribute msg
paddingEach edges viewport =
    Element.paddingEach
        { left =
            viewport
                |> RUI.scale
                    edges.left
                |> round
        , right =
            viewport
                |> RUI.scale
                    edges.right
                |> round
        , top =
            viewport
                |> RUI.scale
                    edges.top
                |> round
        , bottom =
            viewport
                |> RUI.scale
                    edges.bottom
                |> round
        }


{-| -}
type alias EdgeMultiplier =
    { top : Multiplier
    , right : Multiplier
    , bottom : Multiplier
    , left : Multiplier
    }


{-| -}
paddingEachWithOptions : Edges -> List (Option EdgeMultiplier) -> Viewport -> Attribute msg
paddingEachWithOptions edges options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> paddingEach
                    edges

        Just edgeMultiplier ->
            let
                calc : Float -> Multiplier -> Viewport -> Int
                calc size multiplier viewport_ =
                    viewport_
                        |> RUI.useMultiplier
                            size
                            multiplier
                        |> round
            in
            Element.paddingEach
                { left =
                    viewport
                        |> calc
                            edges.left
                            edgeMultiplier.left
                , right =
                    viewport
                        |> calc
                            edges.right
                            edgeMultiplier.right
                , top =
                    viewport
                        |> calc
                            edges.top
                            edgeMultiplier.top
                , bottom =
                    viewport
                        |> calc
                            edges.bottom
                            edgeMultiplier.bottom
                }



-- SPACING


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#spacing>
-}
spacing : Float -> Viewport -> Attribute msg
spacing size viewport =
    viewport
        |> RUI.scale
            size
        |> round
        |> Element.spacing


{-| -}
spacingWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attribute msg
spacingWithOptions size options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> spacing
                    size

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    size
                    multiplier
                |> round
                |> Element.spacing



-- ADJUSTMENT


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#moveUp>
-}
moveUp : Float -> Viewport -> Attr decorative msg
moveUp amount viewport =
    viewport
        |> RUI.scale
            amount
        |> Element.moveUp


{-| -}
moveUpWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attr decorative msg
moveUpWithOptions amount options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> moveUp
                    amount

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    amount
                    multiplier
                |> Element.moveUp


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#moveDown>
-}
moveDown : Float -> Viewport -> Attr decorative msg
moveDown amount viewport =
    viewport
        |> RUI.scale
            amount
        |> Element.moveUp


{-| -}
moveDownWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attr decorative msg
moveDownWithOptions amount options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> moveDown
                    amount

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    amount
                    multiplier
                |> Element.moveDown


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#moveLeft>
-}
moveLeft : Float -> Viewport -> Attr decorative msg
moveLeft amount viewport =
    viewport
        |> RUI.scale
            amount
        |> Element.moveUp


{-| -}
moveLeftWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attr decorative msg
moveLeftWithOptions amount options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> moveLeft
                    amount

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    amount
                    multiplier
                |> Element.moveLeft


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#moveRight>
-}
moveRight : Float -> Viewport -> Attr decorative msg
moveRight amount viewport =
    viewport
        |> RUI.scale
            amount
        |> Element.moveUp


{-| -}
moveRightWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attr decorative msg
moveRightWithOptions amount options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> moveRight
                    amount

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    amount
                    multiplier
                |> Element.moveRight
