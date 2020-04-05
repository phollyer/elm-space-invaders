module ResponsiveUI.Element.Border exposing
    ( width
    , Multiplier
    , widthWithOptions, X, Y, widthXY
    , MultiplierX, MultiplierY
    , widthXYWithOptions, Edges, widthEach, EdgeMultiplier, widthEachWithOptions
    , rounded, roundedWithOptions, Corners, roundEach, CornerMultiplier, roundEachWithOptions
    , glow, glowWithOptions, innerGlow, innerGlowWithOptions, OffsetX, OffsetY, Shadow, shadow, ShadowMultipler, shadowWithOptions, innerShadow, innerShadowWithOptions
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


## Border Widths

@docs width
@docs Multiplier
@docs widthWithOptions, X, Y, widthXY
@docs MultiplierX, MultiplierY
@docs widthXYWithOptions, Edges, widthEach, EdgeMultiplier, widthEachWithOptions


## Rounded Corners

@docs rounded, roundedWithOptions, Corners, roundEach, CornerMultiplier, roundEachWithOptions


## Shadows

@docs glow, glowWithOptions, innerGlow, innerGlowWithOptions, OffsetX, OffsetY, Shadow, shadow, ShadowMultipler, shadowWithOptions, innerShadow, innerShadowWithOptions

-}

import Element exposing (Attr, Attribute, Color)
import Element.Border as Border
import ResponsiveUI as RUI exposing (Option, Viewport)



-- WIDTH


{-| A type alias representing the `Float` to be used to adjust the size
calculated for an [Option](ResponsiveUI.Option#Option). This is simply a
convenience type for readabilty.
-}
type alias Multiplier =
    Float


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#width>
-}
width : Int -> Viewport -> Attribute msg
width size viewport =
    viewport
        |> RUI.scale
            (size
                |> toFloat
            )
        |> round
        |> Border.width


{-| -}
widthWithOptions : Int -> List (Option Multiplier) -> Viewport -> Attribute msg
widthWithOptions size options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> width
                    size

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    (size
                        |> toFloat
                    )
                    multiplier
                |> round
                |> Border.width


{-| A type alias representing an `Int`. This is simply a convenience type for
readabilty.
-}
type alias X =
    Int


{-| A type alias representing an `Int`. This is simply a convenience type for
readabilty.
-}
type alias Y =
    Int


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#widthXY>
-}
widthXY : X -> Y -> Viewport -> Attribute msg
widthXY x y viewport =
    let
        scaledX =
            viewport
                |> RUI.scale
                    (x
                        |> toFloat
                    )
                |> round

        scaledY =
            viewport
                |> RUI.scale
                    (y
                        |> toFloat
                    )
                |> round
    in
    Border.widthXY
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
widthXYWithOptions : X -> Y -> List (Option ( MultiplierX, MultiplierY )) -> Viewport -> Attribute msg
widthXYWithOptions x y options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> widthXY
                    x
                    y

        Just ( multiplierX, multiplierY ) ->
            let
                scaledX =
                    viewport
                        |> RUI.useMultiplier
                            (x
                                |> toFloat
                            )
                            multiplierX
                        |> round

                scaledY =
                    viewport
                        |> RUI.useMultiplier
                            (y
                                |> toFloat
                            )
                            multiplierY
                        |> round
            in
            Border.widthXY
                scaledX
                scaledY


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#widthEach>
-}
widthEach : Edges -> Viewport -> Attribute msg
widthEach edges viewport =
    Border.widthEach
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
type alias Edges =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


{-| -}
type alias EdgeMultiplier =
    { top : Multiplier
    , right : Multiplier
    , bottom : Multiplier
    , left : Multiplier
    }


{-| -}
widthEachWithOptions : Edges -> List (Option EdgeMultiplier) -> Viewport -> Attribute msg
widthEachWithOptions edges options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> widthEach
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
            Border.widthEach
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



-- ROUNDED CORNERS


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#rounded>
-}
rounded : Int -> Viewport -> Attribute msg
rounded size viewport =
    viewport
        |> RUI.scale
            (size
                |> toFloat
            )
        |> round
        |> Border.rounded


{-| -}
roundedWithOptions : Int -> List (Option Multiplier) -> Viewport -> Attribute msg
roundedWithOptions size options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> RUI.scale
                    (size
                        |> toFloat
                    )
                |> round
                |> Border.rounded

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    (size
                        |> toFloat
                    )
                    multiplier
                |> round
                |> Border.rounded


{-| -}
type alias Corners =
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#roundEach>
-}
roundEach : Corners -> Viewport -> Attribute msg
roundEach corners viewport =
    Border.roundEach
        { topLeft =
            viewport
                |> RUI.scale
                    (corners.topLeft
                        |> toFloat
                    )
                |> round
        , topRight =
            viewport
                |> RUI.scale
                    (corners.topRight
                        |> toFloat
                    )
                |> round
        , bottomLeft =
            viewport
                |> RUI.scale
                    (corners.bottomLeft
                        |> toFloat
                    )
                |> round
        , bottomRight =
            viewport
                |> RUI.scale
                    (corners.bottomRight
                        |> toFloat
                    )
                |> round
        }


{-| -}
type alias CornerMultiplier =
    { topLeft : Multiplier
    , topRight : Multiplier
    , bottomLeft : Multiplier
    , bottomRight : Multiplier
    }


{-| -}
roundEachWithOptions : Corners -> List (Option CornerMultiplier) -> Viewport -> Attribute msg
roundEachWithOptions corners options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> roundEach
                    corners

        Just edgeMultiplier ->
            let
                calc : Int -> Multiplier -> Viewport -> Int
                calc size multiplier viewport_ =
                    viewport_
                        |> RUI.useMultiplier
                            (size
                                |> toFloat
                            )
                            multiplier
                        |> round
            in
            Border.roundEach
                { topLeft =
                    viewport
                        |> calc
                            corners.topLeft
                            edgeMultiplier.topLeft
                , topRight =
                    viewport
                        |> calc
                            corners.topRight
                            edgeMultiplier.topRight
                , bottomLeft =
                    viewport
                        |> calc
                            corners.bottomLeft
                            edgeMultiplier.bottomLeft
                , bottomRight =
                    viewport
                        |> calc
                            corners.bottomRight
                            edgeMultiplier.bottomRight
                }



-- SHADOWS


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#glow>
-}
glow : Color -> Float -> Viewport -> Attr decorative msg
glow color size viewport =
    viewport
        |> RUI.scale
            size
        |> Border.glow
            color


{-| -}
glowWithOptions : Color -> Float -> List (Option Multiplier) -> Viewport -> Attr decorative msg
glowWithOptions color size options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> glow
                    color
                    size

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    size
                    multiplier
                |> Border.glow
                    color


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#innerGlow>
-}
innerGlow : Color -> Float -> Viewport -> Attr decorative msg
innerGlow color size viewport =
    viewport
        |> RUI.scale
            size
        |> Border.innerGlow
            color


{-| -}
innerGlowWithOptions : Color -> Float -> List (Option Multiplier) -> Viewport -> Attr decorative msg
innerGlowWithOptions color size options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> innerGlow
                    color
                    size

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    size
                    multiplier
                |> Border.innerGlow
                    color


{-| A type alias representing a `Float`. This is simply a convenience type for
readabilty.
-}
type alias OffsetX =
    Float


{-| A type alias representing a `Float`. This is simply a convenience type for
readabilty.
-}
type alias OffsetY =
    Float


{-| -}
type alias Shadow =
    { offset :
        ( OffsetX, OffsetY )
    , size : Float
    , blur : Float
    , color : Color
    }


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#shadow>
-}
shadow : Shadow -> Viewport -> Attr decorative msg
shadow shadow_ viewport =
    let
        offsetX =
            viewport
                |> RUI.scale
                    (shadow_
                        |> .offset
                        |> Tuple.first
                    )

        offsetY =
            viewport
                |> RUI.scale
                    (shadow_
                        |> .offset
                        |> Tuple.second
                    )

        size =
            viewport
                |> RUI.scale
                    shadow_.size

        blur =
            viewport
                |> RUI.scale
                    shadow_.blur
    in
    Border.shadow
        { offset =
            ( offsetX, offsetY )
        , size = size
        , blur = blur
        , color = shadow_.color
        }


{-| -}
type alias ShadowMultipler =
    { offset :
        ( MultiplierX, MultiplierY )
    , size : Multiplier
    , blur : Multiplier
    }


{-| -}
shadowWithOptions : Shadow -> List (Option ShadowMultipler) -> Viewport -> Attr decorative msg
shadowWithOptions shadow_ options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> shadow
                    shadow_

        Just shadowMultiplier ->
            let
                offsetX =
                    viewport
                        |> RUI.useMultiplier
                            (shadow_
                                |> .offset
                                |> Tuple.first
                            )
                            (shadowMultiplier
                                |> .offset
                                |> Tuple.first
                            )

                offsetY =
                    viewport
                        |> RUI.useMultiplier
                            (shadow_
                                |> .offset
                                |> Tuple.second
                            )
                            (shadowMultiplier
                                |> .offset
                                |> Tuple.second
                            )

                size =
                    viewport
                        |> RUI.useMultiplier
                            shadow_.size
                            shadowMultiplier.size

                blur =
                    viewport
                        |> RUI.useMultiplier
                            shadow_.blur
                            shadowMultiplier.blur
            in
            Border.shadow
                { offset =
                    ( offsetX, offsetY )
                , size = size
                , blur = blur
                , color = shadow_.color
                }


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Border#innderShadow>
-}
innerShadow : Shadow -> Viewport -> Attr decorative msg
innerShadow shadow_ viewport =
    let
        offsetX =
            viewport
                |> RUI.scale
                    (shadow_
                        |> .offset
                        |> Tuple.first
                    )

        offsetY =
            viewport
                |> RUI.scale
                    (shadow_
                        |> .offset
                        |> Tuple.second
                    )

        size =
            viewport
                |> RUI.scale
                    shadow_.size

        blur =
            viewport
                |> RUI.scale
                    shadow_.blur
    in
    Border.innerShadow
        { offset =
            ( offsetX, offsetY )
        , size = size
        , blur = blur
        , color = shadow_.color
        }


{-| -}
innerShadowWithOptions : Shadow -> List (Option ShadowMultipler) -> Viewport -> Attr decorative msg
innerShadowWithOptions shadow_ options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> innerShadow
                    shadow_

        Just shadowMultiplier ->
            let
                offsetX =
                    viewport
                        |> RUI.useMultiplier
                            (shadow_
                                |> .offset
                                |> Tuple.first
                            )
                            (shadowMultiplier
                                |> .offset
                                |> Tuple.first
                            )

                offsetY =
                    viewport
                        |> RUI.useMultiplier
                            (shadow_
                                |> .offset
                                |> Tuple.second
                            )
                            (shadowMultiplier
                                |> .offset
                                |> Tuple.second
                            )

                size =
                    viewport
                        |> RUI.useMultiplier
                            shadow_.size
                            shadowMultiplier.size

                blur =
                    viewport
                        |> RUI.useMultiplier
                            shadow_.blur
                            shadowMultiplier.blur
            in
            Border.innerShadow
                { offset =
                    ( offsetX, offsetY )
                , size = size
                , blur = blur
                , color = shadow_.color
                }
