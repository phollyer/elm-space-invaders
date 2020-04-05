module ResponsiveUI.Element.Font exposing
    ( size, Multiplier, sizeWithOptions
    , glow, glowWithOptions, OffsetX, OffsetY, Shadow, shadow, MultiplierX, MultiplierY, ShadowMultiplier, shadowWithOptions
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


## Sizing

@docs size, Multiplier, sizeWithOptions


## Shadow

@docs glow, glowWithOptions, OffsetX, OffsetY, Shadow, shadow, MultiplierX, MultiplierY, ShadowMultiplier, shadowWithOptions

-}

import Element exposing (Attr, Color)
import Element.Font as Font
import ResponsiveUI as RUI exposing (Option, Viewport)



-- SIZING


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Font#size>
-}
size : Float -> Viewport -> Attr decorative msg
size size_ viewport =
    viewport
        |> RUI.scale
            size_
        |> round
        |> Font.size


{-| A type alias representing the `Float` to be used to adjust the size
calculated for an [Option](ResponsiveUI.Option#Option). This is simply a
convenience type for readabilty.
-}
type alias Multiplier =
    Float


{-| -}
sizeWithOptions : Float -> List (Option Multiplier) -> Viewport -> Attr decorative msg
sizeWithOptions size_ options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> size
                    size_

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    size_
                    multiplier
                |> round
                |> Font.size



-- SHADOWS


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Font#glow>
-}
glow : Color -> Float -> Viewport -> Attr decorative msg
glow color size_ viewport =
    viewport
        |> RUI.scale
            size_
        |> Font.glow
            color


{-| -}
glowWithOptions : Color -> Float -> List (Option Multiplier) -> Viewport -> Attr decorative msg
glowWithOptions color size_ options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> glow
                    color
                    size_

        Just multiplier ->
            viewport
                |> RUI.useMultiplier
                    size_
                    multiplier
                |> Font.glow
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
    , blur : Float
    , color : Color
    }


{-| See
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Font#shadow>
-}
shadow : Shadow -> Viewport -> Attr decorative msg
shadow config viewport =
    let
        offsetX =
            viewport
                |> RUI.scale
                    (config
                        |> .offset
                        |> Tuple.first
                    )

        offsetY =
            viewport
                |> RUI.scale
                    (config
                        |> .offset
                        |> Tuple.second
                    )

        blur =
            viewport
                |> RUI.scale
                    config.blur
    in
    Font.shadow
        { offset =
            ( offsetX, offsetY )
        , blur = blur
        , color = config.color
        }


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
type alias ShadowMultiplier =
    { offset :
        ( MultiplierX, MultiplierY )
    , blur : Multiplier
    }


{-| -}
shadowWithOptions : Shadow -> List (Option ShadowMultiplier) -> Viewport -> Attr decorative msg
shadowWithOptions config options viewport =
    case options |> RUI.maybeFind (viewport |> RUI.device) of
        Nothing ->
            viewport
                |> shadow
                    config

        Just shadowMultiplier ->
            let
                offsetX =
                    viewport
                        |> RUI.useMultiplier
                            (config
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
                            (config
                                |> .offset
                                |> Tuple.second
                            )
                            (shadowMultiplier
                                |> .offset
                                |> Tuple.second
                            )

                blur =
                    viewport
                        |> RUI.useMultiplier
                            config.blur
                            shadowMultiplier.blur
            in
            Font.shadow
                { offset =
                    ( offsetX, offsetY )
                , blur = blur
                , color = config.color
                }
