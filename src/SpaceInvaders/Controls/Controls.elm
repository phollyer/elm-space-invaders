module SpaceInvaders.Controls.Controls exposing
    ( Key(..)
    , Msg(..)
    , subscriptions
    , Touch(..)
    , touchFire, touchLeft, touchRight, touchPause, touchResume, touchQuit
    )

{-| The keyboard and touch controls for the game.


# Keyboard

@docs Key

@docs Msg

@docs subscriptions


# Example

    import SpaceInvaders.Controls as Controls exposing (Msg(..), Key(..))

    type Msg
        = ControlsMsg Controls.Msg
        ...

    update msg model =
        case msg of
            ControlsMsg (KeyDown Pause) ->
                ...

    subscriptions =
        Controls.subscriptions
            |> Sub.map ControlsMsg


# Touch

@docs Touch


# Buttons

@docs touchFire, touchLeft, touchRight, touchPause, touchResume, touchQuit

-}

import Browser.Events exposing (onKeyDown, onKeyUp)
import Element as El exposing (Element)
import Html.Events.Extra.Touch as Touch
import Json.Decode as JD
import SpaceInvaders.Controls.Touch.Fire as TouchFire
import SpaceInvaders.Controls.Touch.Left as TouchLeft
import SpaceInvaders.Controls.Touch.Pause as TouchPause
import SpaceInvaders.Controls.Touch.Resume as TouchResume
import SpaceInvaders.Controls.Touch.Right as TouchRight
import Svg exposing (Svg, circle, defs, g, image, linearGradient, path, radialGradient, rect, stop, svg)
import Svg.Attributes as SA exposing (cx, cy, d, gradientUnits, height, id, offset, r, rotate, rx, stopColor, viewBox, width, x, x1, x2, xlinkHref, y, y1, y2)



-- MODEL


{-| A union type representing all the possible keys that can be pressed in
order to affect the game in some way.

`Pause`, `Resume` and `Quit` are sent when their initial letters are pressed
and/or released and are case insensitive.

-}
type Key
    = LeftArrow
    | RightArrow
    | SpaceBar
    | Pause
    | Resume
    | Quit


{-| A union type representing all the possible touch events during the game.

This module uses
[elm-pointer-events](https://package.elm-lang.org/packages/mpizenberg/elm-pointer-events/latest/)
so the events that are emitted are
[Touch Events](https://package.elm-lang.org/packages/mpizenberg/elm-pointer-events/latest/Html-Events-Extra-Touch).

-}
type Touch
    = TouchLeft Touch.Event
    | TouchRight Touch.Event
    | TouchStop Touch.Event
    | TouchFire Touch.Event
    | TouchPause Touch.Event
    | TouchResume Touch.Event
    | TouchQuit Touch.Event



-- MSG


{-| -}
type Msg
    = KeyDown Key
    | KeyUp Key
    | NoOp



--  SUBSCRIPTIONS


{-| Subscribe to receive keyboard events.
-}
subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ onKeyDown
            (keyDecoder KeyDown)
        , onKeyUp
            (keyDecoder KeyUp)
        ]



-- DECODERS


keyDecoder : (Key -> Msg) -> JD.Decoder Msg
keyDecoder msg =
    JD.string
        |> JD.field "key"
        |> JD.map
            (keyChange msg)


keyChange : (Key -> Msg) -> String -> Msg
keyChange msg key =
    case key of
        "ArrowLeft" ->
            msg LeftArrow

        "ArrowRight" ->
            msg RightArrow

        " " ->
            msg SpaceBar

        "P" ->
            msg Pause

        "p" ->
            msg Pause

        "R" ->
            msg Resume

        "r" ->
            msg Resume

        "Q" ->
            msg Quit

        "q" ->
            msg Quit

        _ ->
            NoOp



-- TOUCH


{-| -}
touchLeft : Float -> Element Touch
touchLeft width =
    El.el
        [ El.htmlAttribute
            (Touch.onStart
                TouchLeft
            )
        , El.htmlAttribute
            (Touch.onEnd
                TouchStop
            )
        ]
        (TouchLeft.view
            width
        )


{-| -}
touchRight : Float -> Element Touch
touchRight width =
    El.el
        [ El.height
            El.fill
        , El.width
            El.fill
        , El.htmlAttribute
            (Touch.onStart
                TouchRight
            )
        , El.htmlAttribute
            (Touch.onEnd
                TouchStop
            )
        ]
        (TouchRight.view
            width
        )


{-| -}
touchFire : Float -> Element Touch
touchFire width =
    El.el
        [ El.htmlAttribute
            (Touch.onStart
                TouchFire
            )
        ]
        (TouchFire.view
            width
        )


{-| -}
touchPause : Float -> Element Touch
touchPause width =
    El.el
        [ El.htmlAttribute
            (Touch.onStart
                TouchPause
            )
        ]
        (TouchPause.view
            width
        )


{-| -}
touchResume : Float -> Element Touch
touchResume width =
    El.el
        [ El.htmlAttribute
            (Touch.onStart
                TouchResume
            )
        ]
        (TouchResume.view
            width
        )


{-| -}
touchQuit : Element Touch
touchQuit =
    El.image
        [ El.htmlAttribute
            (Touch.onStart
                TouchQuit
            )
        , El.height (El.px 50)
        , El.width (El.px 50)
        , El.centerX
        , El.centerY
        ]
        { src = "images/touchQuit.svg"
        , description = "Quit"
        }
