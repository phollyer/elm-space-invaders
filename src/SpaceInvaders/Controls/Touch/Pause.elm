module SpaceInvaders.Controls.Touch.Pause exposing (..)

import Element as El exposing (Element)
import Svg exposing (Svg, circle, defs, g, image, linearGradient, path, radialGradient, rect, stop, svg)
import Svg.Attributes as SA exposing (cx, cy, d, gradientUnits, height, id, offset, r, rotate, rx, stopColor, viewBox, width, x, x1, x2, xlinkHref, y, y1, y2)


view : Float -> Element msg
view width_ =
    El.html <|
        svg
            [ x "0"
            , y "0"
            , width
                (String.fromFloat
                    width_
                )
            , height
                (String.fromFloat
                    width_
                )
            , viewBox "0 0 1004 1004"
            ]
            [ defs_
            , g
                [ SA.style "isolation: isolate"
                ]
                [ g
                    []
                    (circles
                        ++ [ g
                                []
                                [ rect
                                    [ x "351.5"
                                    , y "318"
                                    , width "102"
                                    , height "368"
                                    , rx "12"
                                    , SA.style "fill: #fff"
                                    ]
                                    []
                                , rect
                                    [ x "550.5"
                                    , y "318"
                                    , width "102"
                                    , height "368"
                                    , rx "12"
                                    , SA.style "fill: #fff"
                                    ]
                                    []
                                ]
                           , path
                                [ d "M853.66,436.63C853.66,603.34,696.22,528,502,528s-351.66,75.32-351.66-91.39S307.78,134.78,502,134.78,853.66,269.93,853.66,436.63Z"
                                , SA.style "fill: #1d1d1b;mix-blend-mode: screen"
                                ]
                                []
                           ]
                    )
                ]
            ]


circles : List (Svg msg)
circles =
    [ circle
        [ cx "502"
        , cy "502"
        , r "502"
        , SA.style "fill: #9d9d9c"
        ]
        []
    , circle
        [ cx "502"
        , cy "502"
        , r "493.75"
        , SA.style "fill: url(#Steel_01)"
        ]
        []
    , circle
        [ cx "502"
        , cy "502"
        , r "421.78"
        , SA.style "fill: #b2b2b2"
        ]
        []
    , circle
        [ cx "502"
        , cy "502"
        , r "394.27"
        , SA.style "fill: url(#radial-gradient)"
        ]
        []
    ]


defs_ : Svg msg
defs_ =
    defs
        []
        [ linearGradient
            [ id "Steel_01"
            , x1 "8.25"
            , y1 "502"
            , x2 "995.75"
            , y2 "502"
            , gradientUnits "userSpaceOnUse"
            ]
            [ stop
                [ offset "0"
                , stopColor "#e5e2df"
                ]
                []
            , stop
                [ offset "0"
                , stopColor "#ded9d6"
                ]
                []
            , stop
                [ offset "0.04"
                , stopColor "#cfccc9"
                ]
                []
            , stop
                [ offset "0.11"
                , stopColor "#a8a8a8"
                ]
                []
            , stop
                [ offset "0.21"
                , stopColor "#6a7072"
                ]
                []
            , stop
                [ offset "0.26"
                , stopColor "#465053"
                ]
                []
            , stop
                [ offset "0.37"
                , stopColor "#878d8f"
                ]
                []
            , stop
                [ offset "0.46"
                , stopColor "#b7babb"
                ]
                []
            , stop
                [ offset "0.53"
                , stopColor "#d5d6d6"
                ]
                []
            , stop
                [ offset "0.57"
                , stopColor "#e0e0e0"
                ]
                []
            , stop
                [ offset "0.63"
                , stopColor "#d6d7d8"
                ]
                []
            , stop
                [ offset "0.73"
                , stopColor "#bcbfc2"
                ]
                []
            , stop
                [ offset "0.86"
                , stopColor "#92989e"
                ]
                []
            , stop
                [ offset "0.88"
                , stopColor "#8b9298"
                ]
                []
            , stop
                [ offset "1"
                , stopColor "#d1d3d4"
                ]
                []
            , stop
                [ offset "1"
                , stopColor "#a7a9ac"
                ]
                []
            ]
        , radialGradient
            [ id "radial-gradient"
            , cx "502"
            , cy "502"
            , r "394.27"
            , gradientUnits "userSpaceOnUse"
            ]
            [ stop
                [ offset "0.38"
                , stopColor "#16ace8"
                ]
                []
            , stop
                [ offset "0.5"
                , stopColor "#15a9e4"
                ]
                []
            , stop
                [ offset "0.6"
                , stopColor "#14a1da"
                ]
                []
            , stop
                [ offset "0.69"
                , stopColor "#1193c7"
                ]
                []
            , stop
                [ offset "0.79"
                , stopColor "#0d80ae"
                ]
                []
            , stop
                [ offset "0.88"
                , stopColor "#08668d"
                ]
                []
            , stop
                [ offset "0.97"
                , stopColor "#024865"
                ]
                []
            , stop
                [ offset "1"
                , stopColor "#003b54"
                ]
                []
            ]
        ]
