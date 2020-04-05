module ResponsiveUI exposing
    ( Model
    , MinWidth, MaxWidth, init, initModel, getViewport
    , subscriptions
    , Msg, update
    , DefaultView(..), Option(..), select
    , Height, Width, aspectRatio
    , Viewport, viewport, viewportHeight, viewportWidth
    , device, class, orientation
    , scale, maybeFind, Multiplier, useMultiplier
    )

{-| For some time I've wanted a repeatable way of doing responsive
designs. A way to help me design for mobile first, and then adjust my views as
necessary for larger screens.

I have previously used
[Twitter Bootstrap](https://getbootstrap.com/) and
[Zurb Foundation](https://get.foundation), but have long since dropped using
CSS in favour of
[Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest). As a
result, I want a pure Elm way of reacting to different size screens and
orientations, and while requirements may change from project to project, I
want the process to remain the same. Re-inventing the wheel isn't something
that I feel should be necessary for this particular problem.


# Be Careful

This module has evolved from a current project. As a result, it is still an
experimental work in progress, so more functions could be added and some
functions may disappear or change. Therefore, **the usual caveats apply with
regard to using an experimental package in your projects**.


# Feedback/Collaboration

Feedback and/or collaboration is welcome. Initially, please start an issue on
Github, post a question on
[Discourse](https://discourse.elm-lang.org/u/paulh/) (I try to read it every
day), or private message me on
[Discourse](https://discourse.elm-lang.org/u/paulh/).


# What is Responsiveness?

When I think about responsiveness, I think of scaling fonts, spacing, padding
etc, and changing layouts according to screen size and orientation. This
module is intended to address those types of problems simply and intuitively.


# Layout

For controlling the layout, there is currently the [select](#select) function.
This function takes a [DefaultView](#DefaultView) and a list of
[Option](#Option)s. The list of [Option](#Option)s provides a way for you to
define alternative views to display.

You can provide as many, or as few [Options](ResponsiveUI.Options) as you
require. They are compared against the current screen size in the order that
you list them, with the first match being used. This provides a way to cover
as many different layouts as you need, with as little code as possible.

If none of the [Option](#Option)s match the
current screen configuration, then the [DefaultView](#DefaultView) will be
returned.

The [select](#select) function can be nested as deeply as required and is
view agnostic. Therefore, this can be utilized with any view package,
whether that be
[Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest),
[Html](https://package.elm-lang.org/packages/elm/html/latest/) or any other
package that may be around now or in the future.


# Sizing

**Note:** Functions in this category only support
[Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest). This
is so that the functions map directly to their equivalent in that particular
package. If this package is successful and well received, then I will happily
look into extending it to support other view packages.

My general idea to sizing, is to provide the size for the smallest screen you
wish to support. Then, on larger screen sizes, the size you provide will be
scaled up according to your initial settings. (Thoughts / ideas welcome).

When you initialize your app, you provide a [MinWidth](#MinWidth) and a
[MaxWidth](#MaxWidth), and then, when scaling sizes up, the following formula
is used:

    currentWidth / MinWidth * size

So, if the current width is twice that of [MinWidth](#MinWidth) the size will
be doubled.

Or, if [MaxWidth](#MaxWidth) is three times that of [MinWidth](#MinWidth) and
the current width is equal to or greater than [MaxWidth](#MaxWidth), then the
size will be trebled.

There are two versions of each function. There is the plain vanilla version,
which maps to the equivalent function in
[Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/), and
there is a `...WithOptions` version in order to provide more fine grained
control.

Similar to the [select](#select) function, the `...WithOptions` functions take
a list of [Option](#Option)s. In this scenario, each [Option](#Option) takes a
Float. The Float will be used to adjust the scaling of the size provided when
the [Option](#Option) matches the current screen.

So if you pass `0.5` to an [Option](#Option), the result of the above formula
will be multiplied by `0.5` when the screen matches that [Option](#Option).


# Model

@docs Model


# Initializing

@docs MinWidth, MaxWidth, init, initModel, getViewport


# Subscribing

@docs subscriptions


# Updating

@docs Msg, update


# Selecting a View

@docs DefaultView, Option, select


## Aspect Ratio

@docs Height, Width, aspectRatio


## Viewport

@docs Viewport, viewport, viewportHeight, viewportWidth

## Device

@docs device, class, orientation

## Scaling

These functions are used internally and you are unlikely to need to use these
directly. However, should you wish to experiment with the scaling with a
non-[Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
package, these are the functions to play with.

@docs scale, maybeFind, Multiplier, useMultiplier

-}

import Browser.Dom
import Browser.Events exposing (onResize)
import Element exposing (Device, DeviceClass(..), Element, Orientation(..))
import Task



-- MODEL


{-| This is an opaque type representing the internal model. Use the exposed API
to interact with it.
-}
type Model
    = Model
        { viewport : Viewport
        }



-- INITIALIZING


{-| This is the minimum screen width you wish to calculate sizes from.

Any screen widths below this size will use [MinWidth](#MinWidth) for
their calculations rather than the actual width.

-}
type alias MinWidth =
    Float


{-| This is the maximum screen width you wish to calculate sizes up to.

Any screen widths above this size will use [MaxWidth](#MaxWidth) for
their calculations rather than the actual width.

-}
type alias MaxWidth =
    Float


{-| Initialize the [Viewport](#Viewport) from flags passed in when initializing
your Elm app. This is the recommended approach.

    -- JS

    var app = Elm.Main.init({
      node: document.getElementById('elm'),
      flags:
        { height: window.innerHeight,
          width: window.innerWidth
        }
    });


    -- Elm

    import ResponsiveUI as RUI

    init : { a | height : Float, width : Float } -> (Model, Cmd Msg)
    init flags =
        ( { initModel
            | rui =
                RUI.init
                    400
                    1600
                    flags
          }
        , Cmd.none
        )

-}
init : MinWidth -> MaxWidth -> { a | height : Float, width : Float } -> Model
init min_ max_ viewportDimensions =
    Model
        { viewport =
            Viewport
                { minWidth = min_
                , maxWidth = max_
                , height = viewportDimensions.height
                , width = viewportDimensions.width
                , device =
                    { class = Phone
                    , orientation = Portrait
                    }
                }
                |> classifyDevice
        }


{-| Initialize the [Model](#Model).

This is probably only useful if you are using [getViewport](#getViewport).

-}
initModel : MinWidth -> MaxWidth -> Model
initModel min_ max_ =
    Model
        { viewport =
            Viewport
                { minWidth = min_
                , maxWidth = max_
                , height = 0
                , width = 0
                , device =
                    { class = Phone
                    , orientation = Portrait
                    }
                }
        }


{-| Initialize the [Viewport](#Viewport) using
[Browser.Dom.getViewport](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getViewport).

    import ResponsiveUI as RUI

    type alias Model =
        { rModel : RUI.Model }

    type Msg
        = ResponsiveMsg RUI.Msg

    init : ( Model, Cmd Msg )
    init =
        ( { rModel =
                RUI.initModel
                    400
                    1600
          }
        , RUI.getViewport
            |> Cmd.map
                ResponsiveMsg
        )

-}
getViewport : Cmd Msg
getViewport =
    Task.perform
        InitViewport
        Browser.Dom.getViewport



-- SUBSCRIBING


{-| Subscribe to receive messages when the window size changes.

    import ResponsiveUI as RUI

    type Msg
        = ResponsiveMsg RUI.Msg

    subscriptions : Model -> Sub Msg
    subscriptions model =
        RUI.subscriptions
            |> Sub.map ResponsiveMsg

-}
subscriptions : Sub Msg
subscriptions =
    onResize Resize



-- UPDATING


{-| An opaque type representing the messages sent internally.
-}
type Msg
    = InitViewport Browser.Dom.Viewport
    | Resize Int Int


{-| The update function to handle internal [Msg](#Msg)s.

    import ResponsiveUI as RUI

    type Msg
        = ResponsiveMsg RUI.Msg

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ResponsiveMsg subMsg ->
                ( { model
                    | rModel =
                        model.rModel
                            |> RUI.update
                                subMsg
                  }
                , Cmd.none
                )

    subscriptions : Model -> Sub Msg
    subscriptions model =
        RUI.subscriptions
            |> Sub.map
                ResponsiveMsg

-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        InitViewport window ->
            model
                |> updateViewport
                    (model
                        |> viewport
                        |> changedSize
                            window.viewport
                    )

        Resize width_ height_ ->
            model
                |> updateViewport
                    (model
                        |> viewport
                        |> changedSize
                            { height =
                                height_
                                    |> toFloat
                            , width =
                                width_
                                    |> toFloat
                            }
                    )


updateViewport : Viewport -> Model -> Model
updateViewport viewport_ (Model model) =
    Model
        { viewport = viewport_ }



-- SELECTING


{-| A type representing the default view to return if the current screen size
does not match one of the provided [Option](#Option)s.
-}
type DefaultView a
    = DefaultView a


{-| A union type representing different screen sizes, or groups of screen
sizes.
-}
type Option a
    = AllPhone a
    | AllTablet a
    | AllDesktop a
    | AllBigDesktop a
    | AllTouch a
    | AllLandscape a
    | AllPortrait a
    | PhoneLandscape a
    | PhonePortrait a
    | TabletLandscape a
    | TabletPortrait a
    | DesktopLandscape a
    | DesktopPortrait a
    | BigDesktopLandscape a
    | BigDesktopPortrait a
    | TouchLandscape a
    | TouchPortrait a


{-| This is the function that provides a way to define multiple views to choose
from.

When you provide the list of [Option](#Option)s, they will be checked in the
order they are listed with the first match being used. If none of the
[Option](#Option)s match the current screen size, then the
[DefaultView](#DefaultView) will be returned.

This should provide a way to handle all possible scenarios with minimal effort
by providing a kind of hierarchical list with the most important first, and
least important last, so to speak.

For instance, let's imagine that for one area of your view you want to have
one layout for Phone Portrait, another for all other touch devices and
orientations, and another for everything else. Then you could do something like
this:

    select
        (DefaultView allOtherViews)
        [ PhonePortrait phonePortraitView
        , AllTouch allTouchViews
        ]
        rModel

    -- If the screen profile matches PhonePortrait, use that view
    -- For all other touch devices use the AllTouch option
    -- Otherwise use the DefaultView

-}
select : DefaultView a -> List (Option a) -> Viewport -> a
select (DefaultView default) options viewport_ =
    options
        |> maybeFind
            (viewport_
                |> device
            )
        |> Maybe.withDefault
            default


{-| Maybe find an [Option](#Option) that matches the current
[Device](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#Device)
profile.

This is used by the `...WithOptions` functions. You are unlikely to need to use
it directly.

-}
maybeFind : Device -> List (Option a) -> Maybe a
maybeFind device_ options =
    options
        |> List.filterMap
            (match
                device_
            )
        |> List.head


match : Device -> Option a -> Maybe a
match device_ option =
    let
        class_ =
            device_
                |> .class

        orientation_ =
            device_
                |> .orientation
    in
    case ( class_, orientation_, option ) of
        ( Phone, Portrait, PhonePortrait element ) ->
            Just element

        ( Phone, Landscape, PhoneLandscape element ) ->
            Just element

        ( Phone, _, AllPhone element ) ->
            Just element

        ( Phone, Portrait, TouchPortrait element ) ->
            Just element

        ( Phone, Landscape, TouchLandscape element ) ->
            Just element

        ( Phone, _, AllTouch element ) ->
            Just element

        ( Tablet, Portrait, TabletPortrait element ) ->
            Just element

        ( Tablet, Landscape, TabletLandscape element ) ->
            Just element

        ( Tablet, _, AllTablet element ) ->
            Just element

        ( Tablet, Portrait, TouchPortrait element ) ->
            Just element

        ( Tablet, Landscape, TouchLandscape element ) ->
            Just element

        ( Tablet, _, AllTouch element ) ->
            Just element

        ( Desktop, Portrait, DesktopPortrait element ) ->
            Just element

        ( Desktop, Landscape, DesktopLandscape element ) ->
            Just element

        ( Desktop, _, AllDesktop element ) ->
            Just element

        ( BigDesktop, Portrait, BigDesktopPortrait element ) ->
            Just element

        ( BigDesktop, Landscape, BigDesktopLandscape element ) ->
            Just element

        ( BigDesktop, _, AllBigDesktop element ) ->
            Just element

        ( _, Landscape, AllLandscape element ) ->
            Just element

        ( _, Portrait, AllPortrait element ) ->
            Just element

        _ ->
            Nothing



-- ASPECT RATIO


{-| -}
type alias Height =
    Float


{-| -}
type alias Width =
    Float


{-| Provide the aspect ratio you want, and return the dimensions you need for
the current screen size.
-}
aspectRatio : Width -> Height -> Viewport -> ( Width, Height )
aspectRatio w h viewport_ =
    let
        currentHeight =
            viewport_
                |> viewportHeight

        currentWidth =
            viewport_
                |> viewportWidth
    in
    case viewport_ |> device |> .orientation of
        Portrait ->
            ( currentWidth
            , currentWidth / w * h
            )

        Landscape ->
            case currentWidth / currentHeight == w / h of
                True ->
                    ( currentWidth
                    , currentHeight
                    )

                False ->
                    case currentWidth / currentHeight >= w / h of
                        True ->
                            ( currentHeight / h * w
                            , currentHeight
                            )

                        False ->
                            ( currentWidth
                            , currentWidth / w * h
                            )



-- VIEWPORT


{-| A type to represent the [height](#height), [width](#width) and
[device](#device) profile of the screen.

This is an opaque type, use the exposed API to interact with it.

-}
type Viewport
    = Viewport
        { height : Float
        , width : Float
        , device : Device
        , minWidth : MinWidth
        , maxWidth : MaxWidth
        }


changedSize : { a | height : Float, width : Float } -> Viewport -> Viewport
changedSize dims viewport_ =
    viewport_
        |> updateHeight
            dims.height
        |> updateWidth
            dims.width
        |> classifyDevice


updateHeight : Float -> Viewport -> Viewport
updateHeight height_ (Viewport viewport_) =
    Viewport
        { viewport_
            | height = height_
        }


updateWidth : Float -> Viewport -> Viewport
updateWidth width_ (Viewport viewport_) =
    Viewport
        { viewport_
            | width = width_
        }


classifyDevice : Viewport -> Viewport
classifyDevice viewport_ =
    viewport_
        |> updateDevice
            (Element.classifyDevice
                { height =
                    viewport_
                        |> viewportHeight
                        |> round
                , width =
                    viewport_
                        |> viewportWidth
                        |> round
                }
            )


{-| -}
viewportHeight : Viewport -> Float
viewportHeight (Viewport viewport_) =
    viewport_.height


{-| -}
viewportWidth : Viewport -> Float
viewportWidth (Viewport viewport_) =
    viewport_.width


updateDevice : Device -> Viewport -> Viewport
updateDevice device_ (Viewport viewport_) =
    Viewport
        { viewport_
            | device = device_
        }


{-| -}
viewport : Model -> Viewport
viewport (Model model) =
    model.viewport


{-| -}
maxWidth : Viewport -> MaxWidth
maxWidth (Viewport viewport_) =
    viewport_.maxWidth


{-| -}
minWidth : Viewport -> MinWidth
minWidth (Viewport viewport_) =
    viewport_.minWidth



-- DEVICE


{-| The
[Device](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#Device)
profile that is used for responsiveness.

Whenever the window size is set or changed,
[classifyDevice](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#classifyDevice)
is called, and the
[Device](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#Device)
is updated.

-}
device : Viewport -> Device
device (Viewport viewport_) =
    viewport_.device


{-| See
[DeviceClass](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#DeviceClass)
for more information.
-}
class : Viewport -> DeviceClass
class viewport_ =
    viewport_
        |> device
        |> .class


{-| See
[Orientation](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#Orientation)
for more information.
-}
orientation : Viewport -> Orientation
orientation viewport_ =
    viewport_
        |> device
        |> .orientation



-- SCALING


{-| A type alias representing the `Float` to be used to adjust the size
calculated for an [Option](ResponsiveUI.Option#Option). This is simply a
convenience type for readabilty.
-}
type alias Multiplier =
    Float


{-| -}
useMultiplier : Float -> Multiplier -> Viewport -> Float
useMultiplier size multiplier viewport_ =
    viewport_
        |> scale
            size
        |> (*) multiplier


{-| -}
scale : Float -> Viewport -> Float
scale size viewport_ =
    let
        max_ =
            viewport_
                |> maxWidth

        min_ =
            viewport_
                |> minWidth

        current =
            case (viewport_ |> viewportWidth) >= max_ of
                True ->
                    max_

                False ->
                    viewport_
                        |> viewportWidth
    in
    case current <= min_ of
        True ->
            size

        False ->
            current / min_ * size
