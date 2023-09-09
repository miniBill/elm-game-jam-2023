module Main exposing (Flags, Model, Msg, main)

import Browser
import Browser.Dom
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (group, lineTo, path, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html)
import Html.Attributes
import Task


type alias Flags =
    {}


type alias Model =
    { now : Float
    , width : Int
    , height : Int
    }


type alias FloatModel =
    { now : Float
    , width : Float
    , height : Float
    }


type Msg
    = Frame Float
    | Size Int Int


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { now = 0
            , width = 1
            , height = 1
            }
    in
    ( model
    , Browser.Dom.getViewport
        |> Task.perform
            (\{ viewport } ->
                Size
                    (floor viewport.width)
                    (floor viewport.height)
            )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame dt ->
            ( { model | now = model.now + dt / 1000 }, Cmd.none )

        Size width height ->
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , Browser.Events.onResize Size
        ]


view : Model -> Html Msg
view ({ width, height } as model) =
    let
        floatModel : FloatModel
        floatModel =
            toFloatModel model
    in
    Canvas.toHtml
        ( width, height )
        [ Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "width" "100vw"
        ]
        [ clearScreen floatModel
        , render floatModel
        ]


toFloatModel : Model -> FloatModel
toFloatModel model =
    { now = model.now
    , width = toFloat model.width
    , height = toFloat model.height
    }


clearScreen : FloatModel -> Canvas.Renderable
clearScreen { width, height } =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render : FloatModel -> Canvas.Renderable
render { width, height } =
    let
        count : number
        count =
            11

        rectSize : Float
        rectSize =
            toFloat <| ceiling <| minSize / count

        minSize : Float
        minSize =
            min width height - 20

        centerX : Float
        centerX =
            width / 2

        centerY : Float
        centerY =
            height / 2

        snubSquares : List Canvas.Renderable
        snubSquares =
            List.range 0 (count - 1)
                |> List.map
                    (\xi ->
                        List.range 0 (count - 1)
                            |> List.map (\yi -> snubSquare xi yi)
                            |> group []
                    )

        snubSquare : Int -> Int -> Canvas.Renderable
        snubSquare xi yi =
            let
                p : Float -> Float -> ( Float, Float )
                p dx dy =
                    ( reproject
                        0
                        (count - 1)
                        (centerX - minSize / 2)
                        (centerX + minSize / 2 - rectSize)
                        xi
                        + dx
                        * rectSize
                    , reproject
                        0
                        (count - 1)
                        (centerY - minSize / 2)
                        (centerY + minSize / 2 - rectSize)
                        yi
                        + dy
                        * rectSize
                    )
            in
            [ path (p 0 0.25)
                [ lineTo <| p 0.25 0
                , lineTo <| p 0.75 0
                , lineTo <| p 1 0.25
                , lineTo <| p 1 0.75
                , lineTo <| p 0.75 1
                , lineTo <| p 0.25 1
                , lineTo <| p 0 0.75
                ]
            ]
                |> shapes
                    [ fill
                        (if modBy 2 (xi + yi) == 0 then
                            Color.lightCharcoal

                         else
                            Color.charcoal
                        )
                    ]

        distorters : List Canvas.Renderable
        distorters =
            List.range 0 (count - 2)
                |> List.map
                    (\xi ->
                        List.range 0 (count - 2)
                            |> List.map (\yi -> distorter xi yi)
                            |> group []
                    )

        distorter : Int -> Int -> Canvas.Renderable
        distorter xi yi =
            let
                p : Float -> Float -> ( Float, Float )
                p dx dy =
                    ( reproject
                        0
                        (count - 2)
                        (centerX - minSize / 2 + rectSize)
                        (centerX + minSize / 2 - rectSize)
                        xi
                        + dx
                        * rectSize
                    , reproject
                        0
                        (count - 2)
                        (centerY - minSize / 2 + rectSize)
                        (centerY + minSize / 2 - rectSize)
                        yi
                        + dy
                        * rectSize
                    )

                sign_ : Int -> Int
                sign_ q =
                    if q >= count // 2 then
                        1

                    else
                        0

                ( color, otherColor ) =
                    if modBy 2 (xi + yi + sign_ xi + sign_ yi) == 0 then
                        ( Color.black, Color.white )

                    else
                        ( Color.white, Color.black )
            in
            [ [ path (p 0 0)
                    [ lineTo <| p (-0.25 + 0.125) 0.125
                    , lineTo <| p -0.25 0
                    , lineTo <| p (-0.25 + 0.125) -0.125
                    ]
              , path (p 0 0)
                    [ lineTo <| p (0.25 - 0.125) 0.125
                    , lineTo <| p 0.25 0
                    , lineTo <| p (0.25 - 0.125) -0.125
                    ]
              ]
                |> shapes [ fill color ]
            , [ path (p 0 0)
                    [ lineTo <| p 0.125 (-0.25 + 0.125)
                    , lineTo <| p 0 -0.25
                    , lineTo <| p -0.125 (-0.25 + 0.125)
                    ]
              , path (p 0 0)
                    [ lineTo <| p 0.125 (0.25 - 0.125)
                    , lineTo <| p 0 0.25
                    , lineTo <| p -0.125 (0.25 - 0.125)
                    ]
              ]
                |> shapes [ fill otherColor ]
            ]
                |> group []
    in
    (snubSquares ++ distorters)
        |> group []


reproject : Float -> Float -> Float -> Float -> Int -> Float
reproject fromMin fromMax toMin toMax value =
    (toFloat value - fromMin) / (fromMax - fromMin) * (toMax - toMin) + toMin
