module Main exposing (Flags, Model, Msg, main)

import Browser
import Browser.Dom
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Point, circle, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html)
import Html.Attributes
import Task


type alias Flags =
    {}


type alias Model =
    { now : Float
    , point : Point
    , width : Int
    , height : Int
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
            , point = ( 0, 0 )
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


stepsPerTick : number
stepsPerTick =
    10000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame dt ->
            ( { model
                | now = model.now + dt / 1000
                , point =
                    List.range 0 1000
                        |> List.foldl
                            (\e p ->
                                move model (toFloat e + model.now) p
                            )
                            model.point
              }
            , Cmd.none
            )

        Size width height ->
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )


move : Model -> Float -> Point -> Point
move { width, height } now ( x, y ) =
    let
        crimp : Float -> Float -> Float -> Float
        crimp low high v =
            if v < low then
                high

            else if v > high then
                low

            else
                v
    in
    ( crimp
        (toFloat -width / toFloat height)
        (toFloat width / toFloat height)
        (x + 0.005 * cos (random now))
    , crimp
        -1
        1
        (y + 0.005 * sin (random now))
    )


random : Float -> Float
random now =
    7643 * sin (7649 * now)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Size
        , onAnimationFrameDelta Frame
        ]


view : Model -> Html Msg
view ({ width, height } as model) =
    Canvas.toHtml
        ( width, height )
        [ Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "width" "100vw"
        ]
        [ render model
        ]


render : Model -> Canvas.Renderable
render model =
    let
        width : Float
        width =
            toFloat model.width

        height : Float
        height =
            toFloat model.height

        pointSize : Float
        pointSize =
            minSize * 0.002

        minSize : Float
        minSize =
            max 1 <| min width height - 20

        hue : Float
        hue =
            (137 / 1521) * (toFloat <| floor (model.now / 5))

        circles : ( Float, Float ) -> List Canvas.Shape
        circles ( x, y ) =
            [ circle
                ( reproject (-width / height) (width / height) 0 width x
                , reproject -1 1 0 height y
                )
                pointSize
            , circle
                ( reproject (-width / height) (width / height) 0 width -x
                , reproject -1 1 0 height y
                )
                pointSize
            ]
    in
    List.range 0 stepsPerTick
        |> List.foldl
            (\e ( p, acc ) ->
                let
                    newP : Point
                    newP =
                        move model (toFloat e + model.now) p
                in
                ( newP, circles newP ++ acc )
            )
            ( model.point, [] )
        |> Tuple.second
        |> shapes
            [ fill <|
                Color.hsl
                    (hue - toFloat (ceiling hue))
                    0.8
                    0.8
            ]


reproject : Float -> Float -> Float -> Float -> Float -> Float
reproject fromMin fromMax toMin toMax value =
    (value - fromMin) / (fromMax - fromMin) * (toMax - toMin) + toMin
