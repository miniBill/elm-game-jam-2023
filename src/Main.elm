module Main exposing (Flags, Model, Msg, Point, main)

import Angle exposing (Radians)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import Duration exposing (Duration, Seconds)
import Html exposing (Html)
import Length exposing (Length, Meters)
import Pixels
import Point3d
import Quantity exposing (Quantity, Rate)
import Random
import Scene3d exposing (Entity)
import Scene3d.Material as Material exposing (Material)
import Sphere3d exposing (Sphere3d)
import Task
import Viewpoint3d


type alias Flags =
    {}


type alias Point =
    { latitude : Float
    , longitude : Float
    }


type alias Model =
    { points : List Point
    , elapsed : Duration
    , width : Int
    , height : Int
    }


type Msg
    = Frame Float
    | Resize Int Int


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        generator : Random.Generator (List Point)
        generator =
            Random.list 1000 pointGenerator

        pointGenerator : Random.Generator Point
        pointGenerator =
            Random.map2
                (\latitude longitude ->
                    { latitude = asin latitude
                    , longitude = longitude
                    }
                )
                (Random.float -1 1)
                (Random.float -pi pi)

        model : Model
        model =
            { points =
                Random.step generator (Random.initialSeed 1)
                    |> Tuple.first
            , elapsed = Quantity.zero
            , width = 1
            , height = 1
            }
    in
    ( model
    , Browser.Dom.getViewport
        |> Task.perform
            (\{ viewport } ->
                Resize
                    (floor viewport.width)
                    (floor viewport.height)
            )
    )


view : Model -> Html msg
view { elapsed, points, width, height } =
    Scene3d.sunny
        { upDirection = Direction3d.positiveZ
        , sunlightDirection = Direction3d.xyZ Quantity.zero (Angle.degrees -85)
        , shadows = True
        , dimensions = ( Pixels.pixels width, Pixels.pixels height )
        , camera =
            Camera3d.perspective
                { viewpoint =
                    let
                        rotationRate : Quantity Float (Rate Radians Seconds)
                        rotationRate =
                            Angle.degrees 90
                                |> Quantity.per Duration.second
                    in
                    Viewpoint3d.orbitZ
                        { azimuth = Quantity.at rotationRate elapsed
                        , elevation = Angle.degrees 45
                        , focalPoint = Point3d.origin
                        , distance = Length.meters 10
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.centimeters 10
        , background = Scene3d.backgroundColor Color.black
        , entities = List.map viewPoint points
        }


viewPoint : Point -> Entity Length
viewPoint point =
    let
        sphereRadius : number
        sphereRadius =
            3

        x : Float
        x =
            sphereRadius * cos point.latitude * sin point.longitude

        y : Float
        y =
            sphereRadius * cos point.latitude * cos point.longitude

        z : Float
        z =
            sphereRadius * sin point.latitude

        texture : Material coordinates { a | normals : () }
        texture =
            Material.matte Color.red

        sphere : Sphere3d Meters coordinates
        sphere =
            Sphere3d.atPoint
                (Point3d.meters x y z)
                (Length.centimeters 5)
    in
    Scene3d.sphereWithShadow texture sphere


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Frame dt ->
            ( { model
                | elapsed = model.elapsed |> Quantity.plus (Duration.milliseconds dt)
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Frame
        , Browser.Events.onResize Resize
        ]



-- toX : Keyboard -> Float
-- toX keyboard =
--     (boolToNumber <| keyboard.right || Set.member "KeyD" keyboard.keys)
--         - (boolToNumber <| keyboard.left || Set.member "KeyA" keyboard.keys)
-- toY : Keyboard -> Float
-- toY keyboard =
--     (boolToNumber <| keyboard.up || Set.member "KeyW" keyboard.keys)
--         - (boolToNumber <| keyboard.down || Set.member "KeyS" keyboard.keys)
-- boolToNumber : Bool -> number
-- boolToNumber b =
--     if b then
--         1
--     else
--         0
-- project : Float -> Float -> Float -> Float -> Float -> Float
-- project fromMin fromMax toMin toMax value =
--     (value - fromMin) / (fromMax - fromMin) * (toMax - toMin) + toMin
