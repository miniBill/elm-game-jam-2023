module Main exposing (Model, main)

import MyPlayground exposing (polygon, render)
import Playground exposing (Computer, Keyboard, Msg, Playground, Shape, game, red)
import Set


type alias Model =
    { points :
        List
            { latitude : Float
            , longitude : Float
            }
    }


main : Program () (Playground Model) Msg
main =
    game view update init


init : Model
init =
    { points = [] }


view : Computer -> Model -> List Shape
view computer { points } =
    let
        mx : Float
        mx =
            computer.mouse.x

        my : Float
        my =
            computer.mouse.y
    in
    [ polygon red
        [ ( mx, my )
        , ( mx + 2, my - 20 )
        , ( mx + 10, my - 15 )
        ]
    ]
        |> render computer


update : Computer -> Model -> Model
update computer model =
    -- let
    --     dt : Float
    --     dt =
    --         toFloat computer.time.delta
    --     newX : Float
    --     newX =
    --         computer.mouse.x
    --     newY : Float
    --     newY =
    --         computer.mouse.y
    -- in
    model


toX : Keyboard -> Float
toX keyboard =
    (boolToNumber <| keyboard.right || Set.member "KeyD" keyboard.keys)
        - (boolToNumber <| keyboard.left || Set.member "KeyA" keyboard.keys)


toY : Keyboard -> Float
toY keyboard =
    (boolToNumber <| keyboard.up || Set.member "KeyW" keyboard.keys)
        - (boolToNumber <| keyboard.down || Set.member "KeyS" keyboard.keys)


boolToNumber : Bool -> number
boolToNumber b =
    if b then
        1

    else
        0


screenToWorld : Computer -> ( Float, Float ) -> ( Float, Float )
screenToWorld computer ( x, y ) =
    if computer.screen.width > computer.screen.height then
        ( x, y )

    else
        ( x, y )


project : Float -> Float -> Float -> Float -> Float -> Float
project fromMin fromMax toMin toMax value =
    (value - fromMin) / (fromMax - fromMin) * (toMax - toMin) + toMin
