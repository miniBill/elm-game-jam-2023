module Main exposing (Model, main)

import Playground exposing (Computer, Msg, Playground, Shape, game, move, red, square, toX, toY)


type alias Model =
    ( Float, Float )


main : Program () (Playground Model) Msg
main =
    game view update ( 0, 0 )


view : a -> Model -> List Shape
view computer ( x, y ) =
    [ square red 40
        |> move x y
    ]


update : Computer -> Model -> Model
update computer ( x, y ) =
    ( x + toX computer.keyboard
    , y + toY computer.keyboard
    )


reproject : Float -> Float -> Float -> Float -> Float -> Float
reproject fromMin fromMax toMin toMax value =
    (value - fromMin) / (fromMax - fromMin) * (toMax - toMin) + toMin
