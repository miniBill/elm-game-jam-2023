module MyPlayground exposing (Shape, polygon, render)

import Playground exposing (Color, Computer)


type Shape
    = Shape (Computer -> Playground.Shape)


render : Computer -> List Shape -> List Playground.Shape
render computer shapes =
    List.map (\(Shape shape) -> shape computer) shapes


polygon : Color -> List ( Float, Float ) -> Shape
polygon color points =
    Shape (\_ -> Playground.polygon color points)
