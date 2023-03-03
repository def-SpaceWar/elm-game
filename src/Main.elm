module Main exposing (main, update, view)

import Playground exposing (..)


type alias Model =
    { health : Int
    , playerPosition : Position
    , playerSpeed : Vector
    , playerRotation : Float
    , playerRotationSpeed : Float
    }


type alias Position =
    { x : Float
    , y : Float
    , w : Maybe Float
    , h : Maybe Float
    }


type alias Vector =
    { x : Float
    , y : Float
    }


width : Float
width =
    1200


height : Float
height =
    800


topLeftCorner : Vector
topLeftCorner =
    { x = -width / 2
    , y = height / 2
    }


topRightCorner : Vector
topRightCorner =
    { x = width / 2
    , y = height / 2
    }


bottomLeftCorner : Vector
bottomLeftCorner =
    { x = -width / 2
    , y = -height / 2
    }


bottomRightCorner : Vector
bottomRightCorner =
    { x = width / 2
    , y = -height / 2
    }


maybeOrDefault : Maybe a -> a -> a
maybeOrDefault maybe default =
    case maybe of
        Nothing ->
            default

        Just value ->
            value


main =
    game view
        update
        { health = 100
        , playerPosition =
            { x = 0
            , y = 0
            , w = Just 50
            , h = Nothing
            }
        , playerSpeed =
            { x = 0
            , y = 0
            }
        , playerRotation = 0
        , playerRotationSpeed = 0
        }


view : Computer -> Model -> List Shape
view _ model =
    let
        playerWidth =
            maybeOrDefault model.playerPosition.w 50

        trailSize =
            sqrt (model.playerSpeed.x ^ 2 + model.playerSpeed.y ^ 2) * 5

        leftTrailSize =
            trailSize / max 1 (model.playerRotationSpeed / 2)

        rightTrailSize =
            trailSize / max 1 (-model.playerRotationSpeed / 2)

        leftThrusterOffset =
            -25 + leftTrailSize / 9

        rightThrusterOffset =
            -25 + rightTrailSize / 9

        borderThickness =
            100
    in
    -- Player
    [ triangle darkRed playerWidth
        |> move model.playerPosition.x model.playerPosition.y
        |> rotate (model.playerRotation - 90)
    , triangle lightRed (playerWidth - 10)
        |> move model.playerPosition.x model.playerPosition.y
        |> rotate (model.playerRotation - 90)
    , triangle blue 25
        |> move (model.playerPosition.x + cos (degrees model.playerRotation) * 20) (model.playerPosition.y + sin (degrees model.playerRotation) * 20)
        |> rotate (model.playerRotation + 90)
    , triangle lightBlue 15
        |> move (model.playerPosition.x + cos (degrees model.playerRotation) * 20) (model.playerPosition.y + sin (degrees model.playerRotation) * 20)
        |> rotate (model.playerRotation + 90)

    -- Rocket Trail
    , polygon orange [ ( 0, -10 ), ( 0, 10 ), ( -leftTrailSize, 0 ) ]
        |> move (model.playerPosition.x + cos (degrees model.playerRotation) * -25) (model.playerPosition.y + sin (degrees model.playerRotation) * -25)
        |> rotate model.playerRotation
        |> move (-15 * sin (degrees model.playerRotation)) (15 * cos (degrees model.playerRotation))
    , polygon orange [ ( 0, -10 ), ( 0, 10 ), ( -rightTrailSize, 0 ) ]
        |> move (model.playerPosition.x + cos (degrees model.playerRotation) * -25) (model.playerPosition.y + sin (degrees model.playerRotation) * -25)
        |> rotate model.playerRotation
        |> move (15 * sin (degrees model.playerRotation)) (-15 * cos (degrees model.playerRotation))

    -- Rocket Boosters
    , triangle (rgb 128 128 128) 15
        |> move (model.playerPosition.x + cos (degrees model.playerRotation) * leftThrusterOffset) (model.playerPosition.y + sin (degrees model.playerRotation) * leftThrusterOffset)
        |> rotate (model.playerRotation - 90)
        |> move (-15 * sin (degrees model.playerRotation)) (15 * cos (degrees model.playerRotation))
    , triangle darkGrey 5
        |> move (model.playerPosition.x + cos (degrees model.playerRotation) * leftThrusterOffset) (model.playerPosition.y + sin (degrees model.playerRotation) * leftThrusterOffset)
        |> rotate (model.playerRotation - 90)
        |> move (-15 * sin (degrees model.playerRotation)) (15 * cos (degrees model.playerRotation))
    , triangle (rgb 128 128 128) 15
        |> move (model.playerPosition.x + cos (degrees model.playerRotation) * rightThrusterOffset) (model.playerPosition.y + sin (degrees model.playerRotation) * rightThrusterOffset)
        |> rotate (model.playerRotation - 90)
        |> move (15 * sin (degrees model.playerRotation)) (-15 * cos (degrees model.playerRotation))
    , triangle darkGrey 5
        |> move (model.playerPosition.x + cos (degrees model.playerRotation) * rightThrusterOffset) (model.playerPosition.y + sin (degrees model.playerRotation) * rightThrusterOffset)
        |> rotate (model.playerRotation - 90)
        |> move (15 * sin (degrees model.playerRotation)) (-15 * cos (degrees model.playerRotation))

    -- Game Frame (end)
    , rectangle gray width borderThickness
        |> move 0 (topLeftCorner.y + 50)
    , rectangle gray width borderThickness
        |> move 0 (bottomLeftCorner.y - 50)
    , rectangle gray borderThickness (height + borderThickness + 100)
        |> move (topRightCorner.x + 50) 0
    , rectangle gray borderThickness (height + borderThickness + 100)
        |> move (bottomLeftCorner.x - 50) 0
    ]


update : Computer -> Model -> Model
update computer model =
    let
        playerPosition =
            model.playerPosition

        playerSpeed =
            model.playerSpeed

        dragConstant =
            0.93

        rotationSpeed =
            0.5
    in
    { model
        | playerPosition =
            { playerPosition
                | x = playerPosition.x + playerSpeed.x
                , y = playerPosition.y + playerSpeed.y
            }
        , playerSpeed =
            { playerSpeed
                | x = playerSpeed.x * dragConstant + cos (degrees model.playerRotation) * toY computer.keyboard / max 1 (abs model.playerRotationSpeed / 3)
                , y = playerSpeed.y * dragConstant + sin (degrees model.playerRotation) * toY computer.keyboard / max 1 (abs model.playerRotationSpeed / 3)
            }
        , playerRotation = model.playerRotation + model.playerRotationSpeed
        , playerRotationSpeed = (model.playerRotationSpeed - toX computer.keyboard * rotationSpeed) * dragConstant
    }
        |> unPrecisify 3
        |> wrapAroundScreen


unPrecisify : Float -> Model -> Model
unPrecisify precision model =
    let
        playerSpeed =
            model.playerSpeed

        unPrecise n =
            if abs n < (0.1 ^ precision) then
                0

            else
                n
    in
    { model
        | playerSpeed =
            { playerSpeed
                | x = unPrecise playerSpeed.x
            }
    }


wrapAroundScreen : Model -> Model
wrapAroundScreen model =
    let
        playerPosition =
            model.playerPosition

        wrapX : Float -> Float
        wrapX currentX =
            if currentX > topRightCorner.x then
                topLeftCorner.x

            else if currentX < topLeftCorner.x then
                topRightCorner.x

            else
                currentX

        wrapY : Float -> Float
        wrapY currentY =
            if currentY > topRightCorner.y then
                bottomRightCorner.y

            else if currentY < bottomLeftCorner.y then
                topLeftCorner.y

            else
                currentY
    in
    { model
        | playerPosition =
            { playerPosition
                | x = wrapX playerPosition.x
                , y = wrapY playerPosition.y
            }
    }
