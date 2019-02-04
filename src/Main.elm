module Main exposing (main)


import Array exposing (Array)
import Browser exposing (Document)
-- import Css
import Debug exposing (log)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Grid
import Html exposing (Html)
import Html.Attributes exposing (style)
import Pico exposing (Tile(..), Position, Board)
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Process
import Random exposing (Generator)
import Random.Extra
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs
import Task


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


---- TYPES ----


type alias Model =
    { puzzles : List ( Board, Element.Color )
    , system : System Confetti
    }


type Msg
    = NoOp
    | ToggleTile Position
    | BurstConfetti
    | BurstAtPos ( Float, Float )
    | ParticleMsg (System.Msg Confetti)


type alias Flags =
    ()


---- INITIALIZE ----


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { puzzles =
          [ ( Pico.createBoard heartAnswer 14 12, heartColor )
          , ( Pico.createBoard bunnyAnswer 14 13, bunnyColor )
          ]
      , system = System.init (Random.initialSeed 0)
      }
    , Cmd.none
    -- , Task.perform (\_ -> BurstConfetti) <| Task.succeed ()
    )


---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ puzzles } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )
-- Puzzle
        ToggleTile pos ->
            case puzzles of
                ( board, color ) :: rest ->
                    let
                        updatedBoard = Pico.toggleTile board pos
                        
                        updatedPuzzles =
                            if updatedBoard.isComplete then
                                List.drop 1 puzzles
                            else
                                ( updatedBoard, color ) :: rest
                    in
                        ( { model | puzzles = updatedPuzzles }
                        , Cmd.none
                        )
                [] ->
                    ( model, Cmd.none )
-- Confetti
        BurstConfetti ->
            ( model
              -- it looks much nicer to delay the confetti burst a little bit;
              -- it doesn't all come out at the same time in a real-life party
              -- popper! But we also want to follow the mouse as it comes out,
              -- so we're going to just tell the system to burst four times.
            , Cmd.batch
                [ Process.sleep 0 |> Task.perform (\_ -> BurstAtPos (0, 100))
                , Process.sleep 50 |> Task.perform (\_ -> BurstAtPos (0, 100))
                , Process.sleep 100 |> Task.perform (\_ -> BurstAtPos (0, 100))
                , Process.sleep 150 |> Task.perform (\_ -> BurstAtPos (0, 100))
                ]
            )

        BurstAtPos ( x, y ) ->
            ( { model | system = System.burst (Random.list 25 (particleAt x y)) model.system }
            , Cmd.none
            )

        ParticleMsg particleMsg ->
            ( { model | system = System.update particleMsg model.system }
            , Cmd.none
            )



---- VIEW ----


scaledInt : Int -> Int
scaledInt s =
    floor <| Element.modular 16 1.25 s


tileSize : Int
tileSize =
     scaledInt 7


view : Model -> Document Msg
view { puzzles, system } =
    { title = "Happy Birthday!"
    , body =
          [ Element.layout
              [ Element.inFront <| Element.html <| System.view viewConfetti
                    [ style "width" "100%"
                    , style "height" "100vh"
                    , style "z-index" "1"
                    , style "position" "aboslute"
                    , style "cursor" "none"
                    ]
                    system
              ]
              <| case puzzles of
                  [] ->
                      Element.el
                          [ Element.centerX
                          , Element.centerY
                          , Font.size <| scaledInt 10
                          , Font.family
                                [ Font.external
                                      { url = "https://fonts.googleapis.com/css?family=Indie+Flower"
                                      , name = "Indie Flower"
                                      }
                                ]
                          ]
                          <| Element.text "HAPPY BIRTHDAY!"
                  ( board, color ) :: _ ->
                      Lazy.lazy (viewPico color) board
          ]
    }


viewPico : Element.Color -> Board -> Element Msg
viewPico color { current, width, height } =
    Element.wrappedRow
        [ Element.width <| Element.px (tileSize * width)
        , Element.centerX
        , Element.centerY
        ]
        (Grid.fold2d
            { rows = height
            , cols = width
            }
            (\pos result ->
                let
                    nextTile = case Array.get (Pico.xyToIndex pos width) current of
                        Nothing ->
                            Element.none
                        Just tile ->
                            case tile of
                                Solid ->
                                    viewSolidTile pos color
                                Empty ->
                                    viewEmptyTile pos
                                Spacer ->
                                    viewSpacerTile pos
                                Clue clues ->
                                    viewClueTile clues pos
                in
                    nextTile :: result
            )
            []
            |> List.reverse)


viewSolidTile : Position -> Element.Color -> Element Msg
viewSolidTile pos color =
    Input.button
        [ Element.width <| Element.px tileSize
        , Element.height <| Element.px tileSize
        , Background.color color
        ]
        { onPress = Just <| ToggleTile pos
        , label = Element.none
        }


viewEmptyTile : Position -> Element Msg
viewEmptyTile pos =
    Input.button
        [ Element.width <| Element.px tileSize
        , Element.height <| Element.px tileSize
        , Border.width 1
        , Border.solid
        ]
        { onPress = Just <| ToggleTile pos
        , label = Element.none
        }


viewSpacerTile : Position -> Element Msg
viewSpacerTile (x, y) =
    Element.el
        [ Element.width <| Element.px tileSize
        , Element.height <| Element.px tileSize
        ]
        Element.none


viewClueTile : List Int -> Position -> Element Msg
viewClueTile clues (x, y) =
    Element.el
        [ Element.width <| Element.px tileSize
        , Element.height <| Element.px tileSize
        ]
        <| Element.el
            [ Element.centerX
            , Element.centerY
            ]
            <| Element.text <| "(" ++ (clues |> List.map String.fromInt |> String.join ", ") ++ ")"


---- PUZZLES ----


heartAnswer : Array Tile
heartAnswer =
    Array.fromList
          [ Spacer, Clue [3], Clue [5], Clue [7], Clue [8], Clue [9], Clue [9], Clue [9], Clue [9], Clue [9], Clue [1, 6], Clue [2, 4], Clue [2, 2], Clue [3]
          , Clue [3, 3], Empty, Empty, Solid, Solid, Solid, Empty, Empty, Empty, Solid, Solid, Solid, Empty, Empty
          , Clue [5, 2, 2], Empty, Solid, Solid, Solid, Solid, Solid, Empty, Solid, Solid, Empty, Solid, Solid, Empty
          , Clue [10, 2], Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Empty, Solid, Solid
          , Clue [11, 1], Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Empty, Solid
          , Clue [13], Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid
          , Clue [11], Empty, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Empty
          , Clue [9], Empty, Empty, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Empty, Empty
          , Clue [7], Empty, Empty, Empty, Solid, Solid, Solid, Solid, Solid, Solid, Solid, Empty, Empty, Empty
          , Clue [5], Empty, Empty, Empty, Empty, Solid, Solid, Solid, Solid, Solid, Empty, Empty, Empty, Empty
          , Clue [3], Empty, Empty, Empty, Empty, Empty, Solid, Solid, Solid, Empty, Empty, Empty, Empty, Empty
          , Clue [1], Empty, Empty, Empty, Empty, Empty, Empty, Solid, Empty, Empty, Empty, Empty, Empty, Empty
          ]


heartColor : Element.Color
heartColor =
    Element.rgb 1 0 0


bunnyAnswer : Array Tile
bunnyAnswer =
    Array.fromList
          [ Spacer, Clue [3], Clue [5], Clue [7], Clue [8], Clue [9], Clue [9], Clue [9], Clue [9], Clue [9], Clue [8], Clue [7], Clue [5], Clue [3]
          , Clue [1, 1], Empty, Empty, Empty, Solid, Empty, Solid, Empty, Empty, Empty, Empty, Empty, Empty, Empty
          , Clue [1, 1, 1], Empty, Empty, Solid, Empty, Solid, Empty, Solid, Empty, Empty, Empty, Empty, Empty, Empty
          , Clue [1, 1, 1], Empty, Empty, Solid, Empty, Solid, Empty, Solid, Empty, Empty, Empty, Empty, Empty, Empty
          , Clue [1, 1, 1], Empty, Empty, Solid, Empty, Solid, Empty, Solid, Empty, Empty, Empty, Empty, Empty, Empty
          , Clue [1, 1], Empty, Solid, Empty, Empty, Empty, Empty, Solid, Empty, Empty, Empty, Empty, Empty, Empty
          , Clue [1, 3, 1], Solid, Empty, Empty, Empty, Empty, Empty, Empty, Solid, Solid, Solid, Empty, Solid, Empty
          , Clue [1, 1, 1, 1], Solid, Empty, Empty, Solid, Empty, Empty, Empty, Empty, Empty, Empty, Solid, Empty, Solid
          , Clue [1, 1, 1], Solid, Empty, Empty, Solid, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Solid, Empty
          , Clue [1, 1], Solid, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Solid, Empty
          , Clue [3, 2, 1], Empty, Solid, Solid, Solid, Empty, Empty, Empty, Solid, Solid, Empty, Empty, Solid, Empty
          , Clue [1, 2, 1], Empty, Empty, Solid, Empty, Empty, Solid, Solid, Empty, Empty, Empty, Solid, Empty, Empty
          , Clue [3, 4], Empty, Empty, Solid, Solid, Solid, Empty, Solid, Solid, Solid, Solid, Empty, Empty, Empty
          ]


bunnyColor : Element.Color
bunnyColor =
    Element.rgb 0 0 0


---- CONFETTI ----


type Confetti
    = Square
        { color : Color
        , rotations : Float

        -- we add a rotation offset to our rotations when rendering. It looks
        -- pretty odd if all the particles start or end in the same place, so
        -- this is part of our random generation.
        , rotationOffset : Float
        }
    | Streamer
        { color : Color
        , length : Int
        }


type Color
    = Red
    | Pink
    | Yellow
    | Green
    | Blue


{-| Generate a confetti square, using the color ratios seen in Mutant Standard.
-}
genSquare : Generator Confetti
genSquare =
    Random.map3
        (\color rotations rotationOffset ->
            Square
                { color = color
                , rotations = rotations
                , rotationOffset = rotationOffset
                }
        )
        (Random.weighted
            ( 1 / 5, Red )
            [ ( 1 / 5, Pink )
            , ( 1 / 5, Yellow )
            , ( 2 / 5, Green )
            ]
        )
        (normal 1 1)
        (Random.float 0 1)


{-| Generate a streamer, again using those color ratios
-}
genStreamer : Generator Confetti
genStreamer =
    Random.map2
        (\color length ->
            Streamer
                { color = color
                , length = round (abs length)
                }
        )
        (Random.uniform Pink [ Yellow, Blue ])
        (normal 25 10 |> Random.map (max 10))


{-| Generate confetti according to the ratios in Mutant Standard's tada emoji.
-}
genConfetti : Generator Confetti
genConfetti =
    Random.Extra.frequency
        ( 5 / 8, genSquare )
        [ ( 3 / 8, genStreamer ) ]


{-| We're going to emit particles at the mouse location, so we pass those
parameters in here and use them without modification.
-}
particleAt : Float -> Float -> Generator (Particle Confetti)
particleAt x y =
    Particle.init genConfetti
        |> Particle.withLifetime (normal 1.5 0.25)
        |> Particle.withLocation (Random.constant { x = x, y = y })
        -- our direction is determined by the angle of the party popper cone
        -- (about 47°) as well as it's width (about 60°). We use a normal
        -- distribution here so that most of the confetti will come out in the
        -- same place, with falloff to the sides. We want most of the confetti
        -- to show up in the center 30°, so the standard deviation of the
        -- distribution should be 15°.
        |> Particle.withDirection (normal (degrees 47) (degrees 15))
        |> Particle.withSpeed (normal 600 100)
        |> Particle.withGravity 980
        |> Particle.withDrag
            (\confetti ->
                { density = 0.001226
                , coefficient =
                    case confetti of
                        Square _ ->
                            1.15

                        Streamer _ ->
                            0.85
                , area =
                    case confetti of
                        Square _ ->
                            1

                        Streamer { length } ->
                            toFloat length / 10
                }
            )


viewConfetti : Particle Confetti -> Svg msg
viewConfetti particle =
    let
        lifetime =
            Particle.lifetimePercent particle

        -- turns out that opacity is pretty expensive for browsers to calculate,
        -- and will slow down our framerate if we change it too much. So while
        -- we *could* do this with, like, a bezier curve or something, we
        -- actually want to just keep it as stable as possible until we actually
        -- need to fade out at the end.
        opacity =
            if lifetime < 0.1 then
                lifetime * 10

            else
                1
    in
    case Particle.data particle of
        Square { color, rotationOffset, rotations } ->
            Svg.rect
                [ SAttrs.width "10px"
                , SAttrs.height "10px"
                , SAttrs.x "-5px"
                , SAttrs.y "-5px"
                , SAttrs.rx "2px"
                , SAttrs.ry "2px"
                , SAttrs.fill (fill color)
                , SAttrs.stroke "black"
                , SAttrs.strokeWidth "4px"
                , SAttrs.opacity <| String.fromFloat opacity
                , SAttrs.transform <|
                    "rotate("
                        ++ String.fromFloat ((rotations * lifetime + rotationOffset) * 360)
                        ++ ")"
                ]
                []

        Streamer { color, length } ->
            Svg.rect
                [ SAttrs.height "10px"
                , SAttrs.width <| String.fromInt length ++ "px"
                , SAttrs.y "-5px"
                , SAttrs.rx "2px"
                , SAttrs.ry "2px"
                , SAttrs.fill (fill color)
                , SAttrs.stroke "black"
                , SAttrs.strokeWidth "4px"
                , SAttrs.opacity <| String.fromFloat opacity
                , SAttrs.transform <|
                    "rotate("
                        ++ String.fromFloat (Particle.directionDegrees particle)
                        ++ ")"
                ]
                []


fill : Color -> String
fill color =
    case color of
        Red ->
            "#D72D35"

        Pink ->
            "#F2298A"

        Yellow ->
            "#F2C618"

        Green ->
            "#2ACC42"

        Blue ->
            "#37CBE8"