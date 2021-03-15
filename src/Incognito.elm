module Incognito exposing(..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (appWithTick, AppWithTick, GetKeyState)
import Url exposing (Url)
import Browser exposing (UrlRequest)
import Array
import Dict

type Msg = 
      Tick Float GetKeyState
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | CorrectTest

type alias Model = {
      counter : Int
      , time : Float
      , successful : Dict.Dict Int Bool
    }

init : Model
init =
        {
            time = 0
        ,   counter = 0
        ,   successful = Dict.empty
        }

title : Model -> String
title model =
    "Incognito counter"


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Tick t _ -> ( { model | time = t }, Cmd.none )
        OnUrlRequest _ -> (model, Cmd.none) 
        OnUrlChange url -> (model, Cmd.none)
        CorrectTest -> ( { model | successful = Dict.insert model.counter True model.successful
                                 , counter = clamp 0 (Array.length labels) (model.counter+1) 
                                 } , Cmd.none)

labels = Array.fromList
    [
        "circle 15 |> outlined (solid 1) black |> move (-20, -20)",
        "curve (-2, 0) [Pull (6, 5) (14, 0)] |> outlined (solid 3) black |> move (-6, -11)",
        "circle 15 |> outlined (solid 1) black |> move (20, -20)",
        "line (-40, 0) (40, 0) |> outlined (solid 1) black |> move(0, -2)",
        "ngon 3 28 |> outlined (solid 1) black |> rotate(degrees 90) |> move(0, 15)"    ]

tests = 
    [
        circle 15 |> outlined (solid 1) black |> move (-20, -20),
        curve (-2, 0) [Pull (6, 5) (14, 0)] |> outlined (solid 3) black |> move (-6, -11),
        circle 15 |> outlined (solid 1) black |> move (20, -20),
        line (-40, 0) (40, 0) |> outlined (solid 1) black |> move(0, -2),
        ngon 3 28 |> outlined (solid 1) black |> rotate(degrees 90) |> move(0, 15)
    ]

view : Model -> Collage Msg
view model = 
    let
        (name) = 
            case Array.get model.counter labels of
                Just t -> t
                Nothing -> ("")
            
    in
            
    collage 192 128 
    (if model.counter < Array.length labels then
        List.concat [
            [
             square 200 |> filled lightBrown 
             , text name |> size 4 |> centered |> fixedwidth |> filled black |> move(0, 55)
             , roundedRect 20 10 5 |> filled green |> notifyTap CorrectTest |> move(2, -50)
            ],
            List.take (model.counter+1) tests
        ]
        
    else [renderSuccesses model.successful]
        )

renderSuccesses : Dict.Dict Int Bool -> Shape Msg
renderSuccesses successDict =
    group
        (List.map (\(n,s) -> 
            let
                label = case Array.get n labels of 
                        Just (name) -> group [text name 
                                            |> size 2 
                                            |> fixedwidth 
                                            |> filled (if s then green else red) 
                                            |> move (-60,toFloat n * -3 + 60)
                                            ]
                        Nothing -> group [] 
            in
                label
            ) <| Dict.toList successDict)

main : AppWithTick () Model Msg
main = appWithTick Tick
    { init = \_ url key -> (init, Cmd.none)
    , update = update
    , view = \model -> { body = view model, title = title model }
    , subscriptions = \_ -> Sub.none
    , onUrlRequest = OnUrlRequest
    , onUrlChange = OnUrlChange
    }