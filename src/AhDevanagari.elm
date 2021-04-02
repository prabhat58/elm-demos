module AhDevanagari exposing (main)

import GraphicSVG exposing (..)
import Set exposing (Set)


type alias Model =
    { 
      counter : Int,
      visited : Set Int
    }

initModel : Model
initModel =
    {
     counter = List.length tests,
     visited = Set.empty
    }

tests = 
      [ 
          line (0, 0) (30, 0) 
            |> outlined (solid 2) black 
            |> move(0, 30) 
            |> notifyLeaveAt Notify,
          line (0, 0) (0, -50) 
            |> outlined (solid 2) black 
            |> move(20, 30)
            |> notifyLeaveAt Notify,
          line (-15, 0) (20, 0) 
            |> outlined (solid 2) black 
            |> move(0, 5)
            |> notifyLeaveAt Notify,
          curve (-30, 10) [Pull (9, 0) (-30, -10)] 
            |> outlined (solid 2) black
            |> move(15, 15) 
            |> notifyLeaveAt Notify,
         curve (-30, 10) [Pull (9, 0) (-30, -10)] 
            |> outlined (solid 2) black
            |> move(15, -5) 
            |> notifyLeaveAt Notify
      ]


type Msg
    = Notify ( Float, Float )
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of

        Notify ( x, y ) -> 

            ( {
               counter =
                            model.counter
                                - (if (
                                    (round (abs x) == 30 && round (abs y) == 30 && Set.member (round (abs x)) model.visited == False)
                                    || (round (abs x) == 20 && round (abs y) == 20 && Set.member (round (abs y)) model.visited == False)
                                    || (round (abs x) == 21 && round (abs y) == 5 && Set.member (round (abs x)) model.visited == False)
                                    || (round (abs x) == 15 && round (abs y) == 5 && Set.member (round (abs y)) model.visited == False)
                                    || (round (abs x) == 15 && round (abs y) == 15 && Set.member (round (abs y)) model.visited == False))
                                       then
                                       1

                                   else
                                    0
                                  )
             , visited = 
                if (round (abs x) == 30 && round (abs y) == 30) 
                   then
                     Set.insert (round (abs x)) model.visited
                else if (round (abs x) == 20 && round (abs y) == 20) 
                   then
                    Set.insert (round (abs y)) model.visited
                else if (round (abs x) == 21 && round (abs y) == 5)
                   then
                    Set.insert (round (abs x)) model.visited
                else if (round (abs x) == 15 && round (abs y) == 5)
                   then 
                    Set.insert (round (abs y)) model.visited  
                else if (round (abs x) == 15 && round (abs y) == 15) 
                   then
                    Set.insert (round (abs y)) model.visited
                else
                    model.visited
           } ) 


        _ ->
            model


view : Model -> GraphicSVG Msg
view model =

    collage 100 100
     (
       if model.counter > 0 then
        (
           List.append 

             [ 
                text ("To go:  " ++ String.fromInt model.counter)
                  |> size 10
                  |> bold
                  |> centered
                  |> filled green
                  |> addOutline (solid 0.5) black
                  |> move (0, 42),
                text ("Move the mouse slowly from one end to the other (left to right | top to bottom) to trace a shape.")
                  |> size 2.5 
                  |> bold
                  |> centered 
                  |> filled black
                  |> move (0, 35)
              ]

              tests
        )
        else 
          [
            text ("Well done, you traced all the shapes!!!!")
              |> size 6
              |> bold
              |> centered
              |> filled orange
              |> addOutline (solid 0.2) blue
          ]
     )


main : App () Model Msg
main =
    app
        { init = \_ _ _ -> ( initModel, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = 
            \model ->
                { title = ""
                , body = view model
                }
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }