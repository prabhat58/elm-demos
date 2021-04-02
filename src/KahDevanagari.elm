module KahDevanagari exposing (main)

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
          line (-30, 0) (30, 0) 
            |> outlined (solid 2) black 
            |> move(0, 30) 
            |> notifyLeaveAt Notify,
          line (0, 0) (0, -50) 
            |> outlined (solid 2) black 
            |> move(0, 30)
            |> notifyLeaveAt Notify,
          wedge 15 0.5 
            |> outlined (solid 2) black 
            |> rotate(degrees 180) 
            |> move(0, 6)
            |> notifyLeaveAt Notify,
          curve (0, 21) [Pull (40, 15) (10, -8)] 
            |> outlined (solid 2) black 
            |> move(0, 0)
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
                                    || (round (abs x) == 0 && round (abs y) == 20 && Set.member (round (abs y)) model.visited == False)
                                    || (round (abs x) == 1 && round (abs y) == 9 && Set.member (round (abs y)) model.visited == False)
                                    || (round (abs x) == 10 && round (abs y) == 8 && Set.member (round (abs x)) model.visited == False))
                                       then
                                       1

                                   else
                                    0
                                  )
             , visited = 
                if (round (abs x) == 30 && round (abs y) == 30) 
                   then
                     Set.insert (round (abs x)) model.visited
                else if (round (abs x) == 0 && round (abs y) == 20) 
                   then
                    Set.insert (round (abs y)) model.visited
                else if (round (abs x) == 1 && round (abs y) == 9)
                   then
                    Set.insert (round (abs y)) model.visited
                else if (round (abs x) == 10 && round (abs y) == 8)
                   then 
                    Set.insert (round (abs x)) model.visited  
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