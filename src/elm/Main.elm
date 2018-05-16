module Main exposing (..)

import Date as Date
import Html exposing (..)
import Html.Attributes as HA
import Time as Time


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type State
    = Working
    | DownTime
    | Undefined


type alias Model =
    { currentTime : Time.Time
    , state : State
    }


init : ( Model, Cmd Msg )
init =
    ( { currentTime = -1
      , state = Undefined
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | UpdateTime Time.Time


determineState : Time.Time -> State
determineState t =
    let
        date =
            Date.fromTime t

        m =
            Date.minute date

        mm30 =
            m % 30
    in
        if mm30 <= 24 then
            Working
        else
            DownTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime t ->
            let
                newState =
                    determineState t
            in
                { model | currentTime = t, state = newState } ! []

        NoOp ->
            model ! []


viewHalf : String -> Model -> Html Msg
viewHalf side model =
    div
        [ HA.class <| side ++ " half"
        ]
        [ div
            [ HA.class "time-display" ]
            [ text "99:99" ]
        , div
            [ HA.class "task-header" ]
            [ text "Goals" ]
        , div
            [ HA.class "task-list" ]
            ([ div
                [ HA.class "task" ]
                [ text "Finish this project" ]
             , div
                [ HA.class "task" ]
                [ text "No really, finish it" ]
             ]
                ++ List.repeat 100 (div [ HA.class "task" ] [ text "repeated" ])
            )
        ]


view : Model -> Html Msg
view model =
    div
        [ HA.id "main"
        ]
        [ viewHalf "left enabled" model
        , viewHalf "right enabled" model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second UpdateTime
