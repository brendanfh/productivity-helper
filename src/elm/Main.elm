module Main exposing (..)

import Date as Date
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Material.Icon as Icon
import Models exposing (..)
import Sounds as Sounds
import Storage as Storage
import Time as Time
import Task as Task


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


type alias Model =
    { currentTime : Time.Time
    , state : State
    , workingTime : String
    , breakTime : String
    , tasks : List Task
    , newTaskText : String
    }


init : ( Model, Cmd Msg )
init =
    ( { currentTime = -1
      , state = Working
      , workingTime = ""
      , breakTime = ""
      , tasks = []
      , newTaskText = ""
      }
    , Cmd.batch
        [ Task.perform UpdateTime Time.now
        , Storage.loadFromLocalStorage ()
        ]
    )



-- UPDATE


type TaskAction
    = Delete
    | Complete
    | Uncomplete
    | Edit Editing
    | ChangeText String


type Msg
    = NoOp
    | LoadTasks (List SaveableTask)
    | UpdateTime Time.Time
    | ChangeTaskText String
    | AddTask
    | ModifyTask TaskAction TaskId


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
            BreakTime


determineTimes : Time.Time -> ( String, String )
determineTimes t =
    let
        date =
            Date.fromTime t

        m =
            (Date.minute date) % 30

        s =
            Date.second date

        state =
            determineState t

        pad =
            String.padLeft 2 '0'

        time1 =
            if state == Working then
                (pad <| toString (24 - m)) ++ ":" ++ (pad <| toString (59 - s))
            else
                "25:00"

        time2 =
            if state == BreakTime then
                (pad <| toString (29 - m)) ++ ":" ++ (pad <| toString (59 - s))
            else
                "05:00"
    in
        ( time1, time2 )


modifyTask : TaskAction -> TaskId -> Task -> List Task
modifyTask taskAction taskId1 (( taskId2, taskText, editingTask ) as task) =
    if taskId1 /= taskId2 then
        [ task ]
    else
        let
            ( taskGroup, tid ) =
                taskId2
        in
            case taskAction of
                Delete ->
                    []

                Uncomplete ->
                    [ ( ( UncompletedGroup, tid ), taskText, False ) ]

                Complete ->
                    [ ( ( CompletedGroup, tid ), taskText, False ) ]

                Edit edit ->
                    [ ( ( taskGroup, tid ), taskText, edit ) ]

                ChangeText t ->
                    [ ( ( taskGroup, tid ), t, editingTask ) ]


saveTasks : List Task -> Cmd Msg
saveTasks tasks =
    Storage.saveToLocalStorage <| List.map toSaveableTask tasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadTasks tasks ->
            let
                newTasks =
                    List.map fromSaveableTask tasks
            in
                { model | tasks = newTasks } ! []

        UpdateTime t ->
            let
                newState =
                    determineState t

                soundCmd =
                    if newState /= model.state then
                        Sounds.playChime ()
                    else
                        Cmd.none

                ( time1, time2 ) =
                    determineTimes t
            in
                { model
                    | currentTime = t
                    , state = newState
                    , workingTime = time1
                    , breakTime = time2
                }
                    ! [ soundCmd ]

        AddTask ->
            if model.newTaskText == "" then
                model ! [ saveTasks model.tasks ]
            else
                let
                    taskId =
                        model.tasks
                            |> List.map (\( ( _, tid ), _, _ ) -> tid)
                            |> List.maximum
                            |> Maybe.withDefault 0
                            |> ((+) 1)

                    newTask =
                        ( ( UncompletedGroup, taskId ), model.newTaskText, False )

                    newTasks =
                        newTask :: model.tasks
                in
                    { model | newTaskText = "", tasks = newTasks }
                        ! [ saveTasks newTasks ]

        ModifyTask action taskId ->
            let
                newTasks =
                    List.concatMap (modifyTask action taskId) model.tasks
            in
                { model
                    | tasks = newTasks
                }
                    ! [ saveTasks newTasks ]

        ChangeTaskText t ->
            { model | newTaskText = t } ! []

        NoOp ->
            model ! []


when : Bool -> Html.Attribute msg -> Html.Attribute msg
when cond attr =
    if cond then
        attr
    else
        HA.class ""


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter : Int -> Json.Decoder msg
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        HE.on "keydown" <| Json.andThen isEnter HE.keyCode


obscurer : State -> Html Msg
obscurer state =
    case state of
        Working ->
            (div
                [ HA.class "obscure" ]
                []
            )

        _ ->
            (text "")


viewAction : TaskAction -> Html Msg
viewAction action =
    case action of
        Delete ->
            Icon.i "delete"

        Complete ->
            Icon.i "done"

        Uncomplete ->
            Icon.i "arrow_back"

        Edit _ ->
            Icon.i "create"

        ChangeText _ ->
            text ""


viewTask : ( TaskAction, TaskAction ) -> Task -> Html Msg
viewTask ( action1, action2 ) ( taskId, taskText, editingTask ) =
    let
        viewNormalTask =
            div
                [ HA.class "task" ]
                [ div
                    [ HA.class "task-action1"
                    , HE.onClick (ModifyTask action1 taskId)
                    ]
                    [ viewAction action1 ]
                , div
                    [ HA.class "task-text" ]
                    [ text taskText ]
                , div
                    [ HA.class "task-action2"
                    , HE.onClick (ModifyTask action2 taskId)
                    ]
                    [ viewAction action2 ]
                ]

        viewEditTask =
            div
                [ HA.class "task" ]
                [ div
                    [ HA.class "task-action1"
                    , HE.onClick (ModifyTask (Edit False) taskId)
                    ]
                    [ viewAction Uncomplete ]
                , div
                    [ HA.class "task-text" ]
                    [ input
                        [ HE.onInput (\t -> (ModifyTask (ChangeText t) taskId))
                        , onEnter (ModifyTask (Edit False) taskId)
                        , HA.value taskText
                        ]
                        [ text taskText
                        ]
                    ]
                , div
                    [ HA.class "task-action2"
                    , HE.onClick (ModifyTask (Edit False) taskId)
                    ]
                    [ viewAction Complete ]
                ]
    in
        if editingTask then
            viewEditTask
        else
            viewNormalTask


viewTasks : TaskGroup -> List Task -> List (Html Msg)
viewTasks taskGroup tasks =
    let
        usedTasks =
            List.filter (\( ( tg, _ ), _, _ ) -> tg == taskGroup) tasks
    in
        case taskGroup of
            UncompletedGroup ->
                List.map (viewTask ( Complete, Edit True )) usedTasks

            CompletedGroup ->
                List.map (viewTask ( Uncomplete, Delete )) usedTasks


viewAddTask : String -> Html Msg
viewAddTask taskText =
    div
        [ HA.class "task" ]
        [ div
            [ HA.class "task-text" ]
            [ input
                [ HA.placeholder "Add a goal"
                , HE.onInput ChangeTaskText
                , onEnter AddTask
                ]
                [ text taskText
                ]
            ]
        , div
            [ HA.class "task-action2"
            , HE.onClick AddTask
            ]
            [ Icon.i "add" ]
        ]


view : Model -> Html Msg
view model =
    div
        [ HA.id "main"
        , when (model.state == Working) (HA.class "working")
        ]
        [ div
            [ HA.class "left timer"
            , when (model.state == Working) (HA.class "enabled")
            ]
            [ div
                [ HA.class "time-display" ]
                [ text model.workingTime ]
            ]
        , div
            [ HA.class "left tasks"
            ]
            [ div
                [ HA.class "task-header" ]
                [ text "To Do" ]
            , div
                [ HA.class "task-list" ]
                (List.concat
                    [ (viewTasks UncompletedGroup model.tasks)
                    , [ viewAddTask model.newTaskText ]
                    ]
                )

            -- , obscurer model.state
            ]
        , div
            [ HA.class "right timer"
            , when
                (model.state == BreakTime)
                (HA.class "enabled")
            ]
            [ div
                [ HA.class "time-display" ]
                [ text model.breakTime ]
            ]
        , div
            [ HA.class "right tasks"
            ]
            [ div
                [ HA.class "task-header" ]
                [ text "Completed" ]
            , div
                [ HA.class "task-list" ]
                (viewTasks CompletedGroup model.tasks)

            -- , obscurer model.state
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second UpdateTime
        , Storage.localStorageSubscription LoadTasks
        ]
