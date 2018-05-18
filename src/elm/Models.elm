module Models exposing (..)


type State
    = Working
    | BreakTime


type TaskGroup
    = UncompletedGroup
    | CompletedGroup


type alias TaskId =
    ( TaskGroup, Int )


type alias Editing =
    Bool


type alias Task =
    ( TaskId, String, Editing )


type alias SaveableTaskId =
    ( Int, Int )


type alias SaveableTask =
    ( SaveableTaskId, String )



--Converters


toSaveableTask : Task -> SaveableTask
toSaveableTask ( ( taskGroup, taskId ), taskText, edit ) =
    let
        taskGroupNum =
            case taskGroup of
                CompletedGroup ->
                    1

                UncompletedGroup ->
                    0
    in
        ( ( taskGroupNum, taskId ), taskText )


fromSaveableTask : SaveableTask -> Task
fromSaveableTask ( ( taskGroupNum, taskId ), taskText ) =
    let
        taskGroup =
            case taskGroupNum of
                0 ->
                    UncompletedGroup

                1 ->
                    CompletedGroup

                _ ->
                    UncompletedGroup
    in
        ( ( taskGroup, taskId ), taskText, False )
