port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text, ul, li, input, a)
import Html.Attributes exposing (src, style, placeholder, value, type_, name)
import List exposing (filter, map, length, indexedMap,maximum, sortBy, head)
import Tuple exposing(first,second )
import Maybe exposing (withDefault)
import Html.Events exposing (onInput, onClick)
import Json.Encode exposing (encode)
import Json.Decode


---- MODEL ----


type alias Model =
    {
        todoList:List Todo,
        lavelList:List Lavel,
        todoAddTextBox:String
    }

toJsonModel : Model -> Json.Encode.Value
toJsonModel model =
    Json.Encode.object
    [
        ("todoList", Json.Encode.list toJsonTodo model.todoList),
        ("lavelList", Json.Encode.list toJsonLavel model.lavelList)
    ]
fromJsonModel : Json.Decode.Decoder Model
fromJsonModel =
    Json.Decode.map3 Model
        (Json.Decode.field "todoList" (Json.Decode.list fromJsonTodo))
        (Json.Decode.field "lavelList" (Json.Decode.list fromJsonLavel))
        (Json.Decode.succeed "")

type alias Todo =
    {
        id:Int,
        title:String,
        content:String,
        priority:Int,
        lavelId:Int
    }
toJsonTodo : Todo -> Json.Encode.Value
toJsonTodo todo =
    Json.Encode.object
    [
        ("id", Json.Encode.int todo.id),
        ("title", Json.Encode.string todo.title),
        ("content", Json.Encode.string todo.content),
        ("priority", Json.Encode.int todo.priority),
        ("lavelId", Json.Encode.int todo.lavelId)
    ]
fromJsonTodo : Json.Decode.Decoder Todo
fromJsonTodo =
    Json.Decode.map5 Todo
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
        (Json.Decode.field "priority" Json.Decode.int)
        (Json.Decode.field "lavelId" Json.Decode.int)

type alias Lavel = {
        id:Int,
        lavel:String
    }
toJsonLavel : Lavel -> Json.Encode.Value
toJsonLavel lavel =
    Json.Encode.object
    [
        ("id", Json.Encode.int lavel.id),
        ("lavel", Json.Encode.string lavel.lavel)
    ]
fromJsonLavel : Json.Decode.Decoder Lavel
fromJsonLavel =
    Json.Decode.map2 Lavel
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "lavel" Json.Decode.string)

initModel = {
        todoList=[],
        lavelList=[
            {id=0,lavel="Todo"},
            {id=1,lavel="Doing"},
            {id=2,lavel="Done"}],
        todoAddTextBox=""
    }

init : () -> ( Model, Cmd Msg )
init _ =
    (initModel , Cmd.none )



---- UPDATE ----

type Msg
    = LavelListLavelChange Int String
    | TodoAdd
    | TodoDel Int
    | TodoPriorityChange Int Int
    | TodoTitleChange Int String
    | TodoContentChange Int String
    | TodoAddTextBoxChange String
    | TodoLevelUp Int
    | TodoLevelDown Int
    | GetUpdate String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let

        updateJsonCmd : Model -> Cmd Msg
        updateJsonCmd sendModel = sendUpdate (encode 0 (toJsonModel sendModel))

        send : Model -> ( Model, Cmd Msg )
        send newModel = (newModel, updateJsonCmd newModel)

        doNotSend : Model -> ( Model, Cmd Msg )
        doNotSend newModel = (newModel, Cmd.none)
    in

    case msg of
        LavelListLavelChange n s ->
            let
                newModel =
                    {
                        model | lavelList =
                        map (\m -> if m.id == n then {m | lavel = s} else m) model.lavelList
                    }
            in
                send newModel

        TodoPriorityChange n m ->
            let
                newModel =
                    {
                        model| todoList =
                        map (\t -> if t.id == n then {t | id = m} else t) model.todoList
                    }
            in
                send newModel

        TodoTitleChange n s ->
            let
                newModel =
                    {
                        model| todoList =
                        map (\t -> if t.id == n then {t | title = s} else t) model.todoList
                    }
            in
                send newModel

        TodoContentChange n s ->
            let
                newModel =
                    {
                        model| todoList =
                        map (\t -> if t.id == n then {t | content = s} else t) model.todoList
                    }
            in
                send newModel

        TodoAdd ->
            let
                newModel =
                    {
                        model |
                            todoList =
                                {
                                    id = 1 + withDefault 0 (maximum (map (\t -> t.id) model.todoList)),
                                    title = model.todoAddTextBox,
                                    content="",
                                    priority=0,
                                    lavelId=0
                                } :: model.todoList,
                        todoAddTextBox = ""
                    }
            in
                send newModel

        TodoDel n ->
            let
                newModel =
                    {
                        model | todoList = filter (\t -> t.id /= n)model.todoList
                    }
            in
                send newModel

        TodoAddTextBoxChange s ->
            let
                newModel =
                    {model | todoAddTextBox = s}
            in
                doNotSend newModel

        TodoLevelUp n ->
            let
                newModel =
                    {
                        model| todoList =
                        map (\t -> if t.id == n then {t | lavelId = min 2 (t.lavelId + 1)} else t) model.todoList
                    }
            in
                send newModel

        TodoLevelDown n ->
            let
                newModel =
                    {
                        model| todoList =
                        map (\t -> if t.id == n then {t | lavelId = max 0 (t.lavelId - 1)} else t) model.todoList
                    }
            in
                send newModel

        GetUpdate s ->
            (
                case (Json.Decode.decodeString fromJsonModel s) of
                    Err e ->
                        model
                    Ok newModel ->
                        {
                            newModel | todoAddTextBox = model.todoAddTextBox
                        }
            , Cmd.none)






---- VIEW ----

viewTodo : Todo -> List Lavel -> Html Msg
viewTodo todo lavelList=
    li[][
        text todo.title,
        ul[style "list-style-type" "none"][
            {-li[][ text t.content],
            li[][text (String.fromInt t.priority)],-}
            if todo.lavelId /= 0 then
                li[style "display" "table-cell"][
                    input[type_ "button", name "Prev",
                    value (withDefault "" (head (map (\l -> l.lavel) (filter (\l -> l.id == todo.lavelId - 1) lavelList)))),
                    onClick (TodoLevelDown todo.id)][]
                ]
            else
                text ""
            ,
            if todo.lavelId /= (length lavelList) - 1 then
                li[style "display" "table-cell"][
                    input[type_ "button" , name "Next",
                    value (withDefault "" (head (map (\l -> l.lavel) (filter (\l -> l.id == todo.lavelId + 1) lavelList)))),
                    onClick (TodoLevelUp todo.id)][]
                ]
            else
                text ""
        ],
        ul[style "list-style-type" "none"][
            li[style "display" "table-cell"][
                input[type_ "button", name "Delete",
                value "Delete",
                onClick (TodoDel todo.id)][]
            ]
        ]
    ]



view : Model -> Html Msg
view model =
    div []
        [
        input [ placeholder "New Task" , value model.todoAddTextBox, onInput TodoAddTextBoxChange] [],
        input[type_ "button", name "AddTask", value "Add",  onClick TodoAdd][],
        div[][ul [style "list-style-type" "none"]
        ( map (\m -> li[style "display" "table-cell"][
            text m.lavel,
            ul [style "list-style-type" "none"](
                map (\t ->
                    viewTodo t model.lavelList
                    )
                (sortBy (\t -> t.priority) (filter (\t -> t.lavelId == m.id)model.todoList))
            )
            ]) (sortBy (\m -> m.id) model.lavelList)) ]
        ]



---- PORT ----

port sendUpdate : String -> Cmd msg
port getUpdate : (String -> msg) -> Sub msg



subscriptions : Model -> Sub Msg
subscriptions model =
    getUpdate GetUpdate


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
