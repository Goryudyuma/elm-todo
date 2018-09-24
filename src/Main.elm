module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text, ul, li, input, a)
import Html.Attributes exposing (src, style, placeholder, value, type_, name)
import List exposing (filter, map, length, indexedMap,maximum, sortBy, head)
import Tuple exposing(first,second )
import Maybe exposing (withDefault)
import Html.Events exposing (onInput, onClick)



---- MODEL ----


type alias Model =
    {
        todoList:List Todo,
        lavelList:List Lavel,
        todoAddTextBox:String
    }

type alias Todo =
    {
        id:Int,
        title:String,
        content:String,
        priority:Int,
        lavelId:Int
    }

type alias Lavel = {
        id:Int,
        lavel:String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( {
        todoList=[],
        lavelList=[
            {id=0,lavel="Todo"}, 
            {id=1,lavel="Doing"},
            {id=2,lavel="Done"}],
        todoAddTextBox=""
    }, Cmd.none )



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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LavelListLavelChange n s -> ({model | lavelList = 
                map (\m -> if m.id == n then {m | lavel = s} else m) model.lavelList
            }, Cmd.none)
        TodoPriorityChange n m-> ({model| todoList =
                map (\t -> if t.id == n then {t | id = m} else t) model.todoList
            }, Cmd.none)
        TodoTitleChange n s-> ({model| todoList =
                map (\t -> if t.id == n then {t | title = s} else t) model.todoList
            }, Cmd.none)
        TodoContentChange n s-> ({model| todoList =
                map (\t -> if t.id == n then {t | content = s} else t) model.todoList
            }, Cmd.none)
        TodoAdd -> ({model | todoList =
                {
                    id = 1 + withDefault 0 (maximum (map (\t -> t.id) model.todoList)),
                    title = model.todoAddTextBox,
                    content="",
                    priority=0,
                    lavelId=0
                } :: model.todoList,
                todoAddTextBox = ""
            }, Cmd.none)
        TodoDel n -> ({model | todoList = filter (\t -> t.id /= n)model.todoList}, Cmd.none)
        TodoAddTextBoxChange s -> ({model | todoAddTextBox = s}, Cmd.none)
        TodoLevelUp n -> ({model| todoList =
                map (\t -> if t.id == n then {t | lavelId = min 2 (t.lavelId + 1)} else t) model.todoList
            }, Cmd.none)
        TodoLevelDown n -> ({model| todoList =
                map (\t -> if t.id == n then {t | lavelId = max 0 (t.lavelId - 1)} else t) model.todoList
            }, Cmd.none)







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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
