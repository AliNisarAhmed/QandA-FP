module Main exposing (..)

import Browser
import Colors
import Element as E exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (src)
import Http
import Json.Decode exposing (Decoder, field, int, list, map4, string)


serverUrl : String
serverUrl =
    "http://localhost:5000/api"


explain : Attribute Msg
explain =
    E.explain Debug.todo



---


questionDecoder : Decoder Question
questionDecoder =
    map4 Question
        (field "title" string)
        (field "content" string)
        (field "created" string)
        (field "userId" int)


questionListDecoder : Decoder (List Question)
questionListDecoder =
    list questionDecoder



---- MODEL ----


type alias Question =
    { title : String
    , content : String
    , created : String
    , userId : Int
    }


type Status
    = Loading
    | Loaded
    | Error String


type alias Model =
    { questions : List Question
    , status : Status
    , search : String
    }


initModel : Model
initModel =
    { questions = [], status = Loading, search = "" }


init : ( Model, Cmd Msg )
init =
    ( initModel, getData )


getData : Cmd Msg
getData =
    Http.get { url = serverUrl ++ "/questions", expect = Http.expectJson GotQuestions questionListDecoder }



---- UPDATE ----


type Msg
    = GotQuestions (Result Http.Error (List Question))
    | OnSearchChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotQuestions (Err error) ->
            ( model, Cmd.none )

        GotQuestions (Ok res) ->
            ( { model | questions = res, status = Loaded }, Cmd.none )

        OnSearchChange val ->
            ( { model | search = val }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.status of
        Loading ->
            E.layout [] <| E.text "Loading..."

        Loaded ->
            E.layout [] <| page model

        Error error ->
            E.layout [] <| E.text "Error"


page : Model -> Element Msg
page model =
    E.column [ E.width E.fill ] <|
        [ navbar model
        , E.column [ E.centerX, E.paddingXY 0 20 ] <|
            [ E.row [ E.width E.fill ]
                [ E.el [ Font.bold ] <| E.text "Unanswered Questions"
                , Input.button
                    [ E.alignRight
                    , Background.color Colors.primary
                    , Font.color Colors.white
                    , E.paddingXY 20 10
                    , Border.rounded 5
                    , E.focused [ Background.color Colors.primaryDark ]
                    , E.mouseOver [ Background.color Colors.primaryDark ]
                    ]
                    { onPress = Nothing, label = E.text "Ask a question" }
                ]
            ]
                ++ List.map displayQuestion model.questions
        ]


navbar : Model -> Element Msg
navbar model =
    E.row
        [ E.spaceEvenly
        , E.width E.fill
        , E.paddingXY 10 10
        , Border.shadow
            { offset = ( 1.0, 1.0 )
            , size = 2.0
            , blur = 2.0
            , color = Colors.gray
            }
        ]
        [ E.el [ Font.bold, Font.size 20 ] <| E.text "Q & A"
        , E.el [ E.centerX ] <|
            Input.search [ E.width <| E.px 300 ]
                { onChange = OnSearchChange
                , text = model.search
                , placeholder = Just <| Input.placeholder [] <| E.text "Search"
                , label = Input.labelHidden "Search"
                }
        , E.el [] <| E.text "Sign In"
        ]


displayQuestion : Question -> Element Msg
displayQuestion q =
    E.el
        [ Border.widthEach { bottom = 1, top = 0, right = 0, left = 0 }
        , Border.color Colors.gray
        , E.paddingXY 0 20
        ]
    <|
        E.column [ E.width <| E.px 700 ]
            [ E.el [ Font.bold ] <| E.text q.title
            , E.row [ E.alignLeft ]
                [ E.paragraph [ E.alignLeft ] <|
                    [ E.text <| (String.left 50 q.content ++ "...") ]
                ]
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
