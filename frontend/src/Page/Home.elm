module Page.Home exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Colors
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json exposing (Question, questionIdToString, questionListDecoder)
import Route
import Styles exposing (buttonStyles)


serverUrl : String
serverUrl =
    "http://localhost:5000/api"


type Status
    = Loading
    | Loaded
    | Error String


type alias Model =
    { key : Nav.Key
    , questions : List Question
    , status : Status
    }


type Msg
    = GotQuestions (Result Http.Error (List Question))
    | GoToAskAQuestionPage


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key, questions = [], status = Loading }, getData )


getData : Cmd Msg
getData =
    Http.get
        { url = serverUrl ++ "/questions"
        , expect = Http.expectJson GotQuestions questionListDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotQuestions (Err error) ->
            ( model, Cmd.none )

        GotQuestions (Ok res) ->
            ( { model | questions = res, status = Loaded }, Cmd.none )

        GoToAskAQuestionPage ->
            ( model, Route.pushUrl Route.AskQuestionRoute model.key )


view : Model -> Element Msg
view model =
    case model.status of
        Loading ->
            E.row [ E.centerX, E.centerY ] <| [ E.text "Loading..." ]

        Loaded ->
            page model

        Error error ->
            E.text "Error"


page : Model -> Element Msg
page model =
    E.column [ E.width E.fill ] <|
        [ E.column [ E.centerX, E.paddingXY 0 20 ] <|
            [ E.row [ E.width E.fill ]
                [ E.el [ Font.bold ] <| E.text "Unanswered Questions"
                , Input.button
                    (buttonStyles
                        ++ [ E.alignRight
                           ]
                    )
                    { onPress = Just GoToAskAQuestionPage, label = E.text "Ask a question" }
                ]
            ]
                ++ List.map displayQuestion model.questions
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
            [ E.link [ Font.bold ] <|
                { url = "questions/" ++ questionIdToString q.id
                , label = E.text q.title
                }
            , E.row [ E.alignLeft ]
                [ E.paragraph [ E.alignLeft ] <|
                    [ E.text <| (String.left 50 q.content ++ "...") ]
                ]
            ]
