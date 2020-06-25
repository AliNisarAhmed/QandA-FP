module Page.QuestionDetails exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (Element)
import Element.Input as Input
import Http
import Json
    exposing
        ( Answer
        , AnswerValue
        , Question
        , QuestionId(..)
        , QuestionWithAnswers
        , questionIdToString
        , questionWithAnswersDecoder
        )


serverUrl : String
serverUrl =
    "http://localhost:5000/api"


type Status
    = Loading
    | Loaded


type alias Model =
    { question : Maybe QuestionWithAnswers
    , questionId : QuestionId
    , key : Nav.Key
    , status : Status
    }


type Msg
    = NoOp String
    | ServerResponse (Result Http.Error QuestionWithAnswers)


init : Nav.Key -> QuestionId -> ( Model, Cmd Msg )
init key questionId =
    ( { question = Nothing
      , key = key
      , questionId = questionId
      , status = Loading
      }
    , fetchQuestionDetails questionId
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerResponse (Err err) ->
            ( model, Cmd.none )

        ServerResponse (Ok res) ->
            ( { model | question = Just res, status = Loaded }, Cmd.none )

        NoOp _ ->
            ( model, Cmd.none )



---- COMMANDS ----


fetchQuestionDetails : QuestionId -> Cmd Msg
fetchQuestionDetails (QuestionId id) =
    Http.get
        { url = serverUrl ++ "/questions/" ++ String.fromInt id
        , expect = Http.expectJson ServerResponse questionWithAnswersDecoder
        }



---- VIEW ----


view : Model -> Element Msg
view model =
    let
        questionBox =
            case model.question of
                Nothing ->
                    [ E.el [] <| E.text "No Questiion Found" ]

                Just q ->
                    [ E.row [] [ E.text q.title ]
                    , E.row [] [ E.text q.content ]
                    , E.row [] [ E.text q.created ]
                    ]

        answers =
            case model.question of
                Nothing ->
                    []

                Just q ->
                    q.answers

        content =
            case model.status of
                Loading ->
                    E.el [] <| E.text "Loading..."

                Loaded ->
                    E.column [] <|
                        questionBox
                            ++ List.map displayAnswer answers
                            ++ answerBox
    in
    content


answerBox : List (Element Msg)
answerBox =
    [ E.column []
        [ E.row [] [ E.text "Your answer" ]
        , Input.multiline []
            { onChange = NoOp
            , text = ""
            , placeholder = Nothing
            , label = Input.labelAbove [ E.alignLeft ] <| E.text "Content"
            , spellcheck = False
            }
        , Input.button []
            { onPress = Nothing
            , label = E.text "Submit Answer"
            }
        ]
    ]


displayAnswer : AnswerValue -> Element Msg
displayAnswer answer =
    E.column []
        [ E.row [] [ E.text answer.content ]
        , E.row [] [ E.text answer.created ]
        ]
