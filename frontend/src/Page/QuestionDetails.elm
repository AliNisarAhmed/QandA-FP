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
import RemoteData as RemoteData exposing (RemoteData(..), WebData)


serverUrl : String
serverUrl =
    "http://localhost:5000/api"


type alias Model =
    { question : WebData QuestionWithAnswers
    , questionId : QuestionId
    , key : Nav.Key
    , error : Maybe String
    }


type Msg
    = NoOp String
    | ServerResponse (WebData QuestionWithAnswers)


init : Nav.Key -> QuestionId -> ( Model, Cmd Msg )
init key questionId =
    ( { question = Loading
      , key = key
      , questionId = questionId
      , error = Nothing
      }
    , fetchQuestionDetails questionId
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerResponse res ->
            ( { model | question = res }, Cmd.none )

        NoOp _ ->
            ( model, Cmd.none )



---- COMMANDS ----


fetchQuestionDetails : QuestionId -> Cmd Msg
fetchQuestionDetails (QuestionId id) =
    Http.get
        { url = serverUrl ++ "/questions/" ++ String.fromInt id
        , expect = Http.expectJson (ServerResponse << RemoteData.fromResult) questionWithAnswersDecoder
        }



---- VIEW ----


view : Model -> Element Msg
view model =
    let
        content =
            case model.question of
                Loading ->
                    E.el [] <| E.text "Loading..."

                NotAsked ->
                    E.el [] <| E.text "Initializing..."

                Success question ->
                    E.column [] <|
                        questionBox question
                            ++ List.map displayAnswer question.answers
                            ++ answerBox

                Failure err ->
                    E.el [] <| E.text <| errorToString err
    in
    content


questionBox : QuestionWithAnswers -> List (Element Msg)
questionBox q =
    [ E.row [] [ E.text q.title ]
    , E.row [] [ E.text q.content ]
    , E.row [] [ E.text q.created ]
    ]


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


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.BadBody str ->
            str

        Http.Timeout ->
            "Time out"

        Http.BadUrl str ->
            str

        Http.BadStatus int ->
            "bad status: " ++ String.fromInt int

        Http.NetworkError ->
            "Network Error"
