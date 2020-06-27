module Page.QuestionDetails exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (Element)
import Element.Input as Input
import Http
import Json
    exposing
        ( AnswerValue
        , QuestionId(..)
        , QuestionWithAnswers
        , encodeAnswer
        , questionWithAnswersDecoder
        )
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Styles
import Utils exposing (displayTime)


serverUrl : String
serverUrl =
    "http://localhost:5000/api"


explain =
    E.explain Debug.todo


type alias Model =
    { question : WebData (Maybe QuestionWithAnswers)
    , questionId : QuestionId
    , key : Nav.Key
    , error : Maybe String
    , answer : String
    }


type Msg
    = NoOp String
    | ServerResponse (WebData (Maybe QuestionWithAnswers))
    | OnAnswerChange String
    | SubmitAnswer
    | Submitted (Result Http.Error ())


init : Nav.Key -> QuestionId -> ( Model, Cmd Msg )
init key questionId =
    ( { question = Loading
      , key = key
      , questionId = questionId
      , error = Nothing
      , answer = ""
      }
    , fetchQuestionDetails questionId
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerResponse res ->
            ( { model | question = res }, Cmd.none )

        OnAnswerChange val ->
            ( { model | answer = val }, Cmd.none )

        SubmitAnswer ->
            ( model, submitAnswer model.questionId model.answer )

        Submitted (Err e) ->
            ( { model | error = Just <| errorToString e }, Cmd.none )

        Submitted (Ok _) ->
            ( { model | answer = "", question = Loading }, fetchQuestionDetails model.questionId )

        NoOp _ ->
            ( model, Cmd.none )



---- COMMANDS ----


fetchQuestionDetails : QuestionId -> Cmd Msg
fetchQuestionDetails (QuestionId id) =
    Http.get
        { url = serverUrl ++ "/questions/" ++ String.fromInt id
        , expect = Http.expectJson (ServerResponse << RemoteData.fromResult) questionWithAnswersDecoder
        }


submitAnswer : QuestionId -> String -> Cmd Msg
submitAnswer (QuestionId id) answer =
    Http.post
        { url = serverUrl ++ "/questions/" ++ String.fromInt id ++ "/answers"
        , body = Http.jsonBody (encodeAnswer answer)
        , expect = Http.expectWhatever Submitted
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

                Success mq ->
                    case mq of
                        Nothing ->
                            E.el [] <| E.text "Not Found"

                        Just question ->
                            E.column Styles.questionDetailsPageStyle
                                [ E.column
                                    Styles.contentBoxStyles
                                  <|
                                    [ questionBox question ]
                                        ++ List.map displayAnswer question.answers
                                        ++ answerBox
                                ]

                Failure err ->
                    E.el [] <| E.text <| errorToString err
    in
    content


questionBox : QuestionWithAnswers -> Element Msg
questionBox q =
    E.column Styles.questionBox <|
        [ E.paragraph Styles.titleStyles [ E.text q.title ]
        , E.paragraph Styles.contentStyles [ E.text q.content ]
        , E.paragraph Styles.subTextStyles
            [ E.text <| "Asked on " ++ displayTime q.created
            ]
        ]


answerBox : List (Element Msg)
answerBox =
    [ E.column Styles.answerBox
        [ Input.multiline
            [ E.height (E.fill |> E.minimum 300 |> E.maximum 600)
            ]
            { onChange = OnAnswerChange
            , text = ""
            , placeholder = Nothing
            , label = Input.labelAbove [ E.alignLeft ] <| E.text "Your Answer"
            , spellcheck = False
            }
        , Input.button Styles.buttonStyles
            { onPress = Just SubmitAnswer
            , label = E.text "Submit Answer"
            }
        ]
    ]


displayAnswer : AnswerValue -> Element Msg
displayAnswer answer =
    E.column Styles.answerDisplay
        [ E.row Styles.contentStyles [ E.text answer.content ]
        , E.row Styles.subTextStyles [ E.text <| "Answered on " ++ displayTime answer.created ]
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
