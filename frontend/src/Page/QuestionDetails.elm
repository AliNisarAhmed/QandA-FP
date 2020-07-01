module Page.QuestionDetails exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (Element)
import Element.Input as Input
import Http
import Icons
import Json
    exposing
        ( Answer
        , AnswerId(..)
        , QuestionId(..)
        , QuestionWithAnswers
        , encodeAnswer
        , getQuestionId
        , questionWithAnswersDecoder
        )
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Session exposing (Session)
import Styles
import Utils exposing (displayTime, errorToString, hideElementForGuest, hideElementForUser)


serverUrl : String
serverUrl =
    "http://localhost:5000/api"


explain =
    E.explain Debug.todo


type alias Model =
    { question : WebData (Maybe QuestionWithAnswers)
    , questionId : QuestionId
    , session : Session
    , error : Maybe String
    , answer : String
    }


type Msg
    = NoOp String
    | ServerResponse (WebData (Maybe QuestionWithAnswers))
    | OnAnswerChange String
    | SubmitAnswer
    | Submitted (Result Http.Error ())
    | DeleteAnswer AnswerId
    | DeleteAnswerResponse (Result Http.Error ())


init : Session -> QuestionId -> ( Model, Cmd Msg )
init s questionId =
    ( { question = Loading
      , session = s
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
            ( model, submitAnswer model )

        Submitted (Err e) ->
            ( { model | error = Just <| errorToString e }, Cmd.none )

        Submitted (Ok _) ->
            ( { model | answer = "", question = Loading }, fetchQuestionDetails model.questionId )

        DeleteAnswer answerId ->
            ( model, deleteAnswer model.questionId answerId )

        DeleteAnswerResponse (Err e) ->
            ( model, Cmd.none )

        DeleteAnswerResponse (Ok _) ->
            ( model, fetchQuestionDetails model.questionId )

        NoOp _ ->
            ( model, Cmd.none )



---- COMMANDS ----


deleteAnswer : QuestionId -> AnswerId -> Cmd Msg
deleteAnswer (QuestionId questionId) (AnswerId answerId) =
    Http.request
        { method = "DELETE"
        , headers = []
        , url =
            "/api/questions/"
                ++ String.fromInt questionId
                ++ "/answers/"
                ++ String.fromInt answerId
        , body = Http.emptyBody
        , expect = Http.expectWhatever DeleteAnswerResponse
        , timeout = Nothing
        , tracker = Nothing
        }


fetchQuestionDetails : QuestionId -> Cmd Msg
fetchQuestionDetails (QuestionId id) =
    Http.get
        { url = serverUrl ++ "/questions/" ++ String.fromInt id
        , expect = Http.expectJson (ServerResponse << RemoteData.fromResult) questionWithAnswersDecoder
        }


submitAnswer : Model -> Cmd Msg
submitAnswer { session, questionId, answer } =
    case session.currentUser of
        Nothing ->
            Cmd.none

        Just cu ->
            Http.post
                { url =
                    serverUrl
                        ++ "/questions/"
                        ++ String.fromInt (getQuestionId questionId)
                        ++ "/answers"
                , body = Http.jsonBody (encodeAnswer cu.id answer)
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
                                        ++ List.map (displayAnswer model.session) question.answers
                                        ++ [ hideElementForGuest model.session answerBox ]
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


answerBox : Element Msg
answerBox =
    E.column Styles.answerBox
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


displayAnswer : Session -> Answer -> Element Msg
displayAnswer session answer =
    E.column Styles.answerDisplay
        [ E.row Styles.contentStyles
            [ E.text answer.content
            ]
        , E.row Styles.subTextStyles
            [ E.el [] <| E.text <| "Answered on " ++ displayTime answer.created
            , Input.button [ E.alignRight ]
                { onPress = Just (DeleteAnswer answer.id)
                , label =
                    E.html Icons.trash2
                        |> hideElementForUser session answer.userId
                }
            ]
        ]
