module Page.Home exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Colors
import Element as E exposing (Attribute, Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Icons
import Json exposing (Question, QuestionId(..), questionIdToString, questionListDecoder)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Styles exposing (buttonStyles)
import Utils exposing (displayTime, errorToString, hideElementForGuest, hideElementForUser)


explain =
    E.explain Debug.todo


type alias Model =
    { session : Session
    , questions : WebData (List Question)
    }


type Msg
    = GotQuestions (WebData (List Question))
    | GoToAskAQuestionPage
    | DeleteQuestion QuestionId
    | DeleteQuestionResponse (Result Http.Error ())


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, questions = Loading }, getData )


getData : Cmd Msg
getData =
    Http.get
        { url = "/api/questions"
        , expect = Http.expectJson (RemoteData.fromResult >> GotQuestions) questionListDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotQuestions res ->
            ( { model | questions = res }, Cmd.none )

        GoToAskAQuestionPage ->
            ( model, Route.pushUrl Route.AskQuestionRoute model.session.key )

        DeleteQuestion questionId ->
            ( model, deleteQuestion model questionId )

        DeleteQuestionResponse (Err e) ->
            ( model, Cmd.none )

        DeleteQuestionResponse (Ok _) ->
            ( model, Cmd.none )



---- COMMANDS ----


deleteQuestion : Model -> QuestionId -> Cmd Msg
deleteQuestion model (QuestionId questionId) =
    Http.request
        { method = "DELETE"
        , headers = []
        , url =
            "/api/questions/"
                ++ String.fromInt questionId
        , body = Http.emptyBody
        , expect = Http.expectWhatever DeleteQuestionResponse
        , timeout = Nothing
        , tracker = Nothing
        }


view : Model -> Element Msg
view model =
    case model.questions of
        Loading ->
            E.row [ E.centerX, E.centerY ] <| [ E.text "Loading..." ]

        Failure e ->
            E.row [ E.centerX, E.centerY ] <| [ E.text <| errorToString e ]

        Success q ->
            page q model.session

        _ ->
            page [] model.session


page : List Question -> Session -> Element Msg
page questions session =
    case questions of
        [] ->
            E.column [ E.width E.fill, E.height E.fill ] <|
                [ E.column [ E.centerX, E.paddingXY 0 20, E.height E.fill ] <|
                    [ E.el [] <| E.text "No Questions asked so far..."
                    , E.el [ E.height E.fill, E.centerY ] <|
                        hideElementForGuest session <|
                            askQuestionButton [ E.centerY ]
                    ]
                ]

        _ ->
            E.column [ E.width E.fill ] <|
                [ E.column [ E.centerX, E.paddingXY 0 20 ] <|
                    [ E.row [ E.width E.fill ]
                        [ E.el [ Font.bold ] <| E.text "Unanswered Questions"
                        , hideElementForGuest session <| askQuestionButton [ E.alignRight ]
                        ]
                    ]
                        ++ List.map (displayQuestion session) questions
                ]


askQuestionButton : List (Attribute Msg) -> Element Msg
askQuestionButton styles =
    Input.button
        (buttonStyles
            ++ styles
        )
        { onPress = Just GoToAskAQuestionPage, label = E.text "Ask a question" }


displayQuestion : Session -> Question -> Element Msg
displayQuestion session q =
    E.el
        [ Border.widthEach { bottom = 1, top = 0, right = 0, left = 0 }
        , Border.color Colors.gray
        , E.paddingXY 0 20
        ]
    <|
        E.column [ E.width <| E.px 700 ]
            [ E.link [ Font.bold ] <|
                { url = "/questions/" ++ questionIdToString q.id
                , label = E.text q.title
                }
            , E.row [ E.alignLeft ]
                [ E.paragraph [ E.alignLeft ] <|
                    [ E.text <| (String.left 50 q.content ++ "...") ]
                ]
            , E.row Styles.subTextStyles
                [ E.text <| "Asked on " ++ displayTime q.created ]
            , deleteIcon session q
            ]


deleteIcon : Session -> Question -> Element Msg
deleteIcon session question =
    Input.button [ E.alignRight ]
        { onPress = Just (DeleteQuestion question.id)
        , label =
            E.html Icons.trash2
                |> hideElementForUser session question.userId
        }
