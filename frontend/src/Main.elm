module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Colors
import Element as E exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder, field, int, list, map4, string)
import Url


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
    { url : Url.Url
    , key : Nav.Key
    , questions : List Question
    , status : Status
    , search : String
    }


initModel : Url.Url -> Nav.Key -> Model
initModel url key =
    { url = url, key = key, questions = [], status = Loading, search = "" }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initModel url key, getData )


getData : Cmd Msg
getData =
    Http.get { url = serverUrl ++ "/questions", expect = Http.expectJson GotQuestions questionListDecoder }



---- UPDATE ----


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url.Url
    | GotQuestions (Result Http.Error (List Question))
    | OnSearchChange String
    | GoToAskAQuestionPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        GotQuestions (Err error) ->
            ( model, Cmd.none )

        GotQuestions (Ok res) ->
            ( { model | questions = res, status = Loaded }, Cmd.none )

        OnSearchChange val ->
            ( { model | search = val }
            , Cmd.none
            )

        GoToAskAQuestionPage ->
            ( model, Nav.pushUrl model.key "/ask" )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Q & A"
    , body =
        case model.status of
            Loading ->
                [ E.layout [] <| E.text "Loading..." ]

            Loaded ->
                [ E.layout [] <| page model ]

            Error error ->
                [ E.layout [] <| E.text "Error" ]
    }


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
                    { onPress = Just GoToAskAQuestionPage, label = E.text "Ask a question" }
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



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
