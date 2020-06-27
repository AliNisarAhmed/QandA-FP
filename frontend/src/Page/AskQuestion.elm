module Page.AskQuestion exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Colors
import Element as E exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json exposing (encodeQuestion)
import Route
import Styles exposing (buttonStyles)


serverUrl : String
serverUrl =
    "http://localhost:5000/api"


explain =
    E.explain Debug.todo



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , title : String
    , content : String
    , error : Maybe Http.Error
    }


type Msg
    = OnTitleChange String
    | OnContentChange String
    | SubmitQuestion
    | SubmitSucces (Result Http.Error ())


init : Nav.Key -> ( Model, Cmd Msg )
init k =
    ( { key = k, title = "", content = "", error = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTitleChange newTitle ->
            ( { model | title = newTitle }, Cmd.none )

        OnContentChange newContent ->
            ( { model | content = newContent }, Cmd.none )

        SubmitQuestion ->
            ( model, submitQuestion model )

        SubmitSucces (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        SubmitSucces (Ok res) ->
            ( model, Route.pushUrl Route.HomePageRoute model.key )



---- COMMANDS ----


submitQuestion : Model -> Cmd Msg
submitQuestion model =
    Http.post
        { url = serverUrl ++ "/questions"
        , body = Http.jsonBody (encodeQuestion model)
        , expect = Http.expectWhatever SubmitSucces
        }


view : Model -> Element Msg
view model =
    E.column [ E.centerX, E.width <| E.maximum 500 E.fill, E.paddingXY 30 30, E.spacingXY 0 30 ]
        [ heading
        , questionBox
        , displayError model.error
        ]


heading : Element Msg
heading =
    E.el
        [ Font.bold
        , E.centerX
        , E.centerX
        , E.width E.fill
        , E.centerY
        ]
    <|
        E.text "ASK A QUESTION"


questionBox : Element Msg
questionBox =
    E.column [ E.width E.fill, E.centerX, E.spacingXY 0 40 ]
        [ E.row [ E.centerX, E.width <| E.maximum 300 E.fill ]
            [ Input.text [ E.alignLeft ]
                { onChange = OnTitleChange
                , text = ""
                , placeholder = Nothing
                , label = Input.labelAbove [ E.alignLeft ] <| E.text "Title"
                }
            ]
        , E.row [ E.centerX, E.width E.fill ]
            [ Input.multiline [ E.centerX, E.width <| E.maximum 300 E.fill, E.height <| E.px 200 ]
                { onChange = OnContentChange
                , text = ""
                , placeholder = Nothing
                , label = Input.labelAbove [ E.alignLeft ] <| E.text "Content"
                , spellcheck = False
                }
            ]
        , Input.button (buttonStyles ++ [ E.centerX ])
            { onPress = Just SubmitQuestion
            , label = E.text "Submit Your Question"
            }
        ]


displayError : Maybe Http.Error -> Element Msg
displayError error =
    case error of
        Nothing ->
            E.none

        Just e ->
            E.el [] <| E.text "Some error occurred"
