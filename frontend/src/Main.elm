module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Colors
import Element as E exposing (Attribute, Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (search)
import Page.AskQuestion as AskQuestion exposing (Msg(..))
import Page.Home as Home exposing (Msg(..))
import Page.QuestionDetails as QuestionDetails
import Route exposing (Route(..))
import Styles
import Url


explain : Attribute Msg
explain =
    E.explain Debug.todo



---- PAGES ----


type Page
    = LandingPage
    | HomePage Home.Model
    | AskQuestionPage AskQuestion.Model
    | QuestionDetailsPage QuestionDetails.Model
    | NotFoundPage


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url.Url
    | OnSearchChange String
    | HomePageMsg Home.Msg
    | AskQuestionMsg AskQuestion.Msg
    | QuestionDetailsPageMsg QuestionDetails.Msg



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , search : String
    , route : Route
    , currentPage : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , route = Route.parseUrl url
            , search = ""
            , currentPage = LandingPage
            }
    in
    initCurrentPage ( model, Cmd.none )



-- This function is basically a Route -> Page converter
-- depending on the route, it initiates the corresponding page
-- new Route value is set in update's URLChanged case branch


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, currentCommands ) =
    let
        ( currentPage, mappedCmds ) =
            case model.route of
                Route.HomePageRoute ->
                    let
                        ( pageModel, pageCmds ) =
                            Home.init model.key
                    in
                    ( HomePage pageModel, Cmd.map HomePageMsg pageCmds )

                Route.AskQuestionRoute ->
                    let
                        ( pageModel, pageCmds ) =
                            AskQuestion.init model.key
                    in
                    ( AskQuestionPage pageModel, Cmd.map AskQuestionMsg pageCmds )

                Route.QuestionDetailsRoute questionId ->
                    let
                        ( pageModel, pageCmds ) =
                            QuestionDetails.init model.key questionId
                    in
                    ( QuestionDetailsPage pageModel, Cmd.map QuestionDetailsPageMsg pageCmds )

                _ ->
                    ( NotFoundPage, Cmd.none )
    in
    ( { model | currentPage = currentPage }
    , Cmd.batch [ currentCommands, mappedCmds ]
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.currentPage ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none ) |> initCurrentPage

        ( OnSearchChange searchTerm, _ ) ->
            ( { model | search = searchTerm }, Cmd.none )

        ( HomePageMsg pageMsg, HomePage pageModel ) ->
            let
                ( updatedModel, updatedCmds ) =
                    Home.update pageMsg pageModel
            in
            ( { model | currentPage = HomePage updatedModel }
            , Cmd.map HomePageMsg updatedCmds
            )

        ( AskQuestionMsg pageMsg, AskQuestionPage pageModel ) ->
            let
                ( updatedModel, updatedCmds ) =
                    AskQuestion.update pageMsg pageModel
            in
            ( { model | currentPage = AskQuestionPage updatedModel }
            , Cmd.map AskQuestionMsg updatedCmds
            )

        ( QuestionDetailsPageMsg pageMsg, QuestionDetailsPage pageModel ) ->
            let
                ( updatedModel, updatedCmds ) =
                    QuestionDetails.update pageMsg pageModel
            in
            ( { model | currentPage = QuestionDetailsPage updatedModel }
            , Cmd.map QuestionDetailsPageMsg updatedCmds
            )

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        ( title, currentView ) =
            case model.currentPage of
                LandingPage ->
                    ( "Q & A", E.text "Loading..." )

                NotFoundPage ->
                    ( "Not Found", E.text "Not Found..." )

                HomePage pageModel ->
                    ( "Q & A", Home.view pageModel |> E.map HomePageMsg )

                AskQuestionPage pageModel ->
                    ( "Ask Question", AskQuestion.view pageModel |> E.map AskQuestionMsg )

                QuestionDetailsPage pageModel ->
                    ( "Question Details", QuestionDetails.view pageModel |> E.map QuestionDetailsPageMsg )
    in
    { title = title
    , body =
        [ E.layout [] <|
            E.column [ E.width E.fill, E.height E.fill ]
                [ navbar model
                , currentView
                ]
        ]
    }


navbar : Model -> Element Msg
navbar model =
    E.row
        Styles.navbarStyles
        [ E.link [] { url = "/", label = E.el [ Font.bold, Font.size 20 ] <| E.text "Q & A" }
        , searchBar model
        , E.el [] <| E.text "Sign In"
        ]


searchBar : Model -> Element Msg
searchBar model =
    case model.currentPage of
        HomePage _ ->
            E.el Styles.searchbarStyles <|
                Input.search [ E.width <| E.px 300 ]
                    { onChange = OnSearchChange
                    , text = model.search
                    , placeholder = Just <| Input.placeholder [] <| E.text "Search"
                    , label = Input.labelHidden "Search"
                    }

        _ ->
            E.none



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
