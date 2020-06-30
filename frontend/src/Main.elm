module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Element as E exposing (Attribute, Element)
import Element.Font as Font
import Element.Input as Input exposing (search)
import Http
import Json exposing (CurrentUser, currentUserDecoder)
import Page.AskQuestion as AskQuestion exposing (Msg(..))
import Page.Home as Home exposing (Msg(..))
import Page.Login as Login
import Page.QuestionDetails as QuestionDetails
import Page.Signup as Signup
import Route exposing (Route(..))
import Styles
import Url



-- justinmimbs/time-extra
-- justinmimbs/timezone-data
-- ryannhg/date-format
-- rtfeldman/elm-iso8601-date-strings


explain : Attribute Msg
explain =
    E.explain Debug.todo



---- PAGES ----


type Page
    = LandingPage
    | HomePage Home.Model
    | AskQuestionPage AskQuestion.Model
    | QuestionDetailsPage QuestionDetails.Model
    | SignupPage Signup.Model
    | LoginPage Login.Model
    | NotFoundPage


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url.Url
    | GotCurrentUser (Result Http.Error (Maybe CurrentUser))
    | LogOut
    | LogOutResponse (Result Http.Error ())
    | OnSearchChange String
    | HomePageMsg Home.Msg
    | AskQuestionMsg AskQuestion.Msg
    | QuestionDetailsPageMsg QuestionDetails.Msg
    | LoginPageMsg Login.Msg
    | SignupPageMsg Signup.Msg


type alias Session =
    { key : Nav.Key
    , currentUser : Maybe CurrentUser
    }



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , search : String
    , route : Route
    , currentPage : Page
    , session : Session
    }


initialModel : Nav.Key -> Url.Url -> Model
initialModel key url =
    { key = key
    , route = Route.parseUrl url
    , search = ""
    , currentPage = LandingPage
    , session = { key = key, currentUser = Nothing }
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initialModel key url, fetchCurrentUser )



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

                Route.SignupRoute ->
                    let
                        ( pageModel, pageCmds ) =
                            Signup.init model.key
                    in
                    ( SignupPage pageModel, Cmd.map SignupPageMsg pageCmds )

                Route.LoginRoute ->
                    let
                        ( pageModel, pageCmds ) =
                            Login.init model.key
                    in
                    ( LoginPage pageModel, Cmd.map LoginPageMsg pageCmds )

                _ ->
                    ( NotFoundPage, Cmd.none )
    in
    ( { model | currentPage = currentPage }
    , Cmd.batch [ currentCommands, mappedCmds ]
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ session } as model) =
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

        ( SignupPageMsg pageMsg, SignupPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmds ) =
                    Signup.update pageMsg pageModel
            in
            ( { model | currentPage = SignupPage updatedPageModel }
            , Cmd.map SignupPageMsg updatedCmds
            )

        ( LoginPageMsg pageMsg, LoginPage pageModel ) ->
            let
                ( updatedModel, updatedCmds ) =
                    Login.update pageMsg pageModel
            in
            ( { model
                | currentPage = LoginPage updatedModel
                , session = { session | currentUser = updatedModel.currentUser }
              }
            , Cmd.map LoginPageMsg updatedCmds
            )

        ( GotCurrentUser (Ok mu), _ ) ->
            case mu of
                Nothing ->
                    initCurrentPage ( model, Cmd.none )

                Just user ->
                    initCurrentPage
                        ( { model | session = { session | currentUser = Just user } }
                        , Cmd.none
                        )

        ( GotCurrentUser (Err e), _ ) ->
            initCurrentPage ( model, Cmd.none )

        ( LogOut, _ ) ->
            ( model, logout )

        ( LogOutResponse _, _ ) ->
            initCurrentPage ( model, Nav.load "/" )

        ( _, _ ) ->
            ( model, Cmd.none )



---- COMMANDS ----


logout : Cmd Msg
logout =
    Http.post
        { url = "api/auth/logout"
        , body = Http.emptyBody
        , expect = Http.expectWhatever LogOutResponse
        }


fetchCurrentUser : Cmd Msg
fetchCurrentUser =
    Http.get
        { url = "/api/auth/current-user"
        , expect = Http.expectJson GotCurrentUser currentUserDecoder
        }



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

                SignupPage pageModel ->
                    ( "Sign Up", Signup.view pageModel |> E.map SignupPageMsg )

                LoginPage pageModel ->
                    ( "Log In", Login.view pageModel |> E.map LoginPageMsg )
    in
    { title = title
    , body =
        [ E.layout Styles.layout <|
            E.column Styles.mainSection
                [ navbar model
                , currentView
                ]
        ]
    }


navbar : Model -> Element Msg
navbar ({ session } as model) =
    case session.currentUser of
        Nothing ->
            E.row
                Styles.navbarStyles
                [ E.link [] { url = "/", label = E.el [ Font.bold, Font.size 20 ] <| E.text "Q & A" }
                , searchBar model
                , E.link [] { url = "/login", label = E.text <| "Log In" }
                , E.link [] { url = "/signup", label = E.text <| "Sign up" }
                ]

        Just cu ->
            E.row
                Styles.navbarStyles
                [ E.link [] { url = "/", label = E.el [ Font.bold, Font.size 20 ] <| E.text "Q & A" }
                , searchBar model
                , E.el [] <| E.text <| "Howdy " ++ cu.firstName
                , Input.button [] { onPress = Just LogOut, label = E.text "Log Out" }
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
