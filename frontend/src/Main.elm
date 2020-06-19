module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Colors
import Element as E exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Page.Home as Home exposing (Msg(..))
import Route exposing (Route(..))
import Url


explain : Attribute Msg
explain =
    E.explain Debug.todo



---- PAGES ----


type Page
    = LandingPage
    | HomePage Home.Model
    | NotFoundPage


type Msg
    = HomePageMsg Home.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url.Url
    | OnSearchChange String



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

                Route.NotFoundRoute ->
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
            ( model, Cmd.none )

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

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        ( title, currentView ) =
            case model.currentPage of
                HomePage pageModel ->
                    ( "Q & A", Home.view pageModel |> E.map HomePageMsg )

                LandingPage ->
                    ( "Q & A", E.text "Loading..." )

                NotFoundPage ->
                    ( "Not Found", E.text "Not Found..." )
    in
    { title = title
    , body =
        [ E.layout [] <|
            E.column [ E.width E.fill ]
                [ navbar model
                , currentView
                ]
        ]
    }


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
