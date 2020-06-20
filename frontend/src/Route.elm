module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as UP


type Route
    = NotFoundRoute
    | HomePageRoute
    | AskQuestionRoute


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        HomePageRoute ->
            "/"

        AskQuestionRoute ->
            "/ask"

        NotFoundRoute ->
            "/404"


parseUrl : Url -> Route
parseUrl url =
    case UP.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


matchRoute : UP.Parser (Route -> a) a
matchRoute =
    UP.oneOf
        [ UP.map HomePageRoute UP.top
        , UP.map AskQuestionRoute (UP.s "ask")
        ]