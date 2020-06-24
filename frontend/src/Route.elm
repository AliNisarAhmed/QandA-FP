module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Json exposing (QuestionId(..), questionIdToString)
import Url exposing (Url)
import Url.Parser as UP exposing ((</>))


type Route
    = NotFoundRoute
    | HomePageRoute
    | AskQuestionRoute
    | QuestionDetailsRoute QuestionId


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

        QuestionDetailsRoute questionId ->
            "/questions/" ++ questionIdToString questionId

        NotFoundRoute ->
            "/404"


parseUrl : Url -> Route
parseUrl url =
    UP.parse matchRoute url
        |> Maybe.withDefault NotFoundRoute


matchRoute : UP.Parser (Route -> a) a
matchRoute =
    UP.oneOf
        [ UP.map HomePageRoute UP.top
        , UP.map AskQuestionRoute (UP.s "ask")
        , UP.map QuestionDetailsRoute (UP.s "questions" </> questionIdParser)
        ]


questionIdParser : UP.Parser (QuestionId -> a) a
questionIdParser =
    UP.custom "QuestionId" <|
        \questionId ->
            Maybe.map QuestionId (String.toInt questionId)
