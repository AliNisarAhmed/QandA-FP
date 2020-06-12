module Main exposing (..)

import Browser
import Element as E exposing (Element)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode exposing (Decoder, field, list, map2, string)



---


questionDecoder : Decoder Question
questionDecoder =
    map2 Question
        (field "questionText" string)
        (field "questionAnswer" string)


questionListDecoder : Decoder (List Question)
questionListDecoder =
    list questionDecoder


responseDecoder : Decoder (List Question)
responseDecoder =
    field "questions" questionListDecoder



---- MODEL ----


type alias Question =
    { questionText : String
    , questionAnswer : String
    }


type Status
    = Loading
    | Loaded
    | Error String


type alias Model =
    { questions : List Question
    , status : Status
    }


init : ( Model, Cmd Msg )
init =
    ( { questions = [], status = Loading }, getData )


getData : Cmd Msg
getData =
    Http.get { url = "http://localhost:5000", expect = Http.expectJson GotQuestions responseDecoder }



---- UPDATE ----


type Msg
    = GotQuestions (Result Http.Error (List Question))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotQuestions (Err error) ->
            ( model, Cmd.none )

        GotQuestions (Ok res) ->
            ( { model | questions = res, status = Loaded }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.status of
        Loading ->
            E.layout [] <| E.text "Loading..."

        Loaded ->
            E.layout [] (displayQuestionsList model.questions)

        Error error ->
            E.layout [] <| E.text "Error ..."


displayQuestionsList : List Question -> Element Msg
displayQuestionsList qs =
    E.el [] (E.row [] <| List.map displayQuestion qs)


displayQuestion : Question -> Element Msg
displayQuestion q =
    E.el [] <| E.column [] [ E.row [] [ E.text q.questionText ], E.row [] [ E.text q.questionAnswer ] ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
