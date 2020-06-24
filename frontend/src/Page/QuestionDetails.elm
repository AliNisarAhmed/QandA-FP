module Page.QuestionDetails exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (Element)
import Json exposing (Question, QuestionId(..), questionIdToString)


type alias Model =
    { question : Maybe Question
    , questionId : QuestionId
    , key : Nav.Key
    }


type Msg
    = NoOp


init : Nav.Key -> QuestionId -> ( Model, Cmd Msg )
init key questionId =
    ( { question = Nothing
      , key = key
      , questionId = questionId
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    E.el [] <|
        E.text <|
            "Question Details Page: QuestionId = "
                ++ questionIdToString model.questionId
