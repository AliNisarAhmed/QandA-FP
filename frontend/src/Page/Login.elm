module Page.Login exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (Attribute, Element)



---- MODEL ----


type alias Model =
    { key : Nav.Key
    }


type Msg
    = NoOp


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    E.el [] <| E.text "Login Page"
