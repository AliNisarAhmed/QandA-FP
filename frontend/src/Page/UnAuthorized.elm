module Page.UnAuthorized exposing (..)

import Element as E exposing (Element)



---- MODEL ----


type Model
    = None


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( None
    , Cmd.none
    )



---- VIEW ----


view : Element Msg
view =
    E.el [ E.width E.fill, E.height E.fill ] <|
        E.el [ E.centerX, E.centerY ] <|
            E.text "You are not authorized to view this page, please Login or Register"
