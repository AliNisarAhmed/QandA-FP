module Page.Login exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (Attribute, Element)
import Element.Input as Input
import Http
import Json exposing (currentUserDecoder, encodeLoginForm)
import Page.Signup exposing (Msg(..))
import RemoteData exposing (RemoteData(..), WebData)
import Session exposing (CurrentUser)
import Styles
import Utils exposing (displayErrorText, errorToString)



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , form : LoginForm
    , error : Maybe String
    , currentUser : Maybe CurrentUser
    }


type alias LoginForm =
    { userName : String
    , password : String
    }


initialForm : LoginForm
initialForm =
    { userName = ""
    , password = ""
    }


type Msg
    = NoOp
    | SubmitLoginForm
    | OnUsernameChange String
    | OnPasswordChange String
    | GotCurrentUser (WebData (Maybe CurrentUser))


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , form = initialForm
      , error = Nothing
      , currentUser = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnUsernameChange val ->
            ( { model | form = { form | userName = val } }, Cmd.none )

        OnPasswordChange val ->
            ( { model | form = { form | password = val } }, Cmd.none )

        SubmitLoginForm ->
            ( model, submitLoginForm model )

        GotCurrentUser user ->
            ( { model | currentUser = RemoteData.withDefault Nothing user }
            , Nav.pushUrl model.key "/"
            )



---- COMMANDS ----


submitLoginForm : Model -> Cmd Msg
submitLoginForm model =
    Http.post
        { url = "/api/auth/login"
        , body = Http.jsonBody (encodeLoginForm model.form)
        , expect = Http.expectJson (RemoteData.fromResult >> GotCurrentUser) currentUserDecoder
        }



---- VIEW ----


view : Model -> Element Msg
view model =
    E.el [ E.height E.fill, E.width E.fill, E.paddingXY 0 100 ] <|
        E.column
            [ E.centerX, E.spacingXY 0 30 ]
            [ E.row []
                [ Input.text []
                    { onChange = OnUsernameChange
                    , text = model.form.userName
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| E.text "User Name"
                    }
                ]
            , E.row []
                [ Input.currentPassword []
                    { onChange = OnPasswordChange
                    , text = model.form.password
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| E.text "Password"
                    , show = False
                    }
                ]
            , Input.button Styles.buttonStyles
                { onPress = Just SubmitLoginForm
                , label = E.text "Submit"
                }
            , displayErrorText model.error
            ]
