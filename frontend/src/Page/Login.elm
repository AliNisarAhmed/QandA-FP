module Page.Login exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (Attribute, Element)
import Element.Input as Input
import Http
import Json exposing (CurrentUser, currentUserDecoder, encodeLoginForm)
import Page.Signup exposing (Msg(..))
import RemoteData exposing (RemoteData(..), WebData)
import Styles
import Utils exposing (displayErrorText, errorToString)



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , form : LoginForm
    , error : Maybe String
    , currentUser : WebData CurrentUser
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
    | ServerResponse (Result Http.Error ())
    | FetchCurrentUser
    | GotCurrentUser (WebData CurrentUser)
    | LogOut
    | LogOutResponse (Result Http.Error ())


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , form = initialForm
      , error = Nothing
      , currentUser = NotAsked
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

        ServerResponse (Err e) ->
            ( { model | error = Just <| errorToString e }, Cmd.none )

        ServerResponse (Ok _) ->
            ( model, Nav.pushUrl model.key "/" )

        FetchCurrentUser ->
            ( model, fetchCurrentUser )

        GotCurrentUser user ->
            ( { model | currentUser = user }, Cmd.none )

        LogOut ->
            ( model, logout )

        LogOutResponse (Err e) ->
            ( { model | error = Just <| errorToString e }, Cmd.none )

        LogOutResponse (Ok _) ->
            ( model, Nav.pushUrl model.key "/" )



---- COMMANDS ----


submitLoginForm : Model -> Cmd Msg
submitLoginForm model =
    Http.post
        { url = "/api/auth/login"
        , body = Http.jsonBody (encodeLoginForm model.form)
        , expect = Http.expectWhatever ServerResponse
        }


fetchCurrentUser : Cmd Msg
fetchCurrentUser =
    Http.get
        { url = "/api/auth/current-user"
        , expect = Http.expectJson (RemoteData.fromResult >> GotCurrentUser) currentUserDecoder
        }


logout : Cmd Msg
logout =
    Http.post
        { url = "api/auth/logout"
        , body = Http.emptyBody
        , expect = Http.expectWhatever LogOutResponse
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
            , Input.button Styles.buttonStyles
                { onPress = Just FetchCurrentUser
                , label = E.text "Fetch Current User"
                }
            , Input.button Styles.buttonStyles
                { onPress = Just LogOut
                , label = E.text "Log Out"
                }
            , displayErrorText model.error
            , displayCurrentUser model.currentUser
            ]


displayCurrentUser : WebData CurrentUser -> Element Msg
displayCurrentUser cu =
    case cu of
        Success currentUser ->
            E.el [] <| E.text currentUser.firstName

        Failure e ->
            displayErrorText <| Just <| errorToString e

        _ ->
            E.none
