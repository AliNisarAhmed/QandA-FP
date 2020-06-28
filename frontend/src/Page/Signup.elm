module Page.Signup exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (Attribute, Element)
import Element.Input as Input
import Http
import Json exposing (encodeSignupForm)
import Styles
import Utils exposing (errorToString)


explain =
    E.explain Debug.todo


type alias Model =
    { key : Nav.Key
    , form : Form
    , error : Maybe String
    }


type alias Form =
    { userName : String
    , firstName : String
    , lastName : String
    , password : String
    , confirmPassword : String
    }


initialForm : Form
initialForm =
    { userName = ""
    , firstName = ""
    , lastName = ""
    , password = ""
    , confirmPassword = ""
    }


type Msg
    = NoOp
    | OnUsernameChange String
    | OnFirstNameChange String
    | OnLastNameChange String
    | OnPasswordChange String
    | OnConfirmPasswordChange String
    | SubmitForm
    | ServerResponse (Result Http.Error ())


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key, form = initialForm, error = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnUsernameChange val ->
            ( { model | form = { form | userName = val } }, Cmd.none )

        OnFirstNameChange val ->
            ( { model | form = { form | firstName = val } }, Cmd.none )

        OnLastNameChange val ->
            ( { model | form = { form | lastName = val } }, Cmd.none )

        OnPasswordChange val ->
            ( { model | form = { form | password = val } }, Cmd.none )

        OnConfirmPasswordChange val ->
            ( { model | form = { form | confirmPassword = val } }, Cmd.none )

        SubmitForm ->
            ( model, submitForm model )

        ServerResponse (Err e) ->
            ( { model | error = Just <| errorToString e }, Cmd.none )

        ServerResponse (Ok _) ->
            ( model, Nav.pushUrl model.key "/login" )



---- COMMANDS ----


submitForm : Model -> Cmd Msg
submitForm model =
    Http.post
        { url = "/api/auth/signup"
        , expect = Http.expectWhatever ServerResponse
        , body = Http.jsonBody <| encodeSignupForm model.form
        }


view : Model -> Element Msg
view model =
    let
        errorText =
            case model.error of
                Nothing ->
                    E.none

                Just e ->
                    E.el [] <| E.text e
    in
    E.el [ E.width E.fill ] <|
        E.column [ E.centerX ]
            [ E.row []
                [ Input.text []
                    { onChange = OnUsernameChange
                    , text = model.form.userName
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| E.text "User Name"
                    }
                ]
            , E.row []
                [ Input.text []
                    { onChange = OnFirstNameChange
                    , text = model.form.firstName
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| E.text "First Name"
                    }
                ]
            , E.row []
                [ Input.text []
                    { onChange = OnLastNameChange
                    , text = model.form.lastName
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| E.text "Last Name"
                    }
                ]
            , E.row []
                [ Input.newPassword []
                    { onChange = OnPasswordChange
                    , text = model.form.password
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| E.text "Password"
                    , show = False
                    }
                ]
            , E.row []
                [ Input.newPassword []
                    { onChange = OnConfirmPasswordChange
                    , text = model.form.confirmPassword
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| E.text "Confirm Password"
                    , show = False
                    }
                ]
            , E.row []
                [ Input.button Styles.buttonStyles
                    { onPress = Just SubmitForm, label = E.text <| "Submit" }
                ]
            , errorText
            ]
