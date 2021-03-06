module Json exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Session exposing (CurrentUser, Session, UserId(..), getId)



---- Encoders ----


encodeLoginForm : { r | userName : String, password : String } -> Encode.Value
encodeLoginForm { userName, password } =
    Encode.object
        [ ( "userName", Encode.string userName )
        , ( "password", Encode.string password )
        ]


encodeSignupForm : { r | userName : String, firstName : String, lastName : String, password : String, confirmPassword : String } -> Encode.Value
encodeSignupForm { userName, firstName, lastName, password, confirmPassword } =
    Encode.object
        [ ( "userName", Encode.string userName )
        , ( "firstName", Encode.string firstName )
        , ( "lastName", Encode.string lastName )
        , ( "password", Encode.string password )
        , ( "confirmPassword", Encode.string confirmPassword )
        ]


encodeAnswer : UserId -> String -> Encode.Value
encodeAnswer (UserId id) str =
    Encode.object
        [ ( "content", Encode.string str )
        , ( "userId", Encode.int id )
        ]


encodeQuestion : { r | content : String, title : String, session : Session } -> Encode.Value
encodeQuestion { content, title, session } =
    case session.currentUser of
        Nothing ->
            Encode.object
                [ ( "title", Encode.string title )
                , ( "content", Encode.string content )
                , ( "userId", Encode.null )
                ]

        Just { id } ->
            Encode.object
                [ ( "title", Encode.string title )
                , ( "content", Encode.string content )
                , ( "userId", Encode.int <| getId id )
                ]



---- Decoders ----


currentUserDecoder : Decoder (Maybe CurrentUser)
currentUserDecoder =
    nullableDecoder
        (Decode.succeed CurrentUser
            |> required "id" userIdDecoder
            |> required "firstName" string
            |> required "lastName" string
        )


questionWithAnswersDecoder : Decoder (Maybe QuestionWithAnswers)
questionWithAnswersDecoder =
    nullableDecoder
        (Decode.succeed QuestionWithAnswers
            |> required "title" string
            |> required "content" string
            |> required "created" string
            |> required "userId" userIdDecoder
            |> required "answers" (list answerDecoder)
        )


nullableDecoder : Decoder a -> Decoder (Maybe a)
nullableDecoder decoder =
    Decode.nullable decoder


questionListDecoder : Decoder (List Question)
questionListDecoder =
    list questionDecoder


questionDecoder : Decoder Question
questionDecoder =
    Decode.succeed Question
        |> required "id" questionIdDecoder
        |> required "title" string
        |> required "content" string
        |> required "created" string
        |> required "userId" userIdDecoder


answerDecoder : Decoder Answer
answerDecoder =
    Decode.succeed Answer
        |> required "id" answerIdDecoder
        |> required "questionId" questionIdDecoder
        |> required "content" string
        |> required "created" string
        |> required "userId" userIdDecoder


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "id" userIdDecoder
        |> required "name" string


questionIdDecoder : Decoder QuestionId
questionIdDecoder =
    Decode.map QuestionId int


userIdDecoder : Decoder UserId
userIdDecoder =
    Decode.map UserId int


answerIdDecoder : Decoder AnswerId
answerIdDecoder =
    Decode.map AnswerId int


type QuestionId
    = QuestionId Int


getQuestionId : QuestionId -> Int
getQuestionId (QuestionId id) =
    id


questionIdToString : QuestionId -> String
questionIdToString (QuestionId id) =
    String.fromInt id


type AnswerId
    = AnswerId Int


getAnswerId : AnswerId -> Int
getAnswerId (AnswerId id) =
    id


answerIdToString : AnswerId -> String
answerIdToString (AnswerId id) =
    String.fromInt id


userIdToString : UserId -> String
userIdToString (UserId id) =
    String.fromInt id


type alias QuestionWithAnswers =
    { title : String
    , content : String
    , created : String
    , userId : UserId
    , answers : List Answer
    }


type alias Question =
    { id : QuestionId
    , title : String
    , content : String
    , created : String
    , userId : UserId
    }


type alias Answer =
    { id : AnswerId
    , questionId : QuestionId
    , content : String
    , created : String
    , userId : UserId
    }


type alias User =
    { id : UserId
    , name : String
    }
