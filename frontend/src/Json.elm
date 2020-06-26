module Json exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


questionWithAnswersDecoder : Decoder (Maybe QuestionWithAnswers)
questionWithAnswersDecoder =
    nullableDecoder
        (Decode.succeed QuestionWithAnswers
            |> required "title" string
            |> required "content" string
            |> required "created" string
            |> required "userId" userIdDecoder
            |> required "answers" (list answerValueDecoder)
        )


nullableDecoder : Decoder a -> Decoder (Maybe a)
nullableDecoder decoder =
    Decode.nullable decoder


answerValueDecoder : Decoder AnswerValue
answerValueDecoder =
    Decode.succeed AnswerValue
        |> required "questionId" questionIdDecoder
        |> required "content" string
        |> required "created" string
        |> required "userId" userIdDecoder


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


questionIdToString : QuestionId -> String
questionIdToString (QuestionId id) =
    String.fromInt id


type AnswerId
    = AnswerId Int


answerIdToString : AnswerId -> String
answerIdToString (AnswerId id) =
    String.fromInt id


userIdToString : UserId -> String
userIdToString (UserId id) =
    String.fromInt id


type UserId
    = UserId Int


type alias QuestionWithAnswers =
    { title : String
    , content : String
    , created : String
    , userId : UserId
    , answers : List AnswerValue
    }


type alias AnswerValue =
    { questionId : QuestionId
    , content : String
    , created : String
    , userId : UserId
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
