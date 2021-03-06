
module API.DbQueries where

import           Database                       ( runDb
                                                , DbQuery
                                                )
import           Database.Persist               ( Entity(..)
                                                , Key(..)
                                                )
import qualified Database.Persist              as P

import           Database.Esqueleto             ( select
                                                , from
                                                , where_
                                                , (^.)
                                                , (==.)
                                                , val
                                                , update
                                                , set
                                                , (&&.)
                                                , (=.)
                                                , get
                                                , updateGet
                                                , delete
                                                , insertEntity
                                                , insert_
                                                , getBy
                                                , selectFirst
                                                , InnerJoin(..)
                                                , on
                                                , SqlExpr(..)
                                                , Value(..)
                                                , entityKey
                                                , entityVal
                                                , FullOuterJoin(..)
                                                , LeftOuterJoin(..)
                                                )
import           Model
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time                      ( UTCTime )
import           Data.Maybe                     ( listToMaybe )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           API.Requests                   ( QuestionWithAnswers(..)
                                                , LoginForm(..)
                                                )
import           Data.List                      ( groupBy )
import           Data.ByteString                ( ByteString )
import qualified Crypto.PasswordStore          as PS
import           Control.Monad.IO.Class         ( liftIO )


getQuestions :: DbQuery [Entity Question]
getQuestions = select $ from $ \q -> return q


checkQuestion :: Key Question -> DbQuery (Maybe Question)
checkQuestion = get


updateQuestion :: Key Question -> Text -> Text -> DbQuery Question
updateQuestion k title content =
  updateGet k [QuestionTitle P.=. title, QuestionContent P.=. content]


deleteQuestionById :: Key Question -> DbQuery ()
deleteQuestionById questionId =
  delete $ from $ \q -> where_ (q ^. QuestionId ==. val questionId)


getAnswersByQuestionId :: Key Question -> DbQuery [Entity Answer]
getAnswersByQuestionId questionId = select $ from $ \answer -> do
  where_ (answer ^. AnswerQuestionId ==. val questionId)
  return answer


insertQuestion :: Question -> DbQuery (Entity Question)
insertQuestion = insertEntity


createAnswer
  :: Key Question -> Text -> Key User -> UTCTime -> DbQuery (Entity Answer)
createAnswer questionId content userId now =
  insertEntity (Answer questionId content userId now)


checkAnswer :: Key Question -> Key Answer -> DbQuery (Maybe (Entity Answer))
checkAnswer questionId answerId = fmap listToMaybe $ select $ from $ \ans -> do
  where_
    (   ans
    ^.  AnswerId
    ==. val answerId
    &&. ans
    ^.  AnswerQuestionId
    ==. val questionId
    )
  return ans


updateAnswer :: Key Answer -> Text -> DbQuery Answer
updateAnswer answerId content = updateGet answerId [AnswerContent P.=. content]


deleteAnswerFromDb :: Key Answer -> DbQuery ()
deleteAnswerFromDb answerId =
  delete $ from $ \ans -> where_ (ans ^. AnswerId ==. val answerId)


getQuestionWithAnswers :: Key Question -> DbQuery (Maybe QuestionWithAnswers)
getQuestionWithAnswers questionId = do
  mq <- get questionId
  case mq of
    Nothing       -> return Nothing
    Just question -> do
      answers <- select $ from $ \ans -> do
        where_ (ans ^. AnswerQuestionId ==. val questionId)
        return ans
      return $ Just $ QuestionWithAnswers (questionTitle question)
                                          (questionContent question)
                                          (questionCreated question)
                                          (questionUserId question)
                                          answers



saveUser :: Text -> Text -> Text -> Text -> DbQuery ()
saveUser fn ln em pw = do
  password <- liftIO $ PS.makePassword (encodeUtf8 pw) 17
  insert_ $ User fn ln em password


validateLoginForm :: LoginForm -> DbQuery (Maybe (Key User, User))
validateLoginForm (LoginForm username pwd) = do
  mu <- getBy $ UniqueUserName username
  case mu of
    Nothing -> return Nothing
    Just (Entity k user) ->
      if PS.verifyPassword (encodeUtf8 pwd) (userPwd user)
        then return $ Just (k, user)
        else return Nothing








(!??) :: MonadError e m => m (Maybe a) -> e -> m a
act !?? err = act >>= maybe (throwError err) return
