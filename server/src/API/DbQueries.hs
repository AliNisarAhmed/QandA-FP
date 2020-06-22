module API.DbQueries where

import           Database                       ( runDb
                                                , DbQuery
                                                )
import           Database.Persist               ( Entity )
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
                                                , getBy
                                                , selectFirst
                                                )
import           Model
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Maybe                     ( listToMaybe )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )




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
  :: Key Question -> Text -> Int -> UTCTime -> DbQuery (Entity Answer)
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


(!??) :: MonadError e m => m (Maybe a) -> e -> m a
act !?? err = act >>= maybe (throwError err) return
