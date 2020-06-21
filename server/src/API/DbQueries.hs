module API.DbQueries where

import           Database                       ( runDb
                                                , DbQuery
                                                )
import           Database.Persist               ( Entity(..)
                                                , selectList
                                                , insertEntity
                                                , (=.)
                                                )
import           Database.Persist.Class         ( get
                                                , updateGet
                                                , delete
                                                , insertEntity
                                                )
import           Database.Esqueleto             ( select
                                                , from
                                                , where_
                                                , (^.)
                                                , (==.)
                                                , val
                                                )
import           Model
import           Data.Text                      ( Text )

getQuestions :: DbQuery [Entity Question]
getQuestions = select $ from $ \q -> return q

checkQuestion :: Key Question -> DbQuery (Maybe Question)
checkQuestion = get

updateQuestion :: Key Question -> Text -> Text -> DbQuery Question
updateQuestion k title content =
  updateGet k [QuestionTitle =. title, QuestionContent =. content]

deleteQuestionById :: Key Question -> DbQuery ()
deleteQuestionById = delete

getAnswersByQuestionId :: Key Question -> DbQuery [Entity Answer]
getAnswersByQuestionId questionId = select $ from $ \answer -> do
  where_ (answer ^. AnswerQuestionId ==. val questionId)
  return answer

insertQuestion :: Question -> DbQuery (Entity Question)
insertQuestion = insertEntity
