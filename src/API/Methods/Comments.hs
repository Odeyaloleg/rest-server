module API.Methods.Comments where

import qualified API.Entities.Comments as E
import qualified API.HasResponse as R
import Types (PostId, PageNum)

getComments :: (R.HasResponse a) => PostId -> PageNum -> a
getComments postId pageNum = R.getResponse $ R.CommentsList postId pageNum

publishComment :: (R.HasResponse a) => Maybe E.CommentPublishing -> a
publishComment commentMaybe =
  case commentMaybe of
    Just (E.CommentPublishing postId comment) -> R.getResponse $ R.PublishComment postId comment
    Nothing -> R.wrongJSON

deleteComment :: (R.HasResponse a) => Maybe E.CommentDeletion -> R.AccessLevel -> a
deleteComment commentMaybe access =
  R.withAdminAccess
    access
    (case commentMaybe of
       Just (E.CommentDeletion commentId) -> R.getResponse $ R.DeleteComment commentId
       Nothing -> R.wrongJSON)
