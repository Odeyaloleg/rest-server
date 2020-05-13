module API.Methods.Posts where

import qualified API.Entities.Posts as E
import qualified API.HasResponse as R
import Types (PageNum)

getPosts :: (R.HasResponse a) => PageNum -> a
getPosts pageNum = R.getResponse $ R.PostsList pageNum

publishPost :: (R.HasResponse a) => R.AccessLevel -> Maybe E.PostPublishing -> a
publishPost access postMaybe =
  R.withAuthorAccess
    access
    (case postMaybe of
       Just (E.PostPublishing categoryId title tags content mainPicture additionalPictures) ->
         R.getResponse $
         R.PublishPost
           categoryId
           title
           tags
           content
           mainPicture
           additionalPictures
       Nothing -> R.wrongJSON)

deletePost :: (R.HasResponse a) => R.AccessLevel -> Maybe E.PostDeletion -> a
deletePost access postMaybe =
  R.withAdminAccess
    access
    (case postMaybe of
       Just (E.PostDeletion postId) -> R.getResponse $ R.DeletePost postId
       Nothing -> R.wrongJSON)
