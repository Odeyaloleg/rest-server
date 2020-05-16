module API.Methods.Authors where

import qualified API.Entities.Authors as E
import qualified API.HasResponse as R
import Types (PageNum)

getAuthors :: (R.HasResponse a) => R.AccessLevel -> PageNum -> a
getAuthors access pageNum =
  R.withAdminAccess
    access
    (R.getResponse $ R.AuthorsList pageNum)

createAuthor :: (R.HasResponse a) => R.AccessLevel -> Maybe E.AuthorCreation -> a
createAuthor access authorMaybe =
  R.withAdminAccess
    access
    (case authorMaybe of
       Just (E.AuthorCreation userId description) ->
         R.getResponse $
         R.CreateAuthor userId description
       Nothing -> R.wrongJSON)

deleteAuthor :: (R.HasResponse a) => R.AccessLevel -> Maybe E.AuthorDeletion -> a
deleteAuthor access authorMaybe =
  R.withAdminAccess
    access
    (case authorMaybe of
       Just (E.AuthorDeletion authorId) -> R.getResponse $ R.DeleteAuthor authorId
       Nothing -> R.wrongJSON)

editAuthor :: (R.HasResponse a) => R.AccessLevel -> Maybe E.AuthorEditing -> a
editAuthor access authorMaybe =
  R.withAdminAccess
    access
    (case authorMaybe of
       Just (E.AuthorEditing authorId description) -> R.getResponse $ R.EditAuthor authorId description
       Nothing -> R.wrongJSON)
