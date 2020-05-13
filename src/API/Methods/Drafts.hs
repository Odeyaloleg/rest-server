module API.Methods.Drafts where

import qualified API.Entities.Drafts as E
import qualified API.HasResponse as R
import Types (AuthorId, PageNum, DraftId)

getDrafts :: (R.HasResponse a) => R.AccessLevel -> PageNum -> a
getDrafts access pageNum =
  R.withAuthorAccess
    access
    (R.getResponse $ R.DraftsList pageNum)

publishDraft :: (R.HasResponse a) => R.AccessLevel -> DraftId -> a
publishDraft access draftId =
  R.withAuthorAccess
    access
    (R.getResponse $ R.PublishDraft draftId)

createDraft :: (R.HasResponse a) => R.AccessLevel -> Maybe E.DraftCreation -> a
createDraft access draftMaybe =
  R.withAuthorAccess
    access
    (case draftMaybe of
       Just (E.DraftCreation postId categoryId title tags mainPicture content additionalPictures) ->
         R.getResponse $
         R.CreateDraft
           postId
           categoryId
           title
           tags
           mainPicture
           content
           additionalPictures
       Nothing -> R.wrongJSON)

deleteDraft :: (R.HasResponse a) => R.AccessLevel -> Maybe E.DraftDeletion -> a
deleteDraft access draftMaybe =
  R.withAuthorAccess
    access
    (case draftMaybe of
       Just (E.DraftDeletion draftId) -> R.getResponse $ R.DeleteDraft draftId
       Nothing -> R.wrongJSON)

editDraft :: (R.HasResponse a) => R.AccessLevel -> Maybe E.DraftEditing -> a
editDraft access draftMaybe =
  R.withAuthorAccess
    access
    (case draftMaybe of
       Just (E.DraftEditing draftId category title tags mainPicture content additionalPictures) ->
         R.getResponse $
         R.EditDraft
           draftId
           category
           title
           tags
           mainPicture
           content
           additionalPictures
       Nothing -> R.wrongJSON)
