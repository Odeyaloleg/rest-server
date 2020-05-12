module API.Methods.Drafts where

import qualified API.Entities.Drafts as E
import qualified API.HasResponse as R
import Types (AuthorId, PageNum, DraftId)

getDrafts :: (R.HasResponse a) => AuthorId -> PageNum -> R.AccessLevel -> a
getDrafts authorId pageNum access =
  R.withAuthorAccess
    access
    (R.getResponse $ R.DraftsList authorId pageNum)

publishDraft :: (R.HasResponse a) => DraftId -> R.AccessLevel -> a
publishDraft draftId access =
  R.withAuthorAccess
    access
    (R.getResponse $ R.PublishDraft draftId)

createDraft :: (R.HasResponse a) => Maybe E.DraftCreation -> R.AccessLevel -> a
createDraft draftMaybe access =
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

deleteDraft :: (R.HasResponse a) => Maybe E.DraftDeletion -> R.AccessLevel -> a
deleteDraft draftMaybe access =
  R.withAuthorAccess
    access
    (case draftMaybe of
       Just (E.DraftDeletion draftId) -> R.getResponse $ R.DeleteDraft draftId
       Nothing -> R.wrongJSON)

editDraft :: (R.HasResponse a) => Maybe E.DraftEditing -> R.AccessLevel -> a
editDraft draftMaybe access =
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
