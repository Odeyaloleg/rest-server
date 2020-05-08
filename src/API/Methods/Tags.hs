module API.Methods.Tags where

import qualified API.Entities.Tags as E
import qualified API.HasResponse as R

getTags :: (R.HasResponse a) => Int -> a
getTags pageNum = R.getResponse $ R.TagsList pageNum

createTag ::
     (R.HasResponse a) => Maybe E.TagCreation -> R.AccessLevel -> a
createTag tagMaybe access =
  if access == R.AccessAdmin
    then case tagMaybe of
           Just tag ->
             R.getResponse $
             R.CreateTag tag
           Nothing ->
             R.getResponse $ R.BadRequest "Wrong\\insufficient JSON data."
    else R.getResponse $ R.BadRequest "Resource path does not exist."

deleteTag :: (R.HasResponse a) => Maybe E.TagDeletion -> R.AccessLevel -> a
deleteTag tagDeletionMaybe access =
  if access == R.AccessAdmin
    then case tagDeletionMaybe of
           Just (E.TagDeletion tagId) -> R.getResponse $ R.DeleteTag tagId
           Nothing ->
             R.getResponse $ R.BadRequest "Wrong\\insufficient JSON data."
    else R.getResponse $ R.BadRequest "Resource path does not exist."

editTag :: (R.HasResponse a) => Maybe E.TagEditing -> R.AccessLevel -> a
editTag tagEditingMaybe access =
  if access == R.AccessAdmin
    then case tagEditingMaybe of
           Just (E.TagEditing tagId tag) -> R.getResponse $ R.EditTag tagId tag
           Nothing ->
             R.getResponse $ R.BadRequest "Wrong\\insufficient JSON data."
    else R.getResponse $ R.BadRequest "Resource path does not exist."
