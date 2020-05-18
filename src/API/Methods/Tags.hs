module API.Methods.Tags where

import qualified API.Entities.Tags as E
import qualified API.HasResponse as R

getTags :: (R.HasResponse a) => R.PageNum -> a
getTags pageNum = R.getResponse $ R.TagsList pageNum

createTag :: (R.HasResponse a) => Maybe E.TagCreation -> R.AccessLevel -> a
createTag tagMaybe access =
  R.withAdminAccess
    access
    (case tagMaybe of
       Just (E.TagCreation tag) -> R.getResponse $ R.CreateTag tag
       Nothing -> R.wrongJSON)

deleteTag :: (R.HasResponse a) => Maybe E.TagDeletion -> R.AccessLevel -> a
deleteTag tagDeletionMaybe access =
  R.withAdminAccess
    access
    (case tagDeletionMaybe of
       Just (E.TagDeletion tagId) -> R.getResponse $ R.DeleteTag tagId
       Nothing -> R.wrongJSON)

editTag :: (R.HasResponse a) => Maybe E.TagEditing -> R.AccessLevel -> a
editTag tagEditingMaybe access =
  R.withAdminAccess
    access
    (case tagEditingMaybe of
       Just (E.TagEditing tagId tag) -> R.getResponse $ R.EditTag tagId tag
       Nothing -> R.wrongJSON)
