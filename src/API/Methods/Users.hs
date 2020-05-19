module API.Methods.Users where

import qualified API.Entities.Users as E
import qualified API.HasResponse as R

getUsers :: (R.HasResponse a) => R.PageNum -> a
getUsers pageNum = R.getResponse $ R.UsersList pageNum

createUser :: (R.HasResponse a) => Maybe E.UserCreation -> a
createUser userMaybe =
  case userMaybe of
    Just (E.UserCreation firstName lastName profilePicture) ->
      R.getResponse $
      R.CreateUser firstName lastName profilePicture
    Nothing -> R.wrongJSON

createAdmin :: (R.HasResponse a) => Maybe E.AdminCreation -> R.AccessLevel -> a
createAdmin adminMaybe access =
  R.withAdminAccess
    access
    (case adminMaybe of
       Just (E.AdminCreation firstName lastName profilePicture) ->
         R.getResponse $
         R.CreateAdmin firstName lastName profilePicture
       Nothing -> R.wrongJSON)

deleteUser :: (R.HasResponse a) => Maybe E.UserDeletion -> R.AccessLevel -> a
deleteUser userDeletionMaybe access =
  R.withAdminAccess
    access
    (case userDeletionMaybe of
       Just (E.UserDeletion uId) -> R.getResponse $ R.DeleteUser uId
       Nothing -> R.wrongJSON)
