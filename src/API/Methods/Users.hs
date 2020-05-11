module API.Methods.Users where

import qualified API.Entities.Users as E
import qualified API.HasResponse as R

getUsers :: (R.HasResponse a) => Int -> a
getUsers pageNum = R.getResponse $ R.UsersList pageNum

createUser :: (R.HasResponse a) => Maybe E.UserCreation -> a
createUser userMaybe =
  case userMaybe of
    Just user ->
      R.getResponse $
      R.CreateUser
        (E.firstNameUser user)
        (E.lastNameUser user)
        (E.profilePictureUser user)
    Nothing -> R.wrongJSON

createAdmin :: (R.HasResponse a) => Maybe E.AdminCreation -> R.AccessLevel -> a
createAdmin adminMaybe access =
  R.withAdminAccess
    access
    (case adminMaybe of
       Just admin ->
         R.getResponse $
         R.CreateAdmin
           (E.firstNameAdmin admin)
           (E.lastNameAdmin admin)
           (E.profilePictureAdmin admin)
       Nothing -> R.wrongJSON)

deleteUser :: (R.HasResponse a) => Maybe E.UserDeletion -> R.AccessLevel -> a
deleteUser userDeletionMaybe access =
  R.withAdminAccess
    access
    (case userDeletionMaybe of
       Just (E.UserDeletion uId) -> R.getResponse $ R.DeleteUser uId
       Nothing -> R.wrongJSON)
