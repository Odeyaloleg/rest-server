{-# LANGUAGE OverloadedStrings #-}

module API.Methods.Users where

import qualified API.Entities.Users as E
import qualified API.HasResponse as R

getUsers :: (R.HasResponse a) => Int -> a
getUsers pageNum = R.getResponse $ R.UsersList pageNum

createUser :: (R.HasResponse a) => Int -> Maybe E.UserCreation -> a
createUser uId userMaybe =
  case userMaybe of
    Just user ->
      R.getResponse $
      R.CreateUser $
      E.User
        uId
        (E.firstNameUser user)
        (E.lastNameUser user)
        (E.profilePictureUser user)
        ("Today")
        False
    Nothing -> R.getResponse $ R.BadRequest "Wrong\\insufficient JSON data."

createAdmin :: (R.HasResponse a) => Int -> Maybe E.AdminCreation -> R.AccessLevel -> a
createAdmin uId adminMaybe access =
  if access == R.AccessAdmin
    then case adminMaybe of
           Just admin ->
             R.getResponse $
             R.CreateUser $
             E.User
               uId
               (E.firstNameAdmin admin)
               (E.lastNameAdmin admin)
               (E.profilePictureAdmin admin)
               ("Today")
               True
           Nothing -> R.getResponse $ R.BadRequest "Wrong\\insufficient JSON data."
    else R.getResponse $ R.BadRequest "Resource path does not exist."
