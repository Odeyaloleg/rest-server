{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Users where

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.:!)
  , (.=)
  , object
  )
import Models.User (UserId, FirstName, LastName, ProfilePicture, CreationDate, IsAdmin)

data User =
  User UserId FirstName LastName ProfilePicture CreationDate IsAdmin

instance ToJSON User where
  toJSON (User uId firstName lastName profilePicture creationDate isAdmin) =
    object
      [ "id" .= uId
      , "first_name" .= firstName
      , "last_name" .= lastName
      , "profile_picture" .= profilePicture
      , "creation_date" .= creationDate
      , "is_admin" .= isAdmin
      ]

data UserCreation =
  UserCreation FirstName LastName ProfilePicture

instance FromJSON UserCreation where
  parseJSON (Object user) =
    UserCreation <$> user .: "first_name" <*> user .: "last_name" <*>
    user .:! "profile_picture"

data AdminCreation =
  AdminCreation FirstName LastName ProfilePicture

instance FromJSON AdminCreation where
  parseJSON (Object admin) =
    AdminCreation <$> admin .: "first_name" <*> admin .: "last_name" <*>
    admin .:! "profile_picture"

data UserDeletion =
  UserDeletion UserId

instance FromJSON UserDeletion where
  parseJSON (Object user) = UserDeletion <$> user .: "id"
