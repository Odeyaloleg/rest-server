{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Users where

import Data.Aeson
  ( FromJSON(parseJSON)
  , Object
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.:!)
  , (.=)
  , object
  )

data User =
  User
    { uId :: Int
    , firstName :: String
    , lastName :: String
    , profilePicture :: Maybe String
    , creationDate :: String
    , isAdmin :: Bool
    }

instance ToJSON User where
  toJSON user =
    object
      [ "id" .= uId user
      , "first_name" .= firstName user
      , "last_name" .= lastName user
      , "profile_picture" .= profilePicture user
      , "creation_date" .= creationDate user
      , "is_admin" .= isAdmin user
      ]

data UserCreation =
  UserCreation
    { firstNameUser :: String
    , lastNameUser :: String
    , profilePictureUser :: Maybe String
    }

instance FromJSON UserCreation where
  parseJSON (Object user) =
    UserCreation <$> user .: "first_name" <*> user .: "last_name" <*>
    user .:! "profile_picture"

data AdminCreation =
  AdminCreation
    { firstNameAdmin :: String
    , lastNameAdmin :: String
    , profilePictureAdmin :: Maybe String
    }

instance FromJSON AdminCreation where
  parseJSON (Object admin) =
    AdminCreation <$> admin .: "first_name" <*> admin .: "last_name" <*>
    admin .:! "profile_picture"

data UserDeletion =
  UserDeletion Int

instance FromJSON UserDeletion where
  parseJSON (Object user) = UserDeletion <$> user .: "id"
