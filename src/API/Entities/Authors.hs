{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Authors where

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.=)
  , object
  )
import Models.User (UserId)
import Models.Author (AuthorDescription)

data Author =
  Author UserId AuthorDescription

instance ToJSON Author where
  toJSON (Author authorId description) = object ["id" .= authorId, "description" .= description]

data AuthorCreation =
  AuthorCreation UserId AuthorDescription

instance FromJSON AuthorCreation where
  parseJSON (Object author) = AuthorCreation <$> author .: "user_id" <*> author .: "description"

data AuthorDeletion =
  AuthorDeletion UserId

instance FromJSON AuthorDeletion where
  parseJSON (Object author) = AuthorDeletion <$> author .: "id"

data AuthorEditing =
  AuthorEditing UserId AuthorDescription

instance FromJSON AuthorEditing where
  parseJSON (Object author) = AuthorEditing <$> author .: "id" <*> author .: "new_description"
