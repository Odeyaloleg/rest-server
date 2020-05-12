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
import Types (AuthorId, AuthorDescription)

data Author =
  Author AuthorId AuthorDescription

instance ToJSON Author where
  toJSON (Author authorId description) = object ["id" .= authorId, "description" .= description]

data AuthorCreation =
  AuthorCreation AuthorDescription

instance FromJSON AuthorCreation where
  parseJSON (Object description) = AuthorCreation <$> description .: "description"

data AuthorDeletion =
  AuthorDeletion AuthorId

instance FromJSON AuthorDeletion where
  parseJSON (Object author) = AuthorDeletion <$> author .: "id"

data AuthorEditing =
  AuthorEditing AuthorId AuthorDescription

instance FromJSON AuthorEditing where
  parseJSON (Object author) = AuthorEditing <$> author .: "id" <*> author .: "new_description"
