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

data Author =
  Author Int String

instance ToJSON Author where
  toJSON (Author authorId description) = object ["id" .= authorId, "description" .= description]

data AuthorCreation =
  AuthorCreation String

instance FromJSON AuthorCreation where
  parseJSON (Object description) = AuthorCreation <$> description .: "description"

data AuthorDeletion =
  AuthorDeletion Int

instance FromJSON AuthorDeletion where
  parseJSON (Object author) = AuthorDeletion <$> author .: "id"

data AuthorEditing =
  AuthorEditing Int String

instance FromJSON AuthorEditing where
  parseJSON (Object author) = AuthorEditing <$> author .: "id" <*> author .: "new_description"
