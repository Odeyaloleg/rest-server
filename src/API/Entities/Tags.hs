{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Tags where

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.=)
  , object
  )

data Tag =
  Tag Int String

instance ToJSON Tag where
  toJSON (Tag tagId tag) = object ["id" .= tagId, "tag" .= tag]

data TagCreation =
  TagCreation String

instance FromJSON TagCreation where
  parseJSON (Object tag) = TagCreation <$> tag .: "tag"

data TagDeletion =
  TagDeletion Int

instance FromJSON TagDeletion where
  parseJSON (Object tag) = TagDeletion <$> tag .: "id"

data TagEditing =
  TagEditing Int String

instance FromJSON TagEditing where
  parseJSON (Object tag) = TagEditing <$> tag .: "id" <*> tag .: "new_tag"
