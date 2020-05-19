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
import Models.Tag (TagId)

data Tag =
  Tag TagId String

instance ToJSON Tag where
  toJSON (Tag tagId tag) = object ["id" .= tagId, "tag" .= tag]

data TagCreation =
  TagCreation String

instance FromJSON TagCreation where
  parseJSON (Object tag) = TagCreation <$> tag .: "tag"

data TagDeletion =
  TagDeletion TagId

instance FromJSON TagDeletion where
  parseJSON (Object tag) = TagDeletion <$> tag .: "id"

data TagEditing =
  TagEditing TagId String

instance FromJSON TagEditing where
  parseJSON (Object tag) = TagEditing <$> tag .: "id" <*> tag .: "new_tag"
