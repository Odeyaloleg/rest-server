{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Categories where

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.=)
  , object
  )

data Category
  = Subcategory Int Category
  | Category Int String

instance ToJSON Category where
  toJSON (Category cId category) = object ["id" .= cId, "category" .= category]
  toJSON (Subcategory cId category) =
    object ["id" .= cId, "category" .= category]

data CategoryCreation =
  CategoryCreation String

instance FromJSON CategoryCreation where
  parseJSON (Object category) = CategoryCreation <$> category .: "category"

data CategoryDeletion =
  CategoryDeletion Int

instance FromJSON CategoryDeletion where
  parseJSON (Object category) = CategoryDeletion <$> category .: "id"

data CategoryEditing =
  CategoryEditing Int String

instance FromJSON CategoryEditing where
  parseJSON (Object category) =
    CategoryEditing <$> category .: "id" <*> category .: "new_category"
