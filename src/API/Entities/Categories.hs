{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Categories where

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.:!)
  , (.=)
  , object
  )
import Models.Category (CategoryId, ParentCategoryId)

data Category
  = ParentCategory CategoryId String [Category]
  | CategoryBottom CategoryId String

instance ToJSON Category where
  toJSON (ParentCategory cId category subcategories) = object ["id" .= cId, "category" .= category, "subcategories" .= subcategories]
  toJSON (CategoryBottom cId category) = object ["id" .= cId, "category" .= category]

data CategoryCreation =
  CategoryCreation (Maybe ParentCategoryId) String

instance FromJSON CategoryCreation where
  parseJSON (Object category) = CategoryCreation <$> category .:! "parentcategory_id"  <*> category .: "category"

data CategoryDeletion =
  CategoryDeletion Int

instance FromJSON CategoryDeletion where
  parseJSON (Object category) = CategoryDeletion <$> category .: "id"

data CategoryEditing =
  CategoryEditing Int (Maybe ParentCategoryId) String

instance FromJSON CategoryEditing where
  parseJSON (Object category) =
    CategoryEditing <$> category .: "id" <*> category .:! "new_parentcategory_id" <*> category .: "new_category"
