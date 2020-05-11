{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Drafts where

import API.Entities.Categories (Category)
import API.Entities.Tags (Tag)
import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.:!)
  , (.=)
  , object
  )
import Types (AdditionalPicture, AuthorId, Content, Id, MainPicture, Title, PostId, CategoryId, TagId)

data Draft =
  Draft
    Id
    PostId
    AuthorId
    (Maybe Category)
    (Maybe Title)
    (Maybe [Tag])
    (Maybe MainPicture)
    (Maybe Content)
    (Maybe [AdditionalPicture])

instance ToJSON Draft where
  toJSON (Draft dId postId _ category title tags mainPicture content additionalPictures) =
    object
      [ "id" .= dId
      , "post" .= postId
      , "category" .= category
      , "title" .= title
      , "tags" .= tags
      , "main_picture" .= mainPicture
      , "content" .= content
      , "additional_pictures" .= additionalPictures
      ]

data DraftCreation =
  DraftCreation
    (Maybe CategoryId)
    (Maybe Title)
    (Maybe [TagId])
    (Maybe MainPicture)
    (Maybe Content)
    (Maybe [AdditionalPicture])

instance FromJSON DraftCreation where
  parseJSON (Object draft) =
    DraftCreation <$> draft .:! "category" <*> draft .:! "title" <*>
    draft .:! "tags" <*>
    draft .:! "main_picture" <*>
    draft .:! "content" <*>
    draft .:! "additional_pictures"

data DraftDeletion =
  DraftDeletion Int

instance FromJSON DraftDeletion where
  parseJSON (Object draft) = DraftDeletion <$> draft .: "id"

data DraftEditing =
  DraftEditing
    Id
    (Maybe CategoryId)
    (Maybe Title)
    (Maybe [TagId])
    (Maybe MainPicture)
    (Maybe Content)
    (Maybe [AdditionalPicture])

instance FromJSON DraftEditing where
  parseJSON (Object draft) =
    DraftEditing <$> draft .: "id" <*> draft .:! "new_category" <*>
    draft .:! "new_title" <*>
    draft .:! "new_tags" <*>
    draft .:! "new_main_picture" <*>
    draft .:! "new_content" <*>
    draft .:! "new_additional_pictures"
