{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Posts where

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.:!)
  , (.=)
  , object
  )

import API.Entities.Authors (Author)
import API.Entities.Categories (Category)
import API.Entities.Tags (Tag)
import Models.Post (AdditionalPicture, Content, MainPicture, PostId, Title)
import Models.User (CreationDate)
import Models.Category (CategoryId)
import Models.Tag (TagId)

data Posts =
  Post
    PostId
    Author
    Category
    Title
    (Maybe [Tag])
    (Maybe Content)
    (Maybe MainPicture)
    (Maybe [AdditionalPicture])
    CreationDate

instance ToJSON Posts where
  toJSON (Post postId author category title tags content mainPicture additionalPictures creationDate) =
    object
      [ "id" .= postId
      , "author" .= author
      , "category" .= category
      , "title" .= title
      , "tags" .= tags
      , "content" .= content
      , "main_picture" .= mainPicture
      , "additional_pictures" .= additionalPictures
      , "creation_date" .= creationDate
      ]

data PostPublishing =
  PostPublishing
    CategoryId
    Title
    (Maybe [TagId])
    (Maybe Content)
    (Maybe MainPicture)
    (Maybe [AdditionalPicture])

instance FromJSON PostPublishing where
  parseJSON (Object post) =
    PostPublishing <$> post .: "category" <*> post .: "title" <*> post .:! "tags" <*>
    post .:! "content" <*>
    post .:! "main_picture" <*>
    post .:! "additional_pictures"

data PostDeletion =
  PostDeletion PostId

instance FromJSON PostDeletion where
  parseJSON (Object post) = PostDeletion <$> post .: "id"
