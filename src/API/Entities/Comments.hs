{-# LANGUAGE OverloadedStrings #-}

module API.Entities.Comments where

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , (.=)
  , object
  )
import Models.User (UserId)
import Models.Comment (CommentId)
import Models.Post (PostId)

data Comment
  = Comment CommentId String

instance ToJSON Comment where
  toJSON (Comment cId comment) = object ["id" .= cId, "comment" .= comment]

data CommentPublishing =
  CommentPublishing PostId String

instance FromJSON CommentPublishing where
  parseJSON (Object comment) = CommentPublishing <$> comment .: "post_id" <*> comment .: "comment"

data CommentDeletion =
  CommentDeletion CommentId

instance FromJSON CommentDeletion where
  parseJSON (Object comment) = CommentDeletion <$> comment .: "id"
