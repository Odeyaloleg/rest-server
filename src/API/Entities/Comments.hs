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
import Types (UserId, Id)

data Comment
  = Comment Id String

instance ToJSON Comment where
  toJSON (Comment cId comment) = object ["id" .= cId, "comment" .= comment]

data CommentPublish =
  CommentPublish String

instance FromJSON CommentPublish where
  parseJSON (Object comment) = CommentPublish <$> comment .: "comment"

data CommentDeletion =
  CommentDeletion Int

instance FromJSON CommentDeletion where
  parseJSON (Object comment) = CommentDeletion <$> comment .: "id"

data CommentEditing =
  CommentEditing Int String

instance FromJSON CommentEditing where
  parseJSON (Object comment) =
    CommentEditing <$> comment .: "id" <*> comment .: "new_comment"
