{-# LANGUAGE OverloadedStrings #-}

module API
  ( QueryData(..)
  , runAPI
  ) where

import qualified API.Entities.Authors as Authors
import qualified API.Entities.Categories as Categories
import qualified API.Entities.Comments as Comments
import qualified API.Entities.Drafts as Drafts
import qualified API.Entities.Posts as Posts
import qualified API.Entities.Tags as Tags
import qualified API.Entities.Users as Users
import qualified API.HasResponse as R
import qualified API.Methods.Authors as Authors.M
import qualified API.Methods.Categories as Categories.M
import qualified API.Methods.Comments as Comments.M
import qualified API.Methods.Drafts as Drafts.M
import API.Methods.Generic (badRequest)
import qualified API.Methods.Posts as Posts.M
import qualified API.Methods.Tags as Tags.M
import qualified API.Methods.Users as Users.M
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Text.Read (readMaybe)

data QueryData =
  QueryData
    { requestMethod :: BS.ByteString
    , apiMethod :: ApiMethod
    , params :: QueryParams
    , body :: QueryBody
    }

type ApiMethod = T.Text

type QueryParams = [(BS.ByteString, (Maybe BS.ByteString))]

type QueryBody = BSL.ByteString

runAPI :: (R.HasResponse a) => R.AccessLevel -> QueryData -> a
runAPI access queryData =
  case requestMethod queryData of
    "GET" -> handleGetMethod access (apiMethod queryData) (params queryData)
    "POST" -> handlePostMethod access (apiMethod queryData) (body queryData)
    _ -> badRequest "Bad request method. Use GET or POST."

handleGetMethod ::
     (R.HasResponse a) => R.AccessLevel -> ApiMethod -> QueryParams -> a
handleGetMethod access apiMethod params =
  case apiMethod of
    "users" ->
      case lookup "page" params of
        Just pageNum -> runWithParam (Users.M.getUsers) (readIntParam pageNum)
        Nothing -> Users.M.getUsers 1
    "tags" ->
      case lookup "page" params of
        Just pageNum -> runWithParam Tags.M.getTags (readIntParam pageNum)
        Nothing -> Tags.M.getTags 1
    "authors" ->
      case lookup "page" params of
        Just pageNum ->
          runWithParam (Authors.M.getAuthors access) (readIntParam pageNum)
        Nothing -> Authors.M.getAuthors access 1
    "categories" ->
      case lookup "page" params of
        Just pageNum ->
          runWithParam Categories.M.getCategories (readIntParam pageNum)
        Nothing -> Categories.M.getCategories 1
    "comments" ->
      case lookup "post" params of
        Just post ->
          runWithParam
            (\postId ->
               case lookup "page" params of
                 Just pageNum ->
                   runWithParam
                     (Comments.M.getComments postId)
                     (readIntParam pageNum)
                 Nothing -> Comments.M.getComments postId 1)
            (readIntParam post)
        Nothing -> badRequest "Bad path parameters."
    "drafts" ->
      case lookup "page" params of
        Just pageNum ->
          runWithParam (Drafts.M.getDrafts access) (readIntParam pageNum)
        Nothing -> Drafts.M.getDrafts access 1
    "posts" ->
      case lookup "page" params of
        Just pageNum -> runWithParam Posts.M.getPosts (readIntParam pageNum)
        Nothing -> Posts.M.getPosts 1
    _ -> badRequest "Resource path does not exist."

handlePostMethod ::
     (R.HasResponse a) => R.AccessLevel -> ApiMethod -> QueryBody -> a
handlePostMethod access apiMethod body =
  case apiMethod of
    "createUser" -> Users.M.createUser (decode body :: Maybe Users.UserCreation)
    "createAdmin" ->
      Users.M.createAdmin (decode body :: Maybe Users.AdminCreation) access
    "createTag" ->
      Tags.M.createTag (decode body :: Maybe Tags.TagCreation) access
    "deleteTag" ->
      Tags.M.deleteTag (decode body :: Maybe Tags.TagDeletion) access
    "editTag" -> Tags.M.editTag (decode body :: Maybe Tags.TagEditing) access
    "createAuthor" ->
      Authors.M.createAuthor
        access
        (decode body :: Maybe Authors.AuthorCreation)
    "deleteAuthor" ->
      Authors.M.deleteAuthor
        access
        (decode body :: Maybe Authors.AuthorDeletion)
    "editAuthor" ->
      Authors.M.editAuthor access (decode body :: Maybe Authors.AuthorEditing)
    "createCategory" ->
      Categories.M.createCategory
        access
        (decode body :: Maybe Categories.CategoryCreation)
    "deleteCategory" ->
      Categories.M.deleteCategory
        access
        (decode body :: Maybe Categories.CategoryDeletion)
    "editCategory" ->
      Categories.M.editCategory
        access
        (decode body :: Maybe Categories.CategoryEditing)
    "publishComment" ->
      Comments.M.publishComment
        (decode body :: Maybe Comments.CommentPublishing)
    "deleteComment" ->
      Comments.M.deleteComment
        access
        (decode body :: Maybe Comments.CommentDeletion)
    "createDraft" ->
      Drafts.M.createDraft access (decode body :: Maybe Drafts.DraftCreation)
    "deleteDraft" ->
      Drafts.M.deleteDraft access (decode body :: Maybe Drafts.DraftDeletion)
    "editDraft" ->
      Drafts.M.editDraft access (decode body :: Maybe Drafts.DraftEditing)
    "publishPost" ->
      Posts.M.publishPost access (decode body :: Maybe Posts.PostPublishing)
    "deletePost" ->
      Posts.M.deletePost access (decode body :: Maybe Posts.PostDeletion)
    _ -> badRequest "Resource path does not exist."

readIntParam :: Maybe BS.ByteString -> Maybe Int
readIntParam param = maybe Nothing (readMaybe . BSC.unpack) param

runWithParam :: (R.HasResponse b) => (a -> b) -> Maybe a -> b
runWithParam f maybeParam =
  maybe (badRequest "Bad path parameters.") f maybeParam
