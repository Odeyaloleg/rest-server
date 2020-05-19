{-# LANGUAGE OverloadedStrings #-}

module App
  ( runServer
  ) where

import qualified API as API
import qualified API.Entities.Categories as API.Categories
import qualified API.Entities.Tags as API.Tags
import qualified API.Entities.Users as API.Users
import qualified API.HasResponse as API.R
import qualified API.ResponseBuilder as API.B
import Data.Bool (bool)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as MS
import Data.Streaming.Network.Internal (HostPreference(Host))
import qualified Data.Text as T
import qualified Logic as Logic
import Network.HTTP.Types (status200, status404)
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)

instance API.R.HasResponse WAI.Response where
  getResponse apiData =
    let responseBody =
          case apiData of
            API.R.UsersList pageNum ->
              API.B.buildResponse $ API.B.UsersListBuilder []
            API.R.CreateUser firstName lastname profilePicture ->
              API.B.buildResponse $
              API.B.CreateUserBuilder $
              API.Users.User 3 firstName lastname profilePicture 0 False
            API.R.CreateAdmin firstName lastname profilePicture ->
              API.B.buildResponse $
              API.B.CreateUserBuilder $
              API.Users.User 3 firstName lastname profilePicture 0 True
            API.R.DeleteUser uId ->
              API.B.buildResponse $ API.B.DeleteUserBuilder
            API.R.CreateTag tag ->
              API.B.buildResponse $
              if Logic.isPresent $ Logic.Tag tag
                then API.B.BadRequestBuilder "Tag is already present."
                else API.B.CreateTagBuilder $ API.Tags.Tag (length tags + 1) tag
            API.R.CreateCategory Nothing category ->
              API.B.buildResponse $
              if Logic.isPresent $ Logic.Category category
                then API.B.BadRequestBuilder "Category is already present."
                else API.B.CreateCategoryBuilder $
                     API.Categories.CategoryBottom 4 category
            API.R.CreateCategory (Just parentCategoryId) category ->
              API.B.buildResponse $
              if Logic.isPresent $ Logic.Category category
                then API.B.BadRequestBuilder "Category is already present."
                else if Logic.isParentProper $ Logic.ParentCategory parentCategoryId
                       then API.B.CreateCategoryBuilder $
                            API.Categories.CategoryBottom 4 category
                       else API.B.BadRequestBuilder "Unknown parent category."
            API.R.BadRequest description ->
              API.B.buildResponse $ API.B.BadRequestBuilder description
     in WAI.responseLBS
          status404
          [("Content-Type", "application/json")]
          responseBody

tags = ["haskell", "programming"]

categories =
  [ API.Categories.ParentCategory
      1
      "Programming"
      [API.Categories.CategoryBottom 3 "Haskell"]
  , API.Categories.CategoryBottom 2 "Design"
  ]

instance Logic.MustBeUnique Logic.Tag where
  isPresent (Logic.Tag tag) = elem tag tags

instance Logic.MustBeUnique Logic.Category where
  isPresent (Logic.Category category) = helper categories
    where
      helper [] = False
      helper ((API.Categories.CategoryBottom _ cName):rest) =
        if cName == category
          then True
          else helper rest
      helper ((API.Categories.ParentCategory _ cName subcategories):rest) =
        if cName == category
          then True
          else if helper subcategories == True
                 then True
                 else helper rest

instance Logic.HasParent Logic.ParentCategory where
  isParentProper (Logic.ParentCategory parentId) = helper categories
    where
      helper [] = False
      helper ((API.Categories.CategoryBottom cId _):rest) =
        if cId == parentId
          then True
          else helper rest
      helper ((API.Categories.ParentCategory cId _ subcategories):rest) =
        if cId == parentId
          then True
          else if helper subcategories == True
                 then True
                 else helper rest

runServer :: IO ()
runServer = do
  runSettings
    (setPort 3000 $ setHost (Host "192.168.0.140") defaultSettings)
    application

application :: WAI.Application
application request respond = do
  let pathInfo = removeEmpty $ WAI.pathInfo request
  if length pathInfo == 1
    then do
      requestBody <- WAI.strictRequestBody request
      let queryData =
            API.QueryData
              (WAI.requestMethod request)
              (pathInfo !! 0)
              (WAI.queryString request)
              requestBody
      let response = API.runAPI (API.R.AccessAdmin 123) queryData
      respond response
    else respond $ WAI.responseLBS status404 [] ""

removeEmpty :: [T.Text] -> [T.Text]
removeEmpty [] = []
removeEmpty (x:xs) = bool (x : removeEmpty xs) (removeEmpty xs) (T.null x)
