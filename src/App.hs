{-# LANGUAGE OverloadedStrings #-}

module App
  ( runServer
  ) where

import qualified API
import qualified API.Entities as API.E
import Data.Bool (bool)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as MS
import Data.Streaming.Network.Internal (HostPreference(Host))
import qualified Data.Text as T
import Network.HTTP.Types (status200, status404)
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)

instance API.HasResponse WAI.Response where
  getResponse apiData =
    let responseBody =
          case apiData of
            API.UsersList pageNum ->
              API.buildResponse $ API.UsersListBuilder users
            API.CreateUser user ->
              API.buildResponse $ API.CreateUserBuilder user
            API.NewsList -> API.buildResponse $ API.NewsBuilder "News list."
            API.NewsItem _ ->
              API.buildResponse $ API.NewsItemBuilder "News item."
            API.BadRequest description ->
              API.buildResponse $ API.BadRequestBuilder description
     in WAI.responseLBS
          status404
          [("Content-Type", "application/json")]
          responseBody

users =
  [ API.E.User 1 "Oleg" "Romashin" Nothing "Yesterday" True
  , API.E.User 2 "Yaroslav" "Romashin" Nothing "Today" False
  ]

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
      let queryData = API.QueryData (pathInfo !! 0) (WAI.queryString request) requestBody
      let response = API.runAPI queryData API.AccessAdmin
      respond response
    else respond $ WAI.responseLBS status404 [] ""

removeEmpty :: [T.Text] -> [T.Text]
removeEmpty [] = []
removeEmpty (x:xs) = bool (x : removeEmpty xs) (removeEmpty xs) (T.null x)
