module API.HasResponse where

import qualified API.Entities.Users as Users
import qualified API.Entities.Tags as Tags

data AccessLevel
  = AccessUser
  | AccessAuthor Int
  | AccessAdmin
  deriving (Eq)

type PageNum = Int

data Request
  = UsersList PageNum
  | CreateUser Users.User
  | DeleteUser Int
  | TagsList PageNum
  | CreateTag Tags.TagCreation
  | DeleteTag Int
  | EditTag Int String
  | BadRequest String

class HasResponse a where
  getResponse :: Request -> a
