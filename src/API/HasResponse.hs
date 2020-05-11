module API.HasResponse where

import qualified API.Entities.Tags as Tags
import qualified API.Entities.Users as Users
import qualified Types as T

data AccessLevel
  = AccessUser Int
  | AccessAuthor Int
  | AccessAdmin Int
  deriving (Eq)

data Request
  = UsersList T.PageNum
  | CreateUser T.FirstName T.LastName T.ProfilePicture
  | CreateAdmin T.FirstName T.LastName T.ProfilePicture
  | DeleteUser T.UserId
  | TagsList T.PageNum
  | CreateTag String
  | DeleteTag T.TagId
  | EditTag T.TagId String
  | AuthorsList T.PageNum
  | CreateAuthor T.AuthorDescription
  | DeleteAuthor T.AuthorId
  | EditAuthor T.AuthorId T.AuthorDescription
  | CategoriesList T.PageNum
  | CreateCategory (Maybe T.SubcategoryId) String
  | DeleteCategory T.CategoryId
  | EditCategory T.CategoryId (Maybe T.SubcategoryId) String
  | CommentsList T.PostId T.PageNum
  | DraftsList
  | CreateDraft
      (Maybe T.CategoryId)
      (Maybe T.Title)
      (Maybe [T.TagId])
      (Maybe T.MainPicture)
      (Maybe T.Content)
      (Maybe [T.AdditionalPicture])
  | DeleteDraft T.Id
  | EditDraft
      T.Id
      (Maybe T.CategoryId)
      (Maybe T.Title)
      (Maybe [T.TagId])
      (Maybe T.MainPicture)
      (Maybe T.Content)
      (Maybe [T.AdditionalPicture])
  | CreatePost
      T.CategoryId
      T.Title
      (Maybe [T.TagId])
      (Maybe T.Content)
      (Maybe T.MainPicture)
      (Maybe [T.AdditionalPicture])
  | DeletePost T.PostId
  | EditPost
      T.Id
      (Maybe T.CategoryId)
      (Maybe T.Title)
      (Maybe [T.TagId])
      (Maybe T.MainPicture)
      (Maybe T.Content)
      (Maybe [T.AdditionalPicture])
  | BadRequest String

class HasResponse a where
  getResponse :: Request -> a

withAdminAccess :: (HasResponse a) => AccessLevel -> a -> a
withAdminAccess access request =
  case access of
    AccessAdmin _ -> request
    _ -> wrongPath

wrongJSON :: (HasResponse a) => a
wrongJSON = getResponse $ BadRequest "Wrong\\insufficient JSON data."

wrongPath :: (HasResponse a) => a
wrongPath = getResponse $ BadRequest "Resource path does not exist."
