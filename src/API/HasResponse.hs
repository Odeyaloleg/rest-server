module API.HasResponse where

import qualified API.Entities.Tags as Tags
import qualified API.Entities.Users as Users
import qualified Types as T

data AccessLevel
  = AccessUser T.UserId
  | AccessAuthor T.AuthorId
  | AccessAdmin T.AdminId
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
  | PublishComment T.PostId String
  | DeleteComment T.CommentId
  | DraftsList T.AuthorId T.PageNum
  | PublishDraft T.DraftId
  | CreateDraft
      (Maybe T.PostId)
      (Maybe T.CategoryId)
      (Maybe T.Title)
      (Maybe [T.TagId])
      (Maybe T.MainPicture)
      (Maybe T.Content)
      (Maybe [T.AdditionalPicture])
  | DeleteDraft T.DraftId
  | EditDraft
      T.DraftId
      (Maybe T.CategoryId)
      (Maybe T.Title)
      (Maybe [T.TagId])
      (Maybe T.MainPicture)
      (Maybe T.Content)
      (Maybe [T.AdditionalPicture])
  | PostsList T.PageNum
  | PublishPost
      T.CategoryId
      T.Title
      (Maybe [T.TagId])
      (Maybe T.Content)
      (Maybe T.MainPicture)
      (Maybe [T.AdditionalPicture])
  | DeletePost T.PostId
  | BadRequest String

class HasResponse a where
  getResponse :: Request -> a

withAdminAccess :: (HasResponse a) => AccessLevel -> a -> a
withAdminAccess access request =
  case access of
    AccessAdmin _ -> request
    _ -> wrongPath

withAuthorAccess :: (HasResponse a) => AccessLevel -> a -> a
withAuthorAccess access request =
  case access of
    AccessAdmin _ -> request
    _ -> notAuthor

wrongJSON :: (HasResponse a) => a
wrongJSON = getResponse $ BadRequest "Wrong\\insufficient JSON data."

wrongPath :: (HasResponse a) => a
wrongPath = getResponse $ BadRequest "Resource path does not exist."

notAuthor :: (HasResponse a) => a
notAuthor = getResponse $ BadRequest "No rights provided. Needed to be an author."
