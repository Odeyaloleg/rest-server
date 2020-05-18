module API.Methods.Categories where

import qualified API.Entities.Categories as E
import qualified API.HasResponse as R

getCategories :: (R.HasResponse a) => R.PageNum -> a
getCategories pageNum = R.getResponse $ R.CategoriesList pageNum

createCategory :: (R.HasResponse a) => R.AccessLevel -> Maybe E.CategoryCreation -> a
createCategory access categoryMaybe =
  R.withAdminAccess
    access
    (case categoryMaybe of
       Just (E.CategoryCreation subcategoryId category) -> R.getResponse $ R.CreateCategory subcategoryId category
       Nothing -> R.wrongJSON)

deleteCategory :: (R.HasResponse a) => R.AccessLevel -> Maybe E.CategoryDeletion -> a
deleteCategory access categoryMaybe =
  R.withAdminAccess
    access
    (case categoryMaybe of
       Just (E.CategoryDeletion categoryId) -> R.getResponse $ R.DeleteCategory categoryId
       Nothing -> R.wrongJSON)

editCategory :: (R.HasResponse a) => R.AccessLevel -> Maybe E.CategoryEditing -> a
editCategory access categoryMaybe =
  R.withAdminAccess
    access
    (case categoryMaybe of
       Just (E.CategoryEditing categoryId subcategoryId category) -> R.getResponse $ R.EditCategory categoryId subcategoryId category
       Nothing -> R.wrongJSON)