
module Util (toEither, mtoe, dataTransfer, path, filelistToList, intToString, dragEventToList) where

import Prelude
import Control.Monad.Eff.Exception (Error, error)

import Data.List (List (..), (:), catMaybes)
import Data.Either (Either (..))
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe, maybe)

import DOM.File.Types (File, FileList)
import DOM.File.FileList (length, item)
import DOM.HTML.Event.Types (DragEvent)
import DOM.HTML.Event.DragEvent.DataTransfer (DataTransfer)
import DOM.HTML.Event.DragEvent.DataTransfer (files) as DT

import React (Event) as R

foreign import dataTransferR :: R.Event -> Nullable DataTransfer

foreign import dataTransfer :: DragEvent -> Nullable DataTransfer

-- | defined in nsIFile
foreign import path :: File -> Nullable String

-- | use primitive cast
foreign import intToString :: Int -> String

toEither :: forall a b. b -> Nullable a -> Either b a
toEither l r = mtoe l (toMaybe r)

mtoe :: forall a b. b -> Maybe a -> Either b a
mtoe l r = maybe (Left l) Right r

filelistToList :: FileList -> List File
filelistToList fl = catMaybes $ toMaybe <$> fli 0
  where
    lim = length fl
    fli i | i >= lim = Nil
          | otherwise = (item i fl) : (fli $ i + 1)

dragEventToList :: R.Event -> Either Error (List File)
dragEventToList evn = do
  dt <- toEither (error "missing DragEvent.dataTransfer") (dataTransferR evn)
  fl <- mtoe (error "missing DragEvent.dataTransfer.files") (DT.files dt)
  pure $ filelistToList fl
