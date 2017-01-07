module Main (main, dropEv) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import FFI.Util (setProperty)

import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe (..), maybe)
import Data.List (List (..), (:), head, catMaybes)
import Data.Either (Either (..), either)
import Data.Semigroup ((<>))

import DOM.HTML.Event.Types (DragEvent)
import DOM.Node.Types (Element, ElementId (..))
import DOM.Node.NonElementParentNode (getElementById)
import DOM.HTML.Types (HTMLDocument, htmlDocumentToNonElementParentNode)
import DOM.HTML.Event.DragEvent.DataTransfer as DT
import DOM.HTML.Event.DragEvent.DataTransfer (DataTransfer)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM (DOM)

import Node.ChildProcess (ChildProcess, CHILD_PROCESS, ExecResult)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding (..))
import Node.Buffer (Buffer, BUFFER)
import Node.Buffer as Buffer

import DOM.File.Types (File, FileList)
import DOM.File.FileList (length, item)


main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log "Hello Electron"

thisDocument :: forall eff. Eff (dom :: DOM | eff) HTMLDocument
thisDocument = document =<< window

filelistToList :: FileList -> List File
filelistToList fl = catMaybes $ toMaybe <$> fli 0
  where
    lim = length fl
    fli i | i >= lim = Nil
          | otherwise = (item i fl) : (fli $ i + 1)

foreign import dataTransfer :: DragEvent -> Nullable DataTransfer
-- | defined in nsIFile
foreign import path :: File -> Nullable String

toEither :: forall a b. b -> Nullable a -> Either b a
toEither l r = mtoe l (toMaybe r)

mtoe :: forall a b. b -> Maybe a -> Either b a
mtoe l r = maybe (Left l) Right r


setValue :: forall eff. Either String Element ->
            Either String String  ->
            Eff (console :: CONSOLE,
                 cp :: CHILD_PROCESS,
                 buffer :: BUFFER,
                 dom :: DOM | eff) Unit
setValue (Right e) (Right s) = do
  pure $ setProperty e "value" s
  execTranslate s
                            -- void $ pure $ e {value = s}
setValue _ (Left err) = log err
setValue (Left err) _ = log err

--dropEv :: forall eff. Nullable DragEvent -> 
--          Eff (console :: CONSOLE, dom :: DOM | eff) Unit
dropEv event = do
  d <- thisDocument
  e <- getElementById 
         (ElementId "out")
         (htmlDocumentToNonElementParentNode d)
  setValue 
    (toEither "'out' is not found" e) 
    (getPath =<< getHead =<< deToFiles =<< toEither "DragEvent is null" event)
  where
    deToFiles de = do
      m <- (toEither "missing DragEvent.dataTransfer" <<< dataTransfer) de
      y <- mtoe "missing DragEvent.dataTransfer.files" (DT.files m)
      pure $ filelistToList y
    getHead fl = mtoe "empty FileList" (head fl)
    getPath = toEither "missing nsIFile.path" <<< path

execTranslate :: forall eff. String ->
      Eff (cp :: CHILD_PROCESS, buffer :: BUFFER, console :: CONSOLE | eff) Unit
execTranslate s = 
  ChildProcess.exec 
    ("ls -l " <> s) -- fs使ってjarファイルを自動で探す?
    ChildProcess.defaultExecOptions
    logString

logString :: forall eff. ExecResult ->
             Eff (buffer :: BUFFER,
--                 cp :: CHILD_PROCESS,
                  console :: CONSOLE | eff) Unit
logString r = do
  s <- Buffer.toString UTF8 r.stdout
  log s
