module Main (main, dropEv) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)
import FFI.Util (setProperty)

import Data.Array as Array
import Data.Array (filter)
import Data.Nullable (Nullable)
import Data.Maybe (Maybe, maybe)
import Data.List (head)
import Data.Either (Either (..))
import Data.String.Utils (endsWith)

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
import Node.Path (FilePath)
import Node.FS (FS)
import Node.FS.Sync (readdir)

import Util


main :: forall eff. Eff ("console" :: CONSOLE | eff) Unit
main = do
  log "Hello Electron"

thisDocument :: forall eff. Eff ("dom" :: DOM | eff) HTMLDocument
thisDocument = document =<< window


setValue :: forall eff. Either String Element ->
            Either String String  ->
            Eff ("console" :: CONSOLE,
                 "cp" :: CHILD_PROCESS,
                 "buffer" :: BUFFER,
                 "fs" :: FS,
                 "err" :: EXCEPTION,
                 "dom" :: DOM | eff) Unit
setValue (Right e) (Right s) = do
  pure $ setProperty e "value" s
  execTranslate s
                            -- void $ pure $ e {value = s}
setValue _ (Left err) = log err
setValue (Left err) _ = log err

dropEv :: forall eff. Nullable DragEvent -> 
          Eff ( "console" :: CONSOLE
              , "dom" :: DOM
              , "cp" :: CHILD_PROCESS
              , "buffer" :: BUFFER
              , "fs" :: FS
              , "err" :: EXCEPTION | eff) Unit
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
      Eff ("cp" :: CHILD_PROCESS,
           "buffer" :: BUFFER,
           "fs" :: FS,
           "err" :: EXCEPTION,
           "console" :: CONSOLE | eff) Unit
execTranslate s = do
  mf <- lookupCMH
  maybe (log "cdda-modding-helper.jar is not found") (\fp ->
    ChildProcess.exec 
      ("java -jar " <> fp <> " -p " <> s)
      ChildProcess.defaultExecOptions
      logString
    ) mf

logString :: forall eff. ExecResult ->
             Eff ("buffer" :: BUFFER,
                  "console" :: CONSOLE | eff) Unit
logString r = do
  s <- Buffer.toString UTF8 r.stdout
  log s


lookupCMH :: forall eff. Eff ("fs" :: FS, "err" :: EXCEPTION, "console" :: CONSOLE | eff) (Maybe FilePath)
lookupCMH = do
  files <- readdir "./"
  pure $ Array.head ( filter (endsWith ".jar") files )
