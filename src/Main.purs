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
import String ((++))
import Data.String.Utils (endsWith)

import DOM.HTML.Event.Types (DragEvent)
import DOM.Node.Types (Element, ElementId (..))
import DOM.Node.NonElementParentNode (getElementById)
import DOM.HTML.Types (HTMLDocument, htmlDocumentToNonElementParentNode)
import DOM.HTML.Event.DragEvent.DataTransfer as DT
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

import Main.Data
import Util
import Thermite.MyUtil as TU
import DOM.HTML.MyUtil as HU

import Data.Lens
-- Lens (lens)
-- Prism (prism, Prism' APrism')
-- Getter ((^.))
-- Setter (over, set, Setter')
import Data.Tuple
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as P
import ReactDOM as RD
import Data.Nullable (toMaybe)

import Unsafe.Coerce (unsafeCoerce)

-- InfoItem からは見えない位置を変化させるので pure unit
specInfoItem :: forall eff props . T.Spec eff InfoItem props InfoItemAction
specInfoItem = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render InfoItem props InfoItemAction
  render dispatch _ mi _ =
    [ R.li'
      [ R.a
        [ P.onClick \_ -> dispatch EnterInfo ]
        [ R.text mi.name ]
      ]
    ]


specResultPane :: forall eff props . T.Spec eff HelperResult props BrowserAction
specResultPane = ulist $ T.focus _results _InfoItemAction $ T.foreach \_ -> specInfoItem
  where
  ulist :: forall state action. T.Spec eff state props action -> T.Spec eff state props action
  ulist = over T._render \render d p s c ->
    [ R.ul' (render d p s c) ]


specBrowser :: forall eff props . T.Spec eff CMHFState props CMHFAction
specBrowser =
  mkSpecResizableW (_BrowserLayout <<< _resultPaneWidth) _UIAction $
    T.focus _HelperResult _BrowserAction specResultPane

main :: forall eff. Eff ("dom" :: DOM | eff) Unit
main = do
  d <- thisDocument
  app <- toMaybe <$> HU.getElementById' (ElementId "item_browser")
  TU.defaultMain specBrowser testCMHFState unit app


-- event.clientX - this.width/2
-- event.clientY - this.height/2

-- div > div.content + div.paddle < div ...
-- で width とかを変更するのは外側の div.style のパラメータ指定でやる
-- _px :: Prism' Int String

--    drag中に preventDefault が必要 ;todo
--    型安全でない -20pt
-- ↓ コピペ -20pt
mkSpecResizableW :: forall eff state props action
                    . Lens' state Int
                    -> Prism' action UIAction
                    -> T.Spec eff state props action
                    -> T.Spec eff state props action
mkSpecResizableW _width _pUIA = (catchPaddleAction _width _pUIA) <<< touchRender
  where
    touchRender :: T.Spec eff state props action
                   -> T.Spec eff state props action
    touchRender = over T._render \render dispatch p layout c ->
      [ R.div
        [ P.style {width: (intToString (layout ^. _width) ++ "px")}
        , P.className "expand-w"
        ]
        (
          [ R.span
            [ P.className "paddle paddle-x"
            , P.draggable true
            , P.onDragEnd \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos) (unsafeCoerce e).clientX
            , P.onDrag \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos) (unsafeCoerce e).clientX
            ]
            []
          ]
          <> render dispatch p layout c
        )
      ]
mkSpecResizableH :: forall eff state props action
                    . Lens' state Int
                    -> Prism' action UIAction
                    -> T.Spec eff state props action
                    -> T.Spec eff state props action
mkSpecResizableH _height _pUIA = (catchPaddleAction _height _pUIA) <<< touchRender
  where
    touchRender :: T.Spec eff state props action
                   -> T.Spec eff state props action
    touchRender = over T._render \render dispatch p layout c ->
      [ R.div
        [ P.style {height: (intToString (layout ^. _height) ++ "px")}
        , P.className "expand-h"
        ]
        ( render dispatch p layout c <>
          [ R.div
            [ P.className "paddle paddle-y"
            , P.draggable true
            , P.onDragEnd \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos) (unsafeCoerce e).clientY
            , P.onDrag \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos) (unsafeCoerce e).clientY
            ]
            []
          ]
        )
      ]
catchPaddleAction :: forall eff state props action
                     . Setter' state Int
                     -> APrism' action UIAction
                     -> T.Spec eff state props action
                     -> T.Spec eff state props action
catchPaddleAction _amount _pUIA = over T._performAction \pa a p s ->
  case matching _pUIA a of
    Right (PartialPaddlePos x) -> void (T.cotransform (\state -> set _amount x state))
    _ -> pa a p s



-- ↓こっから下はそのうち消えてなくなる

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
