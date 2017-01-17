module Main (main, dropIv, testP) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, liftEff', makeAff)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throwException)
import Control.Monad.Eff.Console (CONSOLE, log)
import FFI.Util (setProperty)

import Data.Array as Array
import Data.Array (filter)
import Data.Nullable (Nullable)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), head)
import Data.Either (Either (..))
import Data.String.Utils (endsWith)

import DOM.HTML.Event.Types (DragEvent)
import DOM.Node.Types (Element, ElementId (..))
import DOM.Node.NonElementParentNode (getElementById)
import DOM.HTML.Types (HTMLDocument, htmlDocumentToNonElementParentNode)
import DOM.HTML.Event.DragEvent.DataTransfer as DT
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM (DOM)
import DOM.File.Types (File)

import Node.ChildProcess (ChildProcess, CHILD_PROCESS, ExecResult, ExecOptions)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding (..))
import Node.Buffer (Buffer, BUFFER)
import Node.Buffer as Buffer
import Node.Path (FilePath)
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Node.FS.Aff (readdir) as Aff

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
import Data.Argonaut.Parser (jsonParser)

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


-- ResizablePaneW で包む
specBrowser :: forall eff props . T.Spec eff CMHFState props CMHFAction
specBrowser =
  mkSpecResizableW (_BrowserLayout <<< _resultPaneWidth) _UIAction $
    T.focus _HelperResult _BrowserAction specResultPane

main :: forall eff. Eff ("dom" :: DOM | eff) Unit
main = do
  app <- toMaybe <$> HU.getElementById' (ElementId "item_browser")
  TU.defaultMain specBrowser testCMHFState unit app


testP :: forall eff. Eff ("console" :: CONSOLE | eff) Unit
testP = vif $ jsonParser testJson
  where
    vif (Left s) = log s
    vif (Right json) = log $ show json


-- event.clientX - this.width/2
-- event.clientY - this.height/2

-- div > div.content + div.paddle < div ...
-- で width とかを変更するのは外側の div.style のパラメータ指定でやる
-- _px :: Prism' Int String


-- UI Component --
{-
mkSpecResizable :: forall eff state props action
                   . String
                   -> String
                   -> (P.Event -> Int)
                   -> Lens' state Int
                   -> Prism' action UIAction
                   -> T.Spec eff state props action
                   -> T.Spec eff state props action
mkSpecResizable containerClass paddleClass f _amount _UIA =
-}
  
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
        [ P.style {width: (intToString (layout ^. _width) <> "px")}
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
        [ P.style {height: (intToString (layout ^. _height) <> "px")}
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






type TranslationHelperState =
  { processing :: Boolean, hover :: Boolean }
initialTranslationHelperState :: TranslationHelperState
initialTranslationHelperState = { processing: false, hover: false }

data TranslationHelperAction
  = Translate (Either Error (List File))
  | Hover Boolean


specDropArea :: forall  props
                . T.Spec _ TranslationHelperState props TranslationHelperAction
specDropArea = T.simpleSpec performAction render
  where
    render :: T.Render TranslationHelperState props TranslationHelperAction
    render dispatch _ s _ =
      [ R.div
        [ P.className $ cls s.hover
        , P.onDragEnter \e -> do
          R.stopPropagation e
          R.preventDefault e
          (dispatch <<< Hover) true
        , P.onDragLeave \e -> do
          R.stopPropagation e
          R.preventDefault e
          (dispatch <<< Hover) false
        , P.onDrop \e -> do
          R.stopPropagation e
          R.preventDefault e
          (dispatch <<< Translate) $ dragEventToList e
        ]
        [ R.text $ lext s.processing ]
      ]
    lext :: Boolean -> String
    lext false = "ここに翻訳したいModをドロップ"
    lext true  = "変換中..."
    cls :: Boolean -> String
    cls false = "focus droparea"
    cls true  = "dragover focus droparea"
    performAction :: T.PerformAction _ TranslationHelperState props TranslationHelperAction
    performAction (Translate (Right fs)) _ _ = void $ do
      lift $ liftEff $ log "ipp"
      T.cotransform (_ { processing = true})
      lift $ mvp ( (toMaybe <<< path) =<< head fs)
      T.cotransform (_ { processing = false, hover = false})
    performAction (Translate (Left e)) _ _ = void $ do
      lift <<< liftEff <<< log <<< show $ e
    performAction (Hover b) _ _ = void $ T.cotransform (_ {hover = b})
    performAction _ _ _ = pure unit
    mvp :: Maybe String -> Aff _ Unit
    mvp (Just s) = void $ execTranslateAff s
    mvp Nothing  = liftEff $ log "no drop"
    -- Either Error String がいいな

dropIv = do
  app <- toMaybe <$> HU.getElementById' (ElementId "droparea")
  TU.defaultMain specDropArea initialTranslationHelperState unit app



execTranslateAff :: forall eff . String
                    -> Aff ("fs" :: FS, "err" :: EXCEPTION, "cp" :: CHILD_PROCESS | eff) ExecResult
execTranslateAff s = do
  fp <- lookupCMH
  execAff ("java -jar " <> fp <> " -p " <> s) ChildProcess.defaultExecOptions

execAff :: forall eff . String -> ExecOptions -> Aff ( "cp" :: CHILD_PROCESS | eff) ExecResult
execAff arg opts = makeAff \_ callback -> ChildProcess.exec arg opts callback

lookupCMH :: forall eff. Aff ("fs" :: FS, "err" :: EXCEPTION | eff) FilePath
lookupCMH = do
  files <- Aff.readdir "./"
  pass $ Array.head ( filter (endsWith ".jar") files )
  where
    pass (Just file) = pure file
    pass Nothing = (liftEff <<< throwException <<< error) "missing cdda-modding-helper"

