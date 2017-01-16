module Main (main, dropEv) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)
import FFI.Util (setProperty)

import Data.Array as Array
import Data.Array (filter)
import Data.Nullable (Nullable)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.List (List(Cons, Nil), (:), head)
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

import Util
import Thermite.MyUtil as TU
import DOM.HTML.MyUtil as HU

import Data.Lens
-- Lens (lens)
-- Prism (prism, Prism' APrism')
-- Getter ((^.))
-- Setter (over, set, Setter')
--import Optic.Lens (lens)
--import Optic.Prism (prism)
--import Optic.Setter (over)
--import Optic.Types (Lens', Prism')
import Data.Tuple
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as P
import ReactDOM as RD
import Data.Nullable (toMaybe)

import Unsafe.Coerce (unsafeCoerce)

type SymCol =
  { symbol :: String
  , color :: String }

type InfoItem =
  { symcol :: Maybe SymCol
  , name :: String
  , index :: String }

data InfoItemAction
  = EnterInfo

data BrowserAction
  = ItemAction Int InfoItemAction
  | SendQuery String

mockInfoItem :: String -> InfoItem
mockInfoItem n = { symcol: Nothing, name: n, index: "#undefined" }

type HelperResult =
  { results :: List InfoItem
  }

initialHelperResult :: HelperResult
initialHelperResult = { results: Nil }

hrSetTestData :: HelperResult -> HelperResult
hrSetTestData hr = hr { results = ls }
  where
    ls = (mockInfoItem "abc") : (mockInfoItem "123") : Nil

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

_results :: Lens' HelperResult (List InfoItem)
_results = lens _.results (_ {results = _ })

_InfoItemAction :: Prism' BrowserAction (Tuple Int InfoItemAction)
_InfoItemAction = prism (uncurry ItemAction) \iia ->
  case iia of
    ItemAction i a -> Right (Tuple i a)
    _ -> Left iia


specResultPane :: forall eff props . T.Spec eff HelperResult props BrowserAction
specResultPane = ulist $ T.focus _results _InfoItemAction $ T.foreach \_ -> specInfoItem
  where
  ulist :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state props action
  ulist = over T._render \render d p s c ->
    [ R.ul' (render d p s c) ]


specBrowser :: forall eff props . T.Spec eff CMHFState props CMHFAction
specBrowser =
  mkSpecResizableW (_CMHFS2BL <<< _resultPaneWidth) _CMHFA2UIA $
    T.focus _CMHFS2HR _CMHFA2BrA specResultPane

-- T.focus _CMHFS2HR _CMHFA2BrA (T.Spec eff HelperResult props BrowserAction)
-- <>
-- T.focus _CMHFS2BL _CMHFA2UIA (T.Spec eff BrowserLayout props UIAction)
-- mkSpecResizableW _resultPaneWidth _CMHFA2UIA (T.Spec eff BrowserLayout props UIAction) ココに渡したい
-- -- -- -- --
-- mkSpecResizableW :: forall eff state props action0 action1
--                   . Lens' state Int
--                  -> Prism' action0 UIAction
--                  -> Prism' action0 action1
--                  -> Spec eff state props action1
--                  -> Spec eff state props action0


main :: forall eff. Eff ("dom" :: DOM | eff) Unit
main = do
  d <- thisDocument
  app <- toMaybe <$> HU.getElementById' (ElementId "item_browser")
  TU.defaultMain specBrowser testCMHFState unit app


type BrowserLayout =
  { resultPaneWidth :: Int
  , itemInfoHeight :: Int
  }
initialBrowserLayout :: BrowserLayout
initialBrowserLayout = { resultPaneWidth: 100, itemInfoHeight: 100 }

_resultPaneWidth :: Lens' BrowserLayout Int
_resultPaneWidth = lens _.resultPaneWidth (_ {resultPaneWidth = _})
_itemInfoHeight :: Lens' BrowserLayout Int
_itemInfoHeight = lens _.itemInfoHeight (_ {itemInfoHeight = _})
-- x bound: 10px (document.width - 10px)
-- y bound:

type CMHFState =
  { layout :: BrowserLayout
  , result :: HelperResult
  }
initialCMHFState :: BrowserLayout -> HelperResult -> CMHFState
initialCMHFState bl hr = { layout: bl, result: hr }

testCMHFState = initialCMHFState initialBrowserLayout (hrSetTestData initialHelperResult)

_CMHFS2BL :: Lens' CMHFState BrowserLayout
_CMHFS2BL = lens _.layout (_ {layout = _})
_CMHFS2HR :: Lens' CMHFState HelperResult
_CMHFS2HR = lens _.result (_ {result = _})

data UIAction -- ui action ?
  = PartialPaddlePos Int

data CMHFAction
  = BrAct BrowserAction
  | UIAct UIAction

_CMHFA2BrA :: Prism' CMHFAction BrowserAction
_CMHFA2BrA = prism BrAct \a ->
  case a of
    BrAct hr -> Right hr
    _ -> Left a

_CMHFA2UIA :: Prism' CMHFAction UIAction
_CMHFA2UIA = prism UIAct \a ->
  case a of
    UIAct uia -> Right uia
    _ -> Left a

-- event.target.clientX - this.width/2
-- event.target.clientY - this.height/2
-- event.target.parent.style.width?

-- div > div.content + div.paddle < div ...
-- で width とかを変更するのは外側の div.style のパラメータ指定でやる
-- Prism' Int String

-- drag中に preventDefault が必要
-- ↓ コピペ -20点
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
            [ R.text "|" ]
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
        [ P.unsafeMkProps "style" ("height: " ++ intToString (layout ^. _height) ++ "px")
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

-- focus Lens s2 s1 Prism a2 a1 で変換が必要
-- Spec eff state props action-> Spec eff BrowserLayout? props PaddleActon
--specDraggablePaddle :: forall eff state proos action . T.Spec eff state props action
{-
  $('.rsh').draggable({
    axis: 'y', 
    containment: 'parent',
    helper: 'clone', 
    drag: function (event, ui) { 
        var height = ui.offset.top; 
        $(this).prev().height(height); 
    } 
});
-}

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
