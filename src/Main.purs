module Main (main, dropIv) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, liftEff', makeAff)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throwException)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array as Array
import Data.Array (filter)
import Data.Nullable (Nullable)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), head, (!!))
import Data.Either (Either (..))
import Data.String.Utils (endsWith)
import Data.String (Pattern(..))
import Data.String (split, joinWith) as String
import Data.String.Regex (Regex(..))
import Data.String.Regex (split) as Regex

import DOM.Node.Types (ElementId (..))
import DOM (DOM)
import DOM.File.Types (File)

import Node.ChildProcess (ChildProcess, CHILD_PROCESS, ExecResult
                         , ExecOptions, defaultSpawnOptions)
import Node.Stream (onDataString, writeString)
import Node.Stream (read, onReadable, readString) as Stream
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding (..))
import Node.Buffer (Buffer, BUFFER)
import Node.Buffer as Buffer
import Node.Path (FilePath)
import Node.FS (FS)
import Node.FS.Aff (readdir) as Aff
import Node.FS.Sync (readdir) as Eff

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
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)

import Unsafe.Coerce (unsafeCoerce)

-- InfoItem からは見えない位置を変化させるので pure unit
specInfoItem :: forall eff props . T.Spec eff InfoItem props InfoItemAction
specInfoItem = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render InfoItem props InfoItemAction
  render dispatch _ mi _ =
    [ R.li'
      (sym (mi.symcol) <>
       [ R.a
         [ P.onClick \_ -> dispatch EnterInfo ]
         [ R.text mi.name ]
       ]
      )
    ]
  sym (Just s) = [ R.span [P.className ("symcol-" <> s.color)] [R.text s.symbol] ]
  sym Nothing = []

specResultPane :: forall eff props . T.Spec eff HelperResult props BrowserAction
specResultPane = ulist $ T.focus _results _InfoItemAction $ T.foreach \_ -> specInfoItem
  where
  ulist :: forall state action. T.Spec eff state props action -> T.Spec eff state props action
  ulist = over T._render \render d p s c ->
    [ R.ul' (render d p s c) ]

-- ResizablePaneW で包む
specBrowser :: forall eff props . T.Spec eff CMHFState props CMHFAction
specBrowser =
  mkSpecResizableW (_BrowserLayout <<< _resultPaneWidth) _UIAction 
    (T.focus _HelperResult _BrowserAction specResultPane)
  <>
  T.focusState _HelperResult specRawJson

main :: Eff _ Unit
main = do
  app <- toMaybe <$> HU.getElementById' (ElementId "item_browser")
  f <- lookupCMH'
  chap <- approach f
  TU.defaultMain (catchEnterInfo specBrowser) (testCMHFState {process = Just chap}) unit app

catchEnterInfo :: forall props
                  . T.Spec _ CMHFState props CMHFAction
                  -> T.Spec _ CMHFState props CMHFAction
catchEnterInfo = over T._performAction \pa a p s ->
  case a of
    (BrAct (ItemAction n EnterInfo)) -> do
      raw <- lift $ piv s.process (s.result.results !! n)
      lift $ liftEff $ log $ show raw
      addriv raw
    _ -> pa a p s
  where
    -- 連鎖できそう
    piv :: Maybe ChildProcess -> Maybe InfoItem -> Aff _ (Maybe String)
    piv (Just process) (Just item) = rawQuery process item.index
    piv Nothing _ = pure Nothing --"no connection"
    piv _ Nothing = pure Nothing --"no index"
    _Raw = _HelperResult <<< _raw
    addriv r@(Just _) = void $ T.cotransform (\state -> set _Raw r state)
    addriv Nothing = pure unit
    
{-
testP :: forall eff. Eff ("console" :: CONSOLE | eff) Unit
testP = vif $ jsonParser testJson
  where
    vif (Left s) = log s
    vif (Right json) = log $ show json
-}

-- raw mode で発行した Result から Buffer.toString :: Encoding -> Buffer -> Eff _ String でもってくる
specRawJson :: forall eff props action . T.Spec eff HelperResult props action
specRawJson = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render HelperResult props action
    render _ _ s _ =
      [ R.pre'
        [
          R.code'
          [ R.text $ vif s.raw ]
        ]
      ]
    vif Nothing = "(none)"
    vif (Just rawJson) = rawJson -- HTML用にescapeが必要かも

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






-- 内部状態 と 外部状態 とで分けたみある
type TranslationHelperState =
  { processing :: Boolean
  , target :: Maybe String
  , hover :: Boolean }
initialTranslationHelperState :: TranslationHelperState
initialTranslationHelperState = { processing: false, target: Nothing, hover: false }

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
        [ R.text $ lext s.target ]
      ]
    lext :: Maybe String -> String
    lext Nothing = "ここに翻訳したいModをドロップ"
    lext (Just s)  = "変換中...[" <> s <> "]"
    cls :: Boolean -> String
    cls false = "focus droparea"
    cls true  = "dragover focus droparea"
    performAction :: T.PerformAction _ TranslationHelperState props TranslationHelperAction
    performAction (Translate (Right fs)) _ _ = void $ do
      let t = (toMaybe <<< path) =<< head fs
      T.cotransform (_ { target = t, hover = false})
      lift $ mvp t
      T.cotransform (_ { target = Nothing})
    performAction (Translate (Left e)) _ _ = void $ do
      lift <<< liftEff <<< log <<< show $ e
    performAction (Hover b) _ _ = void $ T.cotransform (_ {hover = b})
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

lookupCMH' :: forall eff. Eff ("fs" :: FS, "err" :: EXCEPTION | eff) FilePath
lookupCMH' = do
  files <- Eff.readdir "./"
  pass $ Array.head ( filter (endsWith ".jar") files )
  where
    pass (Just file) = pure file
    pass Nothing = (throwException <<< error) "missing cdda-modding-helper"



rawQuery :: forall eff
            . ChildProcess
            -> String -- | index
            -> Aff _ (Maybe String)
rawQuery p ix = do
  b <- liftEff $ isReady p
  liftEff $ log $ show b
  dop b
  s <- readAsStringAff p
  pure (clip =<< s)
  where
    clip s = String.joinWith "\n" <$> (abc $ String.split (Pattern "\n") $ s)
    abc s = dropPrompt s (Array.unsnoc s)
    dropPrompt s (Just ss) =
      case ss.last of
        "Browser > " ->
          case ss.init of
            [] -> Nothing
            _ -> Just ss.init
        _ -> Just s
    dropPrompt s Nothing = Nothing
    dop true  = writeQueryAff p ("find #" <> ix <> " forFacadeRaw")
    dop false = pure unit

listQuery :: forall eff
             . ChildProcess
             -> String
             -> Aff _ Json
listQuery p q = do
  writeQueryAff p (q <> " short forFacadeList up to 100")
  str <- readStdoutAff p
  pass $ jsonParser str
  where
    pass (Left s) = liftEff <<< throwException <<< error $ s
    pass (Right json) = pure json

approach :: forall eff . String -> Eff ( "cp" :: CHILD_PROCESS | eff ) ChildProcess
approach jar =
  ChildProcess.spawn "java" ["-jar", jar, "-b"] defaultSpawnOptions

writeQuery :: forall eff
             . ChildProcess
             -> String
             -> (Unit -> Eff ("cp" :: CHILD_PROCESS, "console" :: CONSOLE | eff ) Unit)
             -> Eff ("cp" :: CHILD_PROCESS, "console" :: CONSOLE | eff ) Unit
writeQuery p q callback = do
  log (q <> "\n")
  b <- writeString (ChildProcess.stdin p) UTF8 (q <> "\n") (callback unit)
  pure unit

writeQueryAff :: forall eff
             . ChildProcess
             -> String
             -> Aff ("cp" :: CHILD_PROCESS, "console" :: CONSOLE | eff ) Unit
writeQueryAff p q = makeAff \errCb cb -> writeQuery p q cb

-- deprecated
readStdoutAff :: forall eff
              . ChildProcess
              -> Aff ("cp" :: CHILD_PROCESS, "err" :: EXCEPTION | eff ) String
readStdoutAff p =
   makeAff \errCb cb -> onDataString (ChildProcess.stdout p) UTF8 cb

readAsBuffer :: forall eff
                . ChildProcess
                -> Eff ("cp" :: CHILD_PROCESS, "err" :: EXCEPTION | eff) (Maybe Buffer)
readAsBuffer p = do
  Stream.read (ChildProcess.stdout p) Nothing

readAsStringAff :: forall eff
                . ChildProcess
                -> Aff ("cp" :: CHILD_PROCESS, "err" :: EXCEPTION | eff) (Maybe String)
readAsStringAff p = makeAff \errCb cb -> Stream.onReadable src (docb cb)
  where
    src = ChildProcess.stdout p
    docb cb = do
      ss <- Stream.readString src Nothing UTF8
      cb ss


isReady :: forall eff
           . ChildProcess
           -> Eff _ Boolean
isReady p = do
  mb <- readAsBuffer p
  vif mb
  where
    vif (Just b) = do
      str <-Buffer.toString UTF8 b
      log str
      pure $ endsWith "Browser > " str
    vif Nothing = pure true
