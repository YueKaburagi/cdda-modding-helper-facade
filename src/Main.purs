module Main (main, dropIv) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throwException)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List, head, (!!))
import Data.Either (Either (..))
import Data.String (split, joinWith) as String
import Data.String.Regex (regex)
import Data.String.Regex (split) as Regex
import Data.String.Regex.Flags as RegexFlags

import DOM.Node.Types (ElementId (..))
import DOM (DOM)
import DOM.File.Types (File)

import Node.ChildProcess (CHILD_PROCESS, ChildProcess)
import Node.Buffer (BUFFER)

import Main.Data
import Util
import Outer
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
import Data.Argonaut.Core (Json, jsonNull)

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
  sym Nothing = [ R.span [] [] ]

specResultPane :: forall eff props . T.Spec eff HelperResult props BrowserAction
specResultPane = ulist $ T.focus _results _InfoItemAction $ T.foreach \_ -> specInfoItem
  where
  ulist :: forall state action. T.Spec eff state props action -> T.Spec eff state props action
  ulist = over T._render \render d p s c ->
    [ R.ul' (render d p s c) ]

specContain :: forall eff state props action
               . String
               -> T.Spec eff state props action
               -> T.Spec eff state props action
specContain cls = over T._render \render d p s c ->
  [ R.div [ P.className cls ] (render d p s c) ]

-- ResizablePaneW で包む
specBrowser :: forall eff props . T.Spec _ CMHFState props CMHFAction
specBrowser =
  specContain "browser" (
    mkSpecResizableW (_BrowserLayout <<< _resultPaneWidth) _UIAction 
      (T.focus _HelperResult _BrowserAction specResultPane)
    <>
    specContain "expand-w-tail" (
      mkSpecResizableH (_BrowserLayout <<< _itemInfoHeight) _UIAction
        (T.focusState (_HelperResult <<< _focus) specJsonInfo)
      <>
      T.focusState _HelperResult specRawJson
    )
  )
  <>
  T.match _BrowserAction specSearchBar

specSearchBar :: forall eff props . T.Spec _ CMHFState props BrowserAction
specSearchBar = T.simpleSpec performAction render
  where
    render :: T.Render CMHFState props BrowserAction
    render dispatch _ s _ =
      [ R.div
        [ P.className "search-bar" ]
        [ R.input
          [ P._type "search"
          , P.className "search"
          , P.pattern "^(lookup|find).*"
          , P.placeholder " \"lookup ...\" or \"find ...\""
          , P.onKeyUp \e -> handleKey (unsafeCoerce e).keyCode
          , P.onChange \e -> dispatch $ ChangeQuery (unsafeCoerce e).target.value
          ]
          []
        ] 
      ]
      where
        handleKey :: Int -> _
        handleKey 13 = dispatch SendQuery
        handleKey _ = pure unit
    performAction :: T.PerformAction _ CMHFState props BrowserAction
    performAction (ChangeQuery qs) _ _ = do
      ss <- lift <<< liftEff <<< sep $ qs
      void $ T.cotransform (\state -> set _queryString ss state)
      where
        sep :: String -> Eff _ (Array String)
        sep s = do
          r <- esToEff $ regex "\\s+" RegexFlags.global
          pure $ Regex.split r s 
    performAction SendQuery _ s = do
      js <- lift $ send s.process s.queryString
      svit $ jsonToListInfoItem js
      where
        send :: Maybe ChildProcess -> Array String -> Aff _ Json
        send (Just process) ss = listQuery process ss
        send _ _ = pure jsonNull
        svit (Right ls) = void $ T.cotransform (\state -> set (_HelperResult <<< _results) ls state)
        svit (Left e) = do
          lift <<< liftEff <<< log <<< show $ e
          pure unit
    performAction _ _ _ = pure unit

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
    
specRawJson :: forall eff props action . T.Spec eff HelperResult props action
specRawJson = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render HelperResult props action
    render _ _ s _ =
      [ R.div
        [ P.className "raw-json" ]
--        [ R.iframe
        [ R.pre'
          [ R.code'
            [ R.text $ vif s.raw ]
          ]
--          ]
--          [ P.srcDoc ("<pre><code>" <> vif s.raw <> "</code></pre>")
--          , P.className "raw-json" ]
--          []
        ]
      ]
    vif Nothing = "(none)"
    vif (Just rawJson) = rawJson

specJsonInfo :: forall eff props action . T.Spec eff (Maybe Json) props action
specJsonInfo = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render (Maybe Json) props action
    render _ _ s _ =
      [ R.div'
        [
          R.text "<ここに item 情報が入る予定>"
        ]
      ]


-- event.clientX - this.width/2
-- event.clientY - this.height/2

-- div > div.content + div.paddle < div ...
-- で width とかを変更するのは外側の div.style のパラメータ指定でやる
-- _px :: Prism' Int String


-- UI Component --
{-
mkSpecResizable :: forall eff state props action
                   . String -- | container class
                   -> String -- | paddle class
                   -> (Int -> {| style})
                   -> (P.Event -> Int) 
                   -> Lens' state Int
                   -> Prism' action (UIAction state)
                   -> T.Spec eff state props action
                   -> T.Spec eff state props action
mkSpecResizable containerClass paddleClass f _amount _UIA =
-}
  
--    型安全でない -20pt
-- ↓ コピペ -20pt
mkSpecResizableW :: forall eff state props action
                    . Lens' state Int
                    -> Prism' action (UIAction state)
                    -> T.Spec eff state props action
                    -> T.Spec eff state props action
mkSpecResizableW _width _pUIA = catchPaddleAction _pUIA <<< touchRender
  where
    touchRender :: T.Spec eff state props action
                   -> T.Spec eff state props action
    touchRender = over T._render \render dispatch p layout c ->
      [ R.div
        [ P.style {width: (intToString (layout ^. _width) <> "px")}
        , P.className "expand expand-w"
        ]
        (
          [ R.span
            [ P.className "paddle paddle-x"
            , P.draggable true
            , P.onDragEnd \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos _width) (unsafeCoerce e).clientX
            , P.onDrag \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos _width) (unsafeCoerce e).clientX
            ]
            []
          ]
          <> render dispatch p layout c
        )
      ]
mkSpecResizableH :: forall eff state props action
                    . Lens' state Int
                    -> Prism' action (UIAction state)
                    -> T.Spec eff state props action
                    -> T.Spec eff state props action
mkSpecResizableH _height _pUIA = catchPaddleAction _pUIA <<< touchRender
  where
    touchRender :: T.Spec eff state props action
                   -> T.Spec eff state props action
    touchRender = over T._render \render dispatch p layout c ->
      [ R.div
        [ P.style {height: (intToString (layout ^. _height) <> "px")}
        , P.className "expand expand-h"
        ]
        ( render dispatch p layout c <>
          [ R.div
            [ P.className "paddle paddle-y"
            , P.draggable true
            , P.onDragEnd \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos _height) (unsafeCoerce e).clientY
            , P.onDrag \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos _height) (unsafeCoerce e).clientY
            ]
            []
          ]
        )
      ]
catchPaddleAction :: forall eff state props action
                     . APrism' action (UIAction state)
                     -> T.Spec eff state props action
                     -> T.Spec eff state props action
catchPaddleAction _pUIA = over T._performAction \pa a p s ->
  case matching _pUIA a of
    Right (PartialPaddlePos _amount x) -> void (T.cotransform (\state -> set _amount x state))
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



