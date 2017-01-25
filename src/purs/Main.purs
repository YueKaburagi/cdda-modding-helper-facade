module Main (main, dropIv) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throwException)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Trans (runStateT)

import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List, head, (!!))
import Data.Array as Array
import Data.Either (Either (..))
import Data.Foldable (fold)
import Data.String (split, joinWith) as String
import Data.String.Regex (regex)
import Data.String.Regex (split) as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Int as Int

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
import Data.StrMap (lookup)
import Thermite as T
import React (ReactElement)
import React as R
import React.DOM as R
import React.DOM.Props as P
import ReactDOM as RD
import Data.Argonaut.Core (Json, jsonNull, foldJsonObject, JObject, foldJson, foldJsonNumber)
import Data.Argonaut.Core (toObject, fromNumber) as Json

import Unsafe.Coerce (unsafeCoerce)


specInfoItem :: forall eff props . T.Spec eff InfoItem props InfoItemAction
specInfoItem = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render InfoItem props InfoItemAction
  render dispatch _ mi _ =
    [ R.li'
      (sym (mi.symcol) <>
       [ R.a
         [ P.onClick \_ -> dispatch $ ItemQuery mi.index ]
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
  -- foreach しか提供されてなくて、しかもあの形なのはなぜなんだろう？


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
        (T.focusState (_HelperResult <<< _focus) specMayItem)
      <>
      T.focusState _HelperResult specRawJson
    )
  )
  <>
  T.match _BrowserAction specSearchBar
  <>
  T.match _BrowserAction (catchBrowserQuery TU.unitSpec)

makeItemInfo :: JObject -> Array ReactElement
makeItemInfo j =
  rkp' (Just j) "file" <>
  rkp' b "name" <>
  spn' b "id" "\"" "\"" <>
  spn  b "volume" "体積:" "" (mapjn (_ * 0.25)) <>
  spn  b "weight" "質量:" "kg" (mapjn (_ * 0.001)) <> -- 1[kg] == 2.20462[lbs]
  spn' b "to_hit" "命中:" "" <>
  spn' b "bashing" "打撃:" "" <>
  spn' b "cutting" "切断:" "" <>
  mkt' b "description" R.p' "" ""
  where
    b = Json.toObject =<< lookup "body" j
    mkTag :: (Array ReactElement -> ReactElement) -- | tag
             -> String -- | prefix
             -> String -- | suffix
             -> Maybe String -- | content
             -> Array ReactElement
    mkTag tag p s (Just x) = [ tag [ R.text (p <> x <> s) ] ]
    mkTag _ _ _ Nothing = []
    mkTag' = mkTag R.span'
    mapjn :: (Number -> Number) -> Json -> Json
    mapjn f j = foldJsonNumber j (Json.fromNumber <<< f) j
    toText :: Json -> String
    toText = foldJson
             (\_ -> "(null)")
             (\b -> show b)
             (\n -> dispNum n)
             (\s -> s)
             (\arr -> show arr)
             (\jo -> show jo)
    dispNum :: Number -> String
    dispNum n =
      case Int.fromNumber n of
        Just i | Int.toNumber i == n -> show i
        _ -> show n
    mkt :: (Maybe JObject) -- | src
           -> String -- | key
           -> (Array ReactElement -> ReactElement) -- | tag
           -> String -- | prefix
           -> String -- | suffix
           -> (Json -> Json) -- | json transform
           -> Array ReactElement
    mkt  x k t p s f = mkTag t p s (toText <$> f <$> (lookup k =<< x))
    mkt' x k t p s = mkt x k t p s id
    spn  x k = mkt x k R.span'
    spn' x k p s = spn x k p s id
    rkp  x k = spn x k "" ""
    rkp' x k = rkp x k id
    

specMayItem :: forall eff props action . T.Spec _ Json props action
specMayItem = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render Json props action
    render dispatch _ json _ =
      [ R.div
        [ P.className "json-info" ]
        (foldJsonObject [] makeItemInfo json)
      ]


specSearchBar :: forall eff props . T.Spec _ CMHFState props BrowserAction
specSearchBar = T.simpleSpec performAction render
  where
    render :: T.Render CMHFState props BrowserAction
    render dispatch _ s _ =
      [ R.div
        [ P.className "search-area" ]
        [ R.button
          [ P.onClick \_ -> dispatch $ ListQuery s.queryString ]
          [ R.text "=>" ]
        , R.div
          [ P.className "search-bar" ]
          [ R.input
            [ P._type "search"
            , P.className "search"
            , P.placeholder ""
            , P.onKeyUp \e -> handleKey (unsafeCoerce e).keyCode
            , P.onChange \e -> dispatch $ ChangeQuery (unsafeCoerce e).target.value
            ]
            []
          ]
        ]
      ]
      where
        handleKey :: Int -> _
        handleKey 13 = dispatch $ ListQuery s.queryString -- SendQuery
        -- BS  8
        -- Ret 13
        -- C-d 68
        -- C-h 72
        -- C-m 77
        -- chrome系のエンジンだと日本語入力時のEnterでkeyupが発火しない問題
        --- electronもこれに準拠する模様
        handleKey _ = pure unit
    performAction :: T.PerformAction _ CMHFState props BrowserAction
    performAction (ChangeQuery qs) _ _ = do
      ss <- lift <<< liftEff <<< sep $ qs
      void $ T.cotransform (\state -> set _queryString ss state)
      where
        sep :: String -> Eff _ (Array String)
        sep s = do
          r <- esToEff $ regex "\\s+" RegexFlags.global
          pure $ dropEmptyHeads $ Regex.split r s
        dropEmptyHeads :: Array String -> Array String
        dropEmptyHeads arr =
          case Array.uncons arr of -- ここもにょい
            Just {head: "", tail: xs} ->  dropEmptyHeads xs
            _ -> arr
    performAction _ _ _ = pure unit

main :: Eff _ Unit
main = do
  app <- toMaybe <$> HU.getElementById' (ElementId "item_browser")
  f <- lookupCMH'
  chap <- approach f
  TU.defaultMain specBrowser (testCMHFState chap) unit app




catchBrowserQuery :: forall props
                     . T.Spec _ CMHFState props BrowserAction
                     -> T.Spec _ CMHFState props BrowserAction
catchBrowserQuery = over T._performAction \pa a p s ->
  case a of
    ListQuery qs -> do
      lift <<< liftEff <<< log <<< show $ qs
      js <- lift $ runStateT (listQueryP $ normalizeQS qs) s.outer
      TU.stateUpdate_ _OuterState $ snd js
      svit $ jsonToListInfoItem $ fst js
    ItemAction _ (ItemQuery ix) -> do 
      raw <- lift $ runStateT (rawQueryP ix) s.outer
      setp $ fst raw
      info <- lift $ runStateT (infoQueryP ix) $ snd raw
      setr $ fst info
      TU.stateUpdate_ _OuterState $ snd info
    _ -> pa a p s
  where
    svit (Right ls) = void $ T.cotransform (\state -> set (_HelperResult <<< _results) ls state)
    svit (Left e) = lift <<< liftEff <<< log <<< show $ e
    normalizeQS :: Array String -> Array String
    normalizeQS arr =
      case Array.uncons arr of -- もにょい
        Just {head: x, tail: _} | x == "lookup" || x == "find" -> arr
        _ -> Array.cons "lookup" $ map ("name=?" <> _) arr
    _raw' = _HelperResult <<< _raw
    _focus' = _HelperResult <<< _focus
    setp r@(Just _) = void $ T.cotransform (\state -> set _raw' r state)
    setp Nothing = pure unit
    setr j = void $ T.cotransform (\state -> set _focus' j state)
      
    
specRawJson :: forall eff props action . T.Spec eff HelperResult props action
specRawJson = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render HelperResult props action
    render _ _ s _ =
      [ R.div
        [ P.className "raw-json" ]
        [ R.pre'
          [ R.code'
            [ R.text $ vif s.raw ]
          ]
        ]
      ]
    vif Nothing = "(none)"
    vif (Just rawJson) = rawJson



-- event.clientX - this.width/2
-- event.clientY - this.height/2



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
  
-- 型安全でない
-- コピペしてる
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



