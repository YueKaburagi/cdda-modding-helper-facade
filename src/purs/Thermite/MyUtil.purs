
module Thermite.MyUtil (defaultMain, defaultMain',
                        unitSpec, list,
                        stateUpdate, stateUpdate_) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Coroutine (CoTransformer)

import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Data.Foldable (traverse_, fold)
import Data.Lens (Setter', set, (^.))
import Data.List (List)

import DOM.Node.Types (Element)
import DOM.HTML.Types (htmlElementToElement) as HTML
import DOM.HTML (window) as HTML
import DOM.HTML.Window (document) as HTML
import DOM.HTML.Document (body) as HTML
import DOM (DOM)

import Thermite (Spec, Render,
                 createClass, defaultPerformAction, defaultRender, cotransform, simpleSpec,
                 _render, _performAction) as T
import React (createFactory) as R
import ReactDOM (render) as RD


-- | 任意位置に注入できるようにした Thermite.defaultMain
defaultMain :: forall eff state props action
             . T.Spec eff state props action
             -> state
             -> props
             -> Maybe Element
             -> Eff ("dom" :: DOM | eff) Unit
defaultMain spec s p elem = void do
  traverse_ (RD.render (R.createFactory component p)) elem
  where
    component = T.createClass spec s


-- | compatible Thermite.defaultMain through MyUtil.defaultMain
defaultMain':: forall eff state props action
             . T.Spec eff state props action
             -> state
             -> props
             -> Eff ("dom" :: DOM | eff) Unit
defaultMain' spec s p = do
  document <- HTML.document =<< HTML.window
  body <- (map HTML.htmlElementToElement) <$> toMaybe <$> HTML.body document
  defaultMain spec s p body


-- | unit spec
unitSpec :: forall eff state props action . T.Spec eff state props action
unitSpec = T.simpleSpec T.defaultPerformAction T.defaultRender

-- | state update helper for Lens 
stateUpdate :: forall eff state a
               . Setter' state a
               -> a
               -> CoTransformer (Maybe state) (state -> state) (Aff eff) (Maybe state)
stateUpdate _s s = T.cotransform (\state -> set _s s state)

stateUpdate_ :: forall eff state a
                . Setter' state a
                -> a
                -> CoTransformer (Maybe state) (state -> state) (Aff eff) Unit
stateUpdate_ _s = void <<< stateUpdate _s

list :: forall state props action
       . T.Render state props action
       -> T.Render (List state) props action
list r = render
  where
    render d p ss c = fold ((\s -> r d p s c ) <$> ss)
