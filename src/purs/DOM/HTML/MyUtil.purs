
module DOM.HTML.MyUtil (getElementById') where


import Prelude
import Control.Monad.Eff (Eff)

import Data.Nullable (Nullable)

import DOM.Node.Types (Element, ElementId)
import DOM.Node.NonElementParentNode (getElementById) as DOM
import DOM.HTML.Types (htmlDocumentToNonElementParentNode) as HTML
import DOM.HTML (window) as HTML
import DOM.HTML.Window (document) as HTML
import DOM (DOM)


-- 一番期待されるのはカレントwindowのdocumentだよねってことで
getElementById' :: forall eff. ElementId -> Eff ("dom" :: DOM | eff) (Nullable Element)
getElementById' eid = do
  document <- HTML.document =<< HTML.window
  DOM.getElementById eid (HTML.htmlDocumentToNonElementParentNode document)
