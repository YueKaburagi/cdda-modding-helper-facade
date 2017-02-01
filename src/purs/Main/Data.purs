
module Main.Data ( hrSetTestData
                 , testCMHFState
                 , module Main.Data.States
                 , module Main.Data.Actions) where

import Prelude (($))
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Nothing))

import Main.Data.Query
import Main.Data.States
import Main.Data.Actions

hrSetTestData :: HelperResult -> HelperResult
hrSetTestData hr = hr { results = his }
  where
    ls = (mockInfoItem "abc") : (mockInfoItem "123") : Nil
    his = (mkInfoItem Nothing "items" (CATQuery [ Mode Find
                                                , Filter $ HasKey "volume"
                                                , Filter $ No $ HasField "type" "speech" ])) :
          (mkInfoItem Nothing "DDA" (CATQuery [ Mode Find
                                              , Filter $ ModIdent "dda" ])) :
          (mkInfoItem Nothing "monsters" (CATQuery [ Mode Find
                                                   , Filter $ HasField "type" "MONSTER" ])) :
          Nil

testCMHFState = initialCMHFState (hrSetTestData initialHelperResult)

