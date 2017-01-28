
module Main.Data ( hrSetTestData
                 , testCMHFState
                 , module Main.Data.States
                 , module Main.Data.Actions) where

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Nothing))

import Main.Data.States
import Main.Data.Actions

hrSetTestData :: HelperResult -> HelperResult
hrSetTestData hr = hr { results = his }
  where
    ls = (mockInfoItem "abc") : (mockInfoItem "123") : Nil
    his = (mkInfoItem Nothing "items" (CATQuery ["find", "volume=", "no", "type=speech"])) :
          (mkInfoItem Nothing "DDA" (CATQuery ["find", "mod", "dda"])) :
          (mkInfoItem Nothing "monsters" (CATQuery ["find", "type=MONSTER"])) : Nil

testCMHFState = initialCMHFState initialBrowserLayout (hrSetTestData initialHelperResult)

