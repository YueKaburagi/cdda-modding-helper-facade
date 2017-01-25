
module Outer where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throwException)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State

import Data.Array as Array
import Data.Array (filter)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either (..))
import Data.String.Utils (endsWith)
import Data.String (Pattern(..))
import Data.String (split, joinWith) as String
import Data.Lens (set)


import Node.ChildProcess (ChildProcess, CHILD_PROCESS, ExecResult
                         , ExecOptions, defaultSpawnOptions)
import Node.Stream (writeString)
import Node.Stream (read, onReadable, readString) as Stream
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding (..))
import Node.Buffer (Buffer, BUFFER)
import Node.Buffer as Buffer
import Node.Path (FilePath)
import Node.FS (FS)
import Node.FS.Aff (readdir) as Aff
import Node.FS.Sync (readdir) as Eff

import Util
import Main.Data.States

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)



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

-- -- --

detectPrompt :: String -> { detected:: Boolean, dropped:: Maybe String }
detectPrompt = abc <<< String.split (Pattern "\n")
  where
    abc s = dropPrompt s (Array.unsnoc s)
    dropPrompt s (Just ss) =
      case ss.last of
        "Browser > " ->
          case ss.init of
            [] -> ret true Nothing
            xs -> ret true $ Just $ unArray xs
        _ -> ret false $ Just $ unArray s
    dropPrompt _ Nothing = ret false Nothing
    ret b d = {detected: b, dropped: d}
    unArray = String.joinWith "\n"


readOnceAff :: forall eff
            . ChildProcess
            -> Aff ("cp" :: CHILD_PROCESS, "err" :: EXCEPTION | eff) (Maybe String)
readOnceAff p = do
    s <- liftEff $ Stream.readString src Nothing UTF8 -- あればそれを返す、なければ待つ
    emptyIf s 
  where
    emptyIf s@(Just _) = pure s
    emptyIf Nothing = makeAff \errCb cb -> onceReadable src (docb cb)
    src = ChildProcess.stdout p
    docb cb = do
      ss <- Stream.readString src Nothing UTF8
      cb ss

readStringP :: forall eff
               . Prompt _ (Maybe String)
readStringP = do
  p <- gets _.process
  lift $ readOnceAff p

-- 1回だけ見ればだいたい解決するので、とりまこれで
-- 本当なら helper から [EOS] がでてくるまで on で監視しつづけるほうがいいのかもだけど
readOnceP :: forall eff . Prompt _ (Maybe String)
readOnceP = do
  ms <- readStringP
  dpc $ (detectPrompt <$> ms)
  where
    dpc (Just dp) = do
      modify (\s -> set _ready dp.detected s)
      pure dp.dropped
    dpc Nothing = pure Nothing

readyPrompt :: forall eff . Prompt _ Unit
readyPrompt = do
  b <- gets _.ready
  case b of
    true -> pure unit
    false -> do
      e <- readOnceP
      warnJust e
      readyPrompt
  where
    warnJust (Just s) = liftEff $ log $ ("[warn]" <> s)
    warnJust Nothing = pure unit

textQueryP :: forall eff .  String -> Prompt _ (Maybe String)
textQueryP q = do
  p <- gets _.process
  readyPrompt
  lift $ writeQueryAff p q
  s <- readOnceP
  pure s

jsonQueryP :: forall eff . String -> Prompt _ Json
jsonQueryP q = do
  s <- textQueryP q
  pass $ maybe (Left "empty json") jsonParser s
  where
    pass (Left s) = liftEff <<< throwException <<< error $ s
    pass (Right json) = pure json

-- translate は optional にしたいので、追加で Boolean を引数に入れるかも
rawQueryP :: forall eff . String -> Prompt _ (Maybe String)
rawQueryP ix = textQueryP ("find #" <> ix <> " forFacadeRaw")
listQueryP :: forall eff . Array String -> Prompt _ Json
listQueryP q = jsonQueryP ((String.joinWith " " q) <> " forFacadeList translate")
infoQueryP :: forall eff . String -> Prompt _ Json
infoQueryP ix = jsonQueryP ("find #" <> ix <> " forFacadeInfo translate")

