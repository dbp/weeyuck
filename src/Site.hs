{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Site where

import           Control.Lens
import           Control.Logging
import           Control.Monad      (when)
import           Data.Maybe
import Data.Default (def)
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Database.Redis     as R
import           Control.Monad.Reader
import           Network.Wai
import           Web.Fn
import Control.Monad.State
import  Web.Fn.Extra.Heist
import qualified Heist as H
import qualified Heist.Compiled as HC
import           Web.Offset

import           Context

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> (\ctxt -> okText "Hello")
             , path "posts" ==> postsHandler
             , path "heist" ==> heistServe
             , path "static" ==> staticServe "static" ]
  `fallthrough` notFoundText "Not found."

wpConf = WordpressConfig "http://127.0.0.1:5555/wp-json" (Left ("offset", "111")) (CacheSeconds 600) [] Nothing

postsHandler :: Ctxt -> IO (Maybe Response)
postsHandler ctxt = do
  render ctxt "offset"

initializer :: IO Ctxt
initializer = do
  rconn <- R.connect R.defaultConnectInfo
  --let rqURI = return ((T.decodeUtf8 . rawPathInfo . fst) <$> (view requestLens)) :: Lens T.Text
  let wpconf = def { wpConfEndpoint = "http://127.0.0.1:5555/wp-json"
                   , wpConfLogger = Just (putStrLn . T.unpack)
                   , wpConfRequester = Left ("offset", "111")
                   , wpConfCacheBehavior = CacheSeconds 60 }
  (wp, wpSplices) <- initWordpress wpconf rconn rqURI wordpress
  hs' <- heistInit ["templates"] mempty wpSplices
  let hs = case hs' of
        Left errs ->
          errorL' ("Heist failed to load templates: \n" <> T.intercalate "\n" (map T.pack errs))
        Right  hs'' -> hs''
   {--
  envExists <- doesFileExist ".env"
  when envExists $ loadFile False ".env"
  let lookupEnv' key def = fmap (fromMaybe def) (lookupEnv key)--}
  return (Ctxt defaultFnRequest rconn wp hs)

type WPLens b s m = (MonadIO m, MonadState s m) => Lens' Ctxt (Wordpress b)

rqURI :: (MonadIO m, MonadState Ctxt m) => m T.Text
rqURI = do
  (T.decodeUtf8 . rawPathInfo ) <$> (fst <$> use requestLens)

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site
