{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Context where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State
import qualified Database.Redis      as R
import           Web.Fn
import Heist
import Control.Monad.Reader
import           Web.Fn.Extra.Heist
import           Web.Offset

------------------------------------------------------------------------------

data Ctxt = Ctxt { _req       :: FnRequest
                 , _redis     :: R.Connection
                 , _wordpress :: Wordpress Ctxt
                 , _heist     :: FnHeistState Ctxt
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

instance HeistContext Ctxt where
  getHeist = _heist
