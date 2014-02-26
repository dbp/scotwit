{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}
module Twitter where

import Web.Twitter.Conduit
import Web.Twitter.Conduit.Api
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Network.HTTP.Types as HT

import qualified Secret as Secret

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = Secret.consumer_tok
    , oauthConsumerSecret = Secret.consumer_tok_sec
    }

withCredential :: TW (ResourceT (LoggingT IO)) b -> LoggingT IO b
withCredential task = do
    let cred = OA.newCredential Secret.access_tok Secret.access_tok_sec
    let env = setCredential tokens cred def
    runTW env task

run :: TW (ResourceT (LoggingT IO)) a -> IO a
run a = runStderrLoggingT . withCredential $ a

take100 :: (HT.SimpleQuery -> C.Source (TW (ResourceT (LoggingT IO))) a)
        -> TW (ResourceT (LoggingT IO)) [a]
take100 a = a [] C.$$ CL.take 100
