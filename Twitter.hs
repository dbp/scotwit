{-# LANGUAGE OverloadedStrings #-}
module Twitter where

import Web.Twitter.Conduit
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
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)

import qualified Secret as Secret

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = Secret.cons
    , oauthConsumerSecret = Secret.cons_sec
    }

withCredential :: TW (ResourceT (LoggingT IO)) b -> LoggingT IO b
withCredential task = do
    let cred = OA.newCredential Secret.tok Secret.tok_sec
    let env = setCredential tokens cred def
    runTW env task

tweets :: IO [T.Text]
tweets = runStderrLoggingT . withCredential $ do
    statii <- homeTimeline [] C.$$ CL.take 100
    return (map (\status ->
      let sn = userScreenName . statusUser $ status
          tweet = statusText status in
      T.concat [ sn, ": ", tweet, "\n"]) statii)
