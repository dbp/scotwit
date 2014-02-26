{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Control.Monad.Trans (liftIO)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Web.Twitter.Conduit

import Twitter

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    ts <- liftIO $ run $ take100 homeTimeline
    let statii = map (\stat ->
                        let sn = userScreenName . statusUser $ stat
                            tweet = statusText stat in
                        T.concat [ sn, ": ", tweet, "\n"]) ts
    html $ mconcat (map LT.fromStrict statii)
