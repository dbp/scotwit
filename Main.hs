{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Control.Monad.Trans (liftIO)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Web.Twitter.Conduit as Twitter

import Twitter

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    ts <- liftIO $ run $ take100 Twitter.homeTimeline
    let statii = map (\stat ->
                        let sn = Twitter.userScreenName . Twitter.statusUser $ stat
                            tweet = Twitter.statusText stat in
                        T.concat [ sn, ": ", tweet, "<br/>"]) ts
    html $ mconcat (map LT.fromStrict statii)
