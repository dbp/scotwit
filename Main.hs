{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Control.Monad.Trans (liftIO)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as LT

import Twitter

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    ts <- liftIO tweets
    html $ mconcat (map LT.fromStrict ts)
