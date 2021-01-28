{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Prelude                   hiding (FilePath, null)

import           Data.List                 (nub, sort)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text, isPrefixOf, null, unpack)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (Status, ok200)
import           Shelly
import           Test.Hspec
import           Text.HTML.Scalpel.Core

main :: IO ()
main = do
  urls <- fmap mconcat . shelly $ do
    run_ "stack" ["exec", "--", "site", "build"]
    files <- ls "_site/posts"
    traverse (fmap scrapeLinks . readfile) files
  hspec . mapM_ spec . nub . sort $ filter check urls
 where
  check url = not . or . (:) (null url) $ fmap
    (`isPrefixOf` url)
    [ "https://matsubara0507.github.io"
    , "../", "#", "20"
    , "https://github.com"        -- too many requests 対策
    , "https://www.cis.upenn.edu" -- 時々409になる
    , "https://arxiv.org"         -- なんか403になる
    , "https://www.fun-mooc.fr"   -- 重い
    , "http://www.smlnj.org"      -- 重い
    , "https://www.java-users.jp" -- なんか404になる
    , "https://twitter.com"       -- 400 になる
    ]
  spec url = it (unpack url) $ linkStatus url `shouldReturn` ok200

scrapeLinks :: Text -> [Text]
scrapeLinks txt = fromMaybe [] $ scrapeStringLike txt scraper
  where scraper = attrs "href" "a"

linkStatus :: Text -> IO Status
linkStatus url = do
  manager <- newManager setting
  request <- parseRequest $ unpack url
  responseStatus
    <$> httpNoBody (request { requestHeaders = [("User-Agent", "")] }) manager
  where
    setting = tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 60_000_000
      }
