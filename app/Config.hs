{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Config where

import           Control.Lens    hiding (Context)
import           Data.Extensible
import           Data.Map        (Map, foldMapWithKey)
import           Data.Proxy      (Proxy (..))
import           Data.Yaml       (decodeFileEither)
import           GHC.TypeLits    (KnownSymbol, symbolVal)
import           Hakyll

readConfig :: FilePath -> IO Config
readConfig = fmap (either (error . show) id) . decodeFileEither

type Config = Record
  '[ "site_title" >: String
   , "author" >: String
   , "email" >: String
   , "description" >: String
   , "baseurl" >: String
   , "val" >: Map String String
   ]

class ToContext a where
  toContext :: String -> a -> Context String

instance ToContext String where
  toContext _ "" = mempty
  toContext k v  = constField k v

instance ToContext a => ToContext (Map String a) where
  toContext _ = foldMapWithKey toContext

instance ToContext a => ToContext (Identity a) where
  toContext k = toContext k . runIdentity

mkSiteCtx :: Config -> Context String
mkSiteCtx = hfoldMapFor
  (Proxy :: Proxy (KeyValue KnownSymbol ToContext))
  (toContext <$> symbolVal . proxyAssocKey <*> getField)

mkFeedConfig :: Config -> FeedConfiguration
mkFeedConfig conf = FeedConfiguration
  { feedTitle       = conf ^. #site_title
  , feedDescription = conf ^. #description
  , feedAuthorName  = conf ^. #author
  , feedAuthorEmail = conf ^. #email
  , feedRoot        = conf ^. #baseurl
  }
