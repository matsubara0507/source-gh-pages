{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Lens                    hiding (Context)
import           Data.Extensible                 hiding (item, match)
import           Data.Extensible.Instances.Aeson ()
import           Data.List                       (sortBy)
import           Data.Map                        (Map, foldMapWithKey)
import           Data.Ord                        (comparing)
import           Data.Proxy                      (Proxy (..))
import           Data.Time
import           Data.Yaml                       (decodeFileEither)
import           GHC.TypeLits                    (KnownSymbol, symbolVal)
import           Hakyll
import           Skylighting                     (pygments, styleToCss)
import           System.FilePath                 (takeBaseName, takeDirectory,
                                                  takeFileName)

main :: IO ()
main = do
  configYaml <- either (error . show) id <$> decodeFileEither "config.yaml"
  let
    siteCtx = mkSiteCtx configYaml
  hakyllWith config $ do
    match ("templates/*" .||. "includes/*") $ compile templateBodyCompiler

    create ["css/highlight.css"] $ do
        route idRoute
        compile $ makeItem (compressCss $ styleToCss pygments)

    match ("assets/*" .||. "assets/*/*") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    match "posts/*/*" $ do
        route $
          gsubRoute "/[0-9]{4}/" ((++ "-") . init) `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtx `mappend` siteCtx)
            >>= relativizeUrls

    match "about.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" (siteCtx `mappend` defaultContext)
            >>= loadAndApplyTemplate "templates/default.html" (siteCtx `mappend` defaultContext)
            >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst' =<< loadAll "posts/*/*"
          let archiveCtx =
                listField "posts" postCtx (return posts) `mappend`
                constField "title" "Archives" `mappend`
                siteCtx `mappend`
                defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- takeRecentFirst' 4 =<< loadAll "posts/*/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              boolField "isIndex" (const True) `mappend`
              siteCtx `mappend`
              defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst' =<< loadAll "posts/*/*"
          let
            sitemapCtx =
              listField "entries" (postCtx `mappend` siteCtx) (return posts)

          makeItem []
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
          let
            feedCtx = postCtx `mappend` bodyField "description"
          posts <- takeRecentFirst' 10 =<< loadAllSnapshots "posts/**" "content"
          renderAtom (mkFeedConfig configYaml) feedCtx posts

postCtx :: Context String
postCtx = mconcat
  [ dateField' "time" "%Y-%m-%d"
  , dateField' "date" "%b %-d, %Y"
  , dateField' "published" "%Y-%m-%dT%H:%M:%SZ"
  , dateField' "updated" "%Y-%m-%dT%H:%M:%SZ"
  , defaultContext
  ]

dateField' :: String -> String -> Context a
dateField' key format = field key $ \item -> do
  time <- getItemUTC' defaultTimeLocale $ itemIdentifier item
  return $ formatTime defaultTimeLocale format time

getItemUTC' :: MonadMetadata m => TimeLocale -> Identifier -> m UTCTime
getItemUTC' locale ident =
  pure $ parseTimeOrError True locale "%Y%m-%d" (yyyy ++ mmdd)
  where
    path = toFilePath ident
    yyyy = takeFileName $ takeDirectory path
    mmdd = take 5 $ takeBaseName path

chronological' :: MonadMetadata m => [Item a] -> m [Item a]
chronological' =
  sortByM $ getItemUTC' defaultTimeLocale . itemIdentifier

recentFirst' :: MonadMetadata m => [Item a] -> m [Item a]
recentFirst' = fmap reverse . chronological'

takeRecentFirst' :: MonadMetadata m => Int -> [Item a] -> m [Item a]
takeRecentFirst' n = fmap (take n) . recentFirst'

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs =
  map fst . sortBy (comparing snd) <$> mapM (\x -> fmap (x,) (f x)) xs

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
mkSiteCtx =
  hfoldMapFor (Proxy :: Proxy (KeyValue KnownSymbol ToContext)) $
    toContext <$> symbolVal . proxyAssocKey <*> getField

mkFeedConfig :: Config -> FeedConfiguration
mkFeedConfig conf = FeedConfiguration
    { feedTitle       = conf ^. #site_title
    , feedDescription = conf ^. #description
    , feedAuthorName  = conf ^. #author
    , feedAuthorEmail = conf ^. #email
    , feedRoot        = conf ^. #baseurl
    }

config :: Configuration
config = defaultConfiguration
  { deployCommand = mconcat
      [ "cd .site"
      , "&& rsync -a --filter='P .git/' --filter='P .gitignore'"
      , " --delete-excluded ../_site/ ."
      , "&& git checkout master"
      , "&& git add -A"
      , "&& git commit -m 'Publish'"
      , "&& cd ../"
      -- , "&& git submodule update"
      ]
  }
