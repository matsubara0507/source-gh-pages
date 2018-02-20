{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Config
import           Data.List       (sortBy)
import           Data.Ord        (comparing)
import           Data.Time
import           Hakyll
import           Skylighting     (pygments, styleToCss)
import           System.FilePath (takeBaseName, takeDirectory, takeFileName)

main :: IO ()
main = do
  config <- readConfig "config.yaml"
  let
    siteCtx = mkSiteCtx config
  hakyllWith cmdConfig $ do
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
          renderAtom (mkFeedConfig config) feedCtx posts

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

cmdConfig :: Configuration
cmdConfig = defaultConfiguration
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
