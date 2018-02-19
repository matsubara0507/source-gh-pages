{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Monad   (liftM)
import           Data.List       (sortBy)
import           Data.Map        (Map, foldMapWithKey)
import           Data.Ord        (comparing)
import           Data.Time
import           Data.Yaml       (decodeFileEither)
import           Hakyll
import           Skylighting     (pygments, styleToCss)
import           System.FilePath (takeBaseName, takeDirectory, takeFileName)

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
        posts <- take 4 <$> (recentFirst' =<< loadAll "posts/*/*")
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

postCtx :: Context String
postCtx =
  dateField' "time" "%Y-%m-%d" `mappend`
  dateField' "date" "%b %-d, %Y" `mappend`
  defaultContext

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
recentFirst' = liftM reverse . chronological'

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs =
  liftM (map fst . sortBy (comparing snd)) $ mapM (\x -> liftM (x,) (f x)) xs

type Config = Map String String

mkSiteCtx :: Config -> Context String
mkSiteCtx = foldMapWithKey constField

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
