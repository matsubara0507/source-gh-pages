{-# LANGUAGE OverloadedStrings #-}

import           Config
import           Data.Monoid     ((<>))
import           Hakyll          hiding (dateFieldWith)
import           Hakyll.Ext
import           Skylighting     (pygments, styleToCss)
import           System.FilePath

main :: IO ()
main = do
  config <- readConfig "config.yaml"
  let siteCtx = mkSiteCtx config
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

    tags <- buildTags "posts/*/*" (fromCapture "tags/*.html")
    let postCtx' = postCtx tags

    tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst' =<< loadAll pattern
        let tagCtx = mconcat
              [ constField "title" ("Posts tagged “" ++ tag ++ "”")
              , listField "posts" postCtx' (return posts)
              , defaultContext
              ]
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html"     tagCtx
          >>= loadAndApplyTemplate "templates/default.html" (tagCtx <> siteCtx)
          >>= relativizeUrls

    match "posts/*/*" $ do
      route $ composeRoutes (gsubRoute "/[0-9]{4}/" $ (++ "-") . init)
                            (setExtension "html")
      compile
        $   pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx'
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (postCtx' <> siteCtx)
        >>= relativizeUrls

    match "about.md" $ do
      route $ setExtension "html"
      let siteCtx' = siteCtx <> defaultContext
      compile
        $   pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html"    siteCtx'
        >>= loadAndApplyTemplate "templates/default.html" siteCtx'
        >>= relativizeUrls

    archive <- buildPaginateWith
      (fmap (paginateEvery 10) . sortRecentFirst')
      "posts/*/*"
      (fromFilePath . ("archive/" ++) . (<.> "html") . show)

    paginateRules archive $ \pageNum pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst' =<< loadAll pattern
        let archiveCtx = mconcat
              [ constField "title" "Archives"
              , listField "posts" postCtx' (return posts)
              , paginateContext archive pageNum
              , siteCtx
              , defaultContext
              ]
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts    <- takeRecentFirst' 4 =<< loadAll "posts/*/*"
        tagCloud <- renderTagCloud 80.0 120.0 tags
        let indexCtx = mconcat
              [ listField "posts" postCtx' (return posts)
              , boolField  "isIndex"  (const True)
              , constField "tagcloud" tagCloud
              , siteCtx
              , defaultContext
              ]
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst' =<< loadAll "posts/*/*"
        let sitemapCtx =
              listField "entries" (postCtx' <> siteCtx) (return posts)
        makeItem [] >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx' <> bodyField "description"
        posts <- takeRecentFirst' 10 =<< loadAllSnapshots "posts/**" "content"
        renderAtom (mkFeedConfig config) feedCtx posts

postCtx :: Tags -> Context String
postCtx tags = mconcat
  [ dateField' "time"      "%Y-%m-%d"
  , dateField' "date"      "%b %-d, %Y"
  , dateField' "published" "%Y-%m-%dT%H:%M:%SZ"
  , dateField' "updated"   "%Y-%m-%dT%H:%M:%SZ"
  , tagsField' "tags"      tags
  , defaultContext
  ]

toDate :: Identifier -> String
toDate ident = yyyy ++ "-" ++ mmdd
 where
  path = toFilePath ident
  yyyy = takeFileName $ takeDirectory path
  mmdd = take 5 $ takeBaseName path

dateField' :: String -> String -> Context a
dateField' = dateFieldWith toDate

recentFirst' :: MonadMetadata m => [Item a] -> m [Item a]
recentFirst' = recentFirstWith toDate

takeRecentFirst' :: MonadMetadata m => Int -> [Item a] -> m [Item a]
takeRecentFirst' n = fmap (take n) . recentFirst'

sortRecentFirst' :: MonadMetadata m => [Identifier] -> m [Identifier]
sortRecentFirst' = sortRecentFirstWith toDate

tagsField' :: String -> Tags -> Context a
tagsField' = tagsFieldWithSep " "

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
