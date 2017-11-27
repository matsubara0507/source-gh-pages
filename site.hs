--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString.UTF8   (toString)
import           Data.Monoid            (mappend)
import           Data.Yaml.YamlLight
import           Hakyll
import           Text.Highlighting.Kate (pygments, styleToCss)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  configYaml <- parseYamlFile "config.yaml"
  let siteCtx = mkSiteCtx configYaml
  hakyllWith config $ do
    match ("templates/*" .||. "includes/*") $ compile templateBodyCompiler

    create ["css/highlight.css"] $ do
        route   idRoute
        compile $ makeItem (compressCss $ styleToCss pygments)

    match ("assets/*" .||. "assets/*/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" (postCtx `mappend` siteCtx)
            >>= relativizeUrls

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" (siteCtx `mappend` defaultContext)
            >>= loadAndApplyTemplate "templates/default.html" (siteCtx `mappend` defaultContext)
            >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
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
        posts <- take 4 <$> (recentFirst =<< loadAll "posts/*")
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
           posts <- recentFirst =<< loadAll "posts/*"
           let sitemapCtx =
                 listField "entries" (postCtx `mappend` siteCtx) (return posts)

           makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "time" "%Y-%m-%d" `mappend`
  dateField "date" "%b %-d, %Y" `mappend`
  defaultContext

mkSiteCtx :: YamlLight -> Context String
mkSiteCtx = mconcat . fmap mkSiteCtx' . getTerminalsKeys
  where
    mkSiteCtx' (val, [YStr key]) = constField (toString key) (toString val)
    mkSiteCtx' _                 = mempty

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
