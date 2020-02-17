{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Rules where

import           Config
import           Control.Monad.Reader
import           Control.Monad.Tangle
import           Data.Extensible                hiding (TangleT, evalTangleT,
                                                 match)
import           Data.Extensible.Effect
import           Data.Extensible.Effect.Default
import           Data.Maybe                     (fromMaybe)
import           Hakyll                         hiding (dateFieldWith)
import           Hakyll.Ext
import           Skylighting                    (pygments, styleToCss)
import           System.FilePath
import           Tangle
import           Text.HTML.Scalpel.Core         ((@:))
import qualified Text.HTML.Scalpel.Core         as S

type SiteRules = Record Fields

type Fields =
    '[ "templates" >: ()
     , "highlight" >: ()
     , "assets"    >: ()
     , "css"       >: ()
     , "tags"      >: ()
     , "posts"     >: ()
     , "about"     >: ()
     , "archives"  >: ()
     , "index"     >: ()
     , "sitemap"   >: ()
     , "feed"      >: ()
     ]

type MidFields =
    '[ "siteCtx" >: Context String
     , "tags'"   >: Tags
     , "postCtx" >: Context String
     ] ++ Fields

type RulesM = Eff
    '[ ReaderDef Config
     , "Rules" >: Rules
     ]

run :: Config -> RulesM a -> Rules a
run conf = retractEff . flip runReaderDef conf

type FieldI = Field Identity

tangles :: MidFields :& Comp (TangleT (Record' MidFields) RulesM) FieldI
tangles =
  htabulateFor (Proxy :: Proxy MakeRule) $ \m -> Comp $ Field . pure <$> rule m

makeRules :: RulesM SiteRules
makeRules =
  shrink <$> evalTangleT
    (htraverseWithIndex (const . hitchAt') initialRecord)
    (Record' tangles)
    (Record' $ initialRecord)
  where
    initialRecord = hrepeat (Comp Nothing)

class MakeRule kv where
  rule :: proxy kv -> TangleT (Record' MidFields) RulesM (TargetOf kv)

liftR :: MonadTrans t => Rules a -> t RulesM a
liftR = lift . liftEff (Proxy :: Proxy "Rules")

instance MakeRule ("siteCtx" >: Context String) where
  rule _ = lift $ mkSiteCtx <$> ask

instance MakeRule ("tags'" >: Tags) where
  rule _ = liftR $ buildTags "posts/*/*" (fromCapture "tags/*.html")

instance MakeRule ("postCtx" >: Context String) where
  rule _ = postCtx <$> lasso' #tags'

instance MakeRule ("templates" >: ()) where
  rule _ =
    liftR $ match ("templates/*" .||. "includes/*") (compile templateBodyCompiler)

instance MakeRule ("highlight" >: ()) where
  rule _ = liftR . create ["css/highlight.css"] $ do
    route idRoute
    compile $ makeItem (compressCss $ styleToCss pygments)

instance MakeRule ("assets" >: ()) where
  rule _ = liftR . match ("assets/*" .||. "assets/*/*") $ do
    route idRoute
    compile copyFileCompiler

instance MakeRule ("css" >: ()) where
  rule _ = liftR . match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

instance MakeRule ("tags" >: ()) where
  rule _ = do
    (_, tags)          <- lasso2 (#templates, #tags')
    (siteCtx, postCtx') <- lasso2 (#siteCtx, #postCtx)
    liftR . tagsRules tags $ \tag pat -> do
      route idRoute
      compile $ do
        posts <- recentFirst' =<< loadAll pat
        let tagCtx = mconcat
              [ constField "title" ("Posts tagged “" ++ tag ++ "”")
              , listField "posts" postCtx' (pure posts)
              , defaultContext
              ]
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" tagCtx
          >>= loadAndApplyTemplate "templates/default.html" (tagCtx <> siteCtx)
          >>= relativizeUrls

instance MakeRule ("posts" >: ()) where
  rule _ = do
    lasso' #templates
    (siteCtx, postCtx') <- lasso2 (#siteCtx, #postCtx)
    liftR . match "posts/*/*" $ do
      route $ composeRoutes
        (gsubRoute "/[0-9]{4}/" $ (++ "-") . init)
        (setExtension "html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx'
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (postCtx' <> siteCtx)
        >>= relativizeUrls

instance MakeRule ("about" >: ()) where
  rule _ = do
    (_, siteCtx) <- lasso2 (#templates, #siteCtx)
    liftR . match "about.md" $ do
      route $ setExtension "html"
      let siteCtx' = siteCtx <> defaultContext
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" siteCtx'
        >>= loadAndApplyTemplate "templates/default.html" siteCtx'
        >>= relativizeUrls

instance MakeRule ("archives" >: ()) where
  rule _ = do
    lasso' #templates
    (siteCtx, postCtx') <- lasso2 (#siteCtx, #postCtx)
    archive  <- liftR $ buildPaginateWith
      (fmap (paginateEvery 10) . sortRecentFirst')
      "posts/*/*"
      (fromFilePath . ("archive/" ++) . (<.> "html") . show)
    liftR . paginateRules archive $ \pageNum pat -> do
      route idRoute
      compile $ do
        posts <- recentFirst' =<< loadAll pat
        let archiveCtx = mconcat
              [ constField "title" "Archives"
              , listField "posts" postCtx' (pure posts)
              , paginateContext archive pageNum
              , siteCtx
              , defaultContext
              ]
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

instance MakeRule ("index" >: ()) where
  rule _ = do
    (_, tags)           <- lasso2 (#templates, #tags')
    (siteCtx, postCtx') <- lasso2 (#siteCtx, #postCtx)
    liftR . match "index.html" $ do
      route idRoute
      compile $ do
        posts    <- takeRecentFirst' 4 =<< loadAll "posts/*/*"
        tagCloud <- renderTagCloud 80.0 120.0 tags
        let indexCtx = mconcat
              [ listField "posts" postCtx' (pure posts)
              , boolField  "isIndex"  (const True)
              , constField "tagcloud" tagCloud
              , siteCtx
              , defaultContext
              ]
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

instance MakeRule ("sitemap" >: ()) where
  rule _ = do
    lasso' #templates
    (siteCtx, postCtx') <- lasso2 (#siteCtx, #postCtx)
    liftR . create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst' =<< loadAll "posts/*/*"
        let sitemapCtx =
              listField "entries" (postCtx' <> siteCtx) (pure posts)
        makeItem [] >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx


instance MakeRule ("feed" >: ()) where
  rule _ = do
    (_, postCtx') <- lasso2 (#posts, #postCtx)
    config <- lift ask
    liftR . create ["feed.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx' <> postContentField "description"
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

postContentField :: String -> Context String
postContentField key = field key (pure . scrapePostContent . itemBody)

scrapePostContent :: String -> String
scrapePostContent str =
  fromMaybe "" $
    S.scrapeStringLike str (S.html $ "div" @: [S.hasClass "post-content"])
