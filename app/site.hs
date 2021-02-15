{-# LANGUAGE OverloadedStrings #-}

import           Config
import           Hakyll (Configuration (..), defaultConfiguration, hakyllWith)
import           Rules

main :: IO ()
main = hakyllWith cmdConfig . flip run makeRules =<< readConfig "config.yaml"

cmdConfig :: Configuration
cmdConfig = defaultConfiguration
  { deployCommand = mconcat
    [ "cd .site"
    , "&& rm -rfd archive assets css posts tags about.html feed.xml index.html sitemap.xml"
    , "&& cp -r ../_site/* ."
    , "&& git checkout master"
    , "&& git add -A"
    , "&& git commit -m 'Publish'"
    , "&& cd ../"
      -- , "&& git submodule update"
    ]
  }
