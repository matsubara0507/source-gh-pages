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
    , "&& rsync -a --filter='P .git/' --filter='P .gitignore'"
    , " --delete-excluded ../_site/ ."
    , "&& git checkout master"
    , "&& git add -A"
    , "&& git commit -m 'Publish'"
    , "&& cd ../"
      -- , "&& git submodule update"
    ]
  }
