{-#    LANGUAGE OverloadedStrings    #-}
import Hakyll


import Site.Config -- Edit this file to change the configuration.
import Site.Compilers

import qualified Site.Blog as Blog  -- Rules to hand blog
import qualified Site.Publication as Publication

----------------   Rules   --------------------------------------------

rules :: Rules () -- ^ the actual rules for conversion
rules = do

  -- Compass generated style sheets.
  match "stylesheets/**.css" $ do
    route idRoute
    compile compressCssCompiler

  match "font-awesome/font/*" $ do
    route idRoute
    compile copyFileCompiler

  match "legal/*.txt" $ do
    route idRoute
    compile copyFileCompiler

  -- Compile the templates.
  match "templates/**" $ compile templateCompiler

  -- Match urls database
  match "misc/urls.db" $ compile getResourceBody

  -- Compile navigation bar
  match "misc/navigation.md" $ compilePipeline pandoc

  match "index.md" $ do
    route $ setExtension "html"
    compilePipeline mainPage

  Blog.rules
  Publication.rules

  match "**/index.md" $ do
    route $ setExtension "html"
    compilePipeline stdPage


----------------   Main and sundry   ------------------------------------


main :: IO ()
main = hakyllWith config rules

config :: Configuration -- ^ The configuration. Don't edit this
                        -- instead edit the stuff on the top section

config = defaultConfiguration { deployCommand     = deploy
                              , providerDirectory = contentsDir
                              }
  where rsync  dest = "rsync -avvz _site/ " ++ dest
        deploy = rsync rsyncDest ++ "; " ++ rsync githubPages
