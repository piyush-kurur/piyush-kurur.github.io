{-#    LANGUAGE OverloadedStrings    #-}
import Hakyll


import Site.Config -- Edit this file to change the configuration.
import Site.Compilers

----------------   Rules   --------------------------------------------

rules :: Rules () -- ^ the actual rules for conversion
rules = do

  -- Compass generated style sheets.
  match "stylesheets/*.css" $ do
    route idRoute
    compile compressCssCompiler

  -- Compile the templates.
  match "templates/*" $ compile templateCompiler

  -- Match urls database
  match "misc/urls.db" $ compile getResourceBody

  -- Compile navigation bar
  match "misc/navigation.md" $ compilePipeline pandoc

  match "**index.md" $ do
    route $ setExtension "html"
    compilePipeline stdPage

----------------   Main and sundry   ------------------------------------


main :: IO ()
main = hakyllWith config rules

config :: Configuration -- ^ The configuration. Don't edit this
                        -- instead edit the stuff on the top section

config = defaultConfiguration { deployCommand     = rsyncCommand
                              , providerDirectory = contentsDir
                              }
  where rsyncCommand = "rsync -avvz _site/ " ++ rsyncDest