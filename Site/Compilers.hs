
{-# LANGUAGE OverloadedStrings #-}
module Site.Compilers
       ( prePandoc, pandoc, postPandoc
       , Pipeline, compilePipeline
       , stdPage
       , siteContext
       ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Hakyll

import Site.Config

siteContext :: Context String
siteContext =  defaultContext
            <> field "navigation" navC
  where navC _ = loadBody "misc/navigation.md"

----------------   Compilers    ----------------------------------------


-- Any page is compiled using the following pipeline.
--
-- pre-pandoc -> pandoc -> post-pandoc
--
-- The pandoc stage essentially runs the pandoc engine together with
-- the plugins. The three stages are separated out so that one can do
-- other template rendering in between.
--
-- Since it is often easier to work with the following Kliesli arrow,
-- I will define this type as the compiler pipeline.

type Pipeline a b = Item a -> Compiler (Item b)

-- | This is the default pre-pandoc compiler. All it does is that it
-- appends the contents of urls.db to the end of the contents thus
-- making all refernces defined in urls.db available for free
-- reference.
prePandoc :: Pipeline String String
prePandoc iStr = (str++) <$> mUrls  >>= makeItem
  where str   = itemBody iStr
        mUrls = loadBody "misc/urls.db"

-- | This is the default post-pandoc compiler. It applys the layout
-- and wrapper together with relativizing the urls.
postPandoc :: Pipeline String String
postPandoc = apply layoutT >=> apply wrapperT >=> relativizeUrls
  where apply template = loadAndApplyTemplate template siteContext

-- | The pandoc converter.
pandoc :: Pipeline String String
pandoc = reader >=> transform >=> writer
  where transform = plugins . itemBody >=> makeItem
        reader    = return  . readPandocWith readerOptions
        writer    = return  . writePandocWith writerOptions

-- | The pipeline for a standard page.
stdPage :: Pipeline String String
stdPage = prePandoc >=> pandoc >=> postPandoc

-- | Similar to compile but takes a compiler pipeline instead.
compilePipeline ::  Pipeline String String -> Rules ()
compilePipeline pipeline = compile $ getResourceBody >>= pipeline
