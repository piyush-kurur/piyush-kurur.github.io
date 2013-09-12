{-# LANGUAGE OverloadedStrings #-}

module Site.Blog
       ( rules
       ) where

import Control.Monad
import Data.Monoid
import Hakyll

import Site.Config
import Site.Compilers

postContext :: Context String
postContext = siteContext <> dateField "date" dateFormat

-- | Blog post page
blogPostPage :: Pipeline String String
blogPostPage = prePandoc >=> pandoc >=> postRender >=> postPandoc
  where postRender  = loadAndApplyTemplate postT postContext


rules :: Rules ()
rules = do
  match postsPat $ do
    route $ setExtension "html"
    compilePipeline blogPostPage
