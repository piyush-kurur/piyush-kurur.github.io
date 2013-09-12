{-# LANGUAGE OverloadedStrings #-}

module Site.Blog
       ( rules
       ) where

import Control.Monad
import Hakyll

import Site.Config
import Site.Compilers

-- | Blog post page
blogPostPage :: Pipeline String String
blogPostPage = prePandoc >=> pandoc >=> postRender >=> postPandoc
  where postRender  = loadAndApplyTemplate postT postContext


rules :: Rules ()
rules = do
  match postsPat $ do
    route $ setExtension "html"
    compilePipeline blogPostPage
