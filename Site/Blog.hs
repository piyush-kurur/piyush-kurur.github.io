{-# LANGUAGE OverloadedStrings #-}

module Site.Blog
       ( rules
       ) where

import Control.Monad
import Data.Monoid
import Hakyll

import Site.Config
import Site.Compilers

-- | Blog post page
blogPostPage :: Pipeline String String
blogPostPage = prePandoc >=> pandoc
               >=> postRender
               >=> saveSnapshot "feed"
               >=> postPandoc
  where postRender  = loadAndApplyTemplate postT postContext


-- | Generating feeds.
compileFeeds :: Compiler [Item String]
compileFeeds =   loadAllSnapshots postsPat "feed"
             >>= fmap (take postsOnFeed) . recentFirst
             >>= mapM relativizeUrls


rules :: Rules ()
rules = do
  match postsPat $ do
    route $ setExtension "html"
    compilePipeline blogPostPage

  -- Create atom/rss feeds feeds.
  let feedContext = postContext <> bodyField "description" in do
    create ["posts/feeds/atom.xml"] $ do
      route idRoute
      compile $ compileFeeds >>= renderAtom feedConfig feedContext

    create ["posts/feeds/rss.xml"] $ do
      route idRoute
      compile $ compileFeeds >>= renderRss feedConfig feedContext
