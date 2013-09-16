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

-- | Generating a tags page
makeTagRules :: Identifier -> String -> Pattern -> Rules ()
makeTagRules template tag pat = do
  route idRoute
  compile $ makeItem ""
    >>= prePandoc
    >>= pandoc
    >>= loadAndApplyTemplate template tagContext
    >>= postPandoc
    where tagContext = postContext
                       <> constField "tag" tag
                       <> listField "posts" postContext (loadAll pat)

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

  -- Classify posts based on tags.
  postTags <- buildTags "posts/*"
              $ fromCapture "posts/tags/*.html"

  -- Generate the tags page
  tagsRules postTags $ makeTagRules tagT
