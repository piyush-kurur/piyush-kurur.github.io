{-# LANGUAGE OverloadedStrings #-}

module Site.Blog
       ( rules
       ) where

import Control.Monad
import Data.Monoid
import Hakyll
import System.FilePath

import Site.Config
import Site.Compilers

-- | Blog post page
blogPostPage :: Tags -> Pipeline String String
blogPostPage tags =   prePandoc >=> pandoc
                  >=> postRender
                  >=> saveSnapshot "feed"
                  >=> postPandoc
  where postRender  = loadAndApplyTemplate postT cxt
        cxt         = postContext <> tagsField "postTags" tags

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

-- | This function generates the year of the post from its
-- identifier. This is used in building the archives.
getYear :: Identifier -> String
getYear = takeWhile (/= '-') . takeFileName . toFilePath

rules :: Rules ()
rules = do
  --
  -- Classify posts based on tags.
  --
  postTags <- buildTags "posts/*"
              $ fromCapture "posts/tags/*.html"

  -- Generate the tags page
  tagsRules postTags $ makeTagRules tagT

  --
  -- Compiling individual posts.
  --
  match postsPat $ do
    route $ setExtension "html"
    compilePipeline $ blogPostPage postTags

  --
  -- Create atom/rss feeds feeds.
  --
  let feedContext = postContext <> bodyField "description" in do
    create ["posts/feeds/atom.xml"] $ do
      route idRoute
      compile $ compileFeeds >>= renderAtom feedConfig feedContext

    create ["posts/feeds/rss.xml"] $ do
      route idRoute
      compile $ compileFeeds >>= renderRss feedConfig feedContext

  --
  -- Classifying the posts with respect to the year of posting.
  --
  let yearTag ident = return [getYear ident] in do
    dateTags <- buildTagsWith yearTag "posts/*"
                $ fromCapture "posts/archive/*.html"

    tagsRules dateTags $ makeTagRules archiveT
