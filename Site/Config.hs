{-# LANGUAGE OverloadedStrings #-}

-- | Edit this file to make changes. The advantage of spliting it out
-- is that now each module can import this stuff.

module Site.Config where

import qualified Data.Map as Map
import           Hakyll
import           Text.Pandoc

import Site.Pandoc.Links

contentsDir  :: String -- ^ Where the actual webpage source reside
contentsDir  = "contents"


rsyncDest :: String -- ^ Where to sync stuff to
rsyncDest = "ppk@turing:/homepages/global/ppk/"

--
-- Blog configuration
--

postsPat :: Pattern
postsPat = "posts/*"

postsOnMainPage :: Int -- ^ How many posts to show on main page.
postsOnMainPage = 3

postsOnFeed :: Int -- ^ How many posts on atom feeds.
postsOnFeed = 42

-- | Feed configuration.
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Delusions of a chaotic mind"
  , feedDescription = "My views on how the Universe ought to be"
  , feedAuthorName  = "Piyush P Kurur"
  , feedAuthorEmail = "ppk@cse.iitk.ac.in.REMOVETHISIFYOUAREAHUMAN"
  , feedRoot        = "http://cse.iitk.ac.in/users/ppk"
  }


dateFormat :: String
dateFormat = "%B %e, %Y (%A)"

tagCloudMin :: Double -- ^ minimum size in % of a tag cloud item
tagCloudMin = 80
tagCloudMax :: Double -- ^ maximum size in % of a tag cloud item
tagCloudMax = 120

--------------- Pandoc configuration ---------------------------------

-- | Pandoc reader options.
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions

-- | Pandoc writer options.
writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
   { writerHTMLMathMethod = MathML Nothing -- ^ I want math rendering
   }


-- | Namespace for link expansion
linkNamespaces :: Map.Map String Expand
linkNamespaces = Map.fromList [("wikipedia", wikipedia)]

-- | Plugins to use for processing Pandoc
plugins :: Pandoc -> Compiler Pandoc
plugins = linkExpand linkNamespaces

----------------   Templates ------------------------------------------

wrapperT :: Identifier
wrapperT = "templates/wrapper.html"

layoutT  :: Identifier
layoutT  = "templates/layout.html"

--
-- Templates related to blogs
--

postT :: Identifier     -- ^ A blog post template.
postT = "templates/post.html"

tagT :: Identifier      -- ^ A template for a tags page.
tagT = "templates/posts/tag.html"

archiveT :: Identifier  -- ^ A template for a tags page.
archiveT  = "templates/posts/archive.html"

archiveIndexT :: Identifier -- ^ Template for the archive's index page
archiveIndexT = "templates/posts/archiveIndex.html"
