{-# LANGUAGE OverloadedStrings #-}


-- | Module which processes the research publications.
--
-- The overall rules for generating the research pages is similar to
-- that of the blog: I am storing each publication as a "blog" entry
-- in src/research/<category>/YYYY-MM-DD-Title.md. Its meta data
-- should look like like.
--
-- > ---
-- > title:  title of publication          (required)
-- > authors: authors                      (required)
-- > key: publication key to be put it     (required if sources are present)
-- > sources: the ps pdf dvi and bib files (optional)
-- > path: base directory                  (required if sources are present)
-- > arxiv: arxiv abstract key e.g         (optional)
-- > ---
--
-- The processing of these entries is mostly straight forward: The
-- entries are then categorised and listed on the /researc/index.html
-- page with a section for each category of publication.
--
-- There are however some kludges: I want download links for all the
-- research papers. This means the following processing:
--
-- 1. Each entry should be appended with download links. There should
-- be links to local versions in ps, pdf and dvi together with a link
-- to its arxiv url (if available). The formats in which the local
-- file are available have to be infered from the `sources` field.
--
-- 2. Copy the local versions in what ever formats are available to
-- the destination /research/publication/<key>.ext. The path field
-- gives where the root of the publication is located and the sources
-- give the actual files. The key will determine the final name of the
-- local file.

module Site.Publication
       ( rules
       ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.String
import Hakyll
import System.Directory
import System.FilePath

import Site.Compilers
import Site.RemoteFile


-- | Pattern for all publication.
pubPat :: Pattern
pubPat = "research/publication/**.md"

-- | Pattern matching all categories
catPat :: Pattern
catPat = "research/publication/*.cat"

-- | Publication item template
pubItemT :: Identifier
pubItemT = "templates/publication/item.md"

------------------------ The rules -----------------------------------------

rules :: Rules ()
rules = do
  --
  -- Compile each publication
  --
  match pubPat
    $ compilePipeline
    $ loadAndApplyTemplate pubItemT pubContext

  --
  -- Categorise publications
  --
  pubCats <- buildCategories pubPat $ fromCapture catPat
  tagsRules pubCats $ const (compile . catPage)

  --
  -- The index page
  --
  match "research/index.md" $ do
    route $ setExtension "html"
    compilePipeline researchPage

  --
  -- Local download sources
  --

  do downloadItems <- allDownloads
     forM_ downloadItems $ \ (ident, fp) -> do
       create [ident] $ do
         route idRoute
         compile $ copyRemoteFileCompiler fp

------------------- Contexts and Compilers -----------------------------

-- | The field that consists of all categories

researchPage :: Pipeline String String
researchPage = prePandoc
             >=> applyAsTemplate researchContext
             >=> pandoc
             >=> postPandoc siteContext

catPage :: Pattern -> Compiler (Item String)
catPage = loadAll
        >=> recentFirst
        >=> makeItem . unlines . map itemBody


-- | Context for a category of publication.
catContext :: Context String
catContext = titleField "category" <> bodyField "publications"

-- | Context for the index page
researchContext :: Context String
researchContext = listField "categories" catContext $ loadAll catPat


-- | Context for rendering a publication.
pubContext :: Context String
pubContext = defaultContext
             <> field "downloads" (downloads . itemIdentifier)
             <> dateField "year" "%Y"

sources :: ( MonadMetadata m
           , Functor m
           )
        => Identifier -> m [String]
sources ident = maybe [] words <$> getMetadataField ident "sources"

downloads :: Identifier -> Compiler String
downloads ident = do
  srcs <- sources ident
  case srcs of
    [] -> return []
    _  -> do key <- getMetadataField' ident "key"
             return $ concatMap (makeDownload key) srcs


localDownloads :: FilePath -- ^ Home directory
               -> Identifier
               -> Rules [(Identifier, FilePath)]
localDownloads homeDir ident = do
  srcs <- sources ident
  case srcs of
    [] -> return []
    _  -> do key  <- getMetadataField' ident "key"
             path <- getMetadataField' ident "path"
             let identifierOf src = fromString $ "research/publication/"
                                    </> key <.> takeExtension src
                 pathOf      src = attachHome homeDir (splitPath path) </> src
               in return $ [ (identifierOf src, pathOf src) | src <- srcs]

attachHome :: FilePath -> [FilePath] -> FilePath
attachHome homeDir ( "~/" : ps) = joinPath $ homeDir : ps
attachHome _       paths        = joinPath $ paths

allDownloads :: Rules [(Identifier, FilePath)]
allDownloads =  do homeDir <- preprocess $ getHomeDirectory
                   getMatches pubPat >>= concatMapM (localDownloads homeDir)
  where concatMapM f = fmap concat . mapM f

------------------------ Helper functions ------------------------------

makeDownload :: String -> String -> String
makeDownload key src =  li $ markdownLink extName url title
  where ext = takeExtension src
        extName = tail ext
        url     = "/research/publication" </> key <.> ext
        title   = "Download as " ++ extName

li :: String -> String
li = between "<li>" "</li>"

markdownLink :: String -> String -> String -> String
markdownLink txt url title = bracket txt
                             ++ paren (unwords [url, show title])

between :: String -> String -> String -> String
between begin end str = begin ++ str ++ end

bracket :: String -> String
bracket = between "[" "]"

paren :: String -> String
paren = between "(" ")"
