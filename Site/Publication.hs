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
import Hakyll
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
  
  -- Create bib version of publication
  match pubPat $ version "bib" $ do
    route   $ setExtension "bib"
    compile $ copyPublication "bib"

  -- Create pdf version of publications
  match pubPat $ version "pdf" $ do
    route   $ setExtension "pdf"
    compile $ copyPublication "pdf"

  -- Create plain entry.
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

------------------- Contexts and Compilers -----------------------------

-- | The field that consists of all categories

researchPage :: Pipeline String String
researchPage = prePandoc
             >=> applyAsTemplate researchContext
             >=> pandoc
             >=> postPandoc siteContext

catPage :: Pattern -> Compiler (Item String)
catPage = loadCategory
        >=> recentFirst
        >=> makeItem . unlines . map itemBody
  where loadCategory p = loadAll (p .&&. hasNoVersion)

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

-- Create the download list.
downloads :: Identifier -> Compiler String
downloads ident = return $ concat [ li $ markdownLink url "pdf"
                                  , li $ markdownLink url "bib"
                                  ]
  where url = "/" ++ dropExtension (toFilePath ident)


-- | A publication has two version pdf or bib. The metadata fileshould have a sing
copyPublication :: String   -- version (pdf/bib)
                -> Compiler (Item RemoteFile)
copyPublication ver  = do
  ident <- getUnderlying
  name  <- getMetadataField' ident ver
  dir   <- pubSourceDir
  copyRemoteFileCompiler $ dir </> name <.> ver


pubSourceDir :: Compiler FilePath
pubSourceDir = dropExtension <$> getResourceFilePath

{-
-- | runs make for this identifier.
runMake :: Compiler ()
runMake = do dir <- pubSourceDir
             unsafeCompiler $ void $ system $ unwords ["make", "-C", dir , "pdf"]

-}

{-
-- | Build the local d
buildDownloads :: Identifier -> Rules ()
buildDownloads ident = do
  runMake ident
  create [i]
  localDownloads ident >>= mapM_ rulesDownloadItem


-- Rules for a local download
rulesDownloadItem :: (Identifier, FilePath) -> Rules ()
rulesDownloadItem (i,fp) = do
  create [i] $ do
    route idRoute
    compile $ copyRemoteFileCompiler fp
-}

------------------------ Helper functions ------------------------------


       

li :: String -> String
li = between "<li>" "</li>"

markdownLink :: String -> String -> String
markdownLink url ext = bracket ext ++ paren linkDescr
  where title     = unwords ["Download as", ext]
        linkDescr = unwords [url <.> ext, show title]

between :: String -> String -> String -> String
between begin end str = begin ++ str ++ end

bracket :: String -> String
bracket = between "[" "]"

paren :: String -> String
paren = between "(" ")"
