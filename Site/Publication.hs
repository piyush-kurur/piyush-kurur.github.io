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
import Data.List(intercalate)
import Data.Monoid
import Hakyll
import System.FilePath

import Site.Compilers


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
  match pubPat $ compilePipeline $ loadAndApplyTemplate pubItemT pubContext

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

sources :: Identifier -> Compiler [String]
sources ident = maybe [] words <$> getMetadataField ident "sources"

downloads :: Identifier -> Compiler String
downloads ident = do
  srcs <- sources ident
  case srcs of
    [] -> return []
    _  -> do key <- getMetadataField' ident "key"
             return $ intercalate ", " $ map (makeDownload key) srcs

------------------------ Helper functions ------------------------------

makeDownload :: String -> String -> String
makeDownload key src =  bracket extName
                        ++ paren (unwords [url, show title])
  where ext = takeExtension src
        extName = tail ext
        url     = "/research/publication" </> key <.> ext
        title   = "Download as " ++ extName


between :: String -> String -> String -> String
between begin end str = begin ++ str ++ end

bracket :: String -> String
bracket = between "[" "]"

paren :: String -> String
paren = between "(" ")"
