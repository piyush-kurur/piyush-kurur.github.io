-- | This module implements the pandoc plugin that expands links in
-- the document that does not have any url part. How the expansion is
-- done depending on the namespace associated with the link -- a link
-- of the kind [wiki:Title]() has wiki as the namespace.

module Site.Pandoc.Links
       ( Namespace
       , linkExpand
       , wikipedia
       ) where

import           Control.Applicative
import           Data.List          (intercalate)
import           Data.Maybe
import           Text.Pandoc
import           Text.Pandoc.Shared
import           Hakyll
import qualified Data.Map as M

-- | A link expander.
type Expand      = Attr
                 -> [Inline] -- ^ The anchor text
                 -> String   -- ^ The title of the link
                 -> Compiler Inline

-- | A name space is just a map of strings to link expanders
type Namespace = M.Map String Expand

-- | Expands all link which does not have a target url according to
-- the name space. For a markdown link [wikipedia:Text]( "Title") the
-- name space is wikipedia. If there is no namespace prefix then the
-- name spaces is the empty string.
linkExpand :: Namespace -> Pandoc -> Compiler Pandoc
linkExpand mp = bottomUpM $ expand mp

-- | Apply the namespace function to the the inline element.
applyNs :: Namespace -> Attr -> [Inline] -> Maybe (String -> Compiler Inline)
applyNs mp attr ins = do
  (ns, cont) <- parseFieldIs ':' ins
  func       <- M.lookup (stringify ns) mp
  return $ func attr cont
  <|> do func <- M.lookup "" mp
         return $ func attr ins

-- | Expands a link using the namespace.
expand :: Namespace -> Inline -> Compiler Inline
expand mp lnk@(Link attr ins ("", title)) = fromMaybe (return lnk) $ do
  func <- applyNs mp attr ins
  return $ func title
expand _ inln = return inln

-- | The wikipedia link expander. It supports the pipe trick as well.
wikipedia :: Expand
wikipedia attr ins title = return
                           $ maybe nopipe pipe
                           $ parseFieldIs '|' ins
  where nopipe      = Link attr ins $ wikipediaTarget ins title
        pipe        = wikipediaPipeTrick attr title

-- | Compute the target from an given wiki page title and title.
wikipediaTarget :: [Inline] -- ^ wiki page title
                -> String   -- ^ title
                -> (String, String)
wikipediaTarget pageTitle title = (url, actualTitle)
  where pageTitleStr = stringify pageTitle
        url = "http://en.wikipedia.org/wiki/"
              ++ map tr pageTitleStr
        actualTitle
          | null title = "Wikipedia: " ++ pageTitleStr
          | otherwise  = title
        --
        -- transform spaces to _
        --
        tr c
          | c == ' '  = '_'
          | otherwise = c


-- | Generate the anchor text, title and link using the wikipedia pipe
-- trick <http://en.wikipedia.org/wiki/Help:Pipe_trick>.
--
-- <piped-anchor-text> =
--                 <namespace>*<wiki-link><pipechar><actual-anchor-text>
--
-- 1. Generate the actual url and title from the wiki-link.
--
-- 2. If the actual-anchor-text exists then use it as the actual
--    anchor text otherwise guess it from wiki-link as follows.
--
--    2.1 foo, bar, biz will become foo, bar
--    2.2 foo, bar, biz (bhur) will become foo, bar, biz
--

wikipediaPipeTrick  :: Attr
                    -> String -- ^ Title
                    -> ([Inline],[Inline]) -- ^ stuff before and after
                                           -- the pipe
                    -> Inline
wikipediaPipeTrick attr title (pageTitle, txt)
  | null txt   = Link attr [guessTxt]  target
  | otherwise  = Link attr txt         target
   where target   = wikipediaTarget pageTitle title
         guessTxt = Str
                  $ wikiPipeText
                  $ stringify
                  $ normalizeSpaces pageTitle

wikiPipeText :: String -> String
wikiPipeText str =  intercalate ","
                 $ init commaSep ++ handleLast (last commaSep)
  where commaSep = splitField ',' str
        handleLast x
          | last x == ')' = maybeToList $ fst <$> parseField '(' x
          | otherwise     = []

-- | Code to parse at a field value pair.
parseField :: Char -> String -> Maybe (String, String)
parseField c str
  | null rest = Nothing
  | otherwise = Just (first, tail rest)
  where (first,rest) = break (==c) str

-- Splits the string at those fields.
splitField :: Char -> String -> [String]
splitField c str = maybe [str] rec $ parseField c str
  where rec (x,xs) = x : splitField c xs

-- | Code to parse at a field value pair on an Inline.
parseFieldI :: Char
            -> Inline
            -> Maybe (Inline, Inline)
parseFieldI c (Str str)       = applyOnBoth Str       $ parseField   c str
parseFieldI c (Emph ins)      = applyOnBoth Emph      $ parseFieldIs c ins
parseFieldI c (Strong ins)    = applyOnBoth Strong    $ parseFieldIs c ins
parseFieldI c (Strikeout ins) = applyOnBoth Strikeout $ parseFieldIs c ins
parseFieldI c (Math InlineMath str) = applyOnBoth (Math InlineMath)
                                    $ parseField c str
parseFieldI _ _ = Nothing


-- | Code to parse at a field value pair on an Inline list.
parseFieldIs :: Char
             -> [Inline]
             -> Maybe ([Inline], [Inline])

parseFieldIs  c ins = applyOnBoth normalizeSpaces $ parse ins
  where parse []     = Nothing
        parse (x:xs) = do (xfst,xrst) <- parseFieldI c x
                          return ([xfst], xrst:xs)
                       <|> do (xsfst,xsrst) <- parse xs
                              return (x:xsfst, xsrst)

applyOnBoth :: (a -> b)
            -> Maybe (a,a)
            -> Maybe (b,b)
applyOnBoth f m = do (x,y) <- m
                     return (f x, f y)
