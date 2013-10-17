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
import           Data.Maybe
import           Text.Pandoc
import           Text.Pandoc.Shared
import           Hakyll
import qualified Data.Map as M

-- | A link expander.
type Expand      = [Inline] -- ^ The anchor text
                 -> String  -- ^ The title of the link
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
applyNs :: Namespace -> [Inline] -> Maybe (String -> Compiler Inline)
applyNs mp ins = do
  (ns, cont) <- parseFieldIs ':' ins
  func       <- M.lookup (stringify ns) mp
  return $ func cont
  <|> do func <- M.lookup "" mp
         return $ func ins

-- | Expands a link using the namespace.
expand :: Namespace -> Inline -> Compiler Inline
expand mp lnk@(Link ins ("", title)) = fromMaybe (return lnk) $ do
  func <- applyNs mp ins
  return $ func title
expand _ inln = return inln

-- | The wikipedia link expander. It supports the pipe trick as well.
wikipedia :: [Inline] -> String -> Compiler Inline
wikipedia ins title = return
                    $ maybe nopipe pipe
                    $ parseFieldIs '|' ins
  where nopipe      = Link ins $ wikipediaTarget ins title
        pipe        = wikipediaPipeTrick title

-- | Compute the target from an given anchor text and title.
wikipediaTarget :: [Inline] -- ^ anchor text
                -> String   -- ^ title
                -> (String, String)
wikipediaTarget inlns title = (url, actualTitle)
  where url = "http://en.wikipedia.org/wiki/" ++ map tr (stringify inlns)
        actualTitle | null title = "Wikipedia: " ++ stringify inlns
                    | otherwise  = title
        tr c | c == ' '  = '_'
             | otherwise = c

-- | Generate the anchor text, title and link using the wikipedia pipe
-- trick <http://en.wikipedia.org/wiki/Help:Pipe_trick>.
wikipediaPipeTrick  :: String -- ^ Title
                    -> ([Inline],[Inline]) -- ^ stuff before and after
                                           -- the pipe
                    -> Inline
wikipediaPipeTrick title (lnk, cont)
  | not $ null cont  = Link cont                   target
  | isParen lastWord = Link initial                target
  | null rst         = Link lnk                    target
  | otherwise        = Link (normalizeSpaces rst') target
   where initial  = normalizeSpaces $ init lnk
         lastWord = last lnk
         (_,rst)  = break checkComma $ reverse lnk
         rst'     = reverse $ dropComma (head rst) : tail rst
         isParen (Str ('(' : cs)) = last cs == ')'
         isParen _                = False
         target = wikipediaTarget lnk title

         dropComma (Str xs)  = Str $ init xs
         dropComma _         = error "drop comma: this should not have happend"
         checkComma (Str xs) | null xs         = False
                             | last xs  == ',' = True
                             | otherwise       = False
         checkComma _                          = False

-- | Code to parse at a field value pair.
parseField :: Char -> String -> Maybe (String, String)
parseField c str
  | null rest = Nothing
  | otherwise = Just (first, tail rest)
  where (first,rest) = break (==c) str

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
