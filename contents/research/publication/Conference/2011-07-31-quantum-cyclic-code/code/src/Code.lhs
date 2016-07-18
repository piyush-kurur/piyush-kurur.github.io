
> module Code where

> import Text.PrettyPrint
>

> class TeX a where
>     toTeX :: a -> Doc

> class Pretty a where
>     toDoc :: a -> Doc



> data  Stabiliser = Stabiliser {
>       fieldSize :: Int,
>       len       :: Int,
>       dim       :: Int,
>       distance  :: Int
>       }




> instance Pretty Stabiliser where
>     toDoc c | fieldSize c == 2    = simple
>             | otherwise         = detailed
>             where n = int $ len c
>                   k = int $ dim c
>                   d = int $ distance c
>                   q = int $ fieldSize c
>                   simple   = brackets . brackets . hcat $
>                                punctuate comma [n,k,d]
>                   detailed = simple <> text "_" <> q



> instance TeX Stabiliser where
>      toTeX c = text "\\ensuremath" <> braces (toDoc c)



