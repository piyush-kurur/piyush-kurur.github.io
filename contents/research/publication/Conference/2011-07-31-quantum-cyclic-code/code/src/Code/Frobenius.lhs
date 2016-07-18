
> module Code.Frobenius where
>
> galoisOrbit :: Int  -- ^ Which field 
>             -> Int  -- ^ Which cyclotomic polynomial (i.e. n)
>             -> Int  -- ^ which root 
>             -> [Int]

> -- | Computes the list '[k , k q , k q^2 , ...]
> galoisOrbit q n k = powers
>     where powers = kbar : takeWhile (/=kbar) [ p * q `mod` n | p <- powers]
>           kbar   = k `mod` n


> -- | Finds if -1 = q^t (mod n) for some t and returns that t if it exists
> getExponent :: Int -- ^ The base field size q
>             -> Int -- ^ which cyclotomic polynomial (i.e. n)
>             -> Maybe Int -- ^ returns t if any such that (n - 1) = q^t (mod n) for
> 

I really love this code. This clearly shows the connection between the galois orbit
of q and the length

> getExponent q n = lookup (n - 1) gc
>     where gc = zip (galoisOrbit q n q) [1..]
> 


> data Frobenius = Forbenius {
>       gpoly  :: [Int],    -- ^ one root for each irreducible factor over F_q
>       hpoly  :: [Int],    -- ^ one root for each irreducible factor over F_q(\eta)
>       aproot :: (Int,Int) -- ^ AP of roots
>     }


