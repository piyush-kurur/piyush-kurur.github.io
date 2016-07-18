import Data.List
import Text.PrettyPrint

roots_ :: Int -> Int -> Int -> [Int] -> [Int]
roots_ n r s l = let t = (rem (4*s) n) in if (t==r) then l
	else roots_ n r t (t:l)

roots :: Int -> Int -> [Int]
roots n r = (roots_ n r r [r])

grow :: Int -> Int -> [Int] -> [Int]
grow n r f = if (elem (rem (r*2) n) f) then f else
	if (elem r f) then f else (f ++ (roots n r))

multigrow :: Int -> [Int] -> [Int] -> [Int]
multigrow n [] f = f
multigrow n (h:t) f = if (elem (rem (h*2) n) f) then f
	else multigrow n t (grow n h f) 

find_dis :: Int -> [Int] -> (Int,Int)
find_dis r f = let
	lbnd e l = if (elem (e-1) l) then (lbnd (e-1) l) else e
	ubnd e l = if (elem (e+1) l) then (ubnd (e+1) l) else e
	in ((lbnd r f),(ubnd r f))

findcode :: Int -> [(Int,Int,[Int])]
findcode n = ( nub [ let
	f = (multigrow n [r1..r2] [])
	(l,u) = (find_dis r1 f)
	in (l,u,(sort f)) | r1<-[1..(n-2)], r2<-[(r1+1)..(n-1)] ]) 

select_maxk :: Int -> [(Int,Int,[Int])] -> [(Int,Int,[Int])]
select_maxk d codes = let
	sel = [ (l,u,f) | (l,u,f) <- codes, (u-l) == (d-2) ]
	odr = (\(l1,u1,f1) (l2,u2,f2)
		-> (compare (length f1) (length f2)) )
	(lm,um,fm) = (minimumBy (odr) sel)
	t = (length fm)
	in if (sel==[]) then [] else
		[(l,u,f)|(l,u,f)<-sel, (length f)==t]

select_maxd :: Int -> Int -> [(Int,Int,[Int])] -> [(Int,Int,[Int])]
select_maxd n k codes = let
	sel = [ (l,u,f) | (l,u,f) <- codes, 2*(length f) == (n-k) ]
	odr = (\(l1,u1,f1) (l2,u2,f2)
		-> (compare (u1-l1) (u2-l2)) )
	(lm,um,fm) = (maximumBy (odr) sel)
	in if (sel==[]) then [] else
		[(l,u,f)|(l,u,f)<-sel, (u-l)==(um-lm)]


good_codes n codes = let
	mk = ( foldr (++) []
		[select_maxk d codes | d <- [3..((quot n 2)+1)]] )
	in (foldr (++) [] [select_maxd n k mk | k <- [1..n] ] )

sigma :: Int -> [Int] -> [Int]
sigma n f = map (\r -> (rem (r*2) n)) f

compg :: Int -> [Int] -> [Int]
compg n f = ([0..(n-1)] \\ f) \\ (sigma n f)

fr :: Int -> Int -> Int -> Int
fr x p n = mod (x*p) n

facs :: [Int] -> Int -> Int -> [Int]
facs [] p n = []
facs f p n = let 
	m = (minimum f)
	s = (fr m p n):(map (\x -> fr x p n) s)
	Just t = elemIndex m s
	mroots = take (t+1) s
	in (m:( facs(f \\ mroots) p n) )


showcode :: Int -> (Int,Int,[Int]) -> String
showcode n (l,u,f) = let
	g = (compg n f)
	k = length g
	gfac = (facs g 2 n)
	ffac = (facs f 4 n)
	t = findt n 4
	in (show t)
		++" & "++(foldr (++) "" (map (\i -> "g_{"++(show i)++"}") gfac) )
		++" & "	++(foldr (++) "" (map (\i -> "h_{"++(show i)++"}") ffac) ) 
		++" & \\beta^{"++(show l)++"},\\ldots,\\beta^{"++(show u) 
		++"} & [["++(show n)++","++(show k)++","++(show (u-l+2))++"]]"
		++" \\\\ \n" 
--	putStr ("g = "++(show g)++"\n")
--	putStr ("f = "++(show f)++"\n")
--	putStr ("f contains "++(show [l..u])++"\n")
--	putStr ("g_i's = "++(show gfac)++"\n")
--	putStr ("f_i's = "++(show ffac)++"\n\n")

showall :: Int -> [(Int,Int,[Int])] -> String
showall n codes = foldr (++) ""
	[(showcode n c)++"\\cline{2-5}\n"| c <- codes ]

findt :: Int -> Int -> Int
findt n p = findti n p 1 0

findti :: Int -> Int -> Int -> Int -> Int
findti n p pi i = if (i>n) then 0 else
	if ((mod (n-1-pi) n)==0) then i else
		findti n p (mod (pi*p) n) (i+1)

main :: IO ()
main = do
	let codelen = 99
	putStr (foldr (++) "" [code_n i | i <-[1..codelen]])
	

code_n :: Int -> String
code_n n = do 
	let t = findt n 2
	if (t == 0) then ("% "++(show n)++" does not divide 2^t+1\n")
		else if ((rem t 2)==1)
			then ("% "++(show n)++" | 2^"++(show t)++"+1 NO LINEAR CODE\n")
			else let
				m = quot t 2
				codes = (findcode n)
				good = nubBy (\(l1,u1,f1) (l2,u2,f2) ->
					( ((u1-l1)==(u2-l2))&&((length f1)==(length f2)) ) )
					(good_codes n codes)
				in ("% "++(show n)++" | 4^"++(show m)++"+1\n"++(showall n good))




{- 

betapower :: Int -> Doc
betapower p = macro "beta" [] <> text "^" <> brace (int p)

rangeOfRoots :: Int -> Int -> Doc
rangeOfRoots m n =  hcat $ punctuate comma [betapower m , macro "ldots" [], betapower n]

-}