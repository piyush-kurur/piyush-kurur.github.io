{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module MatchTrails where

import           Data.Colour.Palette.ColorSet
import           Data.List.Split              (splitPlaces)
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

colors = map d3Colors1 [0..9]

trail1 :: Trail V2 Double
trail1 = hrule 1 <> hrule 0.5 # rotateBy (1/6) <> hrule 0.5 # rotateBy (-1/6) <> a
  where a = arc (xDir # rotateBy (-1/4)) (1/5 @@ turn)
          # scale 0.7

dia1, dia2, dia3 :: Diagram B

dia1 = trail1 # strokeT # frame 0.1

colorizeTrails :: [Trail V2 Double] -> Diagram B
colorizeTrails ts
  = ts
  # mconcat
  # flip at origin
  # explodeTrail
  # splitPlaces (map (length . trailSegments) ts)
  # map (mconcat . map strokeLocTrail)
  # zipWith lc colors
  # mconcat

dia2 = colorizeTrails (replicate 3 trail1) # frame 0.1

class Monoid m => Action m a where
  (<.>) :: m -> a -> a

data Semi a m = a :>< m

unSemi ::  (a `Semi` m)  ->  (a, m)
unSemi     (a :>< m)     =   (a, m)

instance (Action m a, Monoid a) => Monoid (a `Semi` m) where
  mempty                             =  mempty :>< mempty
  (a1 :>< m1) `mappend` (a2 :>< m2)  =  (a1 `mappend` (m1 <.> a2)) :>< (m1 `mappend` m2)

instance (Floating n, Ord n) => Action (Angle n) (Trail V2 n) where
  (<.>) = rotate

instance Action (Angle n) a => Action (Angle n) [a] where
  (<.>) = map . (<.>)

type TTrail v n = [Trail v n] `Semi` Angle n

toTTrail :: (RealFloat n) => Trail V2 n -> TTrail V2 n
toTTrail t = [t # rotate (signedAngleBetween unitX s)]
           :>< theta
  where
    s = tangentAtStart t
    e = tangentAtEnd t
    theta = signedAngleBetween e s

fromTTrail :: TTrail v n -> [Trail v n]
fromTTrail (t :>< _) = t

dia3 = colorizeTrails
  ( replicate 3 trail1
  # map toTTrail
  # mconcat
  # fromTTrail
  )
  # frame 0.1

main = do
  renderPGF "Trail1.pgf" (mkWidth 200) dia1
  renderPGF "Trail3.pgf" (mkWidth 200) dia2
  renderPGF "TrailT.pgf" (mkWidth 100) dia3
