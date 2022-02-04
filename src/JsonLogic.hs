module JsonLogic (flip3) where

flip3 :: (c, b, a) -> (a, b, c)
flip3 (x, y, z) = (z, y, x)