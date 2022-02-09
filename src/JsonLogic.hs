-- |
-- Module      : JsonLogic
-- Description : JsonLogic library for Haskell
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic (flip3) where

-- | Flips the elements of a 3-tuple.
flip3 ::
  -- | The 'tuple' argument
  (c, b, a) ->
  -- | The return value
  (a, b, c)
flip3 (x, y, z) = (z, y, x)
