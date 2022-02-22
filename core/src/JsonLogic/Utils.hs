module JsonLogic.Utils where

-- Splits string on periods
-- Same definition as words at: https://github.com/ghc/ghc/blob/master/libraries/base/Data/OldList.hs
-- >>> splitOnPeriod "foo.bar.tea"
-- ["foo","bar","tea"]
splitOnPeriod :: String -> [String]
splitOnPeriod "" = []
splitOnPeriod s = case dropWhile ('.' Prelude.==) s of
  "." -> []
  s' -> w : splitOnPeriod s''
    where
      (w, s'') = break ('.' Prelude.==) s'
