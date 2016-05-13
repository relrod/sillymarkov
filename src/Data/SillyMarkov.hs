-----------------------------------------------------------------------------
-- |
-- Module : Data.SillyMarkov
-- Copyright : (C) 2016 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
----------------------------------------------------------------------------
module Data.SillyMarkov where

import Data.List (groupBy, sort)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

-- | A frequency table driven by 'M.Map's. On the outer layer, these are keyed
-- by words of the 'T.Text'. These keys point to new 'M.Map's which are keyed on
-- the next word and its frequency.
newtype FrequencyTable =
  FrequencyTable (M.Map T.Text (M.Map T.Text Integer))
  deriving (Eq, Ord, Show)

-- | Very inefficiently make a 'FrequencyTable' used for the markov chain.
--
-- Also, this is currently broken because it thinks the last word follows
-- itself. i.e.,
--     mkFrequencyTable "foo"
--         == FrequencyTable (fromList [("foo",fromList [("foo",1)])])
mkFrequencyTable :: T.Text -> FrequencyTable
mkFrequencyTable = FrequencyTable . groupFreq . groupWords . splitWords
  where
    splitWords =
      sort . chunksOf 2 . drop 1 . concatMap (replicate 2) . T.words
    groupWords = groupBy (\x y -> head x == head y)
    groupFreq =
      M.fromList . map (\l -> (head (head l), (countFreq . map last $ l)))
    countFreq x = M.fromListWith (+) [(c, 1) | c <- x]
