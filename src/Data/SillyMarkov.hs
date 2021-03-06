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
import Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy as T
import Safe

type FTMap = M.Map T.Text (M.Map T.Text Integer)

-- | A frequency table driven by 'M.Map's. On the outer layer, these are keyed
-- by words of the 'T.Text'. These keys point to new 'M.Map's which are keyed on
-- the next word and its frequency.
newtype FrequencyTable =
  FrequencyTable { _table :: FTMap }
  deriving (Eq, Ord, Show)

-- | Lens for 'FrequencyTable'
table :: Functor f => (FTMap -> f FTMap) -> FrequencyTable -> f FrequencyTable
table f (FrequencyTable a) = fmap FrequencyTable (f a)

-- | Very inefficiently make a 'FrequencyTable' used for the markov chain.
mkFrequencyTable :: T.Text -> Maybe FrequencyTable
mkFrequencyTable t = do
  sp <- initMay . chunksOf 2 . drop 1 . concatMap (replicate 2) . T.words $ t
  let groupWords = groupBy (\x y -> listToMaybe x == listToMaybe y) . sort
      groupFreq =
        M.fromList . map (\l -> (head (head l), (countFreq . map last $ l)))
      countFreq x = M.fromListWith (+) [(c, 1) | c <- x]
  return . FrequencyTable . groupFreq . groupWords $ sp
