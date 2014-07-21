module Solution
  ( Observation(..)
  , solution
  ) where

----------------------------------------
-- STDLIB
----------------------------------------
import Data.Ratio
import Data.Bits
import Data.List
import Data.Word
import Safe
import Debug.Trace

type ObsHash = Word8

data Observation
    = Observation
    { obs_probability :: Double
    -- ^ Probability that a random element would be selected for hashing.
    , obs_serverA :: ObsHash -- ^ Hash received from server A
    , obs_serverB :: ObsHash -- ^ Hash received from server B
    }
    deriving Show

-- | solution obss maxDocuments - returns the maximum likelihood estimate for the count of
-- elements within [0;maxDocuments] that are not on both servers.
solution :: [Observation] -> Int -> Int
solution observations maxDocuments = 42


