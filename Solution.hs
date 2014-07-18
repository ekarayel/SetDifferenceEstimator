module Solution
  ( Observation(..)
  , solution
  ) where

----------------------------------------
-- STDLIB
----------------------------------------
import Data.Ratio
import Data.Bits
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

-- | Returns the maximum likelihood estimate for the count of elements
-- that are not on both servers.
solution :: [Observation] -> Int
solution obss = 42
