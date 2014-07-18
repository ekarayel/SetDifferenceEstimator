import Control.Monad
import Control.Monad.Random
import Data.Hashable
import Data.List
import Data.Bits
import Data.ByteString(ByteString)
import Data.Word
import System.Random
import Crypto.MAC.SipHash(SipKey(..))
import qualified Crypto.MAC.SipHash as SIP
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString as BS

import TestCase
import Solution

try nCommon nDistinct =
    do (sA, sB) <- evalRandIO $ mkServers nCommon nDistinct
       reqParams <- evalRandIO $ genSipKeys $ 2 * (nCommon + nDistinct)
       let [hAs, hBs] = map (flip requestServer reqParams) [sA, sB]
           mkObs hA hB (p,_)  =
               Observation
               { obs_probability = p
               , obs_serverA = hA
               , obs_serverB = hB }
           obss = zipWith3 mkObs hAs hBs reqParams
           estimate = solution obss
       putStrLn $ "Count of transmitted response bytes:    "
           ++ show (length hAs)
       putStrLn $ "Total count of documents:               "
           ++ show (nCommon + 2 * nDistinct)
       putStrLn $ "Count of distinct documents:         e^("
           ++ (show $ log $ fromIntegral $ 2 * nDistinct) ++ ")"
       putStrLn $ "Guessed count of distinct documents: e^("
           ++ (show $ log $ fromIntegral $ estimate) ++ ")"

main =
    do try 1000 10
       try 10000 100
       try 10000 300
       try 10000 500
       try 10000 700
       try 10000 1000
       try 10000 1300
       try 10000 1500
       try 10000 1800
       try 10000 2000

genSipKeys :: (RandomGen g) => Int -> Rand g [(Double, (SipKey, SipKey))]
genSipKeys n = forM ps $ \p ->
    do k1 <- getRandom
       k2 <- getRandom
       return (p, (k1, k2))
    where
      ps :: [Double]
      ps = map (1.0 /) $ dropWhile ((<= 2.0)) $ takeWhile (<= (fromIntegral $ n `div` 8)) $ map ((1.1^)) [1..]

instance Random SipKey where
    randomR _ _ = error "T"
    random = runRand $
       do k1 <- getRandom
          k2 <- getRandom
          return $ SIP.SipKey k1 k2
