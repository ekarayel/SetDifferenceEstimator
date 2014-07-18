module TestCase where

import Data.List
import Data.Word
import Control.Monad
import Control.Monad.Random
import Data.ByteString(ByteString)
import System.Random.Shuffle
import Crypto.MAC.SipHash(SipKey(..),SipHash(..))
import qualified Data.ByteString as BS
import qualified Crypto.MAC.SipHash as SIP

newtype Document = Document { unDocument :: ByteString }
    deriving (Eq,Ord)

data Server =
 Server [Document]

mkServers :: (RandomGen g) => Int -> Int -> Rand g (Server, Server)
mkServers nCommon nDistinct =
    do commonDocs <- forM [1..nCommon] $ const getRandom
       [serverA,serverB] <- forM ["A","B"] $ \_ ->
           do distinctDocs <- forM [1..nDistinct] $ const getRandom
              return $ Server $ sort $ distinctDocs ++ commonDocs
       return (serverA, serverB)

requestServer :: Server -> [(Double, (SipKey, SipKey))] -> [Word8]
requestServer (Server documents) reqs = map request reqs
    where
      request (p, (k1, k2)) =
          fromIntegral
          $ unSipHash
          $ SIP.hash k2
          $ BS.concat
          $ filter ((<= l) . unSipHash . (SIP.hash k1))
          $ map unDocument
          $ documents
          where
            l = floor $
                    (fromIntegral (maxBound :: Word64)) * p +
                    (fromIntegral (minBound :: Word64)) * (1-p)

unSipHash :: SipHash -> Word64
unSipHash (SipHash x) = x

instance Random Document where
 randomR = error "Unimplemented"
 random = runRand $
     do l <- getRandomR (18,19)
        bytes <- forM [1..(l::Int)] $ const getRandom
        return $ Document $ BS.pack bytes

