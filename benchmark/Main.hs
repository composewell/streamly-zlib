import Gauge

import Data.Word (Word8)
import Streamly.Memory.Array (Array)

import Streamly

import System.Random (randomRIO)

import qualified Streamly.Codec.Compression.Zlib as Zlib
import qualified Streamly.Codec.Compression.GZip as GZip
import qualified Streamly.Prelude as S

import qualified Data.ByteString.Lazy as BSL
import qualified Streamly.FileSystem.Handle as H

import qualified Streamly.Internal.FileSystem.File as File

import System.IO

import Corpus

{-# INLINE benchIO #-}
benchIO :: String -> IO () -> Benchmark
benchIO name io = bench name $ nfIO io

{-# INLINE benchFunc #-}
benchFunc ::
       (SerialT IO (Array Word8) -> SerialT IO (Array Word8))
    -> FilePath
    -> IO ()
benchFunc f = S.drain . f . File.toChunks

main :: IO ()
main = do
    makeCanterburyCorpus
    defaultMain
        [ bgroup
              "canterbury"
              [ bgroup "compress" (compressGroupBench canterburyGroup)
              , bgroup "decompress" (decompressGroupBench canterburyGroup)
              ]
        ]
  where
    compressGroupBench (prefix, files) =
        let lp = length prefix
            groupFunc fp =
                bgroup
                    (drop lp fp)
                    [ benchIO "zlib" $ benchFunc Zlib.compress fp
                    , benchIO "gzip" $ benchFunc GZip.compress fp
                    ]
         in map (\x -> groupFunc $ prefix ++ x) files
    decompressGroupBench (prefix, files) =
        let lp = length prefix
            groupFunc fp =
                bgroup
                    (drop lp fp)
                    [ benchIO "zlib" $ benchFunc Zlib.decompress (fp ++ "_zlib")
                    , benchIO "gzip" $ benchFunc GZip.decompress (fp ++ "_gzip")
                    ]
         in map (\x -> groupFunc $ prefix ++ x) files
