import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Streamly

import Data.Word (Word8)
import Streamly.Memory.Array (Array)
-- import Test.QuickCheck.Instances.ByteString ()

import qualified Streamly.Prelude as S
import qualified Streamly.FileSystem.Handle as H
import qualified Streamly.Codec.Compression.Zlib as Zlib
import qualified Streamly.Codec.Compression.Zlib as GZip
import qualified Streamly.External.ByteString.Lazy as Lazy
import qualified Streamly.Internal.FileSystem.File as File

import Corpus

compressionIdentity ::
       (SerialT IO (Array Word8) -> SerialT IO (Array Word8))
    -> (SerialT IO (Array Word8) -> SerialT IO (Array Word8))
    -> SerialT IO (Array Word8)
    -> IO Bool
compressionIdentity compressFunc decompressFunc src = do
    a <- Lazy.fromChunksIO src
    b <- Lazy.fromChunksIO $ decompressFunc $ compressFunc src
    return $ a == b

main :: IO ()
main = do
    makeCanterburyCorpus
    let testFunc compressFunc decompressFunc = do
            -- prop "follows identity with a non-empty stream" $ \bs -> do
            --     let s = Lazy.toChunks bs
            --     ioProperty $ do
            --         compressionIdentity compressFunc decompressFunc s
            idE <- runIO $ compressionIdentity compressFunc decompressFunc S.nil
            it "follows identity with an empty stream" idE
        testFuncFile fp compressFunc decompressFunc = do
            idE <-
                runIO $
                compressionIdentity compressFunc decompressFunc $
                File.toChunks fp
            it ("follows identity with " ++ fp) idE
        testFuncFiles (prefix, files) compressFunc decompressFunc =
            mapM_
                (\x -> testFuncFile (prefix ++ x) compressFunc decompressFunc)
                files
    hspec $ do
        describe "Zlib" $ do
            testFunc Zlib.compress Zlib.decompress
            testFuncFiles canterburyGroup Zlib.compress Zlib.decompress
        describe "GZip" $ do
            testFunc GZip.compress GZip.decompress
            testFuncFiles canterburyGroup GZip.compress GZip.decompress
