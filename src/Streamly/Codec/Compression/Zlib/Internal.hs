-- |
-- Module      : Streamly
-- Copyright   : (c) 2020 Composewell
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Codec.Compression.Zlib.Internal
    ( ZI.Format
    , ZI.gzipFormat
    , ZI.zlibFormat

    , ZI.CompressionLevel
    , ZI.defaultCompression
    , ZI.noCompression
    , ZI.bestSpeed
    , ZI.bestCompression
    , ZI.compressionLevel

    , ZI.Method
    , ZI.deflateMethod

    , ZI.WindowBits
    , ZI.defaultWindowBits
    , ZI.windowBits

    , ZI.MemoryLevel
    , ZI.defaultMemoryLevel
    , ZI.minMemoryLevel
    , ZI.maxMemoryLevel
    , ZI.memoryLevel

    , ZI.CompressionStrategy
    , ZI.defaultStrategy
    , ZI.filteredStrategy
    , ZI.huffmanOnlyStrategy

    , CompressParams
    , defaultCompressParams
    , compress

    , DecompressParams
    , defaultDecompressParams
    , decompress
    ) where

import Streamly
import Streamly.Memory.Array (Array)
import Data.Word (Word8)
import Streamly.Internal.Data.Stream.StreamD (toStreamD)
import Streamly.Internal.Data.Stream.StreamD.Type (Stream(..), Step(..), fromStreamD)
import Codec.Compression.Zlib.Internal (CompressStream(..), DecompressStream(..))

import qualified Codec.Compression.Zlib.Internal as ZI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Streamly.Prelude as S
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy

import Control.Monad.IO.Class (MonadIO(..))

{-# INLINE joinS #-}
joinS :: Monad m => m (SerialT m a) -> SerialT m a
joinS = S.concatMap id . S.yieldM

data CompressParams =
    CompressParams
        { compressLevel :: !ZI.CompressionLevel
        , compressMethod :: !ZI.Method
        , compressWindowBits :: !ZI.WindowBits
        , compressMemoryLevel :: !ZI.MemoryLevel
        , compressStrategy :: !ZI.CompressionStrategy
        , compressBufferSize :: !Int
        , compressDictionary :: Maybe (Array Word8)
        }
    deriving (Show)

{-# INLINE coerceCompressParamsBA #-}
coerceCompressParamsBA :: ZI.CompressParams -> CompressParams
coerceCompressParamsBA x =
    CompressParams
        { compressLevel = ZI.compressLevel x
        , compressMethod = ZI.compressMethod x
        , compressWindowBits = ZI.compressWindowBits x
        , compressMemoryLevel = ZI.compressMemoryLevel x
        , compressStrategy = ZI.compressStrategy x
        , compressBufferSize = ZI.compressBufferSize x
        , compressDictionary = Strict.toArray <$> ZI.compressDictionary x
        }

{-# INLINE coerceCompressParamsAB #-}
coerceCompressParamsAB :: CompressParams -> ZI.CompressParams
coerceCompressParamsAB x =
    ZI.CompressParams
        { ZI.compressLevel = compressLevel x
        , ZI.compressMethod = compressMethod x
        , ZI.compressWindowBits = compressWindowBits x
        , ZI.compressMemoryLevel = compressMemoryLevel x
        , ZI.compressStrategy = compressStrategy x
        , ZI.compressBufferSize = compressBufferSize x
        , ZI.compressDictionary = Strict.fromArray <$> compressDictionary x
        }

{-# INLINE defaultCompressParams #-}
defaultCompressParams :: CompressParams
defaultCompressParams = coerceCompressParamsBA ZI.defaultCompressParams

data InputState b a
    = InputAvailable a b
    | InputUnavailable b

{-# INLINE compressD #-}
compressD ::
       MonadIO m
    => ZI.Format
    -> CompressParams
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
compressD f cp (Stream stp st) = Stream step state
  where
    cp1 = coerceCompressParamsAB cp
    state = InputAvailable st $ ZI.compressIO f cp1
    step gst (InputAvailable st1 state1@(CompressInputRequired next)) = do
        x <- stp gst st1
        case x of
            Yield arr st2 -> do
                state2 <- liftIO $ next (Strict.fromArray arr)
                return $ Skip $ InputAvailable st2 state2
            Skip st2 -> return $ Skip $ InputAvailable st2 state1
            Stop -> return $ Skip $ InputUnavailable state1
    step gst (InputUnavailable (CompressInputRequired next)) = do
        state2 <- liftIO $ next BS.empty
        return $ Skip $ InputUnavailable state2
    step gst (InputAvailable st1 (CompressOutputAvailable output next)) = do
        state2 <- liftIO next
        return $ Yield (Strict.toArray output) (InputAvailable st1 state2)
    step gst (InputUnavailable (CompressOutputAvailable output next)) = do
        state2 <- liftIO next
        return $ Yield (Strict.toArray output) (InputUnavailable state2)
    step _ (InputAvailable st1 CompressStreamEnd) =
        error "Encountered CompressStreamEnd although input is available"
    step _ (InputUnavailable CompressStreamEnd) = return Stop

{-# INLINE compress #-}
compress ::
       ZI.Format
    -> CompressParams
    -> SerialT IO (Array Word8)
    -> SerialT IO (Array Word8)
compress f cp s = fromStreamD (compressD f cp (toStreamD s))

data DecompressParams =
    DecompressParams
        { decompressWindowBits :: !ZI.WindowBits
        , decompressBufferSize :: !Int
        , decompressDictionary :: Maybe (Array Word8)
        , decompressAllMembers :: Bool
        }
    deriving (Show)

{-# INLINE coerceDecompressParamsBA #-}
coerceDecompressParamsBA :: ZI.DecompressParams -> DecompressParams
coerceDecompressParamsBA x =
    DecompressParams
        { decompressWindowBits = ZI.decompressWindowBits x
        , decompressBufferSize = ZI.decompressBufferSize x
        , decompressDictionary = Strict.toArray <$> ZI.decompressDictionary x
        , decompressAllMembers = ZI.decompressAllMembers x
        }

{-# INLINE coerceDecompressParamsAB #-}
coerceDecompressParamsAB :: DecompressParams -> ZI.DecompressParams
coerceDecompressParamsAB x =
    ZI.DecompressParams
        { ZI.decompressWindowBits = decompressWindowBits x
        , ZI.decompressBufferSize = decompressBufferSize x
        , ZI.decompressDictionary = Strict.fromArray <$> decompressDictionary x
        , ZI.decompressAllMembers = decompressAllMembers x
        }

{-# INLINE defaultDecompressParams #-}
defaultDecompressParams :: DecompressParams
defaultDecompressParams = coerceDecompressParamsBA ZI.defaultDecompressParams

-- XXX Make the yield of the output stream "Either (Array Word8)"?
{-# INLINE decompressD #-}
decompressD ::
       MonadIO m
    => ZI.Format
    -> DecompressParams
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
decompressD f dp (Stream stp st) = Stream step state
  where
    dp1 = coerceDecompressParamsAB dp
    state = InputAvailable st $ ZI.decompressIO f dp1
    step gst (InputAvailable st1 state1@(DecompressInputRequired next)) = do
        x <- stp gst st1
        case x of
            Yield arr st2 -> do
                state2 <- liftIO $ next (Strict.fromArray arr)
                return $ Skip $ InputAvailable st2 state2
            Skip st2 -> return $ Skip $ InputAvailable st2 state1
            Stop -> return $ Skip $ InputUnavailable state1
    step gst (InputUnavailable (DecompressInputRequired next)) = do
        state2 <- liftIO $ next BS.empty
        return $ Skip $ InputUnavailable state2
    step gst (InputAvailable st1 (DecompressOutputAvailable output next)) = do
        state2 <- liftIO next
        return $ Yield (Strict.toArray output) (InputAvailable st1 state2)
    step gst (InputUnavailable (DecompressOutputAvailable output next)) = do
        state2 <- liftIO next
        return $ Yield (Strict.toArray output) (InputUnavailable state2)
    step _ (InputAvailable _ (DecompressStreamEnd _)) = return Stop
    step _ (InputUnavailable (DecompressStreamEnd _)) = return Stop
    step _ (InputAvailable _ (DecompressStreamError err)) = error (show err)
    step _ (InputUnavailable (DecompressStreamError err)) = error (show err)

{-# INLINE decompress #-}
decompress ::
       ZI.Format
    -> DecompressParams
    -> SerialT IO (Array Word8)
    -> SerialT IO (Array Word8)
decompress f dp s = fromStreamD (decompressD f dp (toStreamD s))
