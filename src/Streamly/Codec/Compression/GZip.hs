-- |
-- Module      : Streamly
-- Copyright   : (c) 2020 Composewell
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Codec.Compression.GZip
    ( CompressionLevel
    , defaultCompression
    , noCompression
    , bestSpeed
    , bestCompression
    , compressionLevel

    , Method
    , deflateMethod

    , WindowBits
    , defaultWindowBits
    , windowBits

    , MemoryLevel
    , defaultMemoryLevel
    , minMemoryLevel
    , maxMemoryLevel
    , memoryLevel

    , CompressionStrategy
    , defaultStrategy
    , filteredStrategy
    , huffmanOnlyStrategy

    , CompressParams
    , defaultCompressParams
    , compressWith
    , compress

    , DecompressParams
    , defaultDecompressParams
    , decompressWith
    , decompress
    ) where

import Streamly
import Streamly.Memory.Array (Array)
import Data.Word (Word8)

import qualified Streamly.Codec.Compression.Zlib.Internal as Internal
import Streamly.Codec.Compression.Zlib.Internal hiding (compress, decompress)

{-# INLINE decompressWith #-}
decompressWith :: DecompressParams -> SerialT IO (Array Word8) -> SerialT IO (Array Word8)
decompressWith = Internal.decompress gzipFormat

{-# INLINE decompress #-}
decompress :: SerialT IO (Array Word8) -> SerialT IO (Array Word8)
decompress = decompressWith defaultDecompressParams

{-# INLINE compressWith #-}
compressWith :: CompressParams -> SerialT IO (Array Word8) -> SerialT IO (Array Word8)
compressWith = Internal.compress gzipFormat

{-# INLINE compress #-}
compress :: SerialT IO (Array Word8) -> SerialT IO (Array Word8)
compress = compressWith defaultCompressParams
