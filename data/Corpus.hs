module Corpus where

import System.IO
import System.Process (callCommand)

import qualified Streamly.Prelude as S

import qualified Network.Download as D

import qualified Streamly.Codec.Compression.Zlib as Zlib
import qualified Streamly.Codec.Compression.GZip as GZip

import qualified Streamly.FileSystem.Handle as FH

{-# INLINE canterburyGroup #-}
canterburyGroup :: (String, [String])
canterburyGroup =
    let prefix = "data/canterbury/"
        files =
            [ "alice29.txt"
            , "asyoulik.txt"
            , "cp.html"
            , "fields.c"
            , "grammar.lsp"
            , "kennedy.xls"
            , "lcet10.txt"
            , "plrabn12.txt"
            , "ptt5"
            , "sum"
            , "xargs.1"
            ]
     in (prefix, files)

makeCompressedFiles :: (String, [String]) -> IO ()
makeCompressedFiles (p, g) = mapM_ (\x -> mkF (p ++ x)) g
  where
    mkF fp = do
        hr <- openFile fp ReadMode
        hwg <- openFile (fp ++ "_gzip") WriteMode
        hwz <- openFile (fp ++ "_zlib") WriteMode
        S.fold (FH.writeChunks hwg) $ GZip.compress $ S.unfold FH.readChunks hr
        S.fold (FH.writeChunks hwz) $ Zlib.compress $ S.unfold FH.readChunks hr
        hClose hr
        hClose hwg
        hClose hwz

makeCanterburyCorpus :: IO ()
makeCanterburyCorpus = do
    esb <- D.openURI url
    case esb of
        Left err -> error err
        Right bs -> do
            callCommand $ "curl " ++ url ++ " --output " ++ destination
            callCommand "tar -xvf \"data/corpus.tar.gz\" -C \"data/canterbury\""
            makeCompressedFiles canterburyGroup
  where
    url = "http://corpus.canterbury.ac.nz/resources/cantrbry.tar.gz"
    destination = "data/corpus.tar.gz"
