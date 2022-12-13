{-# LANGUAGE DuplicateRecordFields #-}

module Data.Haskdaq.EventStorage (FileContents, parserFileContents) where

import Data.Bits ((.|.), shift)
import Data.Word (Word32, Word64)
import Data.ByteString.Internal (w2c)
import Data.Attoparsec.Combinator (count, many')
import Data.Attoparsec.Binary (word32le, anyWord32le)
import Data.Attoparsec.ByteString (Parser, endOfInput)
import Data.Haskdaq.Helper (parserCountThenPaddedString, parserCountThenWords)

data DataFile a
    = DataFile
    { fileNumber :: Word32
    , appName :: String
    , fileName :: String
    , metadataStrings :: [String]
    , dataRecords :: [a]
    }
    deriving Show

data MergedSubfileHeader
    = MergedSubfileHeader
    { fileSize :: Word64
    , fileName :: String
    }
    deriving Show

data MergedFileHeader a
    = MergedFileHeader
    { numEvents :: Word32
    , numEventsSequence :: Word32
    , numFiles :: Word32
    , numFilesSequence :: Word32
    , numSequence :: Word32
    , guid :: String
    , runNumber :: Word32
    , luminosityBlock :: Word32
    , fileName :: String
    , projectTag :: String
    , streamTag :: String
    , extraInfo :: String
    , subFileHeaders :: [MergedSubfileHeader]
    , subFiles :: [DataFile a]
    }
    deriving Show

data FileContents a
    = MergedFile
    { header :: MergedFileHeader a
    }
    deriving Show

parserDataFile :: Parser a -> Parser (DataFile a)
parserDataFile f = do
    -- Read the data file magic marker.
    _ <- word32le 0x1234aaaa;

    -- Read the size of the record. Ignore it.
    _ <- anyWord32le;

    -- Read the version number, which must be 6.
    _ <- word32le 6;

    -- File number in the sequence.
    fn <- anyWord32le

    -- Date and time when the file was opened.
    _ <- anyWord32le;
    _ <- anyWord32le;

    -- Maximum file size in data blocks and megabytes, which we ignore.
    _ <- anyWord32le;
    _ <- anyWord32le;

    -- Another magic marker for the file name string...
    _ <- word32le 0x1234aabb;

    -- Read the application name.
    an <- parserCountThenPaddedString 4 anyWord32le;

    -- Read the file name.
    fi <- parserCountThenPaddedString 4 anyWord32le;

    -- Another file marker... These are quite nice checkpoints.
    _ <- word32le 0x1234aabc;

    -- Parse the number of free meta-data strings.
    md' <- anyWord32le;

    -- Read the GUID.
    gu <- parserCountThenPaddedString 4 anyWord32le;

    md <- count ((fromIntegral md') - 1) (parserCountThenPaddedString 4 anyWord32le);

    -- Yay, more file markers.
    _ <- word32le 0x1234bbbb;

    -- This is IS data, which we skip for now.
    _ <- count 11 anyWord32le;

    -- Read the data records.
    dr <- many' f;

    -- Marker for the end of the file.
    _ <- word32le 0x1234dddd;

    -- Read the size of this record: 10.
    _ <- word32le 10;

    -- Read some unnecessary data.
    _ <- count 7 anyWord32le;

    -- Read the end-of-file marker.
    _ <- word32le 0x1234eeee;

    -- Read the actual end-of-file.
    _ <- endOfInput;

    return DataFile
        { fileNumber=fn
        , appName=an
        , fileName=fi
        , metadataStrings=gu:md
        , dataRecords=dr
        }

parserMergedFileSubFileHeader :: Parser MergedSubfileHeader
parserMergedFileSubFileHeader = do
    -- Read the location of the contained file
    _ <- anyWord32le;
    _ <- anyWord32le;

    -- Read the size of the contained file
    sl <- anyWord32le;
    su <- anyWord32le;

    -- Read the file name
    fn <- parserCountThenWords anyWord32le anyWord32le;

    return MergedSubfileHeader
        { fileSize=(shift ((fromIntegral su) :: Word64) 32) .|. (fromIntegral sl)
        , fileName=map (w2c . fromIntegral) fn
        }

parserMergedFileHeader :: Parser a -> Parser (MergedFileHeader a)
parserMergedFileHeader f = do
    -- Read the file header, which must be 0x1ba2baba.
    _ <- word32le 0x1ba2baba;

    -- Read the format version, which must be 1.
    _ <- word32le 1;

    -- Read the header size, file size, and file size limit.
    _ <- anyWord32le;
    _ <- anyWord32le;
    _ <- anyWord32le;
    _ <- anyWord32le;

    -- Read the number of contained events, events in sequence, and number of
    -- files
    ne <- anyWord32le;
    se <- anyWord32le;
    nf <- anyWord32le;

    -- Read the file opening date and time, which we ignore.
    _ <- anyWord32le;
    _ <- anyWord32le;

    -- Read the run number.
    rn <- anyWord32le;

    -- Read the luminosity block number.
    lb <- anyWord32le;

    -- Read the number of files in sequence
    fs <- anyWord32le;

    -- Read the current file sequence number.
    ns <- anyWord32le;

    -- Read the previous and next file sequence numbers.
    _ <- anyWord32le;
    _ <- anyWord32le;

    -- Read the GUID words.
    gu <- parserCountThenWords anyWord32le anyWord32le;

    -- Read the filename words.
    fn <- parserCountThenWords anyWord32le anyWord32le;

    -- Read the project tag words.
    pt <- parserCountThenWords anyWord32le anyWord32le;

    -- Read the stream tag words.
    st <- parserCountThenWords anyWord32le anyWord32le;

    -- Read the extra information words.
    ei <- parserCountThenWords anyWord32le anyWord32le;

    -- Read number of words of file data, which we ignore.
    _ <- anyWord32le;

    -- Read the information about the subfiles.
    sh <- count (fromIntegral nf) parserMergedFileSubFileHeader;

    -- Read the actual subfiles.
    sf <- count 1 (parserDataFile f);

    return MergedFileHeader
        { numEvents=ne
        , numEventsSequence=se
        , numFiles=nf
        , numFilesSequence=fs
        , numSequence=ns
        , guid=map (w2c . fromIntegral) gu
        , runNumber=rn
        , luminosityBlock=lb
        , fileName=map (w2c . fromIntegral) fn
        , projectTag=map (w2c . fromIntegral) pt
        , streamTag=map (w2c . fromIntegral) st
        , extraInfo=map (w2c . fromIntegral) ei
        , subFileHeaders=sh
        , subFiles=sf
        }

parserFileContents :: Parser a -> Parser (FileContents a)
parserFileContents f = do
    hd <- parserMergedFileHeader f;

    return (MergedFile { header=hd })
