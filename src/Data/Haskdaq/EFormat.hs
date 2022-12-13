{-# LANGUAGE DuplicateRecordFields #-}

module Data.Haskdaq.EFormat (RecordData, parserDataRecord) where

import Control.Applicative ((<|>))
import Data.Bits ((.|.), shift)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.ByteString (fromStrict, toStrict)
import Data.Attoparsec.Combinator (count)
import Data.Attoparsec.Binary (anyWord16le, word32le, anyWord32le)
import Data.Attoparsec.ByteString (Parser, parseOnly, anyWord8, take)
import Codec.Compression.Zlib (decompress)

import Data.Haskdaq.Helper (parserCountThenWords)

data CompressionType
    = NoCompression
    | ZLib
    | Reserved
    deriving Show

data CheckSumType
    = NoChecksum
    | CRC16
    | Adler32
    deriving Show

data RunType
    = Physics
    | Calibration
    | Cosmics
    | Test
    | Simulation
    deriving Show

data SourceIdentifier
    = SourceIdentifier
    { subDetectorId :: Word8
    , moduleId :: Word16
    , optionalData :: Word8
    }
    deriving Show

data RobData
    = RobData
    {
    }
    deriving Show

data SpecificRecordData
    = FullEventRecordData
    { eventIdentifier :: Word64
    , runType :: RunType
    , luminosityBlock :: Word32
    , compressionType :: CompressionType
    , l1tWords :: [Word32]
    , hltWords :: [Word32]
    , streamTagWords :: [Word32]
    }
    deriving Show

data GenericRecordData
    = GenericRecordData
    { fragmentSize :: Word32
    , headerSize :: Word32
    , sourceIdentifier :: SourceIdentifier
    , statusElements :: [Word32]
    , checkSumType :: CheckSumType
    }
    deriving Show

type RecordData = (GenericRecordData, SpecificRecordData, RobData)

parserCompressionType :: Parser CompressionType
parserCompressionType =
    (word32le 0x00000000 >> return NoCompression) <|>
    (word32le 0x00000001 >> return ZLib) <|>
    (word32le 0x0000000f >> return Reserved)

parserCheckSumType :: Parser CheckSumType
parserCheckSumType =
    (word32le 0x0 >> return NoChecksum) <|>
    (word32le 0x1 >> return CRC16) <|>
    (word32le 0x2 >> return Adler32)

parserRunType :: Parser RunType
parserRunType =
    (word32le 0x00000000 >> return Physics) <|>
    (word32le 0x00000001 >> return Calibration) <|>
    (word32le 0x00000002 >> return Cosmics) <|>
    (word32le 0x0000000f >> return Test) <|>
    (word32le 0x80000000 >> return Simulation)

parserSourceIdentifier :: Parser SourceIdentifier
parserSourceIdentifier = do
    -- Parse the module ID.
    md <- anyWord16le;

    -- Parse the sub-detector ID.
    sd <- anyWord8;

    -- Parse the optional extra data.
    op <- anyWord8;

    return SourceIdentifier
        { subDetectorId=sd
        , moduleId=md
        , optionalData=op
        }

parserRobData :: Parser RobData
parserRobData = do
    _ <- word32le 0xdd1234dd;
    return RobData
        {
        }

parserGenericRecordData :: Parser GenericRecordData
parserGenericRecordData = do
    -- Read the header marker for full events.
    _ <- word32le 0xaa1234aa;

    -- Read the total fragment size.
    fs <- anyWord32le;

    -- Read the total header size.
    hs <- anyWord32le;

    -- Read the version number; must be 5.0-0.0.
    _ <- word32le 0x05000000;

    -- Read the source identifier.
    si <- parserSourceIdentifier;

    -- Count the status-elements.
    se <- parserCountThenWords anyWord32le anyWord32le;

    -- Get the check-sum type.
    cs <- parserCheckSumType;

    return GenericRecordData
        { fragmentSize=fs
        , headerSize=hs
        , sourceIdentifier=si
        , statusElements=se
        , checkSumType=cs
        }

parserSpecificRecordData :: Parser SpecificRecordData
parserSpecificRecordData = do
    -- Skip some data about bunch crossing times.
    _ <- anyWord32le;
    _ <- anyWord32le;

    -- Parse the event ID in two parts.
    el <- anyWord32le;
    eh <- anyWord32le;

    -- Read the run type.
    rt <- parserRunType;

    -- Read the luminosity block number.
    lb <- anyWord32le;

    -- Skip some fields which I do not understand.
    _ <- count 4 anyWord32le;

    -- Get the compression type.
    cm <- parserCompressionType;

    -- Skip the uncompressed payload size.
    _ <- anyWord32le;

    -- Get the L1 trigger info words
    l1tw <- parserCountThenWords anyWord32le anyWord32le;

    -- Get the L2 trigger words, must be 0.
    _ <- word32le 0;

    -- Get the HLT info words.
    hltw <- parserCountThenWords anyWord32le anyWord32le;

    -- Get the stream tag words.
    sstw <- parserCountThenWords anyWord32le anyWord32le;

    return FullEventRecordData
        { eventIdentifier=(shift ((fromIntegral eh) :: Word64) 32) .|. (fromIntegral el)
        , runType=rt
        , luminosityBlock=lb
        , compressionType=cm
        , l1tWords=l1tw
        , hltWords=hltw
        , streamTagWords=sstw
        }


parserFullEventData :: Parser RecordData
parserFullEventData = do
    -- Read the generic header data first.
    gh <- parserGenericRecordData;

    -- Read the specific header data second.
    sh <- parserSpecificRecordData;

    cmp <- Data.Attoparsec.ByteString.take . (* 4) . fromIntegral $ (fragmentSize $ gh) - (headerSize $ gh);

    case parseOnly parserRobData . toStrict . decompress . fromStrict $ cmp of
        (Right rob) -> return (gh, sh, rob)
        (Left e) -> fail e

parserDataRecord :: Parser RecordData
parserDataRecord = do
    -- Read the data record magic number.
    _ <- word32le 0x1234cccc;

    -- Read the record size: 4.
    _ <- word32le 4;;

    -- Read the number of the data block.
    _ <- anyWord32le;

    -- Read the size of the data block (in bytes).
    _ <- anyWord32le;

    -- Read the actual data block
    parserFullEventData;
