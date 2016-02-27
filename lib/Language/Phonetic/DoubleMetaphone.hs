--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE ForeignFunctionInterface #-}

-- | Provides a double metaphone algorithm extracted from the perl library
-- Text-DoubleMetaphone-0.07.
module Language.Phonetic.DoubleMetaphone
(
    -- * Types
    doubleMetaphoneBounded,
    doubleMetaphone
) where

import           Foreign.C.String       (CString)
import           Foreign.C.Types        (CInt (..))
import           Foreign.Marshal.Array  (allocaArray, peekArray)
import           Foreign.Ptr            (Ptr)

import           Data.ByteString        (ByteString, useAsCString)
import           Data.ByteString.Unsafe (unsafePackMallocCString)
import           System.IO.Unsafe       (unsafePerformIO)


-- | Thin wrapper around a C function, returns two metaphone representations.
doubleMetaphoneBounded
    :: Int
    -- ^ Number of characters to stop computing the longest result at.
    -> ByteString
    -- ^ The input string, should be Latin-1 or Char7 encoded.
    -> (ByteString, ByteString)
doubleMetaphoneBounded max_len input = unsafePerformIO $
    useAsCString input $ \c_str ->
        allocaArray 2 $ \results_p -> do
            c_double_metaphone c_str results_p (fromIntegral max_len)
            rs <- peekArray 2 results_p >>= traverse unsafePackMallocCString
            case rs of
                [r1, r2] -> return (r1, r2)
                _ -> error "doubleMetaphone: |array| != 2, impossible?"

-- | Thin wrapper around a C function, returns two metaphone representations.
--
-- The output will be bounded at somewhere around 2 ^ (word size - 1)
doubleMetaphone
    :: ByteString
    -- ^ The input string, should be Latin-1 or Char7 encoded.
    -> (ByteString, ByteString)
doubleMetaphone = doubleMetaphoneBounded cIntMaxBound
  where cIntMaxBound = fromIntegral (maxBound :: CInt)
foreign import ccall unsafe "double-metaphones.h DoubleMetaphone"
    c_double_metaphone
        :: CString
        -> Ptr CString
        -> CInt
        -> IO ()
