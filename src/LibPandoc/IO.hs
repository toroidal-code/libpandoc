{-
 - Copyright (C) 2009-2010  Anton Tayanovskyy <name.surname@gmail.com>
 - Copyright (C) 2015  Shahbaz Youssefi <ShabbyX@gmail.com>
 -
 - This file is part of libpandoc, providing C bindings to Pandoc.
 -
 - libpandoc is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 2 of the License, or
 - (at your option) any later version.
 -
 - libpandoc is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with libpandoc.  If not, see <http://www.gnu.org/licenses/>.
 -}


-- | Provides unterleaved UTF-8 input/output to and from C functions.
module LibPandoc.IO (CReader, CWriter, transform, transformBytes) where

import Foreign
import Foreign.C
import System.IO.Unsafe
import qualified Codec.Binary.UTF8.String as Utf8
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString          as BS
 
-- | Represents an input stream as a function.  Reads UTF-8 encoded
-- | characters by copying them into the buffer and returns the number
-- | of bytes remaining to be read (See loop in @readStream@)
type CReader = CString -> CInt -> Ptr () -> IO CInt

-- | Represents an output stream as a function.  Receives a character
-- | buffer and a number of bytes to read.  The bytes always represent
-- | an integer number of UTF-8 characters.
type CWriter = CString -> CInt -> Ptr () -> IO ()

-- | Lifts a string tranformer to a filter on C streams.  The first
-- | parameter is the size of the buffer in bytes.  The number of
-- | bytes returned by the reader and passed to the writer should not
-- | exceed the buffer size.
transform :: Int -> (String -> String) -> CReader -> CWriter -> Ptr () -> IO ()
transform bufferSize transformer reader writer userData =
    withBuffer bufferSize $ \rbuf ->
    withBuffer bufferSize $ \wbuf ->
    do s <- readStream rbuf reader userData
       writeStream wbuf writer (transformer s) userData

transformBytes :: Int -> (String -> BL.ByteString) -> CReader -> CWriter -> Ptr () -> IO ()
transformBytes bufferSize transformer reader writer userData =
  withBuffer bufferSize $ \rbuf ->
  withBuffer bufferSize $ \wbuf ->
  do s <- readStream rbuf reader userData
     writeByteStream wbuf writer (transformer s) userData

newtype Buffer = Buffer (CString, Int)

withBuffer :: Int -> (Buffer -> IO a) -> IO a
withBuffer bufferSize action = withCString init act where
    init    = replicate bufferSize ' '
    act str = action (Buffer (str, bufferSize))

readStream :: Buffer -> CReader -> Ptr () -> IO String
readStream (Buffer (buf, size)) reader userData = unsafeInterleaveIO result where
    sz     = encodeInt size
    result = reader buf sz userData >>= loop . decodeInt
    loop 0 = return []
    loop n = do
      s <- peekCStringLen (buf, n) 
      k <- reader buf sz userData
      fmap (Utf8.decodeString s ++) (loop (decodeInt k))

writeStream :: Buffer -> CWriter -> String -> Ptr() -> IO ()
writeStream (Buffer (buf, size)) writer text userData = loop text where
    buffer = castPtr buf
    loop text = do
      let (head, tail) = splitAt (div size 4) text
          bytes        = Utf8.encode head
      pokeArray buffer bytes
      writer buffer (encodeInt (length bytes)) userData
      case tail of
        [] -> do
          writer nullPtr (encodeInt 0) userData -- Give our writer a chance to clean up
          return ()
        _  -> loop tail

writeByteStream :: Buffer -> CWriter -> BL.ByteString -> Ptr() -> IO ()
writeByteStream (Buffer (buf, size)) writer text userData = loop strict where
     buffer = castPtr buf
     strict = BL.toStrict text
     loop text = do
       let (head, tail) = BS.splitAt (div size 4) text
       BS.useAsCStringLen head (\(b,i) -> do
         copyArray buffer b i
         writer buffer (encodeInt i) userData
         case BS.null tail of
          True  -> do
            writer nullPtr (encodeInt 0) userData
            return ()
          False -> loop tail)


decodeInt :: CInt -> Int
decodeInt x = fromInteger (toInteger x)

encodeInt :: Int -> CInt
encodeInt x = fromInteger (toInteger x)
