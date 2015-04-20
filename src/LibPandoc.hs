{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-
 - Copyright (C) 2009-2010  Anton Tayanovskyy <name.surname@gmail.com>
 - Copyright (C) 2015  Shahbaz Youssefi <ShabbyX@gmail.com>
 - Copyright (C) 2015  Katherine Whitlock <toroidalcode@gmail.com>
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

-- | Provides FFI interface to Pandoc.
module LibPandoc (pandoc, LibPandocSettings(..), defaultLibPandocSettings) where

import           Control.Arrow              ((>>>))
import           Control.Exception          (catch, Exception(..), SomeException(..))
import           Control.Monad.Except       (MonadError(..))
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Char                  as Char
import qualified Data.List                  as List
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.String (IsString)
import           Data.Typeable              (typeOf)
import           Foreign
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import           LibPandoc.IO
import           LibPandoc.Settings
import           System.IO.Unsafe
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html
import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.JSON
import           Text.JSON.Generic          (toJSON,fromJSON)
import           Text.Pandoc.Highlighting
import qualified Text.Highlighting.Kate     as Hk
import qualified Text.XML.Light             as Xml

-- | The type of the main entry point.
type CPandoc = CInt -> CString -> CString -> CString
             -> FunPtr CReader -> FunPtr CWriter -> Ptr ()
             -> IO CString

type CHighlight = CInt -> CString -> CString -> CInt ->
                  FunPtr CReader -> FunPtr CWriter -> Ptr () -> IO CString

foreign export ccall "valid_language" validLanguage :: CString -> IO CInt
foreign export ccall "highlight" libHighlight :: CHighlight
foreign export ccall "pandoc" pandoc      :: CPandoc
foreign export ccall "increase" increase  :: CInt -> IO CInt
foreign import ccall "dynamic" peekReader :: FunPtr CReader -> CReader
foreign import ccall "dynamic" peekWriter :: FunPtr CWriter -> CWriter

increase :: CInt -> IO CInt
increase x = return (x + 1)

readNativeWrapper :: ReaderOptions -> String -> Either PandocError Pandoc
readNativeWrapper options = readNative

getInputFormat :: String -> Maybe (ReaderOptions -> String -> Either PandocError Pandoc)
getInputFormat x =
    case map Char.toLower x of
      "docbook"    -> Just readDocBook
      "html"       -> Just readHtml
      "latex"      -> Just readLaTeX
      "markdown"   -> Just readMarkdown
      "mediawiki"  -> Just readMediaWiki
      "native"     -> Just readNativeWrapper
      "rst"        -> Just readRST
--      "texmath"    -> Just readTeXMath  TODO: disabled until I figure out how to convert it to ReaderOptions -> String -> Pandoc
      "textile"    -> Just readTextile
      _            -> Nothing

getOutputFormat :: String -> Maybe (WriterOptions -> Pandoc -> String)
getOutputFormat x =
    case map Char.toLower x of
      "asciidoc"     -> Just writeAsciiDoc
      "context"      -> Just writeConTeXt
      "docbook"      -> Just writeDocbook
--      "docx"         -> Just writeDocx  TODO: The following are disabled because they return IO types
--      "epub"         -> Just writeEPUB  TODO: Which I do not know yet how to mix with the non IO type
--      "fb2"          -> Just writeFB2
      "html"         -> Just writeHtmlString
      "latex"        -> Just writeLaTeX
      "man"          -> Just writeMan
      "markdown"     -> Just writeMarkdown
      "mediawiki"    -> Just writeMediaWiki
      "native"       -> Just writeNative
--      "odt"          -> Just writeODT
      "opendocument" -> Just writeOpenDocument
      "org"          -> Just writeOrg
      "rst"          -> Just writeRST
      "rtf"          -> Just writeRTF
      "texinfo"      -> Just writeTexinfo
      "textile"      -> Just writeTextile
      _              -> Nothing


-- | Gives preferential treatment to first argument (should be user options)
joinJSON :: JSValue -> JSValue -> JSValue
joinJSON (JSArray a) (JSArray b) = JSArray (List.zipWith joinJSON a b)
joinJSON (JSObject a) (JSObject b) =
  let aMap = Map.fromList . fromJSObject $ a
      bMap = Map.fromList . fromJSObject $ b
  in JSObject . toJSObject . Map.toList $ Map.unionWith joinJSON aMap bMap
joinJSON a _ = a

getSettings :: CString -> IO LibPandocSettings
getSettings settings | settings == nullPtr = return defaultLibPandocSettings
getSettings settings = do
  let defaults = defaultLibPandocSettings
  s <- peekCString settings
  let userSettings = fromResult . decodeStrict $ s
      combined     = userSettings `joinJSON` toJSON defaults 
  return . fromResult . fromJSON $ combined
  where
    fromResult :: Result a -> a
    fromResult (Ok a)    = a
    fromResult (Error e) = error e

pandoc :: CPandoc
pandoc bufferSize input output settings reader writer userData = do
  let r = peekReader reader
      w = peekWriter writer
  i <- peekCString input
  o <- peekCString output
  s <- getSettings settings
  case (getInputFormat i, getOutputFormat o) of
   (Nothing, _)            -> newCString "Invalid input format."
   (_, Nothing)            -> newCString "Invalid output format."
   (Just read, Just write) ->
     do let run = read (readerOptions s) >>> handleError >>> write (writerOptions s)
        result <- tryMaybe (transform (decodeInt bufferSize) run r w userData)
        case result of
         Just (SomeException res) -> newCString (show (typeOf res) ++ ": " ++ show res)
         Nothing -> return nullPtr

  where
    tryMaybe :: IO a -> IO (Maybe SomeException)
    tryMaybe a = catch (a >> return Nothing) (return . Just)

decodeInt :: CInt -> Int
decodeInt = fromInteger . toInteger
validLanguage :: CString -> IO CInt
validLanguage language
  | language == nullPtr = return 0
  | otherwise = do
      lang <- peekCString language
      case toListingsLanguage lang of
       Just _  -> return 1
       Nothing -> return 0
      
libHighlight :: CHighlight
libHighlight bufferSize lang format block reader writer userData = do
   l <- peekCString lang
   f <- peekCString format
   let formatter = getFormatter f
       result    = highlight formatter ("", [l], [])
       r         = peekReader reader
       w         = peekWriter writer
   transformBytes (decodeInt bufferSize) (result >>> renderMaybeHtml) r w userData
   return nullPtr
  where
    renderMaybeHtml :: Maybe Html -> BL.ByteString
    renderMaybeHtml res =
      case res of
       Just s -> renderHtml s
       Nothing -> BL.empty
--    getFormatter :: forall a. (Show a) => String -> (Hk.FormatOptions -> [Hk.SourceLine] -> a)
    getFormatter format =
      case map Char.toLower format of
--       "latex"    -> if block == 1 then formatLaTeXBlock else formatLaTeXInline
       otherwise  -> if block == 1 then formatHtmlBlock else formatHtmlInline -- Default to html
 
