{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.UCL
( UCL(..)
, parseString
, parseByteString
, parseFile
) where

import Foreign.C
  ( CUInt(..), CInt(..), CSize(..), CDouble(..), CString, CStringLen
  , newCString, newCStringLen, peekCString )
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Foreign as TF
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Clock (DiffTime)
import Data.ByteString (ByteString, useAsCStringLen)
import Control.Monad ((>=>))


newtype ParserHandle = ParserHandle (Ptr ())
newtype UCLObjectHandle = UCLObjectHandle (Ptr ())
newtype UCLIterHandle = UCLIterHandle (Ptr ())

type UCL_TYPE = CUInt
pattern UCL_OBJECT :: UCL_TYPE
pattern UCL_OBJECT = 0
pattern UCL_ARRAY :: UCL_TYPE
pattern UCL_ARRAY = 1
pattern UCL_INT :: UCL_TYPE
pattern UCL_INT = 2
pattern UCL_FLOAT :: UCL_TYPE
pattern UCL_FLOAT = 3
pattern UCL_STRING :: UCL_TYPE
pattern UCL_STRING = 4
pattern UCL_BOOLEAN :: UCL_TYPE
pattern UCL_BOOLEAN = 5
pattern UCL_TIME :: UCL_TYPE
pattern UCL_TIME = 6
pattern UCL_USERDATA :: UCL_TYPE
pattern UCL_USERDATA = 7
pattern UCL_NULL :: UCL_TYPE
pattern UCL_NULL = 8


foreign import ccall "ucl_parser_new" ucl_parser_new :: CInt -> IO ParserHandle
foreign import ccall "ucl_parser_add_string" ucl_parser_add_string :: ParserHandle -> CString -> CSize -> IO Bool
foreign import ccall "ucl_parser_add_file" ucl_parser_add_file :: ParserHandle -> CString -> IO Bool
foreign import ccall "ucl_parser_get_object" ucl_parser_get_object :: ParserHandle -> IO UCLObjectHandle
foreign import ccall "ucl_parser_get_error" ucl_parser_get_error :: ParserHandle -> IO CString

foreign import ccall "ucl_object_iterate_new" ucl_object_iterate_new :: UCLObjectHandle -> IO UCLIterHandle
foreign import ccall "ucl_object_iterate_safe" ucl_object_iterate_safe :: UCLIterHandle -> Bool -> IO UCLObjectHandle
foreign import ccall "ucl_object_type" ucl_object_type :: UCLObjectHandle -> UCL_TYPE
foreign import ccall "ucl_object_key" ucl_object_key :: UCLObjectHandle -> CString
foreign import ccall "ucl_object_toint" ucl_object_toint :: UCLObjectHandle -> CInt
foreign import ccall "ucl_object_todouble" ucl_object_todouble :: UCLObjectHandle -> CDouble
foreign import ccall "ucl_object_tostring" ucl_object_tostring :: UCLObjectHandle -> CString
foreign import ccall "ucl_object_toboolean" ucl_object_toboolean :: UCLObjectHandle -> Bool

foreign import ccall "strlen" c_strlen :: CString -> IO CSize


peekCStringText :: CString -> IO Text
peekCStringText cstr = do
  len <- c_strlen cstr
  TF.peekCStringLen (cstr, fromIntegral len)

-- | Parse a 'ByteString' into a 'UCL', resolving includes, macros, variables...
-- Note that unicode does not get converted when using 'fromString'.
-- Prefer 'parseString' when working on 'String's or literals.
--
-- >>> parseByteString $ fromString "{a: [1,2], b: 3min, a: [4]}"
-- Right (UCLMap (fromList
--   [ ("a", UCLArray [UCLInt 1, UCLInt 2, UCLInt 4])
--   , ("b", UCLTime 180s                           )
--   ]))
--
-- This function is __not__ safe to call on untrusted input: configurations can
-- read files, make http requests, do "billion laughs" attacks, and possibly
-- crash the parser.
parseByteString :: ByteString -> IO (Either String UCL)
parseByteString bs = useAsCStringLen bs parseCStringLen

-- | Parse a 'String' into a 'UCL', resolving includes, macros, variables...
--
-- >>> parseString "{a: [1,2], 🌅: 3min, a: [4]}"
-- Right (UCLMap (fromList
--   [ ("a"      , UCLArray [UCLInt 1, UCLInt 2, UCLInt 4])
--   , ("\127749", UCLTime 180s                           )
--   ]))
--
-- This function is __not__ safe to call on untrusted input: configurations can
-- read files, make http requests, do "billion laughs" attacks, and possibly
-- crash the parser.
parseString :: String -> IO (Either String UCL)
parseString = newCStringLen >=> parseCStringLen

parseCStringLen :: CStringLen -> IO (Either String UCL)
parseCStringLen (cs, len) = do
  p <- ucl_parser_new 0x0
  didParse <- ucl_parser_add_string p cs $ fromIntegral len
  if didParse
  then Right . handleToUCL <$> ucl_parser_get_object p
  else Left <$> (ucl_parser_get_error p >>= peekCString)

-- | Parse the contents of a file into a 'UCL', resolving includes, macros,
-- variables...
--
-- This function is __not__ safe to call on untrusted input: configurations can
-- read files, make http requests, do "billion laughs" attacks, and possibly
-- crash the parser.
parseFile :: FilePath -> IO (Either String UCL)
parseFile s = do
    cs <- newCString s
    p <- ucl_parser_new 0x0
    didParse <- ucl_parser_add_file p cs
    if didParse
    then Right . handleToUCL <$> ucl_parser_get_object p
    else Left <$> (ucl_parser_get_error p >>= peekCString)

-- | An UCL object
data UCL = UCLMap (Map Text UCL)
         | UCLArray [UCL]
         | UCLInt Int
         | UCLDouble Double
         | UCLText Text
         | UCLBool Bool
         | UCLTime DiffTime
  deriving (Show, Eq, Ord)

handleToUCL :: UCLObjectHandle -> UCL
handleToUCL o = typedHandleToUCL (ucl_object_type o) o

typedHandleToUCL :: UCL_TYPE -> UCLObjectHandle -> UCL
typedHandleToUCL UCL_OBJECT   obj = UCLMap $ uclObjectToMap obj
typedHandleToUCL UCL_ARRAY    obj = UCLArray $ uclArrayToList obj
typedHandleToUCL UCL_INT      obj = UCLInt $ fromIntegral $ ucl_object_toint obj
typedHandleToUCL UCL_FLOAT    obj = UCLDouble $ realToFrac $ ucl_object_todouble obj
typedHandleToUCL UCL_STRING   obj = UCLText $ unsafePerformIO $ peekCStringText $ ucl_object_tostring obj
typedHandleToUCL UCL_BOOLEAN  obj = UCLBool $ ucl_object_toboolean obj
typedHandleToUCL UCL_TIME     obj = UCLTime $ realToFrac $ ucl_object_todouble obj
typedHandleToUCL UCL_USERDATA _   = error "Userdata object"
typedHandleToUCL UCL_NULL     _   = error "Null object"
typedHandleToUCL _            _   = error "Unknown Type"

uclObjectToMap :: UCLObjectHandle -> Map Text UCL
uclObjectToMap o = unsafePerformIO $ do
  iter <- ucl_object_iterate_new o
  go iter Map.empty
  where 
    go it m = do
      obj <- ucl_object_iterate_safe it True
      case ucl_object_type obj of
        UCL_NULL -> pure m
        _        -> go it $ Map.insert (getUclKey obj) (handleToUCL obj) m
    getUclKey obj = unsafePerformIO $ peekCStringText $ ucl_object_key obj

uclArrayToList :: UCLObjectHandle -> [UCL]
uclArrayToList o = unsafePerformIO $ do
  iter <- ucl_object_iterate_new o
  go iter
  where 
    go it = do
      obj <- ucl_object_iterate_safe it True
      case ucl_object_type obj of
        UCL_NULL -> pure []
        _        -> (handleToUCL obj :) <$> go it
