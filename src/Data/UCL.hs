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
  , withCString, withCStringLen, peekCString )
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr
import qualified Data.Text.Foreign as TF
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Clock (DiffTime)
import Data.ByteString (ByteString, useAsCStringLen)
import Control.Monad ((>=>))


-- Low-level bindings
---------------------

data Parser
data UCLObject
data UCLIter

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

foreign import ccall "ucl_parser_new" ucl_parser_new :: CInt -> IO (Ptr Parser)
foreign import ccall "ucl_parser_add_string" ucl_parser_add_string :: Ptr Parser -> CString -> CSize -> IO Bool
foreign import ccall "ucl_parser_add_file" ucl_parser_add_file :: Ptr Parser -> CString -> IO Bool
foreign import ccall "ucl_parser_get_object" ucl_parser_get_object :: Ptr Parser -> IO (Ptr UCLObject)
foreign import ccall "ucl_parser_get_error" ucl_parser_get_error :: Ptr Parser -> IO CString
foreign import ccall "&ucl_parser_free" p_ucl_parser_free :: FunPtr (Ptr Parser -> IO ())

foreign import ccall "ucl_object_iterate_new" ucl_object_iterate_new :: Ptr UCLObject -> IO (Ptr UCLIter)
foreign import ccall "ucl_object_iterate_safe" ucl_object_iterate_safe :: Ptr UCLIter -> Bool -> IO (Ptr UCLObject)
foreign import ccall "&ucl_object_iterate_free" p_ucl_object_iterate_free :: FunPtr (Ptr UCLIter -> IO ())
foreign import ccall "ucl_object_type" ucl_object_type :: Ptr UCLObject -> IO UCL_TYPE
foreign import ccall "ucl_object_key" ucl_object_key :: Ptr UCLObject -> IO CString
foreign import ccall "ucl_object_toint" ucl_object_toint :: Ptr UCLObject -> IO CInt
foreign import ccall "ucl_object_todouble" ucl_object_todouble :: Ptr UCLObject -> IO CDouble
foreign import ccall "ucl_object_tostring" ucl_object_tostring :: Ptr UCLObject -> IO CString
foreign import ccall "ucl_object_toboolean" ucl_object_toboolean :: Ptr UCLObject -> IO Bool
foreign import ccall "&ucl_object_unref" p_ucl_object_unref :: FunPtr (Ptr UCLObject -> IO ())

foreign import ccall "strlen" c_strlen :: CString -> IO CSize


-- Mid level interface with ForeignPtr
--------------------------------------

newParser :: IO (ForeignPtr Parser)
newParser = ucl_parser_new 0x0 >>= newForeignPtr p_ucl_parser_free

addString :: ForeignPtr Parser -> CStringLen -> IO Bool
addString fp (cs, len) = withForeignPtr fp $ \p ->
  ucl_parser_add_string p cs $ fromIntegral len

addFile :: ForeignPtr Parser -> FilePath -> IO Bool
addFile fp s = withCString s $ \cs ->
  withForeignPtr fp $ \p -> ucl_parser_add_file p cs

getObject :: ForeignPtr Parser -> IO (ForeignPtr UCLObject)
getObject = (`withForeignPtr` ucl_parser_get_object) >=> newForeignPtr p_ucl_object_unref

getError :: ForeignPtr Parser -> IO String
getError = (`withForeignPtr` (ucl_parser_get_error >=> peekCString))

newIterator :: Ptr UCLObject -> IO (ForeignPtr UCLIter)
newIterator = ucl_object_iterate_new >=> newForeignPtr p_ucl_object_iterate_free


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
parseString = (`withCStringLen` parseCStringLen)

parseCStringLen :: CStringLen -> IO (Either String UCL)
parseCStringLen = parseWith addString

-- | Parse the contents of a file into a 'UCL', resolving includes, macros,
-- variables...
--
-- This function is __not__ safe to call on untrusted input: configurations can
-- read files, make http requests, do "billion laughs" attacks, and possibly
-- crash the parser.
parseFile :: FilePath -> IO (Either String UCL)
parseFile = parseWith addFile

parseWith :: (ForeignPtr Parser -> a -> IO Bool) -> a -> IO (Either String UCL)
parseWith addX x = do
  p <- newParser
  didParse <- addX p x
  if didParse
  then Right <$> (getObject p >>= flip withForeignPtr foreignToUCL)
  else Left <$> getError p

-- | An UCL object
data UCL = UCLMap (Map Text UCL)
         | UCLArray [UCL]
         | UCLInt Int
         | UCLDouble Double
         | UCLText Text
         | UCLBool Bool
         | UCLTime DiffTime
  deriving (Show, Eq, Ord)

foreignToUCL :: Ptr UCLObject -> IO UCL
foreignToUCL obj = do
  ty <- ucl_object_type obj
  case ty of
    UCL_OBJECT   -> UCLMap <$> uclObjectToMap obj
    UCL_ARRAY    -> UCLArray <$> uclArrayToList obj
    UCL_INT      -> UCLInt . fromIntegral <$> ucl_object_toint obj
    UCL_FLOAT    -> UCLDouble . realToFrac <$> ucl_object_todouble obj
    UCL_STRING   -> UCLText <$> (ucl_object_tostring obj >>= peekCStringText)
    UCL_BOOLEAN  -> UCLBool <$> ucl_object_toboolean obj
    UCL_TIME     -> UCLTime . realToFrac <$> ucl_object_todouble obj
    -- TODO use Left instead of error
    UCL_USERDATA -> error "Userdata object"
    UCL_NULL     -> error "Null object"
    _            -> error "Unknown Type"

uclObjectToMap :: Ptr UCLObject -> IO (Map Text UCL)
uclObjectToMap o = do
  iter <- newIterator o
  go iter Map.empty
  where 
    go it m = do
      -- NOTE: the reference count of the returned object is not increased,
      -- so we don't use ForeignPtr
      obj <- withForeignPtr it (`ucl_object_iterate_safe` True)
      ty <- ucl_object_type obj
      case ty of
        -- FIXME this is not how we check for end of object
        UCL_NULL -> pure m
        _        -> do
                      k <- ucl_object_key obj >>= peekCStringText
                      v <- foreignToUCL obj
                      go it $ Map.insert k v m

uclArrayToList :: Ptr UCLObject -> IO [UCL]
uclArrayToList o = do
  iter <- newIterator o
  go iter
  where 
    go it = do
      -- NOTE: the reference count of the returned object is not increased
      -- so we don't use ForeignPtr
      obj <- withForeignPtr it (`ucl_object_iterate_safe` True)
      ty <- ucl_object_type obj
      case ty of
        -- FIXME this is not how we check for end of object
        UCL_NULL -> pure []
        _        -> (:) <$> foreignToUCL obj <*> go it
