{-# LANGUAGE ForeignFunctionInterface #-}

module ST.FFI (st_parse_program_ok) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import ST.Parser (parseProgram)

-- 成功=1、失敗=0 を返すだけ
foreign export ccall "st_parse_program_ok"
  st_parse_program_ok :: CString -> IO CInt

st_parse_program_ok :: CString -> IO CInt
st_parse_program_ok csrc = do
  bs <- BS.packCString csrc
  let src = TE.decodeUtf8 bs
  pure $ case parseProgram src of
    Right _ -> 1
    Left _ -> 0
