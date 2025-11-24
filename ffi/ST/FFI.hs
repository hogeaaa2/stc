{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ST.FFI (parseST, freeInPtr) where

import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word8)
import Foreign.C.String
  ( CString,
    newCString,
  )
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Generics (Generic)
import ST.Parser (parseUnits)
import ST.Semantic
  ( AllErrs,
    elaborateUnits,
  )
import Text.Megaparsec
import Vary qualified as V
import Vary.VEither (VEither (VLeft, VRight))

type OneSemErr = V.Vary AllErrs

-- 総合結果
data ParseResult = ParseResult
  { ok :: Bool,
    ast :: A.Value, -- いまは A.Null 固定（将来: 本格 ToJSON へ）
    ast_show :: Maybe Text, -- show した AST
    errors :: [ErrJson] -- ①/②で整形した JSON エラー
  }
  deriving (Show, Generic)

-- 追加の型（簡素なエラー JSON）
data ErrJson = ErrJson
  { phase :: Text, -- "input" / "parse" / "semantic"
    code :: Text, -- "BadJSON" / "ParseError" / "SemanticError"
    message :: Text -- 人間可読なメッセージ
  }
  deriving (Show, Generic)

-- ParseErrorBundle -> [ErrJson]
parseBundleToErrs :: FilePath -> ParseErrorBundle Text Void -> [ErrJson]
parseBundleToErrs fp perr =
  [ ErrJson
      { phase = "parse",
        code = "ParseError",
        message = T.pack fp <> ":\n" <> T.pack (errorBundlePretty perr)
      }
  ]

semErrToErr :: OneSemErr -> ErrJson
semErrToErr e =
  ErrJson
    { phase = "semantic",
      code = "SemanticError",
      message = tshow e -- 各エラー型に Show が付いていれば Variant も Show 可
    }

-- C# から渡してもらう 1 ファイルの形
data FileIn = FileIn
  { path :: FilePath,
    content :: Text
  }
  deriving (Show, Generic)

instance A.FromJSON FileIn where
  -- JSON は { "path": "...", "text": "..." } 形式にします
  parseJSON = A.withObject "FileIn" $ \o ->
    FileIn
      <$> o A..: "path"
      <*> o A..: "text"

-- JSON は段階導入。とりあえず errors は文字列化で十分ならこう。
instance A.ToJSON ParseResult where
  toJSON ParseResult {..} =
    A.object
      [ "ok" A..= ok,
        "ast" A..= ast,
        "ast_show" A..= ast_show,
        "errors" A..= errors
      ]

instance A.ToJSON ErrJson where
  toJSON ErrJson {..} =
    A.object
      [ "phase" A..= phase,
        "code" A..= code,
        "message" A..= message
      ]

foreign export ccall parseST :: Ptr Word8 -> CSize -> IO CString

parseST :: Ptr Word8 -> CSize -> IO CString
parseST p len = do
  bs <- BS.packCStringLen (castPtr p, fromIntegral len)

  -- 入力 JSON の decode
  case A.eitherDecodeStrict' bs :: Either String [FileIn] of
    Left bad ->
      retJSON $
        ParseResult
          { ok = False,
            ast = A.Null,
            ast_show = Nothing,
            errors = [ErrJson {phase = "input", code = "BadJSON", message = T.pack bad}]
          }
    Right fins -> do
      let files = [(path f, content f) | f <- fins]

      case parseUnits files of
        Left (!fp, !perr) ->
          retJSON $
            ParseResult
              { ok = False,
                ast = A.Null,
                ast_show = Nothing,
                errors = parseBundleToErrs fp perr
              }
        -- Access Violation回避のため、パース結果はまず完全評価してから次工程へ
        Right !us -> do
          let baseFenv = M.empty
          case elaborateUnits baseFenv us of
            VLeft !e ->
              retJSON $
                ParseResult
                  { ok = False,
                    ast = A.Null,
                    ast_show = Nothing,
                    errors = [semErrToErr e]
                  }
            VRight !us' ->
              retJSON $
                ParseResult
                  { ok = True,
                    ast = A.Null,
                    ast_show = Just $ T.pack (show us'),
                    errors = []
                  }

-- | 返却した CString の解放用（C# から必ず呼ぶ）
foreign export ccall freeInPtr :: CString -> IO ()

freeInPtr :: CString -> IO ()
freeInPtr = free

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

retJSON :: (A.ToJSON a) => a -> IO CString
retJSON pr = do
  let lbs = A.encode pr
  newCString (BS8.unpack (BL.toStrict lbs))
