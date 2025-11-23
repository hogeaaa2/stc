{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ST.FFI (parseST, freeInPtr) where

import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Foreign.C.String
  ( CString,
    newCString,
    peekCString
  )
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (free)
import GHC.Generics (Generic)
import ST.AST
import ST.Parser (parseUnits)
import ST.Semantic
  ( AllErrs,
    FuncEnv,
    SemMode (CodesysLike),
    elaborateUnitsWithDecls,
    funcEnvFromUnits,
    typeEnvFromUnits,
  )
import Text.Megaparsec qualified as MP
import Vary.VEither (VEither (VLeft, VRight))

-- 総合結果
data ParseResult = ParseResult
  { ok :: Bool,
    ast :: Maybe Units, -- 成功時は Just
    errors :: [Text] -- 今は最初の1件のみ格納（B で拡張）
  }
  deriving (Show, Generic)

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
        "ast" A..= astValue, -- 下で定義
        "errors" A..= errors
      ]
    where
      -- Units に ToJSON インスタンスがまだ無いなら、当面は省略 or Null を返す
      astValue =
        case ast of
          Nothing -> A.Null
          Just _ -> A.Null -- TODO: Units に ToJSON を付けたらここを実体に差し替え

foreign export ccall parseST :: CInt -> CString -> IO CString
parseST :: CInt -> CString -> IO CString
parseST cmode cjson = do
  -- 入力 CString→String（UTF-8想定）→ByteString
  inStr <- peekCString cjson
  let bs = BS8.pack inStr

  -- JSON をパース
  case A.eitherDecodeStrict' bs :: Either String [FileIn] of
    Left err -> do
      let msg = T.pack ("Bad JSON for files: " <> err)
          pr = ParseResult {ok = False, ast = Nothing, errors = [msg]}
          out = A.encode pr
      newCString (BS8.unpack (BL.toStrict out))
    Right fins -> do
      let files = [(path f, content f) | f <- fins]
      res <- parseAndElaborateProject CodesysLike M.empty files
      let out = A.encode res
      newCString (BS8.unpack (BL.toStrict out))

-- | 返却した CString の解放用（C# から必ず呼ぶ）
foreign export ccall freeInPtr :: CString -> IO ()
freeInPtr :: CString -> IO ()
freeInPtr = free

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

elaborateAllCollect ::
  SemMode ->
  FuncEnv ->
  Units ->
  IO ParseResult
elaborateAllCollect mode baseFenv us = pure $
  case typeEnvFromUnits @AllErrs us of
    VLeft e -> ParseResult {ok = False, ast = Nothing, errors = [tshow e]}
    VRight tenv ->
      case funcEnvFromUnits @AllErrs tenv baseFenv us of
        VLeft e -> ParseResult {ok = False, ast = Nothing, errors = [tshow e]}
        VRight fenv ->
          case elaborateUnitsWithDecls tenv fenv us of
            VLeft e -> ParseResult {ok = False, ast = Nothing, errors = [tshow e]}
            VRight us' -> ParseResult {ok = True, ast = Just us', errors = []}

parseAndElaborateProject ::
  SemMode ->
  FuncEnv ->
  [(FilePath, Text)] ->
  IO ParseResult
parseAndElaborateProject mode baseFenv files =
  case parseUnits files of
    Left (fp, perr) -> do
      let msg = T.pack fp <> ":\n" <> T.pack (MP.errorBundlePretty perr)
      pure
        ParseResult
          { ok = False,
            ast = Nothing,
            errors = [msg]
          }
    Right us ->
      elaborateAllCollect mode baseFenv us
