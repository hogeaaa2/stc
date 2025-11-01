{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module ST.Semantic
  ( elaborateProgram,
    elaborateUnit,
    SemantDiag (..),
    nominalEq,
  )
where

import Control.Monad (forM, forM_, unless, when)
import Data.Either (fromRight)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import ST.AST
import Text.Megaparsec.Pos

type Env = M.Map Text (STType, Bool, Span)

type TypeEnv = M.Map Text STType

data SemantDiag
  = DuplicateVar {dName :: Text, dPrev :: Span, dHere :: Span}
  | UnknownVar {dName :: Text, dWhere :: Span}
  | AssignToConst {dName :: Text, dWhere :: Span}
  | TypeMismatch {dName :: Text, dWhere :: Span, expected :: STType, actual :: STType}
  | MissingInitializer {dName :: Text, dWhere :: Span}
  | OpTypeMismatch {op :: Text, expected :: STType, actual :: STType}
  | UnknownType {tName :: Text, tWhere :: Span}
  | TypeCycle {tName :: Text, tWhere :: Span}
  | NotAStruct {sWhere :: Span, actual :: STType}
  | UnknownField {fName :: Text, fWhere :: Span, baseType :: STType}
  | NotAnArray {dWhere :: Span, actual :: STType}
  | BadIndexCount {dWhere :: Span, expectedN :: Int, actualN :: Int}
  | IndexNotInt {dWhere :: Span, actual :: STType}
  | AssignToLoopVar {dName :: Text, dWhere :: Span}
  | UnknownEnumValue {tName :: Text, eName :: Text, dWhere :: Span}
  | NotAnEnum {tName :: Text, tWhere :: Span, actual :: STType}
  | InvalidCaseRange {low :: Int, high :: Int}
  | OverlappingCase
  | NonConstantExpr {dWhere :: Span}
  | OutOfRange {dName :: Text, dWhere :: Span, target :: STType, value :: Integer}
  | TooManyAggElems {dName :: Text, dWhere :: Span, expectedElems :: Int, actualElems :: Int}
  | IndexOutOfBounds {dName :: Text, dWhere :: Span, index :: Integer, lowb :: Integer, highb :: Integer}
  deriving (Eq, Show)

toIdent :: Text -> Identifier
toIdent txt =
  let start = initialPos "<test>" -- 行=1, 列=1
      end = start {sourceColumn = mkPos (1 + T.length txt)}
   in Loc (Span start end) txt

-- どこだか分からない / パーサで位置を付けていないノード用
unknownSpan :: Span
unknownSpan =
  let p = initialPos "<unknown>" -- 好きな仮想ファイル名でOK
   in Span p p

-- 左右の Span を “連結” する（左の開始～右の終了）
spanFromTo :: Span -> Span -> Span
spanFromTo l r = Span (spanStart l) (spanEnd r)

-- Maybe の最後（Prelude には無いので軽く用意）
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)

-- フィールド名の比較は Text 同士で（Loc を剥ぐ）
lookupByText :: Identifier -> [(Identifier, STType)] -> Maybe STType
lookupByText fld = go
  where
    key = locVal fld
    go [] = Nothing
    go ((n, t) : xs)
      | locVal n == key = Just t
      | otherwise = go xs

-- -- ざっくり Span を取る（式に Loc が無い場合のために適当で可）
spanOfExpr :: Expr -> Span
spanOfExpr = \case
  EVar i -> locSpan i
  -- e.i は e の開始 ～ i の終了 を範囲に
  EField e i -> spanFromTo (spanOfExpr e) (locSpan i)
  -- e[idxs] は e の span をベースに。最後の idx に位置があれば右端に使う（無ければ e のみ）
  EIndex e idxs ->
    case lastMaybe idxs of
      Just lastIx ->
        let l = spanOfExpr e
            r = spanOfExpr lastIx -- EINT 等は unknownSpan になるが、それでもOK
         in spanFromTo l r
      Nothing -> spanOfExpr e
  -- リテラルや現状位置を持たないノードは unknown にフォールバック
  _ -> unknownSpan

-- lookupByName :: Text -> [(Identifier, STType)] -> Maybe STType
-- lookupByName n = fmap snd . find (\(i, _) -> locVal i == n)

-- =========================
-- 整数/ビット列の範囲情報
-- =========================

intBounds :: STType -> Maybe (Integer, Integer)
intBounds = \case
  SINT -> Just (-128, 127)
  INT -> Just (-32768, 32767)
  DINT -> Just (-2147483648, 2147483647)
  LINT -> Just (-9223372036854775808, 9223372036854775807)
  USINT -> Just (0, 255)
  UINT -> Just (0, 65535)
  UDINT -> Just (0, 4294967295)
  ULINT -> Just (0, 18446744073709551615)
  BYTE -> Just (0, 255)
  WORD -> Just (0, 65535)
  DWORD -> Just (0, 4294967295)
  LWORD -> Just (0, 18446744073709551615)
  _ -> Nothing

fitsIn :: STType -> Integer -> Bool
fitsIn ty n = case intBounds ty of
  Just (lo, hi) -> lo <= n && n <= hi
  Nothing -> False

isIntOrBits :: STType -> Bool
isIntOrBits ty =
  let types =
        [ SINT,
          INT,
          DINT,
          LINT,
          USINT,
          UINT,
          UDINT,
          ULINT,
          BYTE,
          WORD,
          DWORD,
          LWORD
        ]
   in ty `elem` types

-- EINT / ENeg (EINT) から整数値を取り出す
numericLiteralValue :: Expr -> Maybe Integer
numericLiteralValue = \case
  EINT n -> Just (fromIntegral n)
  ENeg (EINT n) -> Just (negate (fromIntegral n))
  _ -> Nothing

-- 定数「設計子」を整数に評価（リテラル or VAR CONSTANT）
-- constIntValue :: TypeEnv -> Env -> Expr -> Maybe Integer
-- constIntValue tenv env e =
--   -- まずは純リテラル（負号付き含む）ならそれを使う
--   case numericLiteralValue e of
--     Just n -> Just n
--     Nothing -> case e of
--       -- VAR CONSTANT k のときは初期値に遡って再帰評価
--       EVar i -> case M.lookup (locVal i) env of
--         Just (_ty, True, Just initE) -> constIntValue tenv env initE
--         _ -> Nothing
--       -- それ以外はここでは扱わない
--       _ -> Nothing

-- LValue の「根」変数名（診断メッセージ用）
lvalueRootName :: LValue -> Text
lvalueRootName = \case
  LVar i -> locVal i
  LField lv _ -> lvalueRootName lv
  LIndex lv _ -> lvalueRootName lv

-- 1 つのセレクタを閉区間に変換（値は [v,v]、範囲は [a,b]）
-- intervalOf :: CaseSelector -> Either [SemantDiag] (Int, Int)
-- intervalOf = \case
--   CSValue v -> Right (v, v)
--   CSRange a b
--     | a <= b -> Right (a, b)
--     | otherwise -> Left [InvalidCaseRange {low = a, high = b}]
--   CSEnum ty _ -> Left [OpTypeMismatch {op = "CASE", expected = INT, actual = Named ty}]

-- -- INT 用：全アームの全セレクタについて「不正な向き」と「重複/交差」を検査
-- checkCaseIntSelectors :: [CaseArm] -> Either [SemantDiag] ()
-- checkCaseIntSelectors arms = do
--   let sels = concatMap (\(CaseArm ss _) -> ss) arms
--   ivals <- traverse intervalOf sels
--   let sorted = sortOn fst ivals
--   if hasOverlap sorted
--     then Left [OverlappingCase]
--     else Right ()
--   where
--     -- inclusive 区間の重なり検出（隣接 1..3 と 4..5 は OK、1..3 と 3..5 は重なり）
--     hasOverlap :: [(Int, Int)] -> Bool
--     hasOverlap [] = False
--     hasOverlap [_] = False
--     hasOverlap ((_, b1) : (a2, b2) : xs)
--       | a2 <= b1 = True
--       | otherwise = hasOverlap ((a2, b2) : xs)

-- 整数リテラル or その符号付き
isIntLikeLiteral :: Expr -> Bool
isIntLikeLiteral = \case
  EINT _ -> True
  ENeg (EINT _) -> True
  _ -> False

isCaseScrutineeType :: TypeEnv -> STType -> Bool
isCaseScrutineeType tenv = \case
  INT -> True
  Named tyId ->
    case M.lookup (locVal tyId) tenv of
      Just (Enum _) -> True
      _ -> False
  _ -> False

-- Named が enum かどうか（ENUM の range 不許可判定で使う）
isEnumNamed :: TypeEnv -> STType -> Maybe Identifier
isEnumNamed tenv = \case
  Named tyId ->
    case M.lookup (locVal tyId) tenv of
      Just Enum {} -> Just tyId
      _ -> Nothing
  _ -> Nothing

-- 整数リテラル（負号付き含む）だけ数値化
evalIntLiteral :: Expr -> Maybe Int
evalIntLiteral = \case
  EINT n -> Just n
  ENeg (EINT n) -> Just (-n)
  _ -> Nothing

-- 包含区間の重なり検出（隣接 1..3 と 4..5 は OK、1..3 と 3..5 は重なり）
hasOverlap :: [(Int, Int)] -> Bool
hasOverlap [] = False
hasOverlap [_] = False
hasOverlap ((_, b1) : (a2, b2) : xs)
  | a2 <= b1 = True
  | otherwise = hasOverlap ((a2, b2) : xs)

-- 既定初期化
defaultInitWithTypes :: TypeEnv -> STType -> Maybe Expr
defaultInitWithTypes tenv = go
  where
    go ty = case ty of
      -- 数値/ビット列/論理
      SINT -> Just (EINT 0)
      INT -> Just (EINT 0)
      DINT -> Just (EINT 0)
      LINT -> Just (EINT 0)
      USINT -> Just (EINT 0)
      UINT -> Just (EINT 0)
      UDINT -> Just (EINT 0)
      ULINT -> Just (EINT 0)
      BYTE -> Just (EINT 0)
      WORD -> Just (EINT 0)
      DWORD -> Just (EINT 0)
      LWORD -> Just (EINT 0)
      REAL -> Just (EREAL 0)
      LREAL -> Just (EREAL 0)
      BOOL -> Just (EBOOL False)
      CHAR -> Just (ECHAR '\0')
      WCHAR -> Just (EWCHAR '\0')
      STRING _ -> Just (ESTRING "")
      WSTRING _ -> Just (EWSTRING "")
      -- 構造体: 各フィールドの既定初期化を合成（すべて Just のときだけ）
      Struct fs -> do
        fields <- traverse (\(fld, fty) -> (,) fld <$> go fty) fs
        pure (EStructAgg fields)
      -- 配列: 要素の既定初期化を繰り返し合成
      Array rs elTy -> do
        el0 <- go elTy
        let n = arrayCapacity rs
        pure (EArrayAgg (replicate n el0))
      -- 別名は解決して再帰
      Named _ -> either (const Nothing) go (resolveType tenv ty)
      -- その他は未定義
      _ -> Nothing

-- 暗黙昇格okかどうか
canPromote :: STType -> STType -> Bool
canPromote from to =
  case (from, to) of
    (INT, REAL) -> True
    (INT, LREAL) -> True
    (REAL, LREAL) -> True
    _ -> False

assignCoerce :: TypeEnv -> STType -> STType -> Bool
assignCoerce tenv fromTy toTy =
  nominalEq tenv fromTy toTy || canPromote fromTy toTy

arrayCapacity :: [ArrRange] -> Int
arrayCapacity = product . fmap (\(ArrRange lo hi) -> hi - lo + 1)

inferType :: TypeEnv -> Env -> Expr -> Either [SemantDiag] STType
inferType tenv env = \case
  EINT _ -> Right INT
  EBOOL _ -> Right BOOL
  EREAL _ -> Right REAL
  ELREAL _ -> Right LREAL
  ESTRING _ -> Right $ STRING Nothing
  EWSTRING _ -> Right $ WSTRING Nothing
  ECHAR _ -> Right CHAR
  EWCHAR _ -> Right WCHAR
  ENeg expr -> do
    t <- inferType tenv env expr
    case t of
      INT -> Right INT
      REAL -> Right REAL
      LREAL -> Right LREAL
      _ -> Left [OpTypeMismatch {op = "NEG", expected = INT, actual = t}]
  -- 数値二項演算（INT/REAL 混在は REAL に昇格）
  EAdd a b -> arith2 "+" a b
  ESub a b -> arith2 "-" a b
  EMul a b -> arith2 "*" a b
  EDiv a b -> arith2 "/" a b
  EMod a b -> do
    ta <- inferType tenv env a
    tb <- inferType tenv env b
    if ta == INT && tb == INT
      then pure INT
      else Left [OpTypeMismatch {op = "MOD", expected = INT, actual = if ta /= INT then ta else tb}]
  -- 比較（数値同士は OK、混在も可）。結果は BOOL
  EEq a b -> comp2 "=" a b
  ENe a b -> comp2 "<>" a b
  ELt a b -> comp2 "<" a b
  ELe a b -> comp2 "<=" a b
  EGt a b -> comp2 ">" a b
  EGe a b -> comp2 ">=" a b
  ENot e -> do
    t0 <- inferType tenv env e
    let t = resolvePrim tenv t0
    case t of
      BOOL -> Right BOOL
      BYTE -> Right BYTE
      WORD -> Right WORD
      DWORD -> Right DWORD
      LWORD -> Right LWORD
      _ -> Left [OpTypeMismatch {op = "NOT", expected = BOOL, actual = t0}]
  EAnd a b -> bitwise2 "AND" a b
  EXor a b -> bitwise2 "XOR" a b
  EOr a b -> bitwise2 "OR" a b
  EVar x -> case M.lookup (locVal x) env of
    Nothing -> Left [UnknownVar (locVal x) (locSpan x)]
    Just (ty, _, _) -> Right ty
  -- a.b の型判定：まず構造体フィールドを試し、それ以外は Enum 修飾の可能性を検討
  -- フィールド参照（構造体 or 列挙リテラル）
  EField e fld -> case e of
    -- 左が識別子のとき：まず“変数か？”を優先チェック
    EVar vId ->
      case M.lookup (locVal vId) env of
        -- 変数が見つかる → 構造体アクセスとして扱う
        Just _ -> goStruct fld e
        Nothing ->
          -- 変数ではなかったので“型名か？”を判定
          case resolveType tenv (Named vId) of
            Right (Enum ctors) ->
              if any ((== locVal fld) . locVal . fst) ctors
                then Right (Named vId) -- 列挙は Named を返す（A-4/a）
                else Left [UnknownEnumValue {tName = locVal vId, eName = locVal fld, dWhere = locSpan vId}]
            Right other -> Left [NotAnEnum {tName = locVal vId, tWhere = locSpan vId, actual = other}]
            -- 型にも無い → ここは“値としても無い”ので UnknownVar にする
            Left _ -> Left [UnknownVar (locVal vId) (locSpan vId)]
    -- それ以外は通常の構造体アクセス解決
    _ -> goStruct fld e
  EIndex arr idxs -> do
    tBase <- inferType tenv env arr
    tRes <- resolveType tenv tBase
    case tRes of
      Array ranges elTy -> do
        -- 次元数チェックは必要ならここで
        forM_ (zip idxs ranges) $ \(ie, ArrRange lo hi) -> do
          ti <- inferType tenv env ie
          -- 添字は整数系のみを許可（必要に応じて緩めてもOK）
          unless (ti == INT || isIntOrBits ti) $
            Left [OpTypeMismatch {op = "[]", expected = INT, actual = ti}]
          -- 定数なら静的境界チェック
          case numericLiteralValue ie of
            Just n ->
              unless (fromIntegral lo <= n && n <= fromIntegral hi) $
                Left
                  [ IndexOutOfBounds
                      { dName = lvalueRootName (LIndex (LVar (toIdent "<expr>")) [ie]),
                        dWhere = spanOfExpr ie,
                        index = n,
                        lowb = fromIntegral lo,
                        highb = fromIntegral hi
                      }
                  ]
            Nothing -> pure ()
        pure elTy
      _ -> Left [NotAnArray {dWhere = spanOfExpr arr, actual = tRes}]
  EEnum ty ctor -> do
    -- TypeName を解決して列挙型かチェック
    tResolved <- resolveType tenv (Named ty)
    case tResolved of
      Enum ctors ->
        if any (\i -> locVal (fst i) == locVal ctor) ctors
          then Right tResolved
          else
            Left [UnknownEnumValue {tName = locVal ty, eName = locVal ctor, dWhere = locSpan ctor}]
      other -> Left [NotAnEnum {tName = locVal ty, tWhere = locSpan ty, actual = other}]
  EArrayAgg _ -> Left []
  EStructAgg _ -> Left []
  where
    -- both o ty a b = do
    --   ta <- inferType tenv env a
    --   tb <- inferType tenv env b
    --   check o ty ta >> check o ty tb

    -- check o expTy actTy =
    --   if actTy == expTy
    --     then Right ()
    --     else Left [OpTypeMismatch {op = o, expected = expTy, actual = actTy}]

    eqLike sym ta tb =
      if nominalEq tenv ta tb
        then Right BOOL
        else Left [OpTypeMismatch {op = sym, expected = ta, actual = tb}]
    -- where
    --   sameForEq INT INT = True
    --   sameForEq BOOL BOOL = True
    --   sameForEq REAL REAL = True
    --   sameForEq LREAL LREAL = True
    --   sameForEq (STRING _) (STRING _) = True
    --   sameForEq (Named a) (Named b) = locVal a == locVal b
    --   sameForEq _ _ = False

    ordLike sym ta tb
      -- 同型で数値なら OK
      | ta == tb,
        ta `elem` [INT, REAL, LREAL] =
          Right BOOL
      | both isChar ta tb = Right BOOL
      | both isWChar ta tb = Right BOOL
      | both isString ta tb = Right BOOL
      | both isWString ta tb = Right BOOL
      -- INT と REAL の混在は OK
      | (ta, tb) == (INT, REAL) || (ta, tb) == (REAL, INT) = Right BOOL
      -- LREAL は同型のみ可：片側だけ LREAL は NG
      | ta == LREAL && tb /= LREAL =
          Left [OpTypeMismatch {op = sym, expected = LREAL, actual = tb}]
      | tb == LREAL && ta /= LREAL =
          Left [OpTypeMismatch {op = sym, expected = LREAL, actual = ta}]
      -- その他（BOOL 等を含む）は NG。算術と同様に「INT を期待」として怒る
      | otherwise =
          Left [OpTypeMismatch {op = sym, expected = INT, actual = tb}]
      where
        isChar = \case CHAR -> True; _ -> False
        isWChar = \case WCHAR -> True; _ -> False
        isString = \case STRING _ -> True; _ -> False
        isWString = \case WSTRING _ -> True; _ -> False
        both f a b = f a == f b

    -- 数値の2項演算
    arith2 o a b = do
      ta <- inferType tenv env a
      tb <- inferType tenv env b
      case (ta, tb) of
        (INT, INT) -> Right INT
        (REAL, REAL) -> Right REAL
        (LREAL, LREAL) -> Right LREAL
        (INT, REAL) -> Right REAL
        (REAL, INT) -> Right REAL
        -- LREAL は“同型のみ”を許可（INT/LREAL や REAL/LREAL は NG）
        (LREAL, x) | x /= LREAL -> Left [OpTypeMismatch {op = o, expected = LREAL, actual = x}]
        (x, LREAL) | x /= LREAL -> Left [OpTypeMismatch {op = o, expected = LREAL, actual = x}]
        -- それ以外（BOOL 等）は「算術は INT を期待」で怒る（テスト準拠）
        _ ->
          let bad = (if (ta == BOOL) || (tb == BOOL) then BOOL else tb)
           in Left [OpTypeMismatch {op = o, expected = INT, actual = bad}]
    -- 比較: 数値同士は OK（混在可）→ BOOL
    comp2 o a b = do
      ta <- inferType tenv env a
      tb <- inferType tenv env b
      case o of
        "=" -> eqLike "=" ta tb
        "<>" -> eqLike "<>" ta tb
        _ -> ordLike o ta tb

    goStruct fld base = do
      tr <- inferType tenv env base
      case resolveType tenv tr of
        Right (Struct fs) ->
          case lookup (locVal fld) [(locVal n, t) | (n, t) <- fs] of
            Just t -> Right t
            Nothing -> Left [UnknownField {fName = locVal fld, fWhere = locSpan fld, baseType = tr}]
        -- 構造体以外に .field は不可
        _ -> Left [NotAStruct {sWhere = locSpan fld, actual = tr}]

    resolvePrim :: TypeEnv -> STType -> STType
    resolvePrim tenv' t = case resolveType tenv' t of
      Right u -> u
      Left _ -> t

    isBitString :: STType -> Bool
    isBitString = \case
      BYTE -> True
      WORD -> True
      DWORD -> True
      LWORD -> True
      _ -> False

    -- \| AND/OR/XOR の共通実装：
    --   - BOOL 同士 → BOOL
    --   - 同幅 BITSTRING 同士 → その型
    --   - それ以外 → 型不一致
    bitwise2 :: Text -> Expr -> Expr -> Either [SemantDiag] STType
    bitwise2 op a b = do
      ta0 <- inferType tenv env a
      tb0 <- inferType tenv env b
      let ta = resolvePrim tenv ta0
          tb = resolvePrim tenv tb0
      case (ta, tb) of
        (BOOL, BOOL) -> Right BOOL
        _ | isBitString ta && ta == tb -> Right ta
        _ -> Left [OpTypeMismatch {op = op, expected = ta0, actual = tb0}]

-- 左辺の“ベース変数”を取り出す（const/存在チェック用）
baseIdent :: LValue -> Identifier
baseIdent = \case
  LVar i -> i
  LField l _ -> baseIdent l
  LIndex l _ -> baseIdent l

-- 環境中の型から、LValue に沿って最終型を掘る
lvalueType :: TypeEnv -> Env -> STType -> LValue -> Either [SemantDiag] STType
lvalueType tenv env ty = \case
  -- 変数：必ず環境に存在すること。無ければ UnknownVar
  LVar i ->
    case M.lookup (locVal i) env of
      Nothing ->
        Left [UnknownVar {dName = locVal i, dWhere = locSpan i}]
      Just (ty', isConst, _mInit) ->
        if isConst
          then Left [AssignToConst {dName = locVal i, dWhere = locSpan i}]
          else Right ty'
  LField l fld -> do
    tr <- lvalueType tenv env ty l
    case tr of
      Struct fs ->
        case lookupByText fld fs of
          Just t -> Right t
          Nothing -> Left [UnknownField {fName = locVal fld, fWhere = locSpan fld, baseType = tr}]
      _ -> Left [NotAStruct {sWhere = locSpan fld, actual = tr}]
  LIndex base idxs@(idx : _) -> do
    tBase <- lvalueType tenv env INT base -- 第3引数は未使用なら何でもOK
    tRes <- resolveType tenv tBase
    case tRes of
      Array ranges elTy -> do
        when (length idxs /= length ranges) $ do
          Left
            [ BadIndexCount
                { dWhere = spanOfExpr idx,
                  expectedN = length ranges,
                  actualN = length idxs
                }
            ]
        forM_ (zip idxs ranges) $ \(ie, ArrRange lo hi) -> do
          ti <- inferType tenv env ie
          unless (ti == INT || isIntOrBits ti) $
            Left [OpTypeMismatch {op = "[]", expected = INT, actual = ti}]
          case numericLiteralValue ie of
            Just n ->
              unless (fromIntegral lo <= n && n <= fromIntegral hi) $
                Left
                  [ IndexOutOfBounds
                      { dName = lvalueRootName (LIndex base idxs),
                        dWhere = spanOfExpr ie,
                        index = n,
                        lowb = fromIntegral lo,
                        highb = fromIntegral hi
                      }
                  ]
            Nothing -> pure ()
        pure elTy
      _ -> Left [OpTypeMismatch {op = "[]", expected = Array [] INT, actual = tRes}]
  LIndex _ _ -> Left []

-- 1変数の意味解析：型チェック & 既定初期化付与
elabVar :: TypeEnv -> Env -> Variable -> Either [SemantDiag] Variable
elabVar tenv env v =
  case (varConst v, varInit v) of
    (True, Nothing) -> Left [MissingInitializer (locVal (varName v)) (locSpan (varName v))]
    (True, Just e) -> set =<< check e
    (False, Nothing) ->
      case defaultInitWithTypes tenv (varType v) of
        Just e0 -> set e0
        Nothing -> Right v
    (False, Just e) -> set =<< check e
  where
    check = checkExprAssignable tenv env (varType v) (varName v)
    set e' = Right v {varInit = Just e'}

-- check e = do
-- -- 左辺（宣言型）を一応解決しておく（Named 対応）
-- let tgtTy = fromRight (varType v) (resolveType tenv (varType v))
-- case e of
--   EArrayAgg es -> do
--     eNorm <- checkArrayAggInit tenv env (varName v) tgtTy es
--     Right v {varInit = Just eNorm}
--   EStructAgg fs -> do
--     eNorm <- checkStructAggInit tenv env (varName v) tgtTy fs
--     Right v {varInit = Just eNorm}
--   _ -> do
--     -- 特例: 右辺が整数リテラル かつ 左辺が整数/ビット列 → 範囲チェック
--     case (numericLiteralValue e, isIntOrBits tgtTy) of
--       (Just n, True) ->
--         if fitsIn tgtTy n
--           then Right v
--           else Left [OutOfRange (locVal (varName v)) (spanOfExpr e) tgtTy n]
--       _ -> do
--         ty <- inferType tenv env e
--         if assignCoerce tenv ty (varType v)
--           then Right v
--           else Left [TypeMismatch (locVal (varName v)) (locSpan (varName v)) (varType v) ty]

-- 右辺 e を「左辺型 tgt に代入可能か」を検査し、必要なら正規化して返す。
--   * 集成初期化子はここで型に合わせて展開/パディング（既存の checkArray/Struct… を利用）
--   * 整数/ビット列への整数リテラルは範囲チェック
--   * それ以外は inferType して assignCoerce で許容判定
-- 失敗時は文脈名 who（通常は LHS の識別子名）を使って診断を出す
checkExprAssignable ::
  TypeEnv ->
  Env ->
  -- | tgt: 左辺の期待型（宣言型）
  STType ->
  -- | who: 診断で使う LHS 名
  Identifier ->
  -- | e0: 右辺式
  Expr ->
  -- | 正規化済みの式（初期化子なら整形後の Expr）
  Either [SemantDiag] Expr
checkExprAssignable tenv env tgt who e0 = do
  let tgt' = fromRight tgt (resolveType tenv tgt) -- Named 解決
  case e0 of
    -- 集成初期化子は「期待型ありき」なので、ここで確定させる
    EArrayAgg xs -> checkArrayAggInit tenv env who tgt' xs -- 既存：正規化済み Expr を返す想定
    EStructAgg fs -> checkStructAggInit tenv env who tgt' fs -- 既存：正規化済み Expr を返す想定

    -- 整数/ビット列 への “整数リテラル” は範囲チェック
    _ -> do
      case (numericLiteralValue e0, isIntOrBits tgt') of
        (Just n, True) ->
          if fitsIn tgt' n
            then Right e0
            else Left [OutOfRange (locVal who) (spanOfExpr e0) tgt' n]
        -- 上記以外は通常の型推論 → 代入互換判定
        _ -> do
          ty <- inferType tenv env e0
          if assignCoerce tenv ty tgt'
            then Right e0
            else Left [TypeMismatch (locVal who) (locSpan who) tgt' ty]

-- =========================================
--  集成初期化子（配列）
-- =========================================
checkArrayAggInit ::
  TypeEnv ->
  Env ->
  Identifier -> -- 変数名（診断用）
  STType -> -- 期待型（Named 可能）
  [Expr] -> -- 与えられた要素
  Either [SemantDiag] Expr -- 正規化した EArrayAgg を返す
checkArrayAggInit tenv env vname tgtTy0 es = do
  tgtTy <- resolveType tenv tgtTy0
  case tgtTy of
    Array ranges elTy -> do
      let len = arrayLen ranges

      if length es > len
        then do
          let sp = spanFromTo (spanOfExpr (head es)) (spanOfExpr (last es))
          Left [TooManyAggElems {dName = locVal vname, dWhere = sp, expectedElems = len, actualElems = length es}]
        else do
          -- 既存の単一要素割当ルールで各要素を検査
          mapM_ (checkExprAssignable tenv env elTy vname) es
          -- 足りない分は型の既定初期値で埋める（なければエラーにしたい場合はここで Left）
          let fillers = replicate (len - length es) (fromMaybe (fallbackZero elTy) (defaultInitWithTypes tenv elTy))
          Right (EArrayAgg (es ++ fillers))
    _ -> Left [TypeMismatch (locVal vname) (locSpan vname) (Array [] INT) tgtTy] -- 期待が配列でない
  where
    arrayLen :: [ArrRange] -> Int
    arrayLen = sum . map (\(ArrRange lo hi) -> hi - lo + 1)

    -- 最低限のフォールバック（defaultInitWithTypes が Nothing の型に遭遇した場合の保険）
    fallbackZero :: STType -> Expr
    fallbackZero t
      | isIntOrBits t = EINT 0
      | t == BOOL = EBOOL False
      | otherwise = EINT 0 -- TODO: 必要に応じて拡張

-- =========================================
--  集成初期化子（構造体）
-- =========================================
checkStructAggInit ::
  TypeEnv ->
  Env ->
  Identifier ->
  STType ->
  [(Identifier, Expr)] -> -- (フィールド名 := 式)
  Either [SemantDiag] Expr
checkStructAggInit tenv env varId tgtTy pairs = do
  -- 1) ターゲット型を解決して構造体定義を得る
  resolved <- resolveType tenv tgtTy
  case resolved of
    Struct defFields -> do
      -- 定義済みフィールドのマップ（Text キー）
      let defMap :: M.Map Text (Identifier, STType)
          defMap = M.fromList [(locVal nm, (nm, ty)) | (nm, ty) <- defFields]

      -- 与えられた (field := expr) を一つずつ検査
      normPairs <- forM pairs $ \(fld, e) ->
        case M.lookup (locVal fld) defMap of
          Nothing ->
            Left
              [ UnknownField
                  { fName = locVal varId, -- 変数名 or 型名、あなたの診断定義に合わせて
                    fWhere = locSpan fld,
                    baseType = resolved
                  }
              ]
          Just (_defNm, fTy) -> do
            e' <- checkExprAssignable tenv env fTy fld e -- Expr を正規化して返す版
            pure (locVal fld, (fld, e'))

      -- 2) 重複（同一フィールド二度指定）チェック（任意）
      -- let keys = map fst normPairs
      -- case dupKey keys of
      --   Just dup ->
      --     Left
      --       [ DuplicateFieldInInit
      --           { tName = locVal (varName varId),
      --             fName = dup,
      --             dWhere = locSpan varId
      --           }
      --       ]
      --   Nothing -> pure ()

      -- 3) 足りないフィールドは既定値で補完
      let given :: M.Map Text Expr
          given = M.fromList [(k, e) | (k, (_fldId, e)) <- normPairs]

      filled <- forM defFields $ \(nm, fTy) ->
        case M.lookup (locVal nm) given of
          Just e' -> pure (nm, e')
          Nothing ->
            case defaultInitWithTypes tenv fTy of
              Just defE -> pure (nm, defE)
              Nothing -> Left [] -- pure (nm, EDefault) -- 必要に応じてポリシーを

      -- 4) 正規化済みの集成初期化を返す（フィールドは定義順で並べる）
      pure (EStructAgg filled)
    other ->
      Left [NotAStruct {sWhere = locSpan varId, actual = other}]
  where
    dupKey :: [Text] -> Maybe Text
    dupKey = go Set.empty
      where
        go _ [] = Nothing
        go s (k : ks)
          | k `Set.member` s = Just k
          | otherwise = go (Set.insert k s) ks

-- checkStructAggInit tenv env vname tgtTy0 fs = do
--   tgtTy <- resolveType tenv tgtTy0
--   case tgtTy of
--     Struct declFields -> do
--       let declMap = M.fromList [(locVal f, (f, ty)) | (f, ty) <- declFields]

--       -- 未知フィールド検査
--       forM_ fs $ \(fname, _) ->
--         when (M.notMember (locVal fname) declMap) $
--           Left [TypeMismatch (locVal vname) (locSpan vname) tgtTy0 tgtTy]

--       -- 宣言順に正規化：指定があればそれを、無ければ既定値
--       norm <- forM declFields $ \(dfName, dfTy) -> do
--         case findInitByText (locVal dfName) fs of
--           Just (_, e) -> do
--             _ <- checkExprAssignable tenv env dfTy vname e
--             pure (dfName, e)
--           Nothing ->
--             case defaultInitWithTypes tenv dfTy of
--               Just de -> pure (dfName, de)
--               Nothing -> pure (dfName, fallbackZero dfTy)
--       Right (EStructAgg norm)
--     _ -> Left [TypeMismatch (locVal vname) (locSpan vname) (Struct []) tgtTy]
--   where
--     findInitByText :: Text -> [(Identifier, Expr)] -> Maybe (Identifier, Expr)
--     findInitByText k = find (\(i, _) -> locVal i == k)

--     fallbackZero :: STType -> Expr
--     fallbackZero t
--       | isIntOrBits t = EINT 0
--       | t == BOOL = EBOOL False
--       | otherwise = EINT 0

-- 既存: assignCoerce, inferType, lvalueType, spanOfExpr, locVal/locSpan などを流用

-- elabVar を「型に応じた初期化子の精査＆補完」を挟む形に拡張
-- elabVar :: TypeEnv -> Env -> Variable -> Either [SemantDiag] Variable
-- elabVar tenv env v =
--   case (varConst v, varInit v) of
--     (True, Nothing) -> Left [MissingInitializer (locVal (varName v)) (locSpan (varName v))]
--     (True, Just e) -> do
--       e' <- checkInit tenv env (varName v) (varType v) e
--       Right v {varInit = Just e'}
--     (False, Nothing) ->
--       case defaultInitWithTypes tenv (varType v) of
--         Just e0 -> Right v {varInit = Just e0}
--         Nothing -> Right v
--     (False, Just e) -> do
--       e' <- checkInit tenv env (varName v) (varType v) e
--       Right v {varInit = Just e'}

-- ★ 初期化子の検査＆必要なら補完して式を返す
checkInit ::
  TypeEnv ->
  Env ->
  Identifier ->
  STType ->
  Expr ->
  Either [SemantDiag] Expr
checkInit tenv env vName ty e = do
  ty' <- Right =<< resolveType tenv ty
  case (ty', e) of
    -- 構造体: 不足は既定値で補完、未知/重複はエラー、型は各フィールドで検査
    (Struct fs, EStructAgg kvs) -> do
      let schema = M.fromListWith (\_ _ -> error "dup") [(locVal f, (f, t)) | (f, t) <- fs]
          seen = Set.fromList (map (locVal . fst) kvs)
          -- 未知フィールド検出
          unknowns = [(i, locSpan i) | (i, _) <- kvs, M.notMember (locVal i) schema]
      unless (null unknowns) $
        Left [UnknownField {fName = locVal i, fWhere = sp, baseType = ty'} | (i, sp) <- unknowns]
      -- 各与えられたフィールドを型チェック
      checkedGiven <- forM kvs $ \(i, ex) -> do
        let (_, fty) = schema M.! locVal i
        ex' <- checkAssignable tenv env fty ex
        pure (i, ex')
      -- 欠落フィールドを既定値で補完
      let missing = [(f, fty) | (f, fty) <- fs, Set.notMember (locVal f) seen]
      filled <- forM missing $ \(f, fty) ->
        case defaultInitWithTypes tenv fty of
          Just e0 -> Right (f, e0)
          Nothing -> Right (f, EINT 0) -- 最低限のフォールバック（基本来ない想定）
      pure $ EStructAgg (checkedGiven ++ filled)

    -- 構造体型なのに構造体集成でない
    (Struct _, other) -> do
      t <- inferType tenv env other
      if nominalEq tenv ty' t then Right other else Left [TypeMismatch (locVal vName) (spanOfExpr other) ty' t]

    -- 配列: 個数超過はエラー、個数不足は既定値で埋める。要素ごとに型検査
    (Array rs elTy, EArrayAgg es) -> do
      let cap = arrayCapacity rs
      when (length es > cap) $
        Left
          [ TooManyAggElems
              { dName = locVal vName,
                dWhere = spanOfExpr e,
                expectedElems = cap,
                actualElems = length es
              }
          ]
      es' <- traverse (checkAssignable tenv env elTy) es
      let need = cap - length es'
      tailFill <-
        if need <= 0
          then pure []
          else case defaultInitWithTypes tenv elTy of
            Just e0 -> pure (replicate need e0)
            Nothing -> pure (replicate need (EINT 0))
      pure $ EArrayAgg (es' ++ tailFill)

    -- 配列型なのに配列集成でない
    (Array _ _, other) -> do
      t <- inferType tenv env other
      if nominalEq tenv ty' t then Right other else Left [TypeMismatch (locVal vName) (spanOfExpr other) ty' t]

    -- それ以外は従来どおり（暗黙変換を許す）
    (_, other) -> checkAssignable tenv env ty' other

-- 要素/式が目標型に代入可能か検査し、必要なら許容（暗黙変換OKのときは Right ()）
checkAssignable ::
  TypeEnv ->
  Env ->
  STType ->
  Expr ->
  Either [SemantDiag] Expr
checkAssignable tenv env target expr = do
  actual <- inferType tenv env expr
  if assignCoerce tenv actual target
    then Right expr
    else Left [OpTypeMismatch {op = ":=", expected = target, actual = actual}]

-- 本体の文チェック
checkStmt :: TypeEnv -> Env -> Statement -> Either [SemantDiag] ()
checkStmt tenv env = \case
  Skip -> Right ()
  Assign lv e -> do
    tr <- inferType tenv env e
    tl <- lvalueType tenv env tr lv
    -- 特例: 右辺が整数リテラルで、左辺が整数/ビット列 → 範囲チェック
    case (isIntOrBits tl, numericLiteralValue e) of
      (True, Just n) ->
        if fitsIn tl n
          then Right ()
          else Left [OutOfRange {dName = lvalueRootName lv, dWhere = spanOfExpr e, target = tl, value = n}]
      _ ->
        if assignCoerce tenv tr tl
          then Right ()
          else Left [TypeMismatch {dName = lvalueRootName lv, dWhere = spanOfExpr e, expected = tl, actual = tr}]
  If c0 th0 elsifs els -> do
    t0 <- inferType tenv env c0
    if t0 == BOOL
      then pure ()
      else Left [OpTypeMismatch {op = "IF", expected = BOOL, actual = t0}]
    mapM_ (checkStmt tenv env) th0
    -- 各 ELSIF
    forM_ elsifs $ \(ce, block) -> do
      te <- inferType tenv env ce
      if te == BOOL
        then pure ()
        else Left [OpTypeMismatch {op = "ELSIF", expected = BOOL, actual = te}]
      mapM_ (checkStmt tenv env) block
    -- ELSE
    mapM_ (checkStmt tenv env) els
  -- WHILE：条件は BOOL
  While cond body -> do
    t <- inferType tenv env cond
    if t == BOOL
      then mapM_ (checkStmt tenv env) body
      else Left [OpTypeMismatch {op = "WHILE", expected = BOOL, actual = t}]

  -- REPEAT：UNTIL 条件は BOOL
  Repeat body cond -> do
    mapM_ (checkStmt tenv env) body
    t <- inferType tenv env cond
    if t == BOOL
      then Right ()
      else Left [OpTypeMismatch {op = "REPEAT-UNTIL", expected = BOOL, actual = t}]
  -- CASE：セレクタは Constant_Expr / Subrange（定数式）で、型は scrutinee と一致（名目同一含む）
  Case scrut arms els -> do
    tScrut <- inferType tenv env scrut
    unless (isCaseScrutineeType tenv tScrut) $ Left [OpTypeMismatch {op = "CASE", expected = INT, actual = tScrut}]
    -- ここでは INT / ENUM（Named→Enum 解決）を主対象（必要なら REAL/LREAL 拡張可）
    -- 型チェックは nominalEq を使って “同名 ENUM 同士” を同一扱い。
    -- let checkSel = \case
    --       CSExpr e -> do
    --         checkConstDesignator tenv env e
    --         tSel <- inferType tenv env e
    --         if nominalEq tenv tScrut tSel
    --           then Right ()
    --           else Left [OpTypeMismatch {op = "CASE", expected = tScrut, actual = tSel}]
    --       CSRangeE a b -> do
    --         checkConstDesignator tenv env a
    --         checkConstDesignator tenv env b
    --         ta <- inferType tenv env a
    --         tb <- inferType tenv env b
    --         if nominalEq tenv tScrut ta && nominalEq tenv ta tb
    --           then Right ()
    --           else Left [OpTypeMismatch {op = "CASE", expected = tScrut, actual = tb}]
    -- セレクタ1個の基本チェック（定数式 + 型一致）
    let checkSelBasic :: Expr -> Either [SemantDiag] ()
        checkSelBasic e = do
          tSel <- inferType tenv env e
          if nominalEq tenv tScrut tSel
            then checkConstDesignator tenv env e
            else Left [OpTypeMismatch {op = "CASE", expected = tScrut, actual = tSel}]

    -- アーム本体の文の検査
    let checkArmBody = mapM_ (checkStmt tenv env)

    -- ★ まず各セレクタの基本検査（全型に共通）
    forM_ arms $ \(CaseArm sels _) -> do
      forM_ sels $ \case
        CSExpr e -> checkSelBasic e
        CSRangeE a b -> do
          -- ENUM のとき range は不許可（型ミスマッチ扱いで弾く）
          case isEnumNamed tenv tScrut of
            Just tyId ->
              Left [OpTypeMismatch {op = "CASE", expected = Named tyId, actual = INT}]
            Nothing -> do
              -- INT（など）なら各端点の基本検査（型一致 & const）
              checkSelBasic a
              checkSelBasic b
    -- ここでは本文の検査はまだしない（後で一括）

    -- ★ INT スクルーティニのときだけ、向き&重複/交差チェック（リテラル評価できるもののみ）
    case tScrut of
      INT -> do
        let selIntervals :: CaseSelector -> Either [SemantDiag] (Maybe (Int, Int))
            selIntervals = \case
              CSExpr e ->
                case evalIntLiteral e of
                  Just n -> Right (Just (n, n))
                  Nothing -> Right Nothing -- 値が取れないものは判定スキップ
              CSRangeE a b ->
                case (evalIntLiteral a, evalIntLiteral b) of
                  (Just x, Just y)
                    | x <= y -> Right (Just (x, y))
                    | otherwise -> Left [InvalidCaseRange {low = x, high = y}]
                  _ -> Right Nothing

        -- 各アーム内の重複・交差
        forM_ arms $ \(CaseArm sels _) -> do
          ivs <- catMaybes <$> traverse selIntervals sels
          let sorted = sortOn fst ivs
          when (hasOverlap sorted) $
            Left [OverlappingCase]

        -- アーム間の重複・交差
        let allSels = concatMap (\(CaseArm ss _) -> ss) arms
        allIvs <- catMaybes <$> traverse selIntervals allSels
        when (hasOverlap (sortOn fst allIvs)) $
          Left [OverlappingCase]
      _ -> pure () -- ENUM 等はここでは追加チェックなし
      -- mapM_ (\(CaseArm sels ss) -> mapM_ checkSel sels >> mapM_ (checkStmt tenv env) ss) arms
      -- mapM_ (checkStmt tenv env) els
    forM_ arms $ \(CaseArm _ ss) -> checkArmBody ss
    mapM_ (checkStmt tenv env) els
    where

  -- -- ラベル種別に応じて「期待/実際」の型を作るためのヘルパ
  -- actualInt = INT
  -- actualEnum = Named

  -- 列挙型スクルーティニでは、同じ列挙型名の列挙子のみ許可
  -- ensureEnumSelectors :: STType -> Identifier -> CaseArm -> Either [SemantDiag] ()
  -- ensureEnumSelectors t expectedTy (CaseArm sels _) =
  --   mapM_ checkOne sels
  --   where
  --     checkOne = \case
  --       -- 整数値/範囲は NG
  --       CSValue _ -> Left [OpTypeMismatch {op = "CASE", expected = t, actual = actualInt}]
  --       CSRange _ _ -> Left [OpTypeMismatch {op = "CASE", expected = t, actual = actualInt}]
  --       -- 列挙子は、型名が一致する場合のみ OK
  --       CSEnum ty _ ->
  --         if locVal ty == locVal expectedTy
  --           then Right ()
  --           else
  --             Left [OpTypeMismatch {op = "CASE", expected = t, actual = actualEnum ty}]
  For iv e0 e1 mby body -> do
    -- ループ変数の存在／const でない／型が INT
    let nameTxt = locVal iv
        whereSp = locSpan iv
    case M.lookup nameTxt env of
      Nothing -> Left [UnknownVar nameTxt whereSp]
      Just (_, True, _) -> Left [AssignToConst nameTxt whereSp]
      Just (tyV, False, _) ->
        when (tyV /= INT) $ Left [TypeMismatch nameTxt whereSp INT tyV]

    -- init / end / step は INT
    t0 <- inferType tenv env e0
    if t0 == INT then pure () else Left [OpTypeMismatch {op = "FOR-init", expected = INT, actual = t0}]
    t1 <- inferType tenv env e1
    if t1 == INT then pure () else Left [OpTypeMismatch {op = "FOR-end", expected = INT, actual = t1}]
    case mby of
      Nothing -> pure ()
      Just s -> do
        ts <- inferType tenv env s
        if ts == INT then pure () else Left [OpTypeMismatch {op = "FOR-by", expected = INT, actual = ts}]

    -- 本体でループ変数への代入を禁止
    mapM_ (denyAssignTo nameTxt) body

    -- 本体の通常の型チェック
    mapM_ (checkStmt tenv env) body
  where
    -- 左辺の“ベース変数”は既存の baseIdent を使用
    denyAssignTo nm = \case
      Assign lhs _ ->
        let b = baseIdent lhs
         in if locVal b == nm
              then Left [AssignToLoopVar nm (locSpan b)]
              else Right ()
      If _ th elsifs els ->
        mapM_ (denyAssignTo nm) th
          >> mapM_ (\(_, ss) -> mapM_ (denyAssignTo nm) ss) elsifs
          >> mapM_ (denyAssignTo nm) els
      While _ ss -> mapM_ (denyAssignTo nm) ss
      Repeat ss _ -> mapM_ (denyAssignTo nm) ss
      Case _ arms els ->
        mapM_ (\(CaseArm _ ss) -> mapM_ (denyAssignTo nm) ss) arms
          >> mapM_ (denyAssignTo nm) els
      For _ _ _ _ ss ->
        -- 同じ名前でも“別のループ”という概念は仕様上ないので禁止継続でOK
        mapM_ (denyAssignTo nm) ss
      Skip -> Right ()

-- where
-- もし範囲チェックをしたければ:
-- checkArm (CaseArm sels _) =
--   forM_ sels $ \case
--     CSRange a b | a > b ->
--       Left [InvalidCaseRange a b]   -- Diag を足す
--     _ -> Right ()

-- =========================================================
--  Constant *Designator* checker (CODESYS 風, CASE セレクタ用)
--    * OK: リテラル, 符号付数値リテラル, 列挙リテラル(Type.Ctor),
--          VAR CONSTANT の識別子, その配列要素(添字は整数リテラルのみ),
--          定数ベースのフィールド参照
--    * NG: すべての二項演算/比較/NOT、添字がリテラル以外等
-- =========================================================
checkConstDesignator :: TypeEnv -> Env -> Expr -> Either [SemantDiag] ()
checkConstDesignator tenv env = go
  where
    ok = Right ()

    go = \case
      -- リテラル群は OK
      EINT _ -> ok
      -- 単項マイナスは「中身が数値リテラル」のときのみ OK
      ENeg e | isIntLikeLiteral e -> ok
      -- 列挙リテラル: Type.Ctor の形（Type は Named で Enum に解決できること）
      EField (EVar ty) _ctor
        | isEnumType tenv ty -> ok
      -- VAR CONSTANT な識別子
      EVar i
        | isConstVar env i -> ok
      -- VAR CONSTANT な配列要素（添字は整数リテラルのみ許可）
      EIndex base idxs
        | Right () <- go base,
          all isIntLikeLiteral idxs ->
            ok
      -- 定数構造体のフィールド: ベースが OK なら OK
      EField base _fld ->
        go base
      -- それ以外は NG
      other ->
        Left [NonConstantExpr (spanOfExpr other)]

    -- ヘルパ
    isConstVar m i =
      case M.lookup (locVal i) m of
        Just (_, True, _) -> True
        _ -> False

    isEnumType te tyId =
      case resolveType te (Named tyId) of
        Right Enum {} -> True
        _ -> False

elaborateProgram :: Program -> Either [SemantDiag] Program
elaborateProgram = elaborateProgramWithTypes M.empty

-- TypeEnv あり版
elaborateProgramWithTypes :: TypeEnv -> Program -> Either [SemantDiag] Program
elaborateProgramWithTypes tenv (Program name (VarDecls vs) body) = do
  -- 変数環境: 型は解決済みにして保持
  env <- foldl step (Right M.empty) vs
  -- 既定初期値の付与・初期化式の型チェック
  vs' <- traverse (elabVar tenv env) vs
  -- 本体の文
  mapM_ (checkStmt tenv env) body
  pure (Program name (VarDecls vs') body)
  where
    step acc v = do
      m <- acc
      let n = locVal (varName v)
          sp = locSpan (varName v)
      case M.lookup n m of
        Just (_, _, prev) -> Left [DuplicateVar n prev sp]
        Nothing -> do
          -- ここでは resolveVarTypes 済みの varType を尊重：
          --   列挙なら Named のまま、その他は解決後が入っている
          Right (M.insert n (varType v, varConst v, sp) m)

typeEnvOf :: [TypeDecl] -> M.Map Text STType
typeEnvOf = M.fromList . map (\(TypeDecl n t) -> (locVal n, t))

resolveType :: TypeEnv -> STType -> Either [SemantDiag] STType
resolveType tenv = go Set.empty
  where
    go seen = \case
      -- ★ 別名の解決：Identifier から Text と Span を取り出す
      Named n ->
        let nameTxt = locVal n
            whereSp = locSpan n
         in if nameTxt `Set.member` seen
              then Left [TypeCycle {tName = nameTxt, tWhere = whereSp}]
              else case M.lookup nameTxt tenv of
                Nothing -> Left [UnknownType {tName = nameTxt, tWhere = whereSp}]
                Just t -> go (Set.insert nameTxt seen) t
      -- ★ 構造体の中も再帰的に解決
      Struct fs -> Struct <$> traverse (\(fld, t) -> (fld,) <$> go seen t) fs
      Array rs el -> Array rs <$> go seen el
      -- ★ 既知の基底型はそのまま
      t -> Right t

-- 変数宣言の型を TypeEnv で解決して書き戻す
resolveVarTypes :: TypeEnv -> VarDecls -> Either [SemantDiag] VarDecls
resolveVarTypes tenv (VarDecls vs) = do
  vs' <- traverse step vs
  pure (VarDecls vs')
  where
    step v = do
      let origTy = varType v
      tyResolved <- resolveType tenv origTy
      -- 列挙だけは「名前で区別」したいので Named を保持する（A-4/a）
      let keepNamedEnum =
            case (origTy, tyResolved) of
              (Named tyId, Enum _) -> Just tyId
              _ -> Nothing
      -- 既定初期値（列挙の先頭子）はここで Named を見ながら付与
      case (varInit v, keepNamedEnum, tyResolved) of
        (Nothing, Just tyId, Enum (ctor0 : _)) ->
          pure
            v
              { varType = Named tyId,
                varInit = Just (EField (EVar tyId) (fst ctor0))
              }
        _ -> pure v {varType = maybe tyResolved Named keepNamedEnum}

elaborateUnit :: Unit -> Either [SemantDiag] Unit
elaborateUnit (Unit tys progs) = do
  -- 1) TYPE ブロックの本体を解決（別名をたどる・循環検出など）
  let tenv0 = typeEnvOf tys
  tys' <- traverse (\(TypeDecl n t) -> TypeDecl n <$> resolveType tenv0 t) tys
  -- 2) 解決後の TYPE から、解決済みテーブルを作り直す
  let tenv = typeEnvOf tys'
  -- 3) 各 Program の VAR の型を解決して書き戻し
  progsResolved <-
    traverse
      ( \(Program n vds b) -> do
          vds' <- resolveVarTypes tenv vds
          pure (Program n vds' b)
      )
      progs
  -- 4) 解決済み TypeEnv を渡して elaboration（既定初期値付与・型チェック）
  progs' <- traverse (elaborateProgramWithTypes tenv) progsResolved
  pure (Unit tys' progs')

-- actTy が expTy と互換か？
nominalEq :: TypeEnv -> STType -> STType -> Bool
nominalEq tenv a b =
  case (a, b) of
    -- Named × Named：Enum は“名前で”、それ以外は展開して構造一致
    (Named n1, Named n2) ->
      case (resolveType tenv (Named n1), resolveType tenv (Named n2)) of
        (Right Enum {}, Right Enum {}) -> locVal n1 == locVal n2
        (Right t1, Right t2) -> t1 == t2
        _ -> False
    -- 片側だけ Named：その Named 側を解決してから再比較
    (Named n, t) ->
      case resolveType tenv (Named n) of
        Right t' -> nominalEq tenv t' t
        _ -> False
    (t, Named n) ->
      case resolveType tenv (Named n) of
        Right t' -> nominalEq tenv t t'
        _ -> False
    -- 文字列は“型種別一致”だけここで見る（長さは別で）
    (STRING _, STRING _) -> True
    (WSTRING _, WSTRING _) -> True
    -- それ以外：展開できるなら展開して構造一致。失敗したら素の比較。
    _ ->
      case (resolveType tenv a, resolveType tenv b) of
        (Right r1, Right r2) -> r1 == r2
        _ -> a == b

-- elabProgIn :: TypeEnv -> Program -> Either [SemantDiag] Program
-- elabProgIn tenv (Program name vds body) = do
--   vds' <- resolveVarTypes tenv vds
--   elaborateProgram (Program name vds' body)

-- ★ 型環境を受け取って Program を検査
-- elaborateProgramWith :: TypeEnv -> Program -> Either [SemantDiag] Program
-- elaborateProgramWith tenv (Program name (VarDecls vs) body) = do
--   -- 1) 変数の型を解決（Named/Struct内再帰を含む）
--   vsResolved <- traverse resolveVarType vs

--   -- 2) 解決済みの型で env を構築（重複チェック）
--   env <- foldl step (Right M.empty) vsResolved

--   -- 3) 初期化（既定値付与＋初期化式の型検査）
--   vs' <- traverse (elabVar env) vsResolved

--   -- 4) 本体の文検査
--   mapM_ (checkStmt env) body
--   pure (Program name (VarDecls vs') body)
--   where
--     resolveVarType v = do
--       t' <- resolveType tenv (varType v)
--       pure v {varType = t'}

--     step acc v = do
--       m <- acc
--       let n = locVal (varName v)
--           sp = locSpan (varName v)
--       case M.lookup n m of
--         Just (_, _, prev) -> Left [DuplicateVar n prev sp]
--         Nothing -> Right (M.insert n (varType v, varConst v, sp) m)
