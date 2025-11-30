{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ST.Semantic
  ( elaborateUnits,
    elaborateUnitsWithMode,
    nominalEq,
    FuncEnv,
    FuncSig (..),
    SigTy (..),
    TVarId (..),
    FuncKind (..),
    SemMode (..),
    ParamInfo (..),
    ParamDirKind (..),
    AllErrs,
    DuplicateVar (..),
    UnknownVar (..),
    AssignToConst (..),
    TypeMismatch (..),
    TypeMismatch' (..),
    MissingInitializer (..),
    UnknownType (..),
    TypeCycle (..),
    NotAStruct (..),
    UnknownStructMember (..),
    NotAnArray (..),
    BadIndexCount (..),
    IndexNotInt (..),
    AssignToLoopVar (..),
    UnknownEnumMember (..),
    NotAnEnum (..),
    InvalidCaseRange (..),
    OverlappingCase (..),
    NonConstantExpr (..),
    OutOfRange (..),
    TooManyAggElems (..),
    IndexOutOfBounds (..),
    WhyDidYouComeHere (..),
    UnknownFunction (..),
    BadArgCount (..),
    ArgTypeMismatch (..),
    UnknownArgName (..),
    DuplicateArgName (..),
    PositionalAfterNamed (..),
    DuplicateFunction (..),
    MissingReturn (..),
    UnsupportedGenericReturn (..),
    NoReturnValue (..),
    AssignToInput (..),
    InOutArgNotLValue (..),
    FBNotInstantiated (..),
    FBUsedAsExpr (..),
    TypeFBNameClash (..),
    UnknownFBMember (..),
    MissingFBOutputs (..),
    ArgDirectionMismatch (..),
    InternalError (..),
    AssignToFBField (..),
  )
where

import Control.Monad (foldM, foldM_, forM, forM_, guard, unless, when)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import ST.AST
import Text.Megaparsec.Pos
import Vary ((:|))
import Vary.VEither (VEither (VLeft, VRight))
import Vary.VEither qualified as VEither

type VarEnv = M.Map Text VarInfo

type TypeEnv = M.Map Text STType

type FuncEnv = M.Map Text FuncSig

type TVSubst = M.Map TVarId STType

type GlobalVarEnv = VarEnv

-- CodesysLike:
-- Functionで「到達可能な戻り値代入」が必須ではない（IR木生成側で初期値代入してカバー）
-- Strict:
-- Functionで「到達可能な戻り値代入」が必須
data SemMode = CodesysLike | Strict deriving (Eq, Show)

newtype TVarId = TV Int
  deriving (Eq, Ord, Show)

data Env = Env
  { envTypes :: TypeEnv,
    envGlobals :: GlobalVarEnv,
    envVars :: VarEnv,
    envFuncs :: FuncEnv,
    envMode :: SemMode
  }

-- | 引数の方向
data ParamDirKind
  = ParamIn
  | ParamOut
  | ParamInOut
  deriving (Eq, Show)

data ParamSig = ParamSig
  { psName :: Text, -- パラメータ名
    psType :: STType, -- 解決済みの型
    psDir :: ParamDirKind
  }
  deriving (Eq, Show)

data ParamInfo = ParamInfo
  { piType :: SigTy, -- 例: SigMono INT / SigGen ...
    piDir :: ParamDirKind, -- In / Out / InOut
    piPos :: Int -- 宣言順（0-based）
  }
  deriving (Eq, Show)

data FuncKind
  = FKFunction
  | FKFunctionBlock
  deriving (Eq, Show)

data FuncSig = FuncSig
  { fsName :: Identifier,
    fsKind :: FuncKind,
    fsArgs :: M.Map Text ParamInfo,
    fsRet :: Maybe SigTy
  }
  deriving (Eq, Show)

-- シグネチャ上の「パラメータ/戻り値の型」
data SigTy
  = SigMono STType -- 通常の具体型
  | SigGen GSTType TVarId -- ANY_* 制約 + 型変数
  deriving (Eq, Show)

type ExpectedSTType = STType

type ActualSTType = STType

type BaseSTType = STType

type VariableName = Text

type EnumName = Text

type TypeName = Text

type POUName = Text

type ArgName = Text

type MemberName = Text

type RangeFrom = Int

type RangeTo = Int

type ExpectedElems = Int

type ActualElems = Int

type ArgPos = Int

data DuplicateVar = DuplicateVar VariableName Span deriving (Eq, Show)

data UnknownVar = UnknownVar VariableName Span deriving (Eq, Show)

data AssignToConst = AssignToConst VariableName Span deriving (Eq, Show)

data TypeMismatch = TypeMismatch VariableName Span ExpectedSTType ActualSTType deriving (Eq, Show)

newtype TypeMismatch' = TypeMismatch' ActualSTType deriving (Eq, Show)

data MissingInitializer = MissingInitializer VariableName Span deriving (Eq, Show)

data UnknownType = UnknownType TypeName Span deriving (Eq, Show)

data TypeCycle = TypeCycle TypeName Span deriving (Eq, Show)

data NotAStruct = NotAStruct Span ActualSTType deriving (Eq, Show)

data UnknownStructMember = UnknownStructMember BaseSTType MemberName Span deriving (Eq, Show)

data NotAnArray = NotAnArray Span ActualSTType deriving (Eq, Show)

newtype BadIndexCount = BadIndexCount Span deriving (Eq, Show)

data IndexNotInt = IndexNotInt Span ActualSTType deriving (Eq, Show)

data AssignToLoopVar = AssignToLoopVar VariableName Span deriving (Eq, Show)

data UnknownEnumMember = UnknownEnumMember EnumName MemberName Span deriving (Eq, Show)

data NotAnEnum = NotAnEnum VariableName Span ActualSTType deriving (Eq, Show)

data InvalidCaseRange = InvalidCaseRange RangeFrom RangeTo deriving (Eq, Show)

data OverlappingCase = OverlappingCase deriving (Eq, Show)

newtype NonConstantExpr = NonConstantExpr Span deriving (Eq, Show)

data OutOfRange = OutOfRange VariableName STType Integer Span deriving (Eq, Show)

data TooManyAggElems = TooManyAggElems VariableName Span ExpectedElems ActualElems deriving (Eq, Show)

data IndexOutOfBounds = IndexOutOfBounds VariableName Span Int RangeFrom RangeTo deriving (Eq, Show)

data WhyDidYouComeHere = WhyDidYouComeHere deriving (Eq, Show)

data UnknownFunction = UnknownFunction POUName Span deriving (Eq, Show)

data BadArgCount = BadArgCount POUName Span ExpectedElems ActualElems deriving (Eq, Show)

data ArgTypeMismatch = ArgTypeMismatch POUName (Maybe ArgName) ArgPos ExpectedSTType ActualSTType Span deriving (Eq, Show)

data UnknownArgName = UnknownArgName POUName ArgName Span deriving (Eq, Show) -- 関数名, 未知の引数名

data DuplicateArgName = DuplicateArgName POUName ArgName Span deriving (Eq, Show) -- 重複引数名

data PositionalAfterNamed = PositionalAfterNamed Text Span deriving (Eq, Show) -- 名前付きの後に位置引数が現れた

newtype DuplicateFunction = DuplicateFunction Text deriving (Eq, Show)

newtype InternalError = InternalError Text deriving (Eq, Show)

newtype MissingReturn = MissingReturn POUName deriving (Eq, Show)

newtype UnsupportedGenericReturn = UnsupportedGenericReturn POUName deriving (Eq, Show)

data NoReturnValue = NoReturnValue FilePath POUName deriving (Eq, Show)

data AssignToInput = AssignToInput FilePath VariableName deriving (Eq, Show)

data InOutArgNotLValue = InOutArgNotLValue FilePath POUName VariableName deriving (Eq, Show)

data FBNotInstantiated = FBNotInstantiated POUName Span deriving (Eq, Show)

data FBUsedAsExpr = FBUsedAsExpr POUName Span deriving (Eq, Show)

newtype TypeFBNameClash = TypeFBNameClash Text deriving (Eq, Show)

data UnknownFBMember = UnknownFBMember VariableName MemberName Span deriving (Eq, Show)

data MissingFBOutputs = MissingFBOutputs POUName (Set VariableName) deriving (Eq, Show)

data ArgDirectionMismatch = ArgDirectionMismatch POUName ArgName ParamDirKind Text Span deriving (Eq, Show)

data AssignToFBField = AssignToFBField FilePath VariableName MemberName deriving (Eq, Show)

type AllErrs =
  [ AssignToConst,
    AssignToLoopVar,
    BadIndexCount,
    WhyDidYouComeHere,
    DuplicateVar,
    IndexOutOfBounds,
    InvalidCaseRange,
    MissingInitializer,
    NonConstantExpr,
    NotAnArray,
    NotAnEnum,
    NotAStruct,
    OutOfRange,
    OverlappingCase,
    TooManyAggElems,
    TypeCycle,
    TypeMismatch,
    TypeMismatch',
    UnknownEnumMember,
    UnknownStructMember,
    UnknownType,
    UnknownVar,
    UnknownFunction,
    BadArgCount,
    ArgTypeMismatch,
    UnknownArgName,
    DuplicateArgName,
    PositionalAfterNamed,
    DuplicateFunction,
    InternalError,
    MissingReturn,
    UnsupportedGenericReturn,
    NoReturnValue,
    AssignToInput,
    InOutArgNotLValue,
    FBNotInstantiated,
    FBUsedAsExpr,
    TypeFBNameClash,
    UnknownFBMember,
    MissingFBOutputs,
    ArgDirectionMismatch,
    AssignToFBField
  ]

elaborateUnits ::
  FuncEnv -> Units -> VEither AllErrs Units
elaborateUnits baseFenvc us = do
  elaborateUnitsWithMode CodesysLike baseFenvc us

elaborateUnitsWithMode ::
  SemMode -> FuncEnv -> Units -> VEither AllErrs Units
elaborateUnitsWithMode mode baseFenv us = do
  tenv <- typeEnvFromUnits us
  gvenv <- globalVarEnvFromUnits tenv us
  fenv <- funcEnvFromUnits tenv baseFenv us
  traverse (elaborateUnit mode tenv gvenv fenv) us

elaborateUnit ::
  SemMode -> TypeEnv -> GlobalVarEnv -> FuncEnv -> Unit -> VEither AllErrs Unit
elaborateUnit mode tenv gvenv fenv = \case
  UProgram p -> UProgram <$> elaborateProgram mode tenv gvenv fenv p
  UFunction f -> UFunction <$> elaborateFunction mode tenv gvenv fenv f
  UFunctionBlock fb -> UFunctionBlock <$> elaborateFunctionBlock mode tenv gvenv fenv fb
  -- TYPE はすでに replaceTypes 済みなのでそのまま
  UType tds -> pure (UType tds)
  UGlobalVars vds -> do
    -- 1) 型解決（Named MyInt などを展開）
    vs0 <- resolveVarTypes tenv vds

    -- 2) グローバル用 VarEnv（重複チェック）
    venv <- foldM insertVar M.empty vs0

    -- 3) 必要なら elabVar で初期化チェック / デフォルト付与
    let env =
          Env
            { envTypes = tenv,
              envGlobals = gvenv,
              envVars = venv,
              envFuncs = fenv,
              envMode = mode
            }

    vs1 <- traverse (elabVar env) vs0

    pure (UGlobalVars vs1)

elaborateFunctionBlock ::
  SemMode -> TypeEnv -> GlobalVarEnv -> FuncEnv -> FunctionBlock -> VEither AllErrs FunctionBlock
elaborateFunctionBlock mode tenv gvenv fenv (FunctionBlock fname vds body) = do
  -- 1) 型解決済み VarDecls へ
  vs0 <- resolveVarTypes tenv vds

  -- 2) VarEnv 構築（VAR_EXTERNAL を含む）
  venv <- buildPOUVarEnv gvenv vs0

  let env =
        Env
          { envTypes = tenv,
            envGlobals = gvenv,
            envVars = venv,
            envFuncs = fenv,
            envMode = mode
          }

  -- 3) 既定初期化の付与（CONST 未初期化などの検査もここ）
  vs1 <- traverse (elabVar env) vs0

  -- 4) Strict ルール:
  case mode of
    Strict -> do
      -- 4-a) VAR_INPUT への代入禁止
      let inputs = [locVal (varName v) | v <- vs1, varKind v == VKInput]
          wroteInput =
            [ x
            | x <- inputs,
              any (stmtAssignsTo x) body
            ]
      case wroteInput of
        (bad : _) -> VEither.fromLeft (AssignToInput (sourceNameOf fname) bad)
        [] -> pure ()

      -- 4-b) VAR_OUTPUT は全経路で代入必須（FUNCTION と同じノリ）
      let outs = Set.fromList [locVal (varName v) | v <- vs1, varKind v == VKOutput]
          missing = outs `Set.difference` assignedOnAllPaths outs body
      unless (Set.null missing) $
        VEither.fromLeft (MissingFBOutputs (locVal fname) missing)
    CodesysLike -> pure () -- INPUT 書き込みは許容、OUT 全経路チェックもしない
  mapM_ (checkStmt env) body
  pure $ FunctionBlock fname vs1 body
  where
    stmtAssignsTo :: Text -> Statement -> Bool
    stmtAssignsTo fname' = \case
      Assign lv _ ->
        lvalueWritesTo fname' lv
      If _ th elsifs els ->
        any (stmtAssignsTo fname') th
          || any (any (stmtAssignsTo fname') . snd) elsifs
          || any (stmtAssignsTo fname') els
      While _ body' ->
        any (stmtAssignsTo fname') body'
      Repeat body' _ ->
        any (stmtAssignsTo fname') body'
      Case _ arms els ->
        any (caseArmAssignsTo fname') arms
          || any (stmtAssignsTo fname') els
      For _ _ _ _ body' ->
        any (stmtAssignsTo fname') body'
      FBCall _ binds ->
        any
          ( \case
              CallOut _ lv -> lvalueWritesTo fname' lv
              _ -> False
          )
          binds
      Skip ->
        False

    lvalueWritesTo :: Text -> LValue -> Bool
    lvalueWritesTo fname' = \case
      -- 関数名そのものへの代入:
      LVar ident ->
        locVal ident == fname'
      -- 配列 / フィールド / ネストしていても、最終的な「ベース変数」が関数名なら return 扱い:
      LIndex lv _ ->
        lvalueWritesTo fname' lv
      LField lv _ ->
        lvalueWritesTo fname' lv

    caseArmAssignsTo :: Text -> CaseArm -> Bool
    caseArmAssignsTo fname' (CaseArm _ stmts) =
      any (stmtAssignsTo fname') stmts
    -- \| 与えられた必須集合 `req` について、「本体の全パスを通じて
    --   確実に代入された変数名だけ」を返す。
    assignedOnAllPaths :: Set.Set Text -> [Statement] -> Set.Set Text
    assignedOnAllPaths req = goBlock Set.empty
      where
        goBlock :: Set.Set Text -> [Statement] -> Set.Set Text
        goBlock = foldl' goStmt

        -- 1文ずつ「確定代入」情報を更新
        goStmt :: Set.Set Text -> Statement -> Set.Set Text
        goStmt acc = \case
          -- 代入：LHS の“ベース変数名”が req に含まれていれば確定扱いに
          Assign lv _ ->
            case baseVarName lv of
              Just n | n `Set.member` req -> Set.insert n acc
              _ -> acc
          -- FB 呼び出し文：
          --   ここで「この FB の VAR_OUTPUT が書かれた」とは見なさない
          --   （別の POU の out だから）。よって acc そのまま。
          FBCall {} ->
            acc
          -- IF：全分岐の共通部分のみが確定
          If {ifThen = th, ifElsifs = eifs, ifElse = els} ->
            let -- THEN ブロックで確定したもの
                base :: Set.Set Text
                base = goBlock acc th

                -- ELSIFそれぞれの確定集合とこれまでの共通集合を交差
                mergeElsIf :: Set.Set Text -> (Expr, [Statement]) -> Set.Set Text
                mergeElsIf z (_, bs) = Set.intersection z (goBlock acc bs)

                -- THEN と全 ELSIF の共通集合
                afterElsIfs :: Set.Set Text
                afterElsIfs = foldl' mergeElsIf base eifs

                -- ELSE が無ければ「何もしない」パス = acc
                afterElse :: Set.Set Text
                afterElse = if null els then acc else goBlock acc els
             in -- 全分岐（THEN/ELSIF/ELSE）の共通部分のみ残す
                Set.intersection afterElsIfs afterElse
          -- WHILE：0回実行もあり得る → 追加なし
          While _ _ -> acc
          -- REPEAT：1回は実行される → 本体を反映
          Repeat bs _ -> goBlock acc bs
          -- CASE：各腕＋ELSE（無いときは“何もしない”パス）との共通部分
          Case _ arms els ->
            let armOuts = map (\(CaseArm _ bs) -> goBlock acc bs) arms
                elseOut = if null els then acc else goBlock acc els
             in foldr Set.intersection elseOut armOuts
          -- FOR：0回実行があり得る → 追加なし
          For {} -> acc
          -- Skip：何もしない
          Skip -> acc

-- 旧 elaborateUnit 本体を Program 単位に切り出したもの
elaborateProgram ::
  SemMode -> TypeEnv -> GlobalVarEnv -> FuncEnv -> Program -> VEither AllErrs Program
elaborateProgram mode tenv gvenv fenv (Program n vds body) = do
  -- 型解決済み VarDecls
  vs <- resolveVarTypes tenv vds
  -- ★ VarEnv 構築（VAR_EXTERNAL は GlobalVarEnv と突き合わせ）
  venv <- buildPOUVarEnv gvenv vs

  let env =
        Env
          { envTypes = tenv,
            envGlobals = gvenv,
            envVars = venv,
            envFuncs = fenv,
            envMode = mode
          }

  -- 初期化子チェック＆デフォルト付与
  vs'' <- traverse (elabVar env) vs

  -- ステートメント検査
  mapM_ (checkStmt env) body
  pure (Program n vs'' body)

insertVar ::
  (DuplicateVar :| e) =>
  VarEnv -> Variable -> VEither e VarEnv
insertVar m v = do
  let vname = locVal (varName v)
      sp = locSpan (varName v)
  case M.lookup vname m of
    Just _ ->
      VEither.fromLeft $ DuplicateVar vname sp
    Nothing ->
      pure $
        M.insert
          vname
          VarInfo
            { viType = varType v,
              viSpan = sp,
              viKind = varKind v,
              viInit = varInit v,
              viConst = varConst v,
              viRetain = varRetain v
            }
          m

elaborateFunction ::
  ( UnsupportedGenericReturn :| e,
    DuplicateVar :| e,
    OutOfRange :| e,
    TypeMismatch :| e,
    TypeMismatch' :| e,
    InvalidCaseRange :| e,
    AssignToLoopVar :| e,
    OverlappingCase :| e,
    UnknownVar :| e,
    AssignToConst :| e,
    UnknownStructMember :| e,
    NotAStruct :| e,
    BadIndexCount :| e,
    WhyDidYouComeHere :| e,
    UnknownEnumMember :| e,
    NotAnEnum :| e,
    NotAnArray :| e,
    IndexOutOfBounds :| e,
    NonConstantExpr :| e,
    UnknownFunction :| e,
    ArgTypeMismatch :| e,
    UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e,
    MissingReturn :| e,
    TypeCycle :| e,
    UnknownType :| e,
    NoReturnValue :| e,
    InOutArgNotLValue :| e,
    AssignToInput :| e,
    FBNotInstantiated :| e,
    FBUsedAsExpr :| e,
    InternalError :| e,
    UnknownFBMember :| e,
    ArgDirectionMismatch :| e,
    AssignToFBField :| e
  ) =>
  SemMode -> TypeEnv -> GlobalVarEnv -> FuncEnv -> Function -> VEither e Function
elaborateFunction mode tenv gvenv fenv fn = do
  let fname = locVal $ funcName fn
      fvds = funcVars fn
      fbody = fBody fn

  -- 自分の宣言を解決するだけ
  retTyST <- resolveType tenv (funcRetType fn)
  -- 変数宣言の型解決
  vs <- resolveVarTypes tenv fvds

  venv0 <- buildPOUVarEnv gvenv vs

  -- 暗黙の戻り値変数（関数名と同名）を追加
  -- 既にユーザーが同名を宣言してたらエラーにしてよい（仕様次第）
  let retVarInfo =
        VarInfo
          { viType = retTyST,
            viSpan = noSpan,
            viKind = VKLocal,
            viConst = False,
            viRetain = Nothing,
            viInit = Nothing
          }
  venv1 <-
    case M.lookup fname venv0 of
      Just _ -> VEither.fromLeft (DuplicateVar fname (viSpan retVarInfo))
      Nothing -> pure (M.insert fname retVarInfo venv0)

  -- この FUNCTION 用 Env
  let env =
        Env
          { envTypes = tenv,
            envGlobals = gvenv,
            envVars = venv1,
            envFuncs = fenv,
            envMode = mode
          }

  -- 本体のステートメントを検査
  mapM_ (checkStmt env) fbody

  case envMode env of
    Strict -> do
      -- 要求集合 = {関数名} ∪ {VAR_OUTPUT の変数名たち}
      let retName = fname
          outNames =
            Set.fromList
              [ locVal (varName v)
              | let vs' = funcVars fn,
                v <- vs',
                varKind v == VKOutput
              ]
          reqNames = Set.insert retName outNames
      unless (mustAssignAll reqNames fbody) $
        VEither.fromLeft (MissingReturn fname)
    CodesysLike -> pure ()

  -- resolve 済み VarDecls を差し込んだ Function を返す
  pure fn {funcVars = vs}

-- 複数 Unit の全 TYPE を一括解決して最終 TypeEnv を返す
typeEnvFromUnits ::
  ( TypeCycle :| e,
    UnknownType :| e,
    TypeFBNameClash :| e
  ) =>
  Units -> VEither e TypeEnv
typeEnvFromUnits us = do
  -- A) 衝突検出：DUT 名と FB 名の交差
  let typeNames =
        Set.fromList
          [locVal (typeName td) | UType tds <- us, td <- tds]
      fbNames =
        Set.fromList
          [locVal (fbName fb) | UFunctionBlock fb <- us]
      clashes = Set.toList (Set.intersection typeNames fbNames)
  case clashes of
    n : _ -> VEither.fromLeft $ TypeFBNameClash n
    [] -> pure ()

  let allTds = [td | UType tds <- us, td <- tds]
  let tenv0 = typeEnvOf allTds -- 全名をまず登録
  tds' <- traverse (\(TypeDecl n t) -> TypeDecl n <$> resolveType tenv0 t) allTds
  let tenvTypes = typeEnvOf tds'
      -- 2) FB 名も「型名」として登録（FBMeta）
      --    既に同名の型がある場合は既存を優先（エラー化したければオプションへ）
      tenvWithFBs =
        foldl'
          ( \m u -> case u of
              UFunctionBlock fb ->
                let n = locVal (fbName fb)
                 in M.insertWith (\_ old -> old) n (FBMeta n) m
              _ -> m
          )
          tenvTypes
          us

  pure tenvWithFBs

funcEnvFromUnit ::
  ( DuplicateFunction :| e,
    TypeCycle :| e,
    UnknownType :| e
  ) =>
  TypeEnv -> FuncEnv -> Unit -> VEither e FuncEnv
funcEnvFromUnit tenv fenv = \case
  UFunction f -> addFunction tenv fenv f
  UFunctionBlock fb -> addFunctionBlock tenv fenv fb
  _ -> pure fenv -- UType / UProgram は環境変更なし

funcEnvFromUnits ::
  ( DuplicateFunction :| e,
    TypeCycle :| e,
    UnknownType :| e
  ) =>
  TypeEnv -> FuncEnv -> Units -> VEither e FuncEnv
funcEnvFromUnits tenv = foldM (funcEnvFromUnit tenv)

globalVarEnvFromUnits ::
  ( TypeCycle :| e,
    UnknownType :| e,
    DuplicateVar :| e
  ) =>
  TypeEnv -> Units -> VEither e GlobalVarEnv
globalVarEnvFromUnits tenv us = do
  -- 1) 全 UGlobalVars だけ抜き出して [Variable] を平坦化
  let allGlobals :: [Variable]
      allGlobals =
        [ v
        | UGlobalVars vs <- us,
          v <- vs
        ]

  -- 2) 型解決
  vsResolved <- resolveVarTypes tenv allGlobals

  -- 3) VarEnv 構築（重複チェック込み）
  foldM insertVar M.empty vsResolved

insertFuncSig ::
  (DuplicateFunction :| e) =>
  FuncSig -> FuncEnv -> VEither e FuncEnv
insertFuncSig sig env =
  let name = locVal $ fsName sig
   in case M.lookup name env of
        Just _ -> VEither.fromLeft (DuplicateFunction name)
        Nothing -> pure (M.insert name sig env)

addFunction ::
  forall e.
  ( TypeCycle :| e,
    UnknownType :| e,
    DuplicateFunction :| e
  ) =>
  TypeEnv -> FuncEnv -> Function -> VEither e FuncEnv
addFunction tenv env f = do
  let name = funcName f

  -- 戻り値型を解決
  retTy' <- resolveType tenv (funcRetType f)

  -- パラメータ一覧を構築
  params <- collectPOUParams tenv (funcVars f)

  let fsArgsMap :: M.Map Text ParamInfo
      fsArgsMap =
        M.fromList
          [ ( psName p,
              ParamInfo
                { piType = SigMono (psType p),
                  piDir = psDir p,
                  piPos = ix
                }
            )
          | (ix, p) <- zip [0 ..] params
          ]

      sig =
        FuncSig
          { fsName = name,
            fsKind = FKFunction,
            fsArgs = fsArgsMap,
            fsRet = Just $ SigMono retTy'
          }
  insertFuncSig sig env

addFunctionBlock ::
  forall e.
  ( TypeCycle :| e,
    UnknownType :| e,
    DuplicateFunction :| e
  ) =>
  TypeEnv -> FuncEnv -> FunctionBlock -> VEither e FuncEnv
addFunctionBlock tenv env (FunctionBlock {fbName, fbVars}) = do
  params <- collectPOUParams tenv fbVars
  let fsArgsMap :: M.Map Text ParamInfo
      fsArgsMap =
        M.fromList
          [ ( psName p,
              ParamInfo
                { piType = SigMono (psType p),
                  piDir = psDir p,
                  piPos = ix
                }
            )
          | (ix, p) <- zip [0 ..] params
          ]
      sig =
        FuncSig
          { fsName = fbName,
            fsKind = FKFunctionBlock,
            fsArgs = fsArgsMap,
            fsRet = Nothing
          }
  insertFuncSig sig env

collectPOUParams ::
  forall e.
  ( TypeCycle :| e,
    UnknownType :| e
  ) =>
  TypeEnv -> [Variable] -> VEither e [ParamSig]
collectPOUParams tenv vs =
  fmap catMaybes . forM vs $ \v ->
    case varKind v of
      VKInput -> Just <$> mk ParamIn v
      VKInOut -> Just <$> mk ParamInOut v
      VKOutput -> Just <$> mk ParamOut v
      _ -> pure Nothing
  where
    mk :: ParamDirKind -> Variable -> VEither e ParamSig
    mk dir v = do
      ty' <- resolveType tenv (varType v)
      pure
        ParamSig
          { psName = locVal (varName v),
            psType = ty',
            psDir = dir
          }

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

callArgExpr :: CallArg -> Expr
callArgExpr = \case
  CallArgPos e -> e
  CallArgNamed _name e -> e

callArgName :: CallArg -> Maybe Text
callArgName = \case
  CallArgPos _ -> Nothing
  CallArgNamed i _ -> Just $ locVal i

callArgSpan :: CallArg -> Span
callArgSpan = \case
  CallArgPos e -> spanOfExpr e
  CallArgNamed i _ -> locSpan i

typeEnvOf :: [TypeDecl] -> M.Map Text STType
typeEnvOf = M.fromList . map (\(TypeDecl n t) -> (locVal n, t))

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
constIntValue :: Env -> Expr -> Maybe Int
constIntValue env e = case evalConstExpr env e of
  Just (CVInt n) -> Just n
  _ -> Nothing

-- LValue の「根」変数名（診断メッセージ用）
lvalueRootName :: LValue -> Text
lvalueRootName = \case
  LVar i -> locVal i
  LField lv _ -> lvalueRootName lv
  LIndex lv _ -> lvalueRootName lv

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
      TIME -> Just (ETIME 0)
      LTIME -> Just (ELTIME 0)
      TOD -> Just (ETOD (TimeOfDay 0 0 0 0))
      LTOD -> Just (ELTOD (TimeOfDay 0 0 0 0))
      DATE -> Just (EDATE (Date 1970 1 1))
      LDATE -> Just (ELDATE (Date 1970 1 1))
      DT -> Just (EDT (DateTime (Date 1970 1 1) (TimeOfDay 0 0 0 0)))
      LDT -> Just (ELDT (DateTime (Date 1970 1 1) (TimeOfDay 0 0 0 0)))
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
      Named _ ->
        VEither.veither @'[TypeCycle, UnknownType]
          (const Nothing)
          go
          (resolveType tenv ty)
      _ -> Nothing

-- 暗黙昇格okかどうか
canPromote :: STType -> STType -> Bool
canPromote from to =
  case (from, to) of
    (INT, REAL) -> True
    (INT, LREAL) -> True
    (REAL, LREAL) -> True
    (TIME, LTIME) -> True
    (TOD, LTOD) -> True
    (DATE, LDATE) -> True
    (DT, LDT) -> True
    _ -> False

assignCoerce :: TypeEnv -> STType -> STType -> Bool
assignCoerce tenv fromTy toTy =
  nominalEq tenv fromTy toTy || canPromote fromTy toTy

arrayCapacity :: [ArrRange] -> Int
arrayCapacity = product . fmap (\(ArrRange lo hi) -> hi - lo + 1)

inferType ::
  forall e.
  ( WhyDidYouComeHere :| e,
    IndexOutOfBounds :| e,
    NotAnArray :| e,
    NotAnEnum :| e,
    NotAStruct :| e,
    TypeCycle :| e,
    UnknownEnumMember :| e,
    UnknownStructMember :| e,
    UnknownType :| e,
    UnknownVar :| e,
    TypeMismatch' :| e,
    ArgTypeMismatch :| e,
    UnknownFunction :| e,
    UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e,
    UnsupportedGenericReturn :| e,
    NoReturnValue :| e,
    InOutArgNotLValue :| e,
    FBNotInstantiated :| e,
    FBUsedAsExpr :| e,
    InternalError :| e,
    UnknownFBMember :| e
  ) =>
  Env ->
  Expr ->
  VEither e STType
inferType env = \case
  EINT _ -> VRight INT
  EBOOL _ -> VRight BOOL
  EREAL _ -> VRight REAL
  ELREAL _ -> VRight LREAL
  ESTRING _ -> VRight $ STRING Nothing
  EWSTRING _ -> VRight $ WSTRING Nothing
  ECHAR _ -> VRight CHAR
  EWCHAR _ -> VRight WCHAR
  ENeg expr -> do
    t <- inferType env expr
    case t of
      INT -> VRight INT
      REAL -> VRight REAL
      LREAL -> VRight LREAL
      _ -> VEither.fromLeft $ TypeMismatch' t
  -- 数値二項演算（INT/REAL 混在は REAL に昇格）
  EAdd a b -> arith2 a b
  ESub a b -> arith2 a b
  EMul a b -> arith2 a b
  EDiv a b -> arith2 a b
  EMod a b -> do
    ta <- inferType env a
    tb <- inferType env b
    if ta == INT && tb == INT
      then pure INT
      else VEither.fromLeft $ TypeMismatch' $ if ta /= INT then ta else tb
  -- 比較（数値同士は OK、混在も可）。結果は BOOL
  EEq a b -> comp2 "=" a b
  ENe a b -> comp2 "<>" a b
  ELt a b -> comp2 "<" a b
  ELe a b -> comp2 "<=" a b
  EGt a b -> comp2 ">" a b
  EGe a b -> comp2 ">=" a b
  ENot e -> do
    t0 <- inferType env e
    let t = resolvePrim (envTypes env) t0
    case t of
      BOOL -> VRight BOOL
      BYTE -> VRight BYTE
      WORD -> VRight WORD
      DWORD -> VRight DWORD
      LWORD -> VRight LWORD
      _ -> VEither.fromLeft $ TypeMismatch' t0
  EAnd a b -> bitwise2 a b
  EXor a b -> bitwise2 a b
  EOr a b -> bitwise2 a b
  EVar x -> case M.lookup (locVal x) (envVars env) of
    Nothing -> VEither.fromLeft $ UnknownVar (locVal x) (locSpan x)
    Just info -> VRight $ viType info
  -- a.b の型判定：まず構造体フィールドを試し、それ以外は Enum 修飾の可能性を検討
  -- フィールド参照（構造体 or 列挙リテラル）
  EField e fld -> case e of
    -- 左が識別子のとき：まず“変数か？”を優先チェック
    EVar vId ->
      case M.lookup (locVal vId) (envVars env) of
        -- 変数が見つかる → 構造体アクセスとして扱う
        Just _ -> inferMemberAccess fld e
        Nothing ->
          -- 変数ではなかったので“型名か？”を判定
          case resolveType @e (envTypes env) (Named vId) of
            VRight (Enum ctors) ->
              if any ((== locVal fld) . locVal . fst) ctors
                then VRight (Named vId) -- 列挙は Named を返す（A-4/a）
                else VEither.fromLeft $ UnknownEnumMember (locVal vId) (locVal fld) (locSpan vId)
            VRight other -> VEither.fromLeft $ NotAnEnum (locVal vId) (locSpan vId) other
            -- 型にも無い → ここは“値としても無い”ので UnknownVar にする
            _ -> VEither.fromLeft $ UnknownVar (locVal vId) (locSpan vId)
    -- それ以外は通常の構造体アクセス解決
    _ -> inferMemberAccess fld e
  EIndex arr idxs -> do
    tBase <- inferType env arr
    tRes <- resolveType (envTypes env) tBase

    case tRes of
      Array ranges elTy -> do
        -- 次元数チェックは必要ならここで
        forM_ (zip idxs ranges) $ \(ie, ArrRange lo hi) -> do
          ti <- inferType env ie
          -- 添字は整数系のみを許可（必要に応じて緩めてもOK）
          unless (ti == INT || isIntOrBits ti) $
            VEither.fromLeft $
              TypeMismatch' ti
          -- 定数なら静的境界チェック
          case constIntValue env ie of
            Just n ->
              unless (lo <= n && n <= hi) $
                VEither.fromLeft $
                  IndexOutOfBounds
                    (lvalueRootName (LIndex (LVar (toIdent "<expr>")) [ie]))
                    (spanOfExpr ie)
                    n
                    lo
                    hi
            Nothing -> pure ()

        pure elTy
      _ -> VEither.fromLeft $ NotAnArray (spanOfExpr arr) tRes
  EEnum ty ctor -> do
    -- TypeName を解決して列挙型かチェック
    tResolved <- resolveType (envTypes env) (Named ty)
    case tResolved of
      Enum ctors ->
        if any (\i -> locVal (fst i) == locVal ctor) ctors
          then VRight tResolved
          else
            VEither.fromLeft $ UnknownEnumMember (locVal ty) (locVal ctor) (locSpan ctor)
      other -> VEither.fromLeft $ NotAnEnum (locVal ty) (locSpan ty) other
  ETIME _ -> VRight TIME
  ELTIME _ -> VRight LTIME
  ETOD _ -> VRight TOD
  ELTOD _ -> VRight LTOD
  EDATE _ -> VRight DATE
  ELDATE _ -> VRight LDATE
  EDT _ -> VRight DT
  ELDT _ -> VRight LDT
  EArrayAgg _ ->
    VEither.fromLeft $
      InternalError
        "Array aggregate literal requires an expected type \
        \(LHS array type or parameter type). \
        \Use it only where a concrete array element/range is known."
  EStructAgg _ ->
    VEither.fromLeft $
      InternalError
        "Struct aggregate literal requires an expected struct type \
        \(LHS struct type or parameter type). \
        \Use it only where the field layout is known."
  ECall fn args -> do
    let fname = locVal fn
        fspan = locSpan fn
        tenv = envTypes env

        isFunctionBlockType :: STType -> Bool
        isFunctionBlockType (FBMeta _) = True
        isFunctionBlockType _ = False

    sig@(FuncSig _ _ fsArgs fsRet) <-
      case M.lookup fname (envFuncs env) of
        Nothing ->
          -- VarEnv に fb インスタンス（型が FunctionBlock）の識別子がある？
          case M.lookup fname (envVars env) of
            Just VarInfo {viType = t}
              | isFunctionBlockType t ->
                  VEither.fromLeft $ FBUsedAsExpr fname fspan
            _ ->
              VEither.fromLeft $ UnknownFunction fname fspan
        Just sig'
          | fsKind sig' == FKFunctionBlock ->
              -- POU名を式として使っている（インスタンス化してない）
              VEither.fromLeft $ FBNotInstantiated fname fspan
        Just sig' -> pure sig'

    -- 名前付き/位置引数を正規化して、param順のリストに
    orderedArgs <- assignArgs fname sig args
    -- orderedArgs :: [CallArg], length == paramCount

    -- 型変数束縛を集めながら各引数チェック
    subst <-
      foldM
        ( \subst (i, pname, carg) -> do
            let argE = callArgExpr carg
                mbName = callArgName carg
                spArg = spanOfExpr argE
                ParamInfo {piType = pSig, piDir = pDir} = fsArgs M.! pname

            -- ★ 追加: IN_OUT は実引数が LValue 必須（両モード共通）
            when (pDir == ParamInOut) $
              case exprToLValue argE of
                Nothing -> VEither.fromLeft (InOutArgNotLValue (sourceNameOf fn) fname pname)
                Just _ -> pure ()

            argTy <- inferType env argE

            case pSig of
              -- 具体型パラメータ: これまで通り assignCoerce
              SigMono expectedTy -> do
                unless (assignCoerce tenv argTy expectedTy) $
                  VEither.fromLeft $
                    ArgTypeMismatch fname mbName i expectedTy argTy spArg
                pure subst

              -- ANY_* パラメータ
              SigGen gst tv -> do
                ok <- gstMember tenv argTy gst
                unless ok $
                  VEither.fromLeft $
                    ArgTypeMismatch fname mbName i argTy argTy spArg

                case M.lookup tv subst of
                  -- まだ束縛されてない: この引数型で束縛
                  Nothing ->
                    pure (M.insert tv argTy subst)
                  -- 既に束縛あり: 名目同値チェック
                  Just boundTy ->
                    if nominalEq tenv boundTy argTy
                      then pure subst
                      else
                        VEither.fromLeft $
                          ArgTypeMismatch fname mbName i boundTy argTy spArg
        )
        M.empty
        (zip3 [1 ..] (inputParamOrder sig) orderedArgs)

    -- 束縛済み型変数に従って「戻り値」を具体型に落とす（未束縛ならエラー）
    case fsRet of
      Nothing -> VEither.fromLeft $ NoReturnValue (sourceNameOf fn) fname
      Just rSig -> instantiateSigTyRet tenv subst fname rSig
  where
    eqLike ta tb =
      if nominalEq (envTypes env) ta tb
        then VRight BOOL
        else VEither.fromLeft $ TypeMismatch' tb

    ordLike ta tb
      -- 同型で数値なら OK
      | ta == tb,
        ta `elem` [INT, REAL, LREAL] =
          VRight BOOL
      | both isChar ta tb = VRight BOOL
      | both isWChar ta tb = VRight BOOL
      | both isString ta tb = VRight BOOL
      | both isWString ta tb = VRight BOOL
      | both isTime ta tb = VRight BOOL
      | both isLTime ta tb = VRight BOOL
      | both isTOD ta tb = VRight BOOL
      | both isLTOD ta tb = VRight BOOL
      | both isDate ta tb = VRight BOOL
      | both isLDate ta tb = VRight BOOL
      | both isDT ta tb = VRight BOOL
      | both isLDT ta tb = VRight BOOL
      -- INT と REAL の混在は OK
      | (ta, tb) == (INT, REAL) || (ta, tb) == (REAL, INT) = VRight BOOL
      -- LREAL は同型のみ可：片側だけ LREAL は NG
      | ta == LREAL && tb /= LREAL =
          VEither.fromLeft $ TypeMismatch' tb
      | tb == LREAL && ta /= LREAL =
          VEither.fromLeft $ TypeMismatch' ta
      -- その他（BOOL 等を含む）は NG。算術と同様に「INT を期待」として怒る
      | otherwise =
          VEither.fromLeft $ TypeMismatch' tb
      where
        isChar = \case CHAR -> True; _ -> False
        isWChar = \case WCHAR -> True; _ -> False
        isString = \case STRING _ -> True; _ -> False
        isWString = \case WSTRING _ -> True; _ -> False
        isTime = \case TIME -> True; _ -> False
        isLTime = \case LTIME -> True; _ -> False
        isTOD = \case TOD -> True; _ -> False
        isLTOD = \case LTOD -> True; _ -> False
        isDate = \case DATE -> True; _ -> False
        isLDate = \case LDATE -> True; _ -> False
        isDT = \case DT -> True; _ -> False
        isLDT = \case LDT -> True; _ -> False
        both f a b = f a == f b

    -- 数値の2項演算
    arith2 a b = do
      ta <- inferType env a
      tb <- inferType env b
      case (ta, tb) of
        (INT, INT) -> VRight INT
        (REAL, REAL) -> VRight REAL
        (LREAL, LREAL) -> VRight LREAL
        (INT, REAL) -> VRight REAL
        (REAL, INT) -> VRight REAL
        -- LREAL は“同型のみ”を許可（INT/LREAL や REAL/LREAL は NG）
        (LREAL, x) | x /= LREAL -> VEither.fromLeft $ TypeMismatch' x
        (x, LREAL) | x /= LREAL -> VEither.fromLeft $ TypeMismatch' x
        -- それ以外（BOOL 等）は「算術は INT を期待」で怒る（テスト準拠）
        _ ->
          let bad = (if (ta == BOOL) || (tb == BOOL) then BOOL else tb)
           in VEither.fromLeft $ TypeMismatch' bad
    -- 比較: 数値同士は OK（混在可）→ BOOL
    comp2 :: String -> Expr -> Expr -> VEither e STType
    comp2 o a b = do
      ta <- inferType env a
      tb <- inferType env b
      case o of
        "=" -> eqLike ta tb
        "<>" -> eqLike ta tb
        _ -> ordLike ta tb

    inferMemberAccess :: Identifier -> Expr -> VEither e STType
    inferMemberAccess fld base = do
      tr <- inferType env base
      let tenv = envTypes env
      case resolveType @e tenv tr of
        VRight (Struct fs) ->
          case lookup (locVal fld) [(locVal n, t) | (n, t) <- fs] of
            Just t -> VRight t
            Nothing -> VEither.fromLeft $ UnknownStructMember tr (locVal fld) (locSpan fld)
        VRight (FBMeta fbName) -> do
          case M.lookup fbName (envFuncs env) of
            Just FuncSig {fsKind = FKFunctionBlock, fsArgs = argMap} -> do
              let visible = M.filter (\pinfo -> piDir pinfo /= ParamInOut) argMap
              case M.lookup (locVal fld) visible of
                Just ParamInfo {piType = sigTy} ->
                  maybe
                    (VEither.fromLeft $ InternalError "FB member is generic")
                    VRight
                    (sigTyToSTType sigTy)
                Nothing -> VEither.fromLeft $ UnknownFBMember fbName (locVal fld) (locSpan fld)
            _ -> VEither.fromLeft $ InternalError ("FB meta not found: " <> fbName)
        _ -> VEither.fromLeft $ NotAStruct (locSpan fld) tr

    resolvePrim :: TypeEnv -> STType -> STType
    resolvePrim tenv' t = case resolveType @e tenv' t of
      VRight u -> u
      VLeft _ -> t

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
    bitwise2 a b = do
      ta0 <- inferType env a
      tb0 <- inferType env b
      let ta = resolvePrim (envTypes env) ta0
          tb = resolvePrim (envTypes env) tb0
      case (ta, tb) of
        (BOOL, BOOL) -> VRight BOOL
        _ | isBitString ta && ta == tb -> VRight ta
        _ -> VEither.fromLeft $ TypeMismatch' tb0

assignArgs ::
  forall e.
  ( UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e
  ) =>
  Text -> -- 関数名（診断用）
  FuncSig ->
  [CallArg] -> -- 実引数
  VEither e [CallArg]
assignArgs fname sig args = go 0 False M.empty args
  where
    -- 受け付け対象（In / InOut）だけを宣言順に
    order :: [Text]
    order =
      map fst
        . sortOn (piPos . snd)
        . filter (\(_, p) -> piDir p /= ParamOut)
        . M.toList
        $ fsArgs sig

    nameToIx :: M.Map Text Int
    nameToIx = M.fromList (zip order [0 ..])

    expected :: Int
    expected = length order

    actualCount :: Int
    actualCount = length args

    go :: Int -> Bool -> M.Map Int CallArg -> [CallArg] -> VEither e [CallArg]
    go nextPos seenNamed acc = \case
      -- 引数をすべて消費したタイミングで「足りてるか？」判定
      [] ->
        if M.size acc == expected
          then VRight $ map snd $ M.toList acc
          else
            VEither.fromLeft $
              BadArgCount fname (callArgSpan $ acc M.! M.size acc) expected actualCount
      (c : cs) ->
        case c of
          -- 位置引数
          CallArgPos e
            | seenNamed ->
                VEither.fromLeft $
                  PositionalAfterNamed fname (spanOfExpr e)
            | nextPos >= expected ->
                VEither.fromLeft $
                  BadArgCount fname (callArgSpan c) expected actualCount
            | otherwise ->
                go (nextPos + 1) seenNamed (M.insert nextPos c acc) cs
          -- 名前付き引数
          CallArgNamed ident _ -> do
            let nmTxt = locVal ident
            ix <-
              case M.lookup nmTxt nameToIx of
                Nothing ->
                  VEither.fromLeft $
                    UnknownArgName fname nmTxt (locSpan ident)
                Just i -> VRight i

            when (M.member ix acc) $
              VEither.fromLeft $
                DuplicateArgName fname nmTxt (locSpan ident)

            go nextPos True (M.insert ix c acc) cs

-- 左辺の“ベース変数”を取り出す（const/存在チェック用）
baseIdent :: LValue -> Identifier
baseIdent = \case
  LVar i -> i
  LField l _ -> baseIdent l
  LIndex l _ -> baseIdent l

-- 環境中の型から、LValue に沿って最終型を掘る
lvalueType ::
  forall e.
  ( TypeMismatch' :| e,
    UnknownVar :| e,
    AssignToConst :| e,
    UnknownStructMember :| e,
    NotAStruct :| e,
    BadIndexCount :| e,
    WhyDidYouComeHere :| e,
    TypeCycle :| e,
    UnknownType :| e,
    UnknownEnumMember :| e,
    NotAnEnum :| e,
    NotAnArray :| e,
    IndexOutOfBounds :| e,
    ArgTypeMismatch :| e,
    UnknownFunction :| e,
    UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e,
    UnsupportedGenericReturn :| e,
    NoReturnValue :| e,
    InOutArgNotLValue :| e,
    FBNotInstantiated :| e,
    FBUsedAsExpr :| e,
    InternalError :| e,
    UnknownFBMember :| e
  ) =>
  Env ->
  STType ->
  LValue ->
  VEither e STType
lvalueType env ty = \case
  -- 変数：必ず環境に存在すること。無ければ UnknownVar
  LVar i ->
    case M.lookup (locVal i) (envVars env) of
      Nothing ->
        VEither.fromLeft $ UnknownVar (locVal i) (locSpan i)
      Just info ->
        if viConst info
          then VEither.fromLeft $ AssignToConst (locVal i) (locSpan i)
          else VRight $ viType info
  LField l fld -> do
    tr <- lvalueType env ty l
    case tr of
      Struct fs ->
        case lookupByText fld fs of
          Just t -> VRight t
          Nothing -> VEither.fromLeft $ UnknownStructMember tr (locVal fld) (locSpan fld)
      _ -> VEither.fromLeft $ NotAStruct (locSpan fld) tr
  LIndex base idxs@(idx : _) -> do
    tBase <- lvalueType env INT base -- 第3引数は未使用なら何でもOK
    tRes <- resolveType @e (envTypes env) tBase
    case tRes of
      Array ranges elTy -> do
        when (length idxs /= length ranges) $
          VEither.fromLeft $
            BadIndexCount (spanOfExpr idx)
        forM_ (zip idxs ranges) $ \(ie, ArrRange lo hi) -> do
          ti <- inferType env ie
          unless (ti == INT || isIntOrBits ti) $
            VEither.fromLeft $
              TypeMismatch' ti
          case constIntValue env ie of
            Just n ->
              unless (lo <= n && n <= hi) $
                VEither.fromLeft $
                  IndexOutOfBounds
                    (lvalueRootName (LIndex (LVar (toIdent "<expr>")) [ie]))
                    (spanOfExpr ie)
                    n
                    lo
                    hi
            Nothing -> pure ()
        pure elTy
      _ -> VEither.fromLeft $ TypeMismatch' tRes
  LIndex _ _ -> VEither.fromLeft WhyDidYouComeHere

-- 1変数の意味解析：型チェック & 既定初期化付与
elabVar ::
  ( UnknownStructMember :| e,
    NotAStruct :| e,
    OutOfRange :| e,
    TypeMismatch :| e,
    TooManyAggElems :| e,
    WhyDidYouComeHere :| e,
    TypeCycle :| e,
    UnknownType :| e,
    MissingInitializer :| e,
    TypeMismatch' :| e,
    UnknownEnumMember :| e,
    NotAnEnum :| e,
    IndexOutOfBounds :| e,
    NotAnArray :| e,
    UnknownVar :| e,
    UnknownFunction :| e,
    ArgTypeMismatch :| e,
    UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e,
    UnsupportedGenericReturn :| e,
    NoReturnValue :| e,
    InOutArgNotLValue :| e,
    FBNotInstantiated :| e,
    FBUsedAsExpr :| e,
    InternalError :| e,
    UnknownFBMember :| e
  ) =>
  Env -> Variable -> VEither e Variable
elabVar env v =
  case (varConst v, varInit v) of
    (True, Nothing) -> VEither.fromLeft $ MissingInitializer (locVal (varName v)) (locSpan (varName v))
    (True, Just e) -> set =<< check e
    (False, Nothing) ->
      case defaultInitWithTypes (envTypes env) (varType v) of
        Just e0 -> set e0
        Nothing -> VRight v
    (False, Just e) -> set =<< check e
  where
    check = checkExprAssignable env (varType v) (varName v)
    set e' = VRight v {varInit = Just e'}

-- 右辺 e を「左辺型 tgt に代入可能か」を検査し、必要なら正規化して返す。
--   * 集成初期化子はここで型に合わせて展開/パディング（既存の checkArray/Struct… を利用）
--   * 整数/ビット列への整数リテラルは範囲チェック
--   * それ以外は inferType して assignCoerce で許容判定
-- 失敗時は文脈名 who（通常は LHS の識別子名）を使って診断を出す
checkExprAssignable ::
  forall e.
  ( OutOfRange :| e,
    TypeMismatch :| e,
    TooManyAggElems :| e,
    TypeCycle :| e,
    UnknownType :| e,
    TypeMismatch' :| e,
    UnknownEnumMember :| e,
    NotAnEnum :| e,
    IndexOutOfBounds :| e,
    NotAnArray :| e,
    UnknownVar :| e,
    WhyDidYouComeHere :| e,
    NotAStruct :| e,
    UnknownStructMember :| e,
    UnknownFunction :| e,
    ArgTypeMismatch :| e,
    UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e,
    UnsupportedGenericReturn :| e,
    NoReturnValue :| e,
    InOutArgNotLValue :| e,
    FBNotInstantiated :| e,
    FBUsedAsExpr :| e,
    InternalError :| e,
    UnknownFBMember :| e
  ) =>
  Env ->
  -- | tgt: 左辺の期待型（宣言型）
  STType ->
  -- | who: 診断で使う LHS 名
  Identifier ->
  -- | e0: 右辺式
  Expr ->
  -- | 正規化済みの式（初期化子なら整形後の Expr）
  VEither e Expr
checkExprAssignable env tgt who e0 = do
  let tgt' = case resolveType @e (envTypes env) tgt of
        VRight t -> t
        _ -> tgt -- Named 解決
  case e0 of
    -- 集成初期化子は「期待型ありき」なので、ここで確定させる
    EArrayAgg xs -> checkArrayAggInit env who tgt' xs -- 既存：正規化済み Expr を返す想定
    EStructAgg fs -> checkStructAggInit env who tgt' fs -- 既存：正規化済み Expr を返す想定

    -- 整数/ビット列 への “整数リテラル” は範囲チェック
    _ -> do
      case (numericLiteralValue e0, isIntOrBits tgt') of
        (Just n, True) ->
          if fitsIn tgt' n
            then VRight e0
            else VEither.fromLeft $ OutOfRange (locVal who) tgt' n (spanOfExpr e0)
        -- 上記以外は通常の型推論 → 代入互換判定
        _ -> do
          ty <- inferType env e0
          if assignCoerce (envTypes env) ty tgt'
            then VRight e0
            else VEither.fromLeft $ TypeMismatch (locVal who) (locSpan who) tgt' ty

-- =========================================
--  集成初期化子（配列）
-- =========================================
checkArrayAggInit ::
  forall e.
  ( TypeMismatch' :| e,
    UnknownEnumMember :| e,
    NotAnEnum :| e,
    IndexOutOfBounds :| e,
    NotAnArray :| e,
    UnknownVar :| e,
    WhyDidYouComeHere :| e,
    OutOfRange :| e,
    TypeMismatch :| e,
    TooManyAggElems :| e,
    TypeCycle :| e,
    UnknownType :| e,
    NotAStruct :| e,
    UnknownStructMember :| e,
    UnknownFunction :| e,
    ArgTypeMismatch :| e,
    UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e,
    UnsupportedGenericReturn :| e,
    NoReturnValue :| e,
    InOutArgNotLValue :| e,
    FBNotInstantiated :| e,
    FBUsedAsExpr :| e,
    InternalError :| e,
    UnknownFBMember :| e
  ) =>
  Env ->
  Identifier -> -- 変数名（診断用）
  STType -> -- 期待型（Named 可能）
  [Expr] -> -- 与えられた要素
  VEither e Expr -- 正規化した EArrayAgg を返す
checkArrayAggInit env vname tgtTy0 es = do
  tgtTy <- resolveType (envTypes env) tgtTy0
  case tgtTy of
    Array ranges elTy -> do
      let len = arrayCapacity ranges

      if length es > len
        then do
          let headExpr = fromJust $ listToMaybe es
              sp = spanFromTo (spanOfExpr headExpr) (spanOfExpr (last es))
          VEither.fromLeft $ TooManyAggElems (locVal vname) sp len (length es)
        else do
          -- 既存の単一要素割当ルールで各要素を検査
          mapM_ (checkExprAssignable env elTy vname) es
          -- 足りない分は型の既定初期値で埋める（なければエラーにしたい場合はここで Left）
          let fillers = replicate (len - length es) (fromMaybe (fallbackZero elTy) (defaultInitWithTypes (envTypes env) elTy))
          VRight (EArrayAgg (es ++ fillers))
    _ -> VEither.fromLeft $ NotAnArray (locSpan vname) tgtTy -- 期待が配列でない
  where
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
  ( UnknownStructMember :| e,
    NotAStruct :| e,
    OutOfRange :| e,
    TypeMismatch :| e,
    TooManyAggElems :| e,
    WhyDidYouComeHere :| e,
    TypeCycle :| e,
    UnknownType :| e,
    TypeMismatch' :| e,
    UnknownEnumMember :| e,
    NotAnEnum :| e,
    IndexOutOfBounds :| e,
    NotAnArray :| e,
    UnknownVar :| e,
    UnknownFunction :| e,
    ArgTypeMismatch :| e,
    UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e,
    UnsupportedGenericReturn :| e,
    NoReturnValue :| e,
    InOutArgNotLValue :| e,
    FBNotInstantiated :| e,
    FBUsedAsExpr :| e,
    InternalError :| e,
    UnknownFBMember :| e
  ) =>
  Env ->
  Identifier ->
  STType ->
  [(Identifier, Expr)] -> -- (フィールド名 := 式)
  VEither e Expr
checkStructAggInit env varId tgtTy pairs = do
  -- 1) ターゲット型を解決して構造体定義を得る
  resolved <- resolveType (envTypes env) tgtTy
  case resolved of
    Struct defFields -> do
      -- 定義済みフィールドのマップ（Text キー）
      let defMap :: M.Map Text (Identifier, STType)
          defMap = M.fromList [(locVal nm, (nm, ty)) | (nm, ty) <- defFields]

      -- 与えられた (field := expr) を一つずつ検査
      normPairs <- forM pairs $ \(fld, e) ->
        case M.lookup (locVal fld) defMap of
          Nothing ->
            VEither.fromLeft $
              UnknownStructMember
                resolved
                (locVal fld) -- 変数名 or 型名、あなたの診断定義に合わせて
                (locSpan fld)
          Just (_defNm, fTy) -> do
            e' <- checkExprAssignable env fTy fld e -- Expr を正規化して返す版
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
            case defaultInitWithTypes (envTypes env) fTy of
              Just defE -> pure (nm, defE)
              Nothing -> VEither.fromLeft WhyDidYouComeHere -- pure (nm, EDefault) -- 必要に応じてポリシーを

      -- 4) 正規化済みの集成初期化を返す（フィールドは定義順で並べる）
      pure (EStructAgg filled)
    other ->
      VEither.fromLeft $ NotAStruct (locSpan varId) other

-- 本体の文チェック
checkStmt ::
  forall e.
  ( OutOfRange :| e,
    TypeMismatch :| e,
    TypeMismatch' :| e,
    InvalidCaseRange :| e,
    AssignToLoopVar :| e,
    OverlappingCase :| e,
    UnknownVar :| e,
    AssignToConst :| e,
    UnknownStructMember :| e,
    NotAStruct :| e,
    BadIndexCount :| e,
    WhyDidYouComeHere :| e,
    TypeCycle :| e,
    UnknownType :| e,
    UnknownEnumMember :| e,
    NotAnEnum :| e,
    NotAnArray :| e,
    IndexOutOfBounds :| e,
    NonConstantExpr :| e,
    UnknownFunction :| e,
    ArgTypeMismatch :| e,
    UnknownArgName :| e,
    DuplicateArgName :| e,
    PositionalAfterNamed :| e,
    BadArgCount :| e,
    UnsupportedGenericReturn :| e,
    NoReturnValue :| e,
    InOutArgNotLValue :| e,
    AssignToInput :| e,
    FBNotInstantiated :| e,
    FBUsedAsExpr :| e,
    InternalError :| e,
    UnknownFBMember :| e,
    ArgDirectionMismatch :| e,
    AssignToFBField :| e
  ) =>
  Env ->
  Statement ->
  VEither e ()
checkStmt env = \case
  Skip -> VRight ()
  FBCall fbId binds -> do
    let tenv = envTypes env
        fenv = envFuncs env
        venv = envVars env
        src = sourceNameOf fbId
    -- 1) 変数 f が存在するか
    fbVarTy <-
      case M.lookup (locVal fbId) venv of
        Nothing -> VEither.fromLeft $ UnknownVar (locVal fbId) (locSpan fbId)
        Just ty0 -> pure ty0

    -- 2) f の「FB 名」を取り出す（未解決のまま/メタ型どちらでもOKに）
    fbNameTxt <- case viType fbVarTy of
      Named nm -> pure (locVal nm)
      FBMeta nameT -> pure nameT
      -- 型がそれ以外なら FB ではない → 既存のエラーで落とす
      _ -> VEither.fromLeft $ UnknownFunction (locVal fbId) (locSpan fbId)

    -- 3) FuncEnv から FB シグネチャを取る
    sig <- case M.lookup fbNameTxt fenv of
      Just s | fsKind s == FKFunctionBlock -> pure s
      _ -> VEither.fromLeft $ UnknownFunction fbNameTxt (locSpan fbId)
    let pmap = fsArgs sig

    -- 4) バインド検査（未知名 / 重複 / 型適合 / LValue制約）
    let step (seen :: Set Text) = \case
          CallIn nm expr -> do
            let nmt = locVal nm; sp = locSpan nm
            when (nmt `Set.member` seen) $
              VEither.fromLeft $
                DuplicateArgName fbNameTxt nmt sp
            ParamInfo {piType = pTy, piDir = pDir} <-
              case M.lookup nmt pmap of
                Nothing -> VEither.fromLeft $ UnknownArgName fbNameTxt nmt sp
                Just pi' -> pure pi'
            let pTy' = fromJust $ sigTyToSTType pTy
                -- ★ 配列アグリゲートは「宣言型に合わせて」検査してから通す
                checkArrayAggAgainst :: [Expr] -> VEither e ()
                checkArrayAggAgainst es =
                  case resolveType @e tenv pTy' of
                    VRight (Array rs elTy) -> do
                      let need = arrayCapacity rs
                      -- 長さ
                      unless (length es == need) $
                        VEither.fromLeft $
                          ArgTypeMismatch fbNameTxt (Just nmt) 1 pTy' (Array rs elTy) (spanOfExpr expr)
                      -- 各要素の型
                      forM_ es $ \ei -> do
                        ti <- inferType env ei
                        unless (assignCoerce tenv ti elTy) $
                          VEither.fromLeft $
                            ArgTypeMismatch fbNameTxt (Just nmt) 1 elTy ti (spanOfExpr ei)
                    _ -> do
                      -- 期待型が配列でないのに [..] が来た → 不一致
                      VEither.fromLeft $
                        ArgTypeMismatch fbNameTxt (Just nmt) 1 pTy' pTy' (spanOfExpr expr)

            case pDir of
              ParamIn -> do
                case expr of
                  EArrayAgg es -> checkArrayAggAgainst es
                  -- 将来 Struct アグリゲートにも同様の「宣言型駆動」検査を足せる:
                  -- EStructAgg fs -> checkStructAggAgainst pTy' fs
                  _ -> do
                    aty <- inferType env expr
                    unless (assignCoerce tenv aty pTy') $
                      VEither.fromLeft $
                        ArgTypeMismatch fbNameTxt (Just nmt) 1 pTy' aty (spanOfExpr expr)
                pure (Set.insert nmt seen)
              ParamInOut -> do
                -- IN_OUT は LValue 必須（:= でも）
                case exprToLValue expr of
                  Nothing -> VEither.fromLeft (InOutArgNotLValue src fbNameTxt nmt)
                  Just _ -> do
                    aty <- inferType env expr
                    unless (assignCoerce tenv aty pTy') $
                      VEither.fromLeft $
                        ArgTypeMismatch fbNameTxt (Just nmt) 1 pTy' aty (spanOfExpr expr)
                    pure (Set.insert nmt seen)
              ParamOut ->
                VEither.fromLeft $ ArgDirectionMismatch fbNameTxt nmt ParamOut ":=" sp
          CallOut nm lval -> do
            let nmt = locVal nm; sp = locSpan nm
            when (nmt `Set.member` seen) $
              VEither.fromLeft $
                DuplicateArgName fbNameTxt nmt sp
            ParamInfo {piType = pTy, piDir = pDir} <-
              case M.lookup nmt pmap of
                Nothing -> VEither.fromLeft $ UnknownArgName fbNameTxt nmt sp
                Just pi' -> pure pi'
            -- OUT 先は LValue（パーサで保証済み）。とりあえず ParamOut を想定
            -- （ParamInOut の => 許可/不許可は後で仕様決め可）
            unless (pDir == ParamOut) $
              VEither.fromLeft $
                ArgDirectionMismatch fbNameTxt nmt pDir "=>" sp
            -- 左辺型の特定：ルート変数の型を引いて lvalueType で最終型へ
            rootId <- case rootVarId lval of
              Nothing -> VEither.fromLeft WhyDidYouComeHere
              Just i -> pure i

            baseTy <- case M.lookup (locVal rootId) (envVars env) of
              Nothing -> VEither.fromLeft $ UnknownVar (locVal rootId) (locSpan rootId)
              Just t -> pure t

            lhsTy <- lvalueType env (viType baseTy) lval

            -- 型適合（「FB 内部の out の型 pTy を lhs に書けるか？」の向き）
            let pTy' = fromJust $ sigTyToSTType pTy
            unless (assignCoerce (envTypes env) pTy' lhsTy) $
              VEither.fromLeft $
                ArgTypeMismatch fbNameTxt (Just nmt) 1 lhsTy pTy' (spanOfLValue lval)
            pure (Set.insert nmt seen)

    foldM_ step Set.empty binds
    VRight ()
  Assign lv e -> do
    mfb <- fbSubfieldTarget @e env lv
    case mfb of
      Just (vnm, mFld) ->
        VEither.fromLeft $ AssignToFBField (sourceNameOf' lv) vnm (fromMaybe "" mFld)
      Nothing -> do
        tr <- inferType env e
        tl <- lvalueType env tr lv
        -- 特例: 右辺が整数リテラルで、左辺が整数/ビット列 → 範囲チェック
        case (isIntOrBits tl, numericLiteralValue e) of
          (True, Just n) ->
            if fitsIn tl n
              then VRight ()
              else VEither.fromLeft $ OutOfRange (lvalueRootName lv) tl n (spanOfExpr e)
          _ ->
            if assignCoerce (envTypes env) tr tl
              then VRight ()
              else VEither.fromLeft $ TypeMismatch (lvalueRootName lv) (spanOfExpr e) tl tr

        case baseVarName lv of
          Just n ->
            case M.lookup n (envVars env) of
              Just vi
                -- ① CONSTANT ならモードに関係なく常に禁止
                | viConst vi -> VEither.fromLeft $ AssignToConst n (spanOfLValue lv)
                -- ② Strict モードで VAR_INPUT なら禁止
                | envMode env == Strict,
                  viKind vi == VKInput ->
                    VEither.fromLeft
                      (AssignToInput (sourceNameOf $ fromLValue lv) n)
              _ -> pure ()
          Nothing ->
            pure ()
  If c0 th0 elsifs els -> do
    t0 <- inferType env c0
    if t0 == BOOL
      then pure ()
      else VEither.fromLeft $ TypeMismatch' t0
    mapM_ (checkStmt env) th0
    -- 各 ELSIF
    forM_ elsifs $ \(ce, block) -> do
      te <- inferType env ce
      if te == BOOL
        then pure ()
        else VEither.fromLeft $ TypeMismatch' te
      mapM_ (checkStmt env) block
    -- ELSE
    mapM_ (checkStmt env) els
  -- WHILE：条件は BOOL
  While cond body -> do
    t <- inferType env cond
    if t == BOOL
      then mapM_ (checkStmt env) body
      else VEither.fromLeft $ TypeMismatch' t

  -- REPEAT：UNTIL 条件は BOOL
  Repeat body cond -> do
    mapM_ (checkStmt env) body
    t <- inferType env cond
    if t == BOOL
      then VRight ()
      else VEither.fromLeft $ TypeMismatch' t
  -- CASE：セレクタは Constant_Expr / Subrange（定数式）で、型は scrutinee と一致（名目同一含む）
  Case scrut arms els -> do
    tScrut <- inferType env scrut
    unless (isCaseScrutineeType (envTypes env) tScrut) $ VEither.fromLeft $ TypeMismatch' tScrut
    -- ここでは INT / ENUM（Named→Enum 解決）を主対象（必要なら REAL/LREAL 拡張可）
    -- 型チェックは nominalEq を使って “同名 ENUM 同士” を同一扱い。
    -- let checkSel = \case
    --       CSExpr e -> do
    --         checkConstDesignator tenv env e
    --         tSel <- inferType tenv env e
    --         if nominalEq tenv tScrut tSel
    --           then VRight ()
    --           else Left [OpTypeMismatch {op = "CASE", expected = tScrut, actual = tSel}]
    --       CSRangeE a b -> do
    --         checkConstDesignator tenv env a
    --         checkConstDesignator tenv env b
    --         ta <- inferType tenv env a
    --         tb <- inferType tenv env b
    --         if nominalEq tenv tScrut ta && nominalEq tenv ta tb
    --           then VRight ()
    --           else Left [OpTypeMismatch {op = "CASE", expected = tScrut, actual = tb}]
    -- セレクタ1個の基本チェック（定数式 + 型一致）
    let checkSelBasic e = do
          tSel <- inferType env e
          if nominalEq (envTypes env) tScrut tSel
            then checkConstDesignator env e
            else VEither.fromLeft $ TypeMismatch' tSel

    -- アーム本体の文の検査
    let checkArmBody = mapM_ (checkStmt env)

    -- ★ まず各セレクタの基本検査（全型に共通）
    forM_ arms $ \(CaseArm sels _) -> do
      forM_ sels $ \case
        CSExpr e -> checkSelBasic e
        CSRangeE a b -> do
          -- ENUM のとき range は不許可（型ミスマッチ扱いで弾く）
          case isEnumNamed (envTypes env) tScrut of
            Just _ ->
              VEither.fromLeft $ TypeMismatch' INT
            Nothing -> do
              -- INT（など）なら各端点の基本検査（型一致 & const）
              checkSelBasic a
              checkSelBasic b
    -- ここでは本文の検査はまだしない（後で一括）

    -- ★ INT スクルーティニのときだけ、向き&重複/交差チェック（リテラル評価できるもののみ）
    case tScrut of
      INT -> do
        let selIntervals = \case
              CSExpr e ->
                case evalIntLiteral e of
                  Just n -> VRight (Just (n, n))
                  Nothing -> VRight Nothing -- 値が取れないものは判定スキップ
              CSRangeE a b ->
                case (evalIntLiteral a, evalIntLiteral b) of
                  (Just x, Just y)
                    | x <= y -> VRight (Just (x, y))
                    | otherwise -> VEither.fromLeft $ InvalidCaseRange x y
                  _ -> VRight Nothing

        -- 各アーム内の重複・交差
        forM_ arms $ \(CaseArm sels _) -> do
          ivs <- catMaybes <$> traverse selIntervals sels
          let sorted = sortOn fst ivs
          when (hasOverlap sorted) $
            VEither.fromLeft OverlappingCase

        -- アーム間の重複・交差
        let allSels = concatMap (\(CaseArm ss _) -> ss) arms
        allIvs <- catMaybes <$> traverse selIntervals allSels
        when (hasOverlap (sortOn fst allIvs)) $
          VEither.fromLeft OverlappingCase
      _ -> pure () -- ENUM 等はここでは追加チェックなし
    forM_ arms $ \(CaseArm _ ss) -> checkArmBody ss
    mapM_ (checkStmt env) els
  For iv e0 e1 mby body -> do
    -- ループ変数の存在／const でない／型が INT
    let nameTxt = locVal iv
        whereSp = locSpan iv
    case M.lookup nameTxt (envVars env) of
      Nothing -> VEither.fromLeft $ UnknownVar nameTxt whereSp
      Just info ->
        case viConst info of
          True -> VEither.fromLeft $ AssignToConst nameTxt whereSp
          _ -> when (viType info /= INT) $ VEither.fromLeft $ TypeMismatch nameTxt whereSp INT $ viType info

    -- init / end / step は INT
    t0 <- inferType env e0
    if t0 == INT then pure () else VEither.fromLeft $ TypeMismatch' t0
    t1 <- inferType env e1
    if t1 == INT then pure () else VEither.fromLeft $ TypeMismatch' t1
    case mby of
      Nothing -> pure ()
      Just s -> do
        ts <- inferType env s
        if ts == INT then pure () else VEither.fromLeft $ TypeMismatch' ts

    -- 本体でループ変数への代入を禁止
    mapM_ (denyAssignTo nameTxt) body

    -- 本体の通常の型チェック
    mapM_ (checkStmt env) body
  where
    -- 左辺の“ベース変数”は既存の baseIdent を使用
    denyAssignTo nm = \case
      Assign lhs _ ->
        let b = baseIdent lhs
         in if locVal b == nm
              then VEither.fromLeft $ AssignToLoopVar nm (locSpan b)
              else VRight () :: VEither e ()
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
      FBCall _ binds ->
        let checkBind = \case
              CallOut _ lv ->
                let b = baseIdent lv
                 in if locVal b == nm
                      then VEither.fromLeft $ AssignToLoopVar nm (locSpan b)
                      else VRight ()
              CallIn _ _ -> VRight ()
         in mapM_ checkBind binds
      Skip -> VRight ()

rootVarId :: LValue -> Maybe Identifier
rootVarId = \case
  LVar i -> Just i
  LField lv _ -> rootVarId lv
  LIndex lv _ -> rootVarId lv

--   f.o.p なら Just "p"
--   f.o[0] なら Just "o"
--   f       なら Nothing
lastFieldIdent :: LValue -> Maybe Identifier
lastFieldIdent = \case
  LVar _ -> Nothing
  LIndex lv _ -> lastFieldIdent lv
  LField _lv fld -> Just fld

-- =========================================================
--  Constant *Designator* checker (CODESYS 風, CASE セレクタ用)
--    * OK: リテラル, 符号付数値リテラル, 列挙リテラル(Type.Ctor),
--          VAR CONSTANT の識別子, その配列要素(添字は整数リテラルのみ),
--          定数ベースのフィールド参照
--    * NG: すべての二項演算/比較/NOT、添字がリテラル以外等
-- =========================================================
checkConstDesignator ::
  forall e.
  ( NonConstantExpr :| e,
    TypeCycle :| e,
    UnknownType :| e
  ) =>
  Env -> Expr -> VEither e ()
checkConstDesignator env = go
  where
    ok = VRight ()

    go = \case
      -- リテラル群は OK
      EINT _ -> ok
      -- 単項マイナスは「中身が数値リテラル」のときのみ OK
      ENeg e | isIntLikeLiteral e -> ok
      -- 列挙リテラル: Type.Ctor の形（Type は Named で Enum に解決できること）
      EField (EVar ty) _ctor
        | isEnumType (envTypes env) ty -> ok
      -- VAR CONSTANT な識別子
      EVar i
        | isConstVar (envVars env) i -> ok
      -- VAR CONSTANT な配列要素（添字は整数リテラルのみ許可）
      EIndex base idxs
        | VRight () <- go base,
          all isIntLikeLiteral idxs ->
            ok
      -- 定数構造体のフィールド: ベースが OK なら OK
      EField base _fld ->
        go base
      -- それ以外は NG
      other ->
        VEither.fromLeft $ NonConstantExpr (spanOfExpr other)

    -- ヘルパ
    isConstVar m i =
      case M.lookup (locVal i) m of
        Just vi -> viConst vi
        _ -> False

    isEnumType te tyId =
      case resolveType @e te (Named tyId) of
        VRight Enum {} -> True
        _ -> False

resolveType ::
  ( TypeCycle :| e,
    UnknownType :| e
  ) =>
  TypeEnv -> STType -> VEither e STType
resolveType tenv = go Set.empty
  where
    go seen = \case
      -- ★ 別名の解決：Identifier から Text と Span を取り出す
      Named n ->
        let nameTxt = locVal n
            whereSp = locSpan n
         in if nameTxt `Set.member` seen
              then VEither.fromLeft $ TypeCycle nameTxt whereSp
              else case M.lookup nameTxt tenv of
                Nothing -> VEither.fromLeft $ UnknownType nameTxt whereSp
                Just t -> go (Set.insert nameTxt seen) t
      -- ★ 構造体の中も再帰的に解決
      Struct fs -> Struct <$> traverse (\(fld, t) -> (fld,) <$> go seen t) fs
      Array rs el -> Array rs <$> go seen el
      -- ★ 既知の基底型はそのまま
      t -> VRight t

-- 変数宣言の型を TypeEnv で解決して書き戻す
resolveVarTypes ::
  ( TypeCycle :| e,
    UnknownType :| e
  ) =>
  TypeEnv -> [Variable] -> VEither e [Variable]
resolveVarTypes tenv = traverse step
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

-- actTy が expTy と互換か？
nominalEq :: TypeEnv -> STType -> STType -> Bool
nominalEq tenv a b =
  let res :: STType -> VEither '[TypeCycle, UnknownType] STType
      res = resolveType tenv
   in case (a, b) of
        (Named n1, Named n2) ->
          case (res (Named n1), res (Named n2)) of
            (VRight Enum {}, VRight Enum {}) -> locVal n1 == locVal n2
            (VRight t1, VRight t2) -> t1 == t2
            _ -> False
        (Named n, t) ->
          case res (Named n) of
            VRight t' -> nominalEq tenv t' t
            _ -> False
        (t, Named n) ->
          case res (Named n) of
            VRight t' -> nominalEq tenv t t'
            _ -> False
        (STRING _, STRING _) -> True
        (WSTRING _, WSTRING _) -> True
        (FBMeta n1, FBMeta n2) -> n1 == n2
        _ ->
          case (res a, res b) of
            (VRight r1, VRight r2) -> r1 == r2
            _ -> a == b

evalConstExpr :: Env -> Expr -> Maybe ConstVal
evalConstExpr env = go
  where
    go = \case
      -- 数値（符号も含む）
      EINT n -> Just (CVInt n)
      ENeg e -> do CVInt n <- go e; Just (CVInt (negate n))

      -- 論理/文字/文字列
      EBOOL b -> Just (CVBool b)
      ECHAR c -> Just (CVChar c)
      EWCHAR c -> Just (CVWChar c)
      ESTRING t -> Just (CVString t)
      EWSTRING t -> Just (CVWString t)
      -- 時刻/日付
      ETIME dur -> Just (CVTime dur)
      ETOD tod -> Just (CVTOD tod)
      EDATE d -> Just (CVDate d)
      EDT dt -> Just (CVDT dt)
      -- 列挙 Type.Ctor
      EField (EVar ty) ctor ->
        case resolveType (envTypes env) (Named ty) :: VEither [TypeCycle, UnknownType] STType of
          VRight (Enum ctors)
            | any ((== locVal ctor) . locVal . fst) ctors ->
                Just (CVEnum (locVal ty) (locVal ctor))
          _ -> Nothing
      -- VAR CONSTANT の伝播
      EVar i -> do
        vi <- M.lookup (locVal i) (envVars env)
        guard (viConst vi)
        initE <- viInit vi
        go initE
      _ -> Nothing

-- GST に属するか（Named は解決してから見る）
gstMember ::
  forall e.
  ( TypeCycle :| e,
    UnknownType :| e
  ) =>
  TypeEnv -> STType -> GSTType -> VEither e Bool
gstMember tenv ty0 gst = do
  -- Named などはできるだけ解決してから判定
  ty <- case resolveType @e tenv ty0 of
    VRight t -> pure t
    VLeft _ -> pure ty0 -- 解決失敗時は生値で判定（テスト的には稀）
  let isIntFamily = \case
        SINT -> True
        INT -> True
        DINT -> True
        LINT -> True
        USINT -> True
        UINT -> True
        UDINT -> True
        ULINT -> True
        -- 将来: Subrange base → isIntFamily base
        _ -> False

      isRealFamily = \case
        REAL -> True
        LREAL -> True
        _ -> False

      isNumFamily t = isIntFamily t || isRealFamily t

      isBitFamily = \case
        BOOL -> True
        BYTE -> True
        WORD -> True
        DWORD -> True
        LWORD -> True
        _ -> False

      isStringFamily = \case
        CHAR -> True
        WCHAR -> True
        STRING _ -> True
        WSTRING _ -> True
        _ -> False

      isDateFamily = \case
        DATE -> True
        TOD -> True
        DT -> True
        -- 将来: LDATE / LTOD / LDT を追加
        _ -> False

      isDurationFamily = \case
        TIME -> True
        -- 将来: LTIME を追加
        _ -> False

      -- 「配列/構造体以外」を elementary として扱う（Enum/別名は含む）
      isElementary = \case
        Struct _ -> False
        Array _ _ -> False
        _ -> True

  pure $ case gst of
    GSTAny -> isElementary ty
    GSTAnyInt -> isIntFamily ty
    GSTAnyNum -> isNumFamily ty
    GSTAnyReal -> isRealFamily ty
    GSTAnyBit -> isBitFamily ty
    GSTAnyString -> isStringFamily ty
    GSTAnyDate -> isDateFamily ty
    GSTAnyDuration -> isDurationFamily ty

-- SigTy を具体型に落とす
instantiateSigTy ::
  TypeEnv -> TVSubst -> SigTy -> VEither e STType
instantiateSigTy _ _ (SigMono t) = pure t
instantiateSigTy _tenv subst (SigGen gst tv) =
  case M.lookup tv subst of
    Just t -> pure t
    Nothing -> pure (defaultForGST gst)
  where
    -- 未束縛TVに対するデフォルト代表（実際ほぼ来ない想定）
    defaultForGST = \case
      GSTAny -> INT
      GSTAnyInt -> INT
      GSTAnyNum -> INT
      GSTAnyReal -> REAL
      GSTAnyBit -> BOOL
      GSTAnyString -> STRING Nothing
      GSTAnyDate -> DATE
      GSTAnyDuration -> TIME

-- 戻り値位置専用：未束縛TVなら UnsupportedGenericReturn を投げる
instantiateSigTyRet ::
  (UnsupportedGenericReturn :| e) =>
  TypeEnv -> TVSubst -> POUName -> SigTy -> VEither e STType
instantiateSigTyRet _ _ _ (SigMono t) = pure t
instantiateSigTyRet _ subst fname (SigGen _gst tv) =
  case M.lookup tv subst of
    Just t -> pure t
    Nothing -> VEither.fromLeft (UnsupportedGenericReturn fname)

noSpan :: Span
noSpan = Span dummyPos dummyPos
  where
    dummyPos = initialPos "<no-span>"

sourceNameOf :: Identifier -> FilePath
sourceNameOf ident = sourceName $ spanStart $ locSpan ident

sourceNameOf' :: LValue -> FilePath
sourceNameOf' = \case
  LVar i -> sourceNameOf i
  LField l _ -> sourceNameOf' l
  LIndex l _ -> sourceNameOf' l

-- LValue の“ベース変数名”を抽出
baseVarName :: LValue -> Maybe Text
baseVarName = \case
  LVar ident -> Just (locVal ident)
  LIndex lv _ -> baseVarName lv
  LField lv _ -> baseVarName lv

fromLValue :: LValue -> Identifier
fromLValue = \case
  LVar ident -> ident
  LIndex lv _ -> fromLValue lv
  LField lv _ -> fromLValue lv

-- 要求された変数名集合 req が、ブロック実行後に「確実に代入済み」になるか
mustAssignAll :: Set Text -> [Statement] -> Bool
mustAssignAll req body =
  let out = goBlock Set.empty body
   in req `Set.isSubsetOf` out
  where
    -- ブロック：順次実行。確定代入集合を前から伝播
    goBlock :: Set Text -> [Statement] -> Set Text
    goBlock acc [] = acc
    goBlock acc (s : ss) = goBlock (goStmt acc s) ss

    -- 文：確定代入集合の更新
    goStmt :: Set Text -> Statement -> Set Text
    goStmt acc = \case
      Assign lv _ ->
        case baseVarName lv of
          Just n | n `Set.member` req -> Set.insert n acc
          _ -> acc
      If _ th elsifs els ->
        let b0 = goBlock acc th
            bEls = foldr (\(_, bs) z -> Set.intersection (goBlock acc bs) z) b0 elsifs
            bE = if null els then acc else goBlock acc els
         in Set.intersection bEls bE -- 全分岐の共通部分だけが「確定」
      While _ _ ->
        acc -- 0回実行があり得る
      Repeat body' _ ->
        goBlock acc body' -- 1回は実行される
      Case _ arms els ->
        let armOuts = map (\(CaseArm _ bs) -> goBlock acc bs) arms
            elseOut = if null els then acc else goBlock acc els
         in foldr Set.intersection elseOut armOuts
      For {} ->
        acc -- 0回実行があり得る
      FBCall _ binds ->
        foldl'
          ( \a b -> case b of
              CallOut _ lv ->
                case baseVarName lv of
                  Just n | n `Set.member` req -> Set.insert n a
                  _ -> a
              _ -> a
          )
          acc
          binds
      Skip ->
        acc

exprToLValue :: Expr -> Maybe LValue
exprToLValue = \case
  EVar x -> Just (LVar x)
  EField e f -> LField <$> exprToLValue e <*> pure f
  EIndex e ix -> LIndex <$> exprToLValue e <*> pure ix
  _ -> Nothing

inputParamOrder :: FuncSig -> [Text]
inputParamOrder =
  map fst
    . sortOn (piPos . snd)
    . filter (\(_, p) -> piDir p /= ParamOut)
    . M.toList
    . fsArgs

sigTyToSTType :: SigTy -> Maybe STType
sigTyToSTType = \case
  SigMono t -> Just t
  _ -> Nothing

-- LValue のソース範囲をざっくり復元
-- 先頭は LValue の先頭、末尾は最後の識別子 or 最後の添字式の末尾を使う
spanOfLValue :: LValue -> Span
spanOfLValue = \case
  LVar ident ->
    locSpan ident
  LField lv fld ->
    spanFromTo (spanOfLValue lv) (locSpan fld)
  LIndex lv es ->
    case reverse es of
      (eLast : _) -> spanFromTo (spanOfLValue lv) (spanOfExpr eLast)
      [] -> spanOfLValue lv -- 文法上 [] は来ないはずだけど保険

-- 「FBの配下を指している代入か？」判定。返り値は (FB変数名, 直近のフィールド名)
fbSubfieldTarget ::
  Env ->
  LValue ->
  VEither e (Maybe (Text, Maybe Text))
fbSubfieldTarget env lv = do
  case rootVarId lv of
    Nothing -> pure Nothing
    Just rid -> do
      let vnm = locVal rid
          fenv = envFuncs env
          isRoot = case lv of LVar _ -> True; _ -> False
      case M.lookup vnm (envVars env) of
        Nothing -> pure Nothing
        Just vi -> case viType vi of
          FBMeta _fbName
            | not isRoot ->
                pure (Just (vnm, fmap locVal (lastFieldIdent lv)))
          Named fbTyId
            | not isRoot,
              Just sig <- M.lookup (locVal fbTyId) fenv,
              fsKind sig == FKFunctionBlock ->
                pure (Just (vnm, fmap locVal (lastFieldIdent lv)))
          _ -> pure Nothing

-- POU 用 VarEnv 構築（VAR_EXTERNAL を GlobalVarEnv と突き合わせる）
buildPOUVarEnv ::
  ( DuplicateVar :| e,
    UnknownVar :| e,
    TypeMismatch :| e
  ) =>
  GlobalVarEnv ->
  [Variable] ->
  VEither e VarEnv
buildPOUVarEnv gvenv =
  foldM step M.empty
  where
    step m v =
      case varKind v of
        VKExternal -> do
          let name = locVal (varName v)
              sp = locSpan (varName v)
          case M.lookup name gvenv of
            Nothing ->
              -- 対応する VAR_GLOBAL がない
              VEither.fromLeft $ UnknownVar name sp
            Just gvi -> do
              let globalTy = viType gvi
                  localTy = varType v -- resolveVarTypes 済みの想定
              if localTy == globalTy
                then insertVar m v
                else
                  -- 期待: グローバル宣言の型 / 実際: VAR_EXTERNAL 宣言の型
                  VEither.fromLeft $
                    TypeMismatch name sp globalTy localTy
        -- それ以外は従来通り
        _ -> insertVar m v