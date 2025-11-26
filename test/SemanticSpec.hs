{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Monad (forM_)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable, typeRep)
import ST.AST
import ST.Parser (parseProgram, parseUnit', parseUnits')
import ST.Semantic
import Test.Hspec
import Text.Megaparsec
import Vary (Vary, into, (:|))
import Vary.VEither (VEither (VLeft, VRight))

toIdent :: Text -> Identifier
toIdent txt =
  let start = initialPos "<test>" -- 行=1, 列=1
      end = start {sourceColumn = mkPos (1 + T.length txt)}
   in Loc (Span start end) txt

paramMap :: [(Text, SigTy)] -> M.Map Text ParamInfo
paramMap xs =
  M.fromList
    [ (n, ParamInfo {piType = ty, piDir = ParamIn, piPos = ix})
    | (ix, (n, ty)) <- zip [0 ..] xs
    ]

paramMapDir :: [(Text, SigTy, ParamDirKind)] -> M.Map Text ParamInfo
paramMapDir xs =
  M.fromList
    [ (n, ParamInfo {piType = ty, piDir = dir, piPos = ix})
    | (ix, (n, ty, dir)) <- zip [0 ..] xs
    ]

elaborateProjectTest :: FuncEnv -> [Text] -> VEither AllErrs [Unit]
elaborateProjectTest fenv srcs =
  case parseUnits' srcs of
    Left (i, e) ->
      error $ "parse error in input-" <> show i <> ":\n" <> errorBundlePretty e
    Right us ->
      elaborateUnits fenv us

elaborateUnitTest :: Unit -> VEither AllErrs Unit
elaborateUnitTest = elaborateUnitWithDecls M.empty

elaborateProgramTest :: Program -> VEither AllErrs Program
elaborateProgramTest = elaborateProgramTestWithTypes M.empty

elaborateProgramTestWithTypes :: FuncEnv -> Program -> VEither AllErrs Program
elaborateProgramTestWithTypes fenv prog =
  case elaborateUnitWithDecls fenv (UProgram prog) of
    VRight (UProgram p') -> VRight p'
    VRight _ ->
      -- 基本ここには来ない想定だけど、一応保険
      error "elaborateProgramTestWithFuns: unexpected error"
    VLeft e -> VLeft e

expectParsed :: Text -> (Program -> Expectation) -> Expectation
expectParsed src k = case parseProgram src of
  Left e -> expectationFailure (show e)
  Right p -> k p

expectParsedUnit :: Text -> (Unit -> Expectation) -> Expectation
expectParsedUnit src k =
  case parseUnit' src of
    Left e -> expectationFailure (show e)
    Right u -> k u

-- | open-sum の Left から、指定エラー型だけを引き抜く（あるなら Just）
--   ※ Vary のプロジェクタ名が環境で違う場合はここを書き換えるだけでOK
prj :: forall err es. (err :| es) => Vary es -> Maybe err
prj = Vary.into @err

-- 成功/失敗（型に依存しないやつは曖昧性なし）
shouldSucceedV :: VEither es a -> Expectation
shouldSucceedV = \case
  VRight _ -> pure ()
  VLeft _ -> expectationFailure "expected VRight, got VLeft"

-- shouldFailV :: VEither es a -> Expectation
-- shouldFailV v = case v of
--   VLeft _ -> pure ()
--   VRight _ -> expectationFailure "expected VLeft, got VRight"

-- 失敗（中身も検査）
shouldFailWithDetail ::
  forall err es a.
  (err :| es, Typeable err, Show (Vary es)) =>
  VEither es a -> (err -> Bool) -> Expectation
shouldFailWithDetail v p = case v of
  VRight _ ->
    expectationFailure "expected VLeft, got VRight"
  VLeft es' ->
    case prj @err es' of
      Just e -> p e `shouldBe` True
      Nothing ->
        let expectedTy = show (typeRep (Proxy @err))
            actualRep = show es' -- 例: "Vary.from @TypeMismatch (TypeMismatch ...)"
         in expectationFailure $
              "Expected VLeft containing "
                <> expectedTy
                <> ", but got VLeft "
                <> actualRep

-- | 「このエラー型で失敗した（中身は見ない）」をチェック
shouldFail ::
  forall err es a.
  (err :| es, Typeable err, Show (Vary es)) =>
  VEither es a -> Expectation
shouldFail v = shouldFailWithDetail @err v (const True)

expectUnitPass :: Text -> Expectation
expectUnitPass src =
  case parseUnit' src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnitTest u
       in shouldSucceedV v

expectUnitFail ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  Text -> Expectation
expectUnitFail src =
  case parseUnit' src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnitTest u
       in shouldFail @err v

expectUnitFailWithDetail ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  Text -> (err -> Bool) -> Expectation
expectUnitFailWithDetail src pred' =
  case parseUnit' src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnitTest u
       in shouldFailWithDetail @err v pred'

-- FunEnv / elaborateUnitWithFuns を前提にした Unit 用ヘルパ
-- （ST.Semantic から FunEnv, FunSig, elaborateUnitWithFuns を import 済み想定）

-- 成功を期待（関数シグネチャ付き）
expectUnitPassWithFuns :: FuncEnv -> Text -> Expectation
expectUnitPassWithFuns funs src =
  case parseUnit' src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnitWithDecls funs u
       in shouldSucceedV v

-- 失敗を期待（型だけ指定、詳細は見ない）
expectUnitFailWithFuns ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  FuncEnv -> Text -> Expectation
expectUnitFailWithFuns funs src =
  case parseUnit' src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnitWithDecls funs u
       in shouldFail @err v

-- 失敗を期待（この型で、かつ中身も predicate でチェック）
expectUnitFailWithDetailWithFuns ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  FuncEnv -> Text -> (err -> Bool) -> Expectation
expectUnitFailWithDetailWithFuns funs src pred' =
  case parseUnit' src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnitWithDecls funs u
       in shouldFailWithDetail @err v pred'

-- Units版

expectParsedUnits :: [Text] -> ([Unit] -> Expectation) -> Expectation
expectParsedUnits srcs k =
  case parseUnits' srcs of
    Left e -> expectationFailure (show e)
    Right u -> k u

expectUnitsPass :: [Text] -> Expectation
expectUnitsPass srcs =
  case parseUnits' srcs of
    Left (i, e) ->
      expectationFailure $
        "Parse error in input-" <> show i <> ":\n" <> errorBundlePretty e
    Right us ->
      let v :: VEither AllErrs Units
          v = elaborateUnits M.empty us
       in shouldSucceedV v

expectUnitsFail ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  [Text] -> Expectation
expectUnitsFail srcs =
  case parseUnits' srcs of
    Left (i, e) ->
      expectationFailure $
        "Parse error in input-" <> show i <> ":\n" <> errorBundlePretty e
    Right us ->
      let v :: VEither AllErrs Units
          v = elaborateUnits M.empty us
       in shouldFail @err v

expectUnitsFailWithDetail ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  [Text] -> (err -> Bool) -> Expectation
expectUnitsFailWithDetail srcs pred' =
  case parseUnits' srcs of
    Left (i, e) ->
      expectationFailure $
        "Parse error in input-" <> show i <> ":\n" <> errorBundlePretty e
    Right us ->
      let v :: VEither AllErrs Units
          v = elaborateUnits M.empty us
       in shouldFailWithDetail @err v pred'

expectUnitsFailWithDetail'' ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  FuncEnv -> [Text] -> (err -> Bool) -> Expectation
expectUnitsFailWithDetail'' funs srcs pred' =
  case parseUnits' srcs of
    Left e -> expectationFailure (show e)
    Right us ->
      let v :: VEither AllErrs Units
          v = elaborateUnits funs us
       in shouldFailWithDetail @err v pred'

-- Pass 版（mode 指定）
expectUnitsPassWithMode ::
  SemMode -> FuncEnv -> [Text] -> Expectation
expectUnitsPassWithMode mode baseFenv srcs =
  case parseUnits' srcs of
    Left (i, e) ->
      expectationFailure $
        "Parse error in input-" <> show i <> ":\n" <> errorBundlePretty e
    Right us ->
      case elaborateUnitsWithMode mode baseFenv us of
        VLeft err -> expectationFailure (show err)
        VRight _ -> pure ()

-- Fail 版（mode 指定）
expectUnitsFailWithDetailWithMode ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  SemMode -> FuncEnv -> [Text] -> (err -> Bool) -> Expectation
expectUnitsFailWithDetailWithMode mode baseFenv srcs pred' =
  case parseUnits' srcs of
    Left (i, e) ->
      expectationFailure $
        "Parse error in input-" <> show i <> ":\n" <> errorBundlePretty e
    Right us ->
      let v :: VEither AllErrs Units
          v = elaborateUnitsWithMode mode baseFenv us
       in shouldFailWithDetail @err v pred'

main :: IO ()
main = hspec $ do
  describe "elaborateProgram" $ do
    it "fills default init for INT/BOOL when missing" $ do
      let src = "PROGRAM P\nVAR\nx:INT; y:BOOL;\nEND_VAR\n"
      case parseProgram src of
        Left e -> expectationFailure (show e)
        Right prog ->
          let v :: VEither AllErrs Program
              v = elaborateProgramTest prog
           in v `shouldSatisfy` \case
                VRight (Program _ (VarDecls [vx, vy]) _) ->
                  varInit vx == Just (EINT 0)
                    && varInit vy == Just (EBOOL False)
                _ -> False

    it "rejects type mismatch in initializer" $ do
      let src = "PROGRAM P\nVAR\nx:INT := TRUE;\nEND_VAR\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch vname _ _ _) -> vname == "x"

    it "rejects duplicate variable names" $ do
      let src = "PROGRAM P\nVAR\nx:INT; x:BOOL;\nEND_VAR\n"
      expectUnitFailWithDetail @DuplicateVar src $
        \(DuplicateVar vname _) -> vname == "x"

    it "rejects non-initialized const" $ do
      let src = "PROGRAM P\nVAR CONSTANT\nx:INT;\nEND_VAR\n"
      expectUnitFailWithDetail @MissingInitializer src $
        \(MissingInitializer vname _) -> vname == "x"

  describe "assignment semantics" $ do
    it "accepts type-correct assignments" $ do
      let src = "PROGRAM P\nVAR\nx:INT; y:BOOL;\nEND_VAR\nx := 0;\ny := FALSE;\n"
      expectUnitPass src

    it "parses two assignments after defaultInit" $ do
      let src = "PROGRAM P\nVAR\nx:INT; y:BOOL;\nEND_VAR\nx := 1;\ny := TRUE;\n"
      expectUnitPass src

    it "rejects unknown variable" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nz := 1;\n"
      expectUnitFailWithDetail @UnknownVar src $
        \(UnknownVar vname _) -> vname == "z"

    it "rejects type mismatch" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nx := TRUE;\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch vname _ _ _) -> vname == "x"

  describe "assignment semantics (arithmetic operations)" $ do
    let src :: Text
        src = "PROGRAM P\nVAR\nx:INT;y:BOOL;\nEND_VAR\n"

        ok :: [Text]
        ok =
          [ "x := 1 + 2 * 3;",
            "x := (1 + 2) * 3;",
            "x := -1;"
          ]

        -- 失敗ケース（詳細もチェック）
        ng :: [(Text, TypeMismatch' -> Bool)]
        ng =
          [ ( "y := TRUE + 1;",
              \(TypeMismatch' actual) -> actual == BOOL
            )
            -- 追加したければ（論理演算にINTを渡すのはNG → actual は INT）
            -- , ( "y := 1 AND 2;"
            --   , \(TypeMismatch' actual) -> actual == INT
            --   )
          ]

    -- OK 群
    forM_ ok $ \s ->
      it ("accepts " <> T.unpack s) $
        expectUnitPass (src <> s)

    -- NG 群（型も中身もチェック）
    forM_ ng $ \(tc, p) ->
      it ("rejects " <> T.unpack tc) $
        expectUnitFailWithDetail @TypeMismatch' (src <> tc) p

  describe "NOT semantics" $ do
    let base = "PROGRAM P\nVAR\nx:INT; y:BOOL;\nEND_VAR\n"

    -- it "accepts NOT on BOOL and keeps BOOL" $ do
    --   shouldParse (base <> "y := NOT TRUE;\n")

    -- it "accepts NOT on INT and keeps INT" $ do
    --   shouldParse (base <> "x := NOT 2;\n")

    it "rejects assigning NOT 2 (INT) to BOOL" $ do
      expectUnitFailWithDetail @TypeMismatch' (base <> "y := NOT 2;\n") $
        \(TypeMismatch' actual) -> actual == INT

    it "rejects assigning NOT TRUE (BOOL) to INT" $ do
      expectUnitFailWithDetail @TypeMismatch (base <> "x := NOT TRUE;\n") $
        \(TypeMismatch vname _ expected actual) -> vname == "x" && expected == INT && actual == BOOL

  describe "IF semantics" $ do
    it "accepts when type of conditions are BOOL" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nIF x=0 THEN x := 1; ELSIF x=1 THEN x := 2; ELSE x := 3; END_IF\n"
      expectUnitPass src

    it "rejects when condition of IF is not BOOL (1)" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nIF 1 THEN x := 1; END_IF\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == INT

    it "rejects when condition of ELSIF is not BOOL (1)" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nIF x=0 THEN x := 1; ELSIF 1 THEN x := 2; END_IF\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == INT

    it "rejects unknown variable inside IF-THEN-ELSE-END_IF" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nIF TRUE THEN z := 1; END_IF\n"
      expectUnitFailWithDetail @UnknownVar src $
        \(UnknownVar vname _) -> vname == "z"

  describe "WHILE/REPEAT/CASE semantics" $ do
    it "requires BOOL in WHILE condition" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nWHILE 1 DO x := 0; END_WHILE\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == INT

    it "requires BOOL in REPEAT-UNTIL condition" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nREPEAT x := 1; UNTIL 0 END_REPEAT\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == INT

    it "requires INT scrutinee in CASE (minimal)" $ do
      let src = "PROGRAM P\nVAR\ny:BOOL;\nEND_VAR\nCASE y OF 0: y := TRUE; END_CASE\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

    it "accepts CASE with arms and ELSE" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nCASE x OF 0: x := 1; 1,2..3: x := 4; ELSE x := 0; END_CASE\n"
      expectUnitPass src

  describe "TYPE resolution (semantics)" $ do
    it "resolves alias in VAR and fills default init" $ do
      let srcs =
            [ "TYPE MyInt : INT; END_TYPE\n",
              "PROGRAM P\nVAR\nx: MyInt;\nEND_VAR\n"
            ]
      elaborateProjectTest M.empty srcs `shouldSatisfy` \case
        VRight us ->
          case [v | UProgram (Program _ (VarDecls [v]) _) <- us] of
            [v] -> varType v == INT && varInit v == Just (EINT 0)
            _ -> False
        _ -> False

    it "fails on unknown type" $ do
      let src = "PROGRAM P\nVAR\nx: Foo;\nEND_VAR\n"
      expectUnitFailWithDetail @UnknownType src $
        \(UnknownType tname _) -> tname == "Foo"

    it "fails on type cycle" $ do
      let srcs =
            [ "TYPE A : B; B : A; END_TYPE\n",
              "PROGRAM P\nVAR\nx: A;\nEND_VAR\n"
            ]
      expectUnitsFailWithDetail @TypeCycle srcs $
        \(TypeCycle tname _) -> tname == "B"

  describe "STRUCT field access (semantics)" $ do
    it "allows reading r.a when r: STRUCT{a:INT}" $ do
      let srcType =
            "TYPE R : STRUCT a: INT; END_STRUCT; END_TYPE\n"
          srcProg =
            "PROGRAM P\nVAR\nr: R; x: INT;\nEND_VAR\n\
            \x := r.a;\n"
      expectUnitsPass [srcType, srcProg]

    it "rejects unknown field r.z" $ do
      let srcs =
            [ "TYPE R : STRUCT a: INT; END_STRUCT; END_TYPE\n",
              "PROGRAM P\nVAR\nr: R; x: INT;\nEND_VAR\n\
              \x := r.z;\n"
            ]
      expectUnitsFailWithDetail @UnknownStructMember srcs $
        \(UnknownStructMember _ member _) -> member == "z"

    it "rejects r.a when r is INT (not a struct)" $ do
      let src =
            "PROGRAM P\nVAR\nr: INT; x: INT;\nEND_VAR\n\
            \x := r.a;\n"
      expectUnitFail @NotAStruct src

  describe "array/struct semantics" $ do
    it "accepts struct field - read and write" $ do
      let srcs =
            [ "TYPE Point : STRUCT x: INT; y: BOOL; END_STRUCT; END_TYPE\n",
              "PROGRAM P\nVAR r: Point; x: INT; y: BOOL; END_VAR\n\
              \x := r.x; y := r.y;\n\
              \r.x := 1; r.y := TRUE;\n"
            ]
      expectUnitsPass srcs

    it "rejects unknown field" $ do
      let srcs =
            [ "TYPE Point : STRUCT x: INT; END_STRUCT; END_TYPE\n",
              "PROGRAM P\nVAR r: Point; END_VAR\n\
              \r.z := 1;\n"
            ]
      expectUnitsFail @UnknownStructMember srcs

    it "accepts array index" $ do
      let src =
            "PROGRAM P\nVAR a: ARRAY [0..2] OF INT; i: INT; END_VAR\n\
            \a[1] := 42; a[i] := 0;\n"
      expectUnitPass src

    it "rejects bad index count" $ do
      let src =
            "PROGRAM P\nVAR a: ARRAY [0..2] OF INT; END_VAR\n\
            \a[1,2] := 5;\n"
      expectUnitFail @BadIndexCount src

    it "rejects bad index type" $ do
      let src =
            "PROGRAM P\nVAR a: ARRAY [0..2] OF INT; b: BOOL; END_VAR\n\
            \a[b] := 5;\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

    it "type-checks element type through index" $ do
      let src =
            "PROGRAM P\nVAR a: ARRAY [0..2] OF INT; y: BOOL; END_VAR\n\
            \y := a[0];\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch _ _ expected actual) -> expected == BOOL && actual == INT

  describe "FOR semantics" $ do
    it "accepts simple FOR over INT" $ do
      let src = "PROGRAM P\nVAR i:INT; x:INT; END_VAR\nFOR i := 0 TO 3 DO x := i; END_FOR\n"
      expectUnitPass src

    it "rejects writing to loop var" $ do
      let src = "PROGRAM P\nVAR i:INT; END_VAR\nFOR i := 0 TO 3 DO i := 1; END_FOR\n"
      expectUnitFailWithDetail @AssignToLoopVar src $
        \(AssignToLoopVar vname _) -> vname == "i"

    it "rejects FOR when loop var is not INT" $ do
      let src = "PROGRAM P\nVAR i:BOOL; END_VAR\nFOR i := 0 TO 1 DO END_FOR\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch vname _ expected _) -> vname == "i" && expected == INT

    it "rejects non-INT (init)" $ do
      let src = "PROGRAM P\nVAR i:INT; END_VAR\nFOR i := TRUE TO 1 DO END_FOR\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

    it "rejects non-INT (end)" $ do
      let src = "PROGRAM P\nVAR i:INT; END_VAR\nFOR i := 0 TO FALSE DO END_FOR\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

    it "rejects non-INT (by)" $ do
      let src = "PROGRAM P\nVAR i:INT; END_VAR\nFOR i := 0 TO 10 BY TRUE DO END_FOR\n"
      expectUnitFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

  describe "ENUM semantics" $ do
    it "accepts assignment and equality" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \c := Color.Red;\n\
              \IF c = Color.Red THEN c := Color.Green; END_IF\n"
            ]
      expectUnitsPass srcs

    it "rejects unknown enum value" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \c := Color.Blue;\n"
            ]
      expectUnitsFailWithDetail @UnknownEnumMember srcs $
        \(UnknownEnumMember ename mname _) -> ename == "Color" && mname == "Blue"

    it "rejects Type.Ctor when Type is not enum" $ do
      let srcs =
            [ "TYPE MyInt : INT; END_TYPE\n",
              "PROGRAM P\nVAR x: INT; END_VAR\n\
              \x := MyInt.X;\n"
            ]
      expectUnitsFailWithDetail @NotAnEnum srcs $
        \(NotAnEnum name _ _) -> name == "MyInt"

  describe "CASE with ENUM semantics" $ do
    it "accepts CASE with enum selectors" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; x: INT; END_VAR\n\
              \CASE c OF Color.Red: x := 1; ELSE x := 0; END_CASE\n"
            ]
      expectUnitsPass srcs

    it "rejects mixing INT selector for enum scrutinee" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR c:Color; x:INT; END_VAR\n\
              \CASE c OF 0: x := 1; END_CASE\n"
            ]
      expectUnitsFail @TypeMismatch' srcs

  describe "Enum default init" $ do
    it "fills first enumerator as default init" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n"
            ]
      expectParsedUnits srcs $ \us ->
        case (elaborateUnits M.empty us :: VEither AllErrs [Unit]) of
          VLeft e -> expectationFailure (show e)
          VRight units ->
            case [v | UProgram (Program _ (VarDecls [v]) _) <- units] of
              [v] ->
                case varInit v of
                  Just (EField (EVar ty) ctor) -> do
                    locVal ty `shouldBe` "Color"
                    locVal ctor `shouldBe` "Red"
                  other ->
                    expectationFailure ("unexpected init: " <> show other)
              _ ->
                expectationFailure "expected exactly one PROGRAM with one variable"

  describe "nominal identity (ENUM)" $ do
    it "rejects assigning different enum types" $ do
      let srcs =
            [ "TYPE A : (X,Y); END_TYPE\n",
              "TYPE B : (X,Y); END_TYPE\n",
              "PROGRAM P\nVAR a: A; b: B; END_VAR\n\
              \a := B.X;\n"
            ]
      expectUnitsFailWithDetail @TypeMismatch srcs $
        \(TypeMismatch _ _ (Named nA) (Named nB)) -> locVal nA == "A" && locVal nB == "B"

    it "accepts equality for same enum name" $ do
      let srcs =
            [ "TYPE A : (X); END_TYPE\n",
              "TYPE B : (X); END_TYPE\n",
              "PROGRAM P\nVAR a: A; END_VAR\n\
              \IF a = A.X THEN a := A.X; END_IF\n"
            ]
      expectUnitsPass srcs

    it "rejects equality for different enum name" $ do
      let srcs =
            [ "TYPE A : (X); END_TYPE\n",
              "TYPE B : (X); END_TYPE\n",
              "PROGRAM P\nVAR a: A; END_VAR\n\
              \IF a = B.X THEN a := A.X; END_IF\n"
            ]
      expectUnitsFailWithDetail @TypeMismatch' srcs $
        \(TypeMismatch' (Named nB)) -> locVal nB == "B"

  describe "Nominal type equality" $ do
    -- ダミー位置つき Identifier
    let idOf t =
          let p = initialPos "<test>"
           in Loc (Span p p) t

    -- ダミー TypeEnv:
    --  - Color は列挙型
    --  - A, B は別々の列挙型
    --  - MyInt は INT の別名（別名の構造同値も確認用）
    let tenv :: M.Map Text STType
        tenv =
          M.fromList
            [ ("Color", Enum [(idOf "Red", Just (EINT 0)), (idOf "Green", Just (EINT 1))]),
              ("A", Enum [(idOf "X", Just (EINT 2))]),
              ("B", Enum [(idOf "Y", Just (EINT 3))]),
              ("MyInt", INT)
            ]

    it "treats base types structurally" $ do
      nominalEq tenv INT INT `shouldBe` True
      nominalEq tenv BOOL BOOL `shouldBe` True
      nominalEq tenv INT BOOL `shouldBe` False

    it "treats enums nominally by name" $ do
      nominalEq tenv (Named (idOf "Color")) (Named (idOf "Color")) `shouldBe` True
      nominalEq tenv (Named (idOf "A")) (Named (idOf "B")) `shouldBe` False

    it "does not consider Named equal to raw Enum body" $ do
      nominalEq tenv (Named (idOf "Color")) (Enum [(idOf "Red", Just (EINT 0)), (idOf "Green", Just (EINT 1))]) `shouldBe` True

    it "resolves aliases to base types structurally (e.g. MyInt ~ INT)" $ do
      nominalEq tenv (Named (idOf "MyInt")) INT `shouldBe` True
      nominalEq tenv INT (Named (idOf "MyInt")) `shouldBe` True

  describe "ENUM explicit values (semantics integration)" $ do
    it "accepts assignment to an enum with explicit values" $ do
      let srcs =
            [ "TYPE Color : (Red := 0, Green := 2); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \c := Color.Green;\n"
            ]
      expectUnitsPass srcs

    it "rejects unknown enum value even with explicit values" $ do
      let srcs =
            [ "TYPE Color : (Red := 0, Green := 2); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \c := Color.Blue;\n"
            ]
      expectUnitsFailWithDetail @UnknownEnumMember srcs $
        \(UnknownEnumMember nA nB _) -> nA == "Color" && nB == "Blue"

  describe "ENUM explicit values with expressions (semantics integration)" $ do
    it "accepts assignment using an enumerator with explicit expr value" $ do
      let srcs =
            [ "TYPE Color : (Red := 1+2*3, Green := (1+2)*3); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \c := Color.Green;\n"
            ]
      expectUnitsPass srcs

    it "still rejects unknown ctor even with explicit expr values" $ do
      let srcs =
            [ "TYPE Color : (Red := 0, Green := 2); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \c := Color.Blue;\n"
            ]
      expectUnitsFailWithDetail @UnknownEnumMember srcs $
        \(UnknownEnumMember nA nB _) -> nA == "Color" && nB == "Blue"

  describe "MOD/XOR semantics" $ do
    let base = "PROGRAM P\nVAR x:INT; b:BOOL; END_VAR\n"
    it "accepts MOD on INT" $
      expectUnitPass (base <> "x := 7 MOD 4;\n")

    it "rejects MOD type mismatch" $
      expectUnitFailWithDetail @TypeMismatch' (base <> "x := TRUE MOD 2;\n") $
        \(TypeMismatch' actual) -> actual == BOOL

    it "accepts XOR on BOOL" $
      expectUnitPass (base <> "b := TRUE XOR FALSE;\n")

    it "rejects XOR type mismatch" $
      expectUnitFail @TypeMismatch' (base <> "b := 1 XOR 0;\n")

  describe "REAL semantics (minimal)" $ do
    it "allows arithmetic mixing INT and REAL; result is REAL" $ do
      let src = "PROGRAM P\nVAR r:REAL; x:INT; END_VAR\nr := x + 2.5;\n"
      expectUnitPass src

    it "rejects MOD on REAL" $ do
      let src = "PROGRAM P\nVAR r:REAL; END_VAR\nr := 5.0 MOD 2.0;\n"
      expectUnitFail @TypeMismatch' src

    it "rejects NOT on REAL" $ do
      let src = "PROGRAM P\nVAR r:REAL; END_VAR\nIF NOT 1.0 THEN r := 0.0; END_IF\n"
      expectUnitFail @TypeMismatch' src

  describe "LREAL semantics" $ do
    it "accepts assignment of LREAL typed literal and equality" $ do
      let src =
            "PROGRAM P\nVAR x: LREAL; END_VAR\n\
            \x := LREAL#1.5;\n\
            \IF x = LREAL#1.5 THEN x := LREAL#2.0; END_IF\n"
      expectUnitPass src

    it "arith/comp require same floating type" $ do
      let src =
            "PROGRAM P\nVAR a:LREAL; b:LREAL; END_VAR\n\
            \a := LREAL#1.0 + LREAL#2.0;\n\
            \IF a >= LREAL#3.0 THEN a := a; END_IF\n"
      expectUnitPass src

  describe "numeric promotion (INT → REAL)" $ do
    it "allows assigning INT to REAL" $ do
      let src = "PROGRAM P\nVAR a: REAL; END_VAR\na := 1;\n"
      expectUnitPass src

    it "allows IF with mixed REAL/INT comparison" $ do
      let src = "PROGRAM P\nVAR x: INT; END_VAR\nIF 1.0 < 2 THEN x := 0; END_IF\n"
      expectUnitPass src

    it "rejects assigning REAL to INT" $ do
      let src = "PROGRAM P\nVAR i: INT; END_VAR\ni := 1.5;\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch _ _ expected actual) -> expected == INT && actual == REAL

    it "still allows arithmetic mixing (result REAL) and assign to REAL" $ do
      let src = "PROGRAM P\nVAR a: REAL; END_VAR\na := 1 + 2.0;\n"
      expectUnitPass src

  describe "numeric promotion (INT → LREAL)" $ do
    it "allows assigning INT to LREAL" $ do
      let src = "PROGRAM P\nVAR a: LREAL; END_VAR\na := 1;\n"
      expectUnitPass src

    it "allows initializer INT to LREAL" $ do
      let src = "PROGRAM P\nVAR a: LREAL := 1; END_VAR\n"
      expectUnitPass src

    it "rejects assigning LREAL to INT" $ do
      let src = "PROGRAM P\nVAR i: INT; END_VAR\ni := 1.0;\n" -- 小数は REAL/LREAL 扱い → INT へは不可
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch _ _ expected actual) -> expected == INT && actual == REAL

  describe "STRING default init (semantics)" $ do
    it "fills empty string for STRING" $ do
      let src = "PROGRAM P\nVAR s: STRING; END_VAR\n"
      expectParsed src $ \prog ->
        let v :: VEither AllErrs Program
            v = elaborateProgramTest prog
         in v `shouldSatisfy` \case
              VRight (Program _ (VarDecls [val]) _) -> varInit val == Just (ESTRING "")
              _ -> False

    it "fills empty string for STRING(80) and STRING[32]" $ do
      let s1 = "PROGRAM P\nVAR s: STRING(80); END_VAR\n"
          s2 = "PROGRAM P\nVAR t: STRING[32]; END_VAR\n"
      expectParsed s1 $ \p1 ->
        let v :: VEither AllErrs Program
            v = elaborateProgramTest p1
         in v `shouldSatisfy` \case
              VRight (Program _ (VarDecls [val]) _) -> varInit val == Just (ESTRING "")
              _ -> False

      expectParsed s2 $ \p2 ->
        let v :: VEither AllErrs Program
            v = elaborateProgramTest p2
         in v `shouldSatisfy` \case
              VRight (Program _ (VarDecls [val]) _) -> varInit val == Just (ESTRING "")
              _ -> False

  describe "STRING semantics" $ do
    it "allows comparison" $ do
      let src = "PROGRAM P\nVAR b: BOOL; END_VAR\nb := 'a' < 'b';\n"
      expectUnitPass src

    it "rejects mixing STRING with INT in +" $ do
      -- 連結は不可
      let src = "PROGRAM P\nVAR s: STRING; END_VAR\ns := 'a' + 1;\n"
      expectUnitFail @TypeMismatch' src

  describe "CASE type consistency (baseline)" $ do
    it "accepts INT scrutinee with INT selectors (value/range/ELSE)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 0: ; 1..3: ; ELSE ; END_CASE\n"
      expectUnitPass src

    it "rejects enum selector when scrutinee is INT" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR x: INT; END_VAR\n\
              \CASE x OF Color.Red: ; END_CASE\n"
            ]
      expectUnitsFail @TypeMismatch' srcs

    it "accepts enum scrutinee with matching enum constructors" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \CASE c OF Color.Red: ; Color.Green: ; END_CASE\n"
            ]
      expectUnitsPass srcs

    it "rejects mismatched enum type in selector" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "TYPE Shape : (Circle); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \CASE c OF Shape.Circle: ; END_CASE\n"
            ]
      expectUnitsFail @TypeMismatch' srcs

    it "rejects BOOL scrutinee (only INT or enum allowed)" $ do
      let src =
            "PROGRAM P\nVAR b: BOOL; END_VAR\n\
            \CASE b OF 0: ; END_CASE\n"
      expectUnitFail @TypeMismatch' src

  describe "CASE selector validity (INT)" $ do
    it "accepts disjoint values and ranges across arms" $ do
      -- 全て不交差：{0}, {2..4}, {5}
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 0: ; 2..4: ; 5: ; END_CASE\n"
      expectUnitPass src

    it "rejects a range with low > high (e.g., 5..3)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 5..3: ; END_CASE\n"
      expectUnitFail @InvalidCaseRange src

    it "rejects duplicate literal within the same arm (1,1)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1,1: ; END_CASE\n"
      expectUnitFail @OverlappingCase src

    it "rejects overlap value vs range within the same arm (1,1..3)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1,1..3: ; END_CASE\n"
      expectUnitFail @OverlappingCase src

    it "rejects overlap range vs range within the same arm (1..3,3..5)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1..3,3..5: ; END_CASE\n"
      expectUnitFail @OverlappingCase src

    it "rejects overlap across different arms (1..3: ; 3..5: ;)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1..3: ; 3..5: ; END_CASE\n"
      expectUnitFail @OverlappingCase src

    it "accepts touching but non-overlapping ranges (1..3, 4..5)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1..3: ; 4..5: ; END_CASE\n"
      expectUnitPass src

  describe "CASE selectors with constant expressions (semantics)" $ do
    it "accepts INT labels that evaluate from constant variable" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \VAR CONSTANT k0: INT := 5; k1: INT := 7; END_VAR\n\
            \CASE x OF k0: ; k0..k1: ; END_CASE\n"
      expectUnitPass src

    it "rejects INT labels that evaluate from constant expressions" $ do
      -- k0, k1 はコンパイル時定数
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \VAR CONSTANT k0: INT := 5; k1: INT := 7; END_VAR\n\
            \CASE x OF k0+1: ; k0..(k1-1): ; END_CASE\n"
      expectUnitFail @NonConstantExpr src

    it "rejects non-constant variable in a label" $ do
      let src =
            "PROGRAM P\nVAR x: INT; y: INT; END_VAR\n\
            \CASE x OF y+1: ; END_CASE\n"
      expectUnitFail @NonConstantExpr src

    it "accepts enum labels from a constant of that enum" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \VAR CONSTANT kc: Color := Color.Red; END_VAR\n\
              \CASE c OF kc: ; END_CASE\n"
            ]
      expectUnitsPass srcs

    it "rejects INT constant expression for enum scrutinee" $ do
      let srcs =
            [ "TYPE Color : (Red, Green); END_TYPE\n",
              "PROGRAM P\nVAR c: Color; END_VAR\n\
              \CASE c OF 1+2: ; END_CASE\n"
            ]
      expectUnitsFail @TypeMismatch' srcs

    it "rejects indexer in label (not a constant expression)" $ do
      -- y は非定数。v[0] は「式」だがコンパイル時定数ではない。
      let src =
            "PROGRAM P\nVAR x: INT; v: ARRAY[0..1] OF INT; END_VAR\n\
            \CASE x OF v[0]: ; END_CASE\n"
      expectUnitFail @NonConstantExpr src

  -- describe "CASE unreachable arms (semantics)" $ do
  --   -- INT: 完全に覆われている腕（値）
  --   it "flags a later singleton value fully covered by a previous range" $ do
  --     let src =
  --           "PROGRAM P\nVAR x: INT; END_VAR\n\
  --           \CASE x OF 1..10: ; 5: ; END_CASE\n"
  --     expectParsed src $ \p ->
  --       elaborateProgram p
  --         `shouldFailWith1` ( \case
  --                               UnreachableCaseArm {} -> True
  --                               _ -> False
  --                           )

  --   -- INT: 完全に覆われている腕（範囲）
  --   it "flags a later range fully covered by a previous range" $ do
  --     let src =
  --           "PROGRAM P\nVAR x: INT; END_VAR\n\
  --           \CASE x OF 0..10: ; 2..8: ; END_CASE\n"
  --     expectParsed src $ \p ->
  --       elaborateProgram p
  --         `shouldFailWith1` ( \case
  --                               UnreachableCaseArm {} -> True
  --                               _ -> False
  --                           )

  --   -- ENUM: 先に同じ列挙子が出ていて、後続が実行不能
  --   it "flags a duplicate enum label as unreachable" $ do
  --     let src =
  --           "TYPE Color : (Red, Green); END_TYPE\n\
  --           \PROGRAM P\nVAR c: Color; END_VAR\n\
  --           \CASE c OF Color.Red: ; Color.Green: ; Color.Red: ; END_CASE\n"
  --     expectParsedUnit src $ \u ->
  --       elaborateUnit u
  --         `shouldFailWith1` ( \case
  --                               UnreachableCaseArm {} -> True
  --                               _ -> False
  --                           )

  --   -- ENUM: すべての列挙子をカバー済みなら ELSE は実行不能
  --   it "flags ELSE as unreachable when all enum values are already covered" $ do
  --     let src =
  --           "TYPE Color : (Red, Green); END_TYPE\n\
  --           \PROGRAM P\nVAR c: Color; END_VAR\n\
  --           \CASE c OF Color.Red: ; Color.Green: ; ELSE c := Color.Red; END_CASE\n"
  --     expectParsedUnit src $ \u ->
  --       elaborateUnit u
  --         `shouldFailWith1` ( \case
  --                               UnreachableCaseArm {} -> True
  --                               _ -> False
  --                           )

  describe "extended integer & bitstring types (semantics)" $ do
    it "fills default init to 0 for all extended int/bitstring types" $ do
      let src =
            "PROGRAM P\nVAR\n\
            \  s:SINT; i:INT; d:DINT; l:LINT;\n\
            \  us:USINT; ui:UINT; ud:UDINT; ul:ULINT;\n\
            \  b:BYTE; w:WORD; dw:DWORD; lw:LWORD;\n\
            \END_VAR\n"
      expectParsed src $ \prog ->
        let p :: VEither AllErrs Program
            p = elaborateProgramTest prog
         in p `shouldSatisfy` \case
              VRight (Program _ (VarDecls vs) _) -> all (\v -> varInit v == Just (EINT 0)) vs
              _ -> False

    it "accepts assigning integer literal to each extended int/bitstring variable" $ do
      let src =
            "PROGRAM P\nVAR\n\
            \  s:SINT; i:INT; d:DINT; l:LINT;\n\
            \  us:USINT; ui:UINT; ud:UDINT; ul:ULINT;\n\
            \  b:BYTE; w:WORD; dw:DWORD; lw:LWORD;\n\
            \END_VAR\n\
            \s := 0; i := 0; d := 0; l := 0;\n\
            \us := 0; ui := 0; ud := 0; ul := 0;\n\
            \b := 0; w := 0; dw := 0; lw := 0;\n"
      expectUnitPass src

    it "rejects type mismatch (BOOL -> USINT)" $ do
      let src =
            "PROGRAM P\nVAR us:USINT; END_VAR\n\
            \us := TRUE;\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch vname _ _ _) -> vname == "us"

  describe "extended int/bitstring literals into narrower types (semantics)" $ do
    it "accepts 0 into every int/bit type" $ do
      let src =
            "PROGRAM P\nVAR\n\
            \  s:SINT := 0; us:USINT := 0; b:BYTE := 0;\n\
            \  i:INT := 0;  ui:UINT  := 0;  w:WORD := 0;\n\
            \  d:DINT := 0; ud:UDINT := 0; dw:DWORD := 0;\n\
            \  l:LINT := 0; ul:ULINT := 0; lw:LWORD := 0;\n\
            \END_VAR\n"
      expectUnitPass src

    it "rejects out-of-range literal (e.g., 256 -> USINT)" $ do
      let src = "PROGRAM P\nVAR us:USINT; END_VAR\nus := 256;\n"
      expectUnitFailWithDetail @OutOfRange src $
        \(OutOfRange vname tgt _ _) -> vname == "us" && tgt == USINT

  describe "Based integer literals (semantics)" $ do
    -- OK: USINT に 2#1111_1111 (255)
    it "accepts assigning 2#1111_1111 to USINT (255)" $ do
      let src =
            "PROGRAM P\nVAR u: USINT; END_VAR\n\
            \u := 2#1111_1111;\n"
      expectUnitPass src

    -- NG: SINT に 16#80 (128) は範囲外
    it "rejects assigning 16#80 to SINT (out of range)" $ do
      let src =
            "PROGRAM P\nVAR s: SINT; END_VAR\n\
            \s := 16#80;\n"
      expectUnitFail @OutOfRange src

    -- NG: UINT に 16#10000 (65536) は範囲外
    it "rejects assigning 16#10000 to UINT (out of range)" $ do
      let src =
            "PROGRAM P\nVAR x: UINT; END_VAR\n\
            \x := 16#10000;\n"
      expectUnitFail @OutOfRange src

    -- OK: UDINT に 16#FFFF_FFFF (4294967295)
    it "accepts assigning 16#FFFF_FFFF to UDINT" $ do
      let src =
            "PROGRAM P\nVAR d: UDINT; END_VAR\n\
            \d := 16#FFFF_FFFF;\n"
      expectUnitPass src

    -- 単項マイナス付き: SINT に -16#80 は -128 → これは範囲内
    it "accepts assigning -16#80 to SINT (-128)" $ do
      let src =
            "PROGRAM P\nVAR s: SINT; END_VAR\n\
            \s := -16#80;\n"
      expectUnitPass src

  describe "Numeric underscores (semantics)" $ do
    it "treats underscored numeric literals as equal to their plain values" $ do
      let src =
            "PROGRAM P\nVAR x: INT; y: REAL; END_VAR\n\
            \x := 1_2_3_4;\n\
            \y := 3_._1_4;\n"
      expectUnitPass src

    it "reports OutOfRange for underscored decimal literal" $ do
      -- 128 は SINT の範囲外
      let src = "PROGRAM P\nVAR s: SINT; END_VAR\ns := 1_2_8;\n"
      expectUnitFailWithDetail @OutOfRange src $
        \(OutOfRange _ tgt _ _) -> tgt == SINT

    it "reports OutOfRange for underscored based literal" $ do
      -- 2#1_0000_0000 = 256 は SINT の範囲外
      let src = "PROGRAM P\nVAR s: SINT; END_VAR\ns := 2#1_0000_0000;\n"
      expectUnitFailWithDetail @OutOfRange src $
        \(OutOfRange _ tgt _ _) -> tgt == SINT

  --   describe "CASE over extended ints/bitstrings (semantics)" $ do
  --     -- 受理系 ------------------------------------------

  --     it "accepts SINT scrutinee with signed literal/range labels" $ do
  --       let src =
  --             "PROGRAM P\nVAR s:SINT; END_VAR\n"
  --               <> "CASE s OF -3..-1: ; 0,1,2: ; END_CASE\n"
  --       shouldSucceedV $ elaborateUnit src

  --     it "accepts USINT scrutinee with hex labels and ranges" $ do
  --       let src =
  --             "PROGRAM P\nVAR u:USINT; END_VAR\n"
  --               <> "CASE u OF 16#FF: ; 0..16#0F: ; END_CASE\n"
  --       shouldSucceedV $ elaborateUnit src

  --     it "accepts BYTE scrutinee with based-int selectors" $ do
  --       let src =
  --             "PROGRAM P\nVAR b:BYTE; END_VAR\n"
  --               <> "CASE b OF 2#1010_1010: ; 8#77: ; END_CASE\n"
  --       shouldSucceedV $ elaborateUnit src

  --     it "accepts WORD/DWORD/LWORD scrutinee with small hex labels" $ do
  --       -- 小さめの値で十分（意味は型側で担保）
  --       let src =
  --             "PROGRAM P\n"
  --               <> "VAR w:WORD; dw:DWORD; lw:LWORD; END_VAR\n"
  --               <> "CASE w  OF 16#00FF: ; END_CASE\n"
  --               <> "CASE dw OF 16#00FF_FF00: ; END_CASE\n"
  --               <> "CASE lw OF 16#0000_FFFF_0000_FFFF: ; END_CASE\n"
  --       shouldSucceedV $ elaborateUnit src

  --     it "accepts DINT/UDINT scrutinee with decimal and hex ranges" $ do
  --       let src =
  --             "PROGRAM P\nVAR d:DINT; u:UDINT; END_VAR\n"
  --               <> "CASE d OF -10..10: ; END_CASE\n"
  --               <> "CASE u OF 0..16#FFFF: ; END_CASE\n"
  --       shouldSucceedV $ elaborateUnit src

  -- --   -- 拒否系（型／範囲チェック）-----------------------

  -- --   it "rejects negative selector for USINT (unsigned) scrutinee" $ do
  -- --     let src =
  -- --           "PROGRAM P\nVAR u:USINT; END_VAR\n"
  -- --             <> "CASE u OF -1: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u'
  -- --         `shouldFailWith1` ( \case
  -- --                               OutOfRange {dName = "CASE"} -> True
  -- --                               _ -> False
  -- --                           )

  -- --   it "rejects enum selector for SINT scrutinee" $ do
  -- --     let src =
  -- --           "TYPE Color : (Red, Green); END_TYPE\n"
  -- --             <> "PROGRAM P\nVAR s:SINT; END_VAR\n"
  -- --             <> "CASE s OF Color.Red: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u'
  -- --         `shouldFailWith1` ( \case
  -- --                               OpTypeMismatch {op = "CASE"} -> True
  -- --                               _ -> False
  -- --                           )

  -- --   it "rejects overlapping ranges for DINT scrutinee" $ do
  -- --     let src =
  -- --           "PROGRAM P\nVAR d:DINT; END_VAR\n"
  -- --             <> "CASE d OF 1..3: ; 3..5: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u'
  -- --         `shouldFailWith1` ( \case
  -- --                               OverlappingCase -> True
  -- --                               _ -> False
  -- --                           )

  -- --   it "rejects mixing signedness via literal out-of-domain for UINT" $ do
  -- --     let src =
  -- --           "PROGRAM P\nVAR u:UINT; END_VAR\n"
  -- --             <> "CASE u OF -2..-1: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u'
  -- --         `shouldFailWith1` ( \case
  -- --                               OutOfRange {dName = "CASE"} -> True
  -- --                               _ -> False
  -- --                           )

  -- --   it "rejects REAL selector for INT-family scrutinee" $ do
  -- --     let src =
  -- --           "PROGRAM P\nVAR i:INT; END_VAR\n"
  -- --             <> "CASE i OF 1.0: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u'
  -- --         `shouldFailWith1` ( \case
  -- --                               OpTypeMismatch {op = "CASE"} -> True
  -- --                               _ -> False
  -- --                           )

  -- -- describe "CASE over extended ints/bitstrings (semantics)" $ do
  -- --   -- 受理系 -----------------------------------------------------

  -- --   it "accepts SINT scrutinee with signed literal/range labels" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR s:SINT; END_VAR\n\
  -- --           \CASE s OF -3..-1: ; 0,1,2: ; END_CASE\n"
  -- --     shouldSucceedV $ elaborateUnit src

  -- --   it "accepts USINT scrutinee with hex labels and ranges" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR u:USINT; END_VAR\n\
  -- --           \CASE u OF 16#FF: ; 0..16#0F: ; END_CASE\n"
  -- --     shouldSucceedV $ elaborateUnit src

  -- --   it "accepts BYTE scrutinee with based-int selectors" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR b:BYTE; END_VAR\n\
  -- --           \CASE b OF 2#1010_1010: ; 8#77: ; END_CASE\n"
  -- --     shouldSucceedV $ elaborateUnit src

  -- --   it "accepts WORD/DWORD/LWORD scrutinee with small hex labels" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR w:WORD; dw:DWORD; lw:LWORD; END_VAR\n\
  -- --           \CASE w  OF 16#00FF: ; END_CASE\n\
  -- --           \CASE dw OF 16#00FF_FF00: ; END_CASE\n\
  -- --           \CASE lw OF 16#0000_FFFF_0000_FFFF: ; END_CASE\n"
  -- --     shouldSucceedV $ elaborateUnit src

  -- --   it "accepts DINT/UDINT scrutinee with decimal and hex ranges" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR d:DINT; u:UDINT; END_VAR\n\
  -- --           \CASE d OF -10..10: ; END_CASE\n\
  -- --           \CASE u OF 0..16#FFFF: ; END_CASE\n"
  -- --     shouldSucceedV $ elaborateUnit src

  -- --   -- 拒否系 -----------------------------------------------------

  -- --   it "rejects negative selector for USINT (unsigned) scrutinee" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR u:USINT; END_VAR\n\
  -- --           \CASE u OF -1: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u' `shouldFailWith1` outOfRange "CASE"

  -- --   it "rejects enum selector for SINT scrutinee" $ do
  -- --     let src =
  -- --           "TYPE Color : (Red, Green); END_TYPE\n\
  -- --           \PROGRAM P\n\
  -- --           \VAR s:SINT; END_VAR\n\
  -- --           \CASE s OF Color.Red: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u' `shouldFailWith1` opTypeMismatch "CASE"

  -- --   it "rejects overlapping ranges for DINT scrutinee" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR d:DINT; END_VAR\n\
  -- --           \CASE d OF 1..3: ; 3..5: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u' `shouldFailWith1` isOverlappingCase

  -- --   it "rejects mixing signedness via literal out-of-domain for UINT" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR u:UINT; END_VAR\n\
  -- --           \CASE u OF -2..-1: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u' `shouldFailWith1` outOfRange "CASE"

  -- --   it "rejects REAL selector for INT-family scrutinee" $ do
  -- --     let src =
  -- --           "PROGRAM P\n\
  -- --           \VAR i:INT; END_VAR\n\
  -- --           \CASE i OF 1.0: ; END_CASE\n"
  -- --     expectParsedUnit src $ \u' ->
  -- --       elaborateUnit u' `shouldFailWith1` opTypeMismatch "CASE"

  -- ========== Based int semantics: assignment + range checks ==========
  describe "Based integer literals (semantics)" $ do
    it "BYTE accepts 16#FF and rejects 16#100 (OutOfRange)" $ do
      let ok =
            "PROGRAM P\nVAR x: BYTE; END_VAR\n\
            \x := 16#FF;\n"
      expectUnitPass ok

      let ng =
            "PROGRAM P\nVAR x: BYTE; END_VAR\n\
            \x := 16#100;\n"
      expectUnitFail @OutOfRange ng

    it "USINT accepts 2#1111_1111 and rejects 2#1_0000_0000" $ do
      let ok =
            "PROGRAM P\nVAR u: USINT; END_VAR\n\
            \u := 2#1111_1111;\n" -- 255
      expectUnitPass ok

      let ng =
            "PROGRAM P\nVAR u: USINT; END_VAR\n\
            \u := 2#1_0000_0000;\n" -- 256
      expectUnitFail @OutOfRange ng

    it "INT accepts 16#7FFF and -16#8000; rejects 16#8000 and -16#8001" $ do
      let ok1 =
            "PROGRAM P\nVAR z: INT; END_VAR\n\
            \z := 16#7FFF;\n" --  32767 OK
      expectUnitPass ok1

      let ok2 =
            "PROGRAM P\nVAR z: INT; END_VAR\n\
            \z := -16#8000;\n" -- -32768 OK
      expectUnitPass ok2

      let ng1 =
            "PROGRAM P\nVAR z: INT; END_VAR\n\
            \z := 16#8000;\n" --  32768 NG
      expectUnitFail @OutOfRange ng1

      let ng2 =
            "PROGRAM P\nVAR z: INT; END_VAR\n\
            \z := -16#8001;\n" -- -32769 NG
      expectUnitFail @OutOfRange ng2

    it "UDINT accepts 16#FFFF_FFFF and rejects 16#1_0000_0000" $ do
      let ok =
            "PROGRAM P\nVAR d: UDINT; END_VAR\n\
            \d := 16#FFFF_FFFF;\n" -- 4294967295 OK
      expectUnitPass ok

      let ng =
            "PROGRAM P\nVAR d: UDINT; END_VAR\n\
            \d := 16#1_0000_0000;\n" -- 4294967296 NG
      expectUnitFail @OutOfRange ng

  describe "Bitwise operators over bitstrings (semantics)" $ do
    it "accepts BYTE AND/OR/XOR" $ do
      let src =
            "PROGRAM P\n\
            \VAR b1: BYTE; b2: BYTE; b3: BYTE; END_VAR\n\
            \b1 := b1 AND b2; b2 := b2 OR b3; b3 := b1 XOR b2;\n"
      expectUnitPass src

    it "accepts WORD NOT (unary)" $ do
      let src =
            "PROGRAM P\n\
            \VAR w: WORD; END_VAR\n\
            \w := NOT w;\n"
      expectUnitPass src

    it "accepts DWORD/LWORD with AND/OR/XOR" $ do
      let src =
            "PROGRAM P\n\
            \VAR dw: DWORD; dw2: DWORD; lw: LWORD; lw2: LWORD; END_VAR\n\
            \dw := dw AND dw2; dw := dw OR dw2; dw := dw XOR dw2;\n\
            \lw := lw AND lw2; lw := lw OR  lw2; lw := lw XOR lw2;\n"
      expectUnitPass src

    it "accepts BOOL with AND/OR/NOT" $ do
      let src =
            "PROGRAM P\n\
            \VAR b1: BOOL; b2: BOOL; END_VAR\n\
            \b1 := b1 AND NOT b2; b2 := b1 OR b2;\n"
      expectUnitPass src

    -- 拒否系：INTなど算術整数は対象外
    it "rejects AND on INT (bitwise only for BOOL/BITSTRING)" $ do
      let src =
            "PROGRAM P\n\
            \VAR i: INT; END_VAR\n\
            \i := i AND i;\n"
      expectUnitFail @TypeMismatch' src

    it "rejects NOT on INT" $ do
      let src =
            "PROGRAM P\n\
            \VAR i: INT; END_VAR\n\
            \i := NOT i;\n"
      expectUnitFail @TypeMismatch' src

    it "rejects mixing sizes: WORD AND BYTE" $ do
      let src =
            "PROGRAM P\n\
            \VAR w: WORD; b: BYTE; END_VAR\n\
            \w := w AND b;\n"
      expectUnitFail @TypeMismatch' src

    it "rejects OR with REAL" $ do
      let src =
            "PROGRAM P\n\
            \VAR w: WORD; r: REAL; END_VAR\n\
            \w := w OR r;\n"
      expectUnitFail @TypeMismatch' src

    it "rejects BOOL with WORD (no cross-kind mixing)" $ do
      let src =
            "PROGRAM P\n\
            \VAR b: BOOL; w: WORD; END_VAR\n\
            \b := b XOR w;\n"
      expectUnitFail @TypeMismatch' src

  describe "CHAR/WCHAR/WSTRING semantics" $ do
    it "accepts assignment: CHAR <- 'Z', WCHAR <- \"Z\", STRING <- 'hi', WSTRING <- \"hi\"" $ do
      let src =
            "PROGRAM P\n\
            \VAR c: CHAR; wc: WCHAR; s: STRING(10); ws: WSTRING(10); END_VAR\n\
            \c := 'Z';\n\
            \wc := \"Z\";\n\
            \s := 'hi';\n\
            \ws := \"hi\";\n"
      expectUnitPass src

    it "rejects assigning STRING literal to WSTRING variable" $ do
      let src =
            "PROGRAM P\n\
            \VAR ws: WSTRING(10); END_VAR\n\
            \ws := 'abc';\n"
      expectUnitFail @TypeMismatch src

    it "rejects assigning WSTRING literal to STRING variable" $ do
      let src =
            "PROGRAM P\n\
            \VAR s: STRING(10); END_VAR\n\
            \s := \"abc\";\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch vname _ _ _) -> vname == "s"

    it "rejects assigning multi-char STRING literal to CHAR" $ do
      let src =
            "PROGRAM P\n\
            \VAR c: CHAR; END_VAR\n\
            \c := 'AB';\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch vname _ _ _) -> vname == "c"

    it "rejects assigning multi-char WSTRING literal to WCHAR" $ do
      let src =
            "PROGRAM P\n\
            \VAR wc: WCHAR; END_VAR\n\
            \wc := \"AB\";\n"
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch vname _ _ _) -> vname == "wc"

    -- 比較（同型同士はOK、混在はNG）
    it "allows STRING comparisons (=, <>, <, <=, >, >=) within STRING" $ do
      let src =
            "PROGRAM P\n\
            \VAR s1: STRING(10); s2: STRING(10); b: BOOL; END_VAR\n\
            \b := s1 = s2; b := s1 <> s2; b := s1 < s2; b := s1 <= s2; b := s1 > s2; b := s1 >= s2;\n"
      expectUnitPass src

    it "allows WSTRING comparisons within WSTRING" $ do
      let src =
            "PROGRAM P\n\
            \VAR w1: WSTRING(10); w2: WSTRING(10); b: BOOL; END_VAR\n\
            \b := w1 = w2; b := w1 <> w2; b := w1 < w2; b := w1 <= w2; b := w1 > w2; b := w1 >= w2;\n"
      expectUnitPass src

    it "rejects STRING vs WSTRING comparison" $ do
      let src =
            "PROGRAM P\n\
            \VAR s: STRING(10); w: WSTRING(10); b: BOOL; END_VAR\n\
            \b := s = w;\n"
      expectUnitFail @TypeMismatch' src

    -- エスケープの相互排他を意味論側でも軽く確認（パースで弾けない場合に備え）
    it "accepts STRING literal with $' in assignment" $ do
      let src =
            "PROGRAM P\n\
            \VAR s: STRING(10); END_VAR\n\
            \s := 'a$'b';\n"
      expectUnitPass src

    it "accepts WSTRING literal with $\" in assignment" $ do
      let src =
            "PROGRAM P\n\
            \VAR w: WSTRING(10); END_VAR\n\
            \w := \"a$\"b\";\n"
      expectUnitPass src

  describe "Array aggregate initialization (semantics)" $ do
    it "accepts exact-length initializer" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT := [1,2,3]; END_VAR\n"
      expectUnitPass src

    it "accepts too-few initializer elements (pads with defaults)" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT := [1,2]; END_VAR\n"
      expectUnitPass src

    it "rejects too-many initializer elements" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT := [1,2,3,4]; END_VAR\n"
      expectUnitFail @TooManyAggElems src

    it "allows element promotion (INT -> LREAL)" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..1] OF LREAL := [1, 2.0]; END_VAR\n"
      expectUnitPass src

    it "accepts USINT element from -1 via implicit SINT->USINT (pending)" $ do
      pendingWith "literal typing (SINT) & implicit SINT->USINT not implemented yet"
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..0] OF USINT := [-1]; END_VAR\n"
      expectUnitPass src

  describe "Struct aggregate initialization (semantics)" $ do
    it "accepts exact fields in any order" $ do
      let srcs =
            [ "TYPE R : STRUCT x: INT; y: LREAL; END_STRUCT; END_TYPE\n",
              "PROGRAM P\n\
              \VAR r1: R := (x := 1, y := 2.0); r2: R := (y := 2.0, x := 1); END_VAR\n"
            ]
      expectUnitsPass srcs

    it "accepts missing fields (fills omitted with defaults)" $ do
      let srcs =
            [ "TYPE R : STRUCT x: INT; y: INT; END_STRUCT; END_TYPE\n",
              "PROGRAM P\n\
              \VAR r: R := (x := 1); END_VAR\n"
            ]
      expectUnitsPass srcs

    it "rejects unknown field in aggregate" $ do
      let srcs =
            [ "TYPE R : STRUCT x: INT; y: INT; END_STRUCT; END_TYPE\n",
              "PROGRAM P\n\
              \VAR r: R := (x := 1, z := 0); END_VAR\n"
            ]
      expectUnitsFail @UnknownStructMember srcs

    it "allows field-wise promotion (INT -> LREAL)" $ do
      let srcs =
            [ "TYPE R : STRUCT x: INT; y: LREAL; END_STRUCT; END_TYPE\n",
              "PROGRAM P\n\
              \VAR r: R := (x := 1, y := 2); END_VAR\n"
            ]
      expectUnitsPass srcs

  describe "Static index bounds checking (semantics)" $ do
    it "flags out-of-bounds when both bounds and index are constants (literal)" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT; x: INT; END_VAR\n\
            \x := a[3];\n"
      expectUnitFail @IndexOutOfBounds src

    it "flags out-of-bounds when index is VAR CONSTANT" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT; x: INT; END_VAR\n\
            \VAR CONSTANT k: INT := -1; END_VAR\n\
            \x := a[k];\n"
      expectUnitFail @IndexOutOfBounds src

    it "does NOT flag when index is non-constant variable" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT; i: INT; x: INT; END_VAR\n\
            \x := a[i];\n"
      expectUnitPass src

  describe "Date/Time (semantics)" $ do
    -- ====== TIME ======
    it "accepts assignment: TIME <- TIME literal" $ do
      let s = "PROGRAM P\nVAR t: TIME; END_VAR\nt := T#1h2m3s400ms;\n"
      expectUnitPass s

    it "rejects assignment: TIME <- TOD literal" $ do
      let s = "PROGRAM P\nVAR t: TIME; END_VAR\nt := TOD#12:00:00;\n"
      expectUnitFail @TypeMismatch s

    it "allows equality within TIME" $ do
      let s = "PROGRAM P\nVAR t: TIME; END_VAR\nIF T#1s = T#1000ms THEN t := T#0s; END_IF\n"
      expectUnitPass s

    it "rejects equality TIME = INT" $ do
      let s = "PROGRAM P\nVAR b: BOOL; END_VAR\nIF T#1s = 1 THEN b := TRUE; END_IF\n"
      -- TypeMismatch' の actual 片が INT になるはず
      expectUnitFailWithDetail @TypeMismatch' s (\(TypeMismatch' actual) -> actual == INT)

    -- ====== TIME_OF_DAY / TOD ======
    it "accepts assignment: TOD <- TOD literal" $ do
      let s = "PROGRAM P\nVAR x: TOD; END_VAR\nx := TOD#23:59:59.999;\n"
      expectUnitPass s

    it "rejects assignment: TOD <- TIME literal" $ do
      let s = "PROGRAM P\nVAR x: TOD; END_VAR\nx := T#1s;\n"
      expectUnitFail @TypeMismatch s

    it "allows equality within TOD" $ do
      let s = "PROGRAM P\nVAR b: BOOL; END_VAR\nIF TOD#12:34:56 = TOD#12:34:56 THEN b := TRUE; END_IF\n"
      expectUnitPass s

    -- ====== DATE / D ======
    it "accepts assignment: DATE <- DATE literal" $ do
      let s = "PROGRAM P\nVAR d: DATE; END_VAR\nd := D#1970-01-01;\n"
      expectUnitPass s

    it "rejects assignment: DATE <- DT literal" $ do
      let s = "PROGRAM P\nVAR d: DATE; END_VAR\nd := DT#1970-01-01-00:00:00;\n"
      expectUnitFail @TypeMismatch s

    it "allows equality within DATE" $ do
      let s = "PROGRAM P\nVAR b: BOOL; END_VAR\nIF D#2025-11-03 = DATE#2025-11-03 THEN b := TRUE; END_IF\n"
      expectUnitPass s

    -- ====== DATE_AND_TIME / DT ======
    it "accepts assignment: DT <- DT literal" $ do
      let s = "PROGRAM P\nVAR dt: DT; END_VAR\ndt := DT#1999-12-31-23:59:59.999;\n"
      expectUnitPass s

    it "rejects assignment: DT <- DATE literal" $ do
      let s = "PROGRAM P\nVAR dt: DT; END_VAR\ndt := D#1999-12-31;\n"
      expectUnitFail @TypeMismatch s

    it "allows equality within DT" $ do
      let s = "PROGRAM P\nVAR b: BOOL; END_VAR\nIF DT#1999-12-31-23:59:59 = DATE_AND_TIME#1999-12-31-23:59:59 THEN b := TRUE; END_IF\n"
      expectUnitPass s

    -- ====== デフォルト初期化（中身までは縛らず、Just かどうかだけ確認） ======
    it "fills default init for TIME/TOD/DATE/DT when missing" $ do
      let s =
            "PROGRAM P\n\
            \VAR t: TIME; tod: TOD; d: DATE; dt: DT; END_VAR\n"
      case parseProgram s of
        Left e -> expectationFailure (show e)
        Right p ->
          case elaborateProgramTest p :: VEither AllErrs Program of
            VLeft es -> expectationFailure ("elaboration failed: " <> show es)
            VRight (Program _ (VarDecls vs) _) -> do
              let lookupInit nm = varInit =<< find (\v -> locVal (varName v) == nm) vs
              isJust (lookupInit "t") `shouldBe` True
              isJust (lookupInit "tod") `shouldBe` True
              isJust (lookupInit "d") `shouldBe` True
              isJust (lookupInit "dt") `shouldBe` True

  describe "Static index bounds checking (semantics) revisited" $ do
    it "flags OOB on LHS when index is VAR CONSTANT" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT; END_VAR\n\
            \VAR CONSTANT k: INT := -1; END_VAR\n\
            \a[k] := 0;\n"
      expectUnitFail @IndexOutOfBounds src

    it "accepts LHS index when VAR CONSTANT is in range" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT; END_VAR\n\
            \VAR CONSTANT k: INT := 2; END_VAR\n\
            \a[k] := 0;\n"
      expectUnitPass src

    it "does not statically reject when index is non-constant" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT; k: INT; END_VAR\n\
            \a[k] := 0;\n"
      expectUnitPass src

  describe "Function calls (semantics) with injected signatures" $ do
    let base = "PROGRAM P\nVAR x: INT; y: INT; b: BOOL; END_VAR\n"

        funsSimple :: FuncEnv
        funsSimple =
          M.fromList
            [ ("ADD", FuncSig {fsName = toIdent "ADD", fsArgs = paramMap [("x", SigMono INT), ("y", SigMono INT)], fsRet = Just (SigMono INT), fsKind = FKFunction}),
              ("SUB", FuncSig {fsName = toIdent "SUB", fsArgs = paramMap [("x", SigMono INT), ("y", SigMono INT)], fsRet = Just (SigMono INT), fsKind = FKFunction}),
              ("NOP", FuncSig {fsName = toIdent "NOP", fsArgs = paramMap [], fsRet = Just (SigMono INT), fsKind = FKFunction})
            ]

        funsReal :: FuncEnv
        funsReal =
          M.fromList
            [ ("MIX", FuncSig {fsName = toIdent "MIX", fsArgs = paramMap [("a", SigMono INT), ("b", SigMono REAL)], fsRet = Just (SigMono REAL), fsKind = FKFunction})
            ]
        -- 成功ケース
        ok =
          [ "x := ADD(1, 2);",
            "x := SUB(5, 2);",
            "x := ADD(y := 2, x := 1);", -- 名前付き順不同
            "x := ADD(1, y := 2);", -- 位置→名前付きはOK
            "x := ADD(SUB(3,1), 2);", -- ネスト
            "x := NOP();" -- 引数なし
          ]
    forM_ ok $ \s ->
      it ("accepts: " <> T.unpack s) $
        expectUnitPassWithFuns funsSimple (base <> s)

    -- 未知関数
    it "rejects unknown function" $
      expectUnitFailWithFuns @UnknownFunction M.empty (base <> "x := ADD(1);\n")

    -- 引数個数エラー
    it "rejects bad arg count (too few)" $
      expectUnitFailWithDetailWithFuns @BadArgCount funsSimple (base <> "x := ADD(1);\n") $
        \(BadArgCount fname _exp _act _sp) -> fname == "ADD"

    it "rejects bad arg count (too many)" $
      expectUnitFailWithFuns @BadArgCount funsSimple (base <> "x := ADD(1,2,3);\n")

    -- 引数型不一致（位置）
    it "rejects arg type mismatch (pos #1 expected INT got BOOL)" $
      expectUnitFailWithDetailWithFuns @ArgTypeMismatch funsSimple (base <> "x := ADD(TRUE, 1);\n") $
        \(ArgTypeMismatch fname mbName pos expTy actTy _sp) ->
          fname == "ADD" && isNothing mbName && pos == 1 && expTy == INT && actTy == BOOL

    -- 引数型不一致（名前付き）
    it "rejects arg type mismatch (named y expected INT got BOOL)" $
      expectUnitFailWithDetailWithFuns @ArgTypeMismatch funsSimple (base <> "x := ADD(x := 1, y := TRUE);\n") $
        \(ArgTypeMismatch fname mbName _pos expTy actTy _sp) ->
          fname == "ADD" && mbName == Just "y" && expTy == INT && actTy == BOOL

    -- 未知引数名
    it "rejects unknown named arg" $
      expectUnitFailWithDetailWithFuns @UnknownArgName funsSimple (base <> "x := ADD(z := 1, y := 2);\n") $
        \(UnknownArgName fname arg _sp) -> fname == "ADD" && arg == "z"

    -- 重複引数名
    it "rejects duplicate named arg" $
      expectUnitFailWithDetailWithFuns @DuplicateArgName funsSimple (base <> "x := ADD(x := 1, x := 2);\n") $
        \(DuplicateArgName fname arg _sp) -> fname == "ADD" && arg == "x"

    -- 名前付きの後に位置引数（順序違反）
    it "rejects positional arg after a named arg" $
      expectUnitFailWithFuns @PositionalAfterNamed funsSimple (base <> "x := ADD(x := 1, 2);\n")

    -- 返り値と代入先の不一致
    it "rejects assigning INT-return to BOOL" $
      expectUnitFailWithDetailWithFuns @TypeMismatch funsSimple (base <> "b := ADD(1,2);\n") $
        \(TypeMismatch vname _ expected actual) ->
          vname == "b" && expected == BOOL && actual == INT

    -- 異なる FunEnv の例（REAL 返却）
    it "propagates callee's return type from custom FunEnv" $ do
      let src = "PROGRAM P\nVAR r: REAL; END_VAR\nr := MIX(1, 2.0);\n"
      expectUnitPassWithFuns funsReal src

    it "mismatch with custom FunEnv (REAL -> INT assign)" $ do
      let src = "PROGRAM P\nVAR x: INT; END_VAR\nx := MIX(1, 2.0);\n"
      expectUnitFailWithDetailWithFuns @TypeMismatch funsReal src $
        \(TypeMismatch vname _ expected actual) ->
          vname == "x" && expected == INT && actual == REAL

  describe "Generic ANY-family function calls" $ do
    let funsAny :: FuncEnv
        funsAny =
          M.fromList
            [ -- ID_INT: IN : ANY_INT -> SAME_TYPE
              ( "ID_ANY_INT",
                FuncSig
                  { fsName = toIdent "ID_ANY_INT",
                    fsArgs = paramMap [("IN", SigGen GSTAnyInt (TV 0))],
                    fsRet = Just $ SigGen GSTAnyInt (TV 0),
                    fsKind = FKFunction
                  }
              ),
              -- ADD_NUM: X,Y : ANY_NUM (同じ型) -> SAME_TYPE
              ( "ADD_ANY_NUM",
                FuncSig
                  { fsName = toIdent "ADD_ANY_NUM",
                    fsArgs =
                      paramMap
                        [ ("X", SigGen GSTAnyNum (TV 0)),
                          ("Y", SigGen GSTAnyNum (TV 0))
                        ],
                    fsRet = Just $ SigGen GSTAnyNum (TV 0),
                    fsKind = FKFunction
                  }
              ),
              -- SEL_ANY: G:BOOL, IN0/IN1: ANY (同じ型) -> SAME_TYPE
              ( "SEL_ANY",
                FuncSig
                  { fsName = toIdent "SEL_ANY",
                    fsArgs =
                      paramMap
                        [ ("G", SigMono BOOL),
                          ("IN0", SigGen GSTAny (TV 0)),
                          ("IN1", SigGen GSTAny (TV 0))
                        ],
                    fsRet = Just $ SigGen GSTAny (TV 0),
                    fsKind = FKFunction
                  }
              ),
              -- PAIR2: A,B : ANY_INT (別々の型変数) -> BOOL
              -- 「別 TVar は独立に選べる」をテストする用
              ( "PAIR2",
                FuncSig
                  { fsName = toIdent "PAIR2",
                    fsArgs =
                      paramMap
                        [ ("A", SigGen GSTAnyInt (TV 0)),
                          ("B", SigGen GSTAnyInt (TV 1))
                        ],
                    fsRet = Just $ SigMono BOOL,
                    fsKind = FKFunction
                  }
              )
            ]
        anyInt = "ANY_INT (ID_ANY_INT) "
        anyNum = "ANY_NUM (ADD_ANY_NUM) "
        sel = "ANY (SEL_ANY) "
    it (anyInt <> "accepts SINT") $
      expectUnitPassWithFuns
        funsAny
        "PROGRAM P\nVAR x: SINT; END_VAR\nx := ID_ANY_INT(x);\n"

    it (anyInt <> "accepts DINT") $
      expectUnitPassWithFuns
        funsAny
        "PROGRAM P\nVAR x: DINT; END_VAR\nx := ID_ANY_INT(x);\n"

    it (anyInt <> "accepts UINT") $
      expectUnitPassWithFuns
        funsAny
        "PROGRAM P\nVAR x: UINT; END_VAR\nx := ID_ANY_INT(x);\n"

    it (anyInt <> "rejects BOOL for ANY_INT")
      $ expectUnitFailWithDetailWithFuns @ArgTypeMismatch
        funsAny
        "PROGRAM P\nVAR b: BOOL; x: INT; END_VAR\nx := ID_ANY_INT(b);\n"
      $ \(ArgTypeMismatch fname _mbName pos _expTy actTy _sp) ->
        fname == "ID_ANY_INT"
          && pos == 1
          && actTy == BOOL

    it (anyNum <> "accepts INT branches") $
      expectUnitPassWithFuns
        funsAny
        "PROGRAM P\nVAR x: INT; END_VAR\nx := SEL_ANY(TRUE, 1, 2);\n"

    it (sel <> "accepts BOOL branches") $
      expectUnitPassWithFuns
        funsAny
        "PROGRAM P\nVAR b: BOOL; END_VAR\nb := SEL_ANY(FALSE, TRUE, FALSE);\n"

    it (sel <> "rejects mismatched ANY branches (INT vs BOOL)")
      $ expectUnitFailWithDetailWithFuns @ArgTypeMismatch
        funsAny
        "PROGRAM P\nVAR x: INT; END_VAR\nx := SEL_ANY(TRUE, 1, TRUE);\n"
      $ \(ArgTypeMismatch fname _mbName _pos _expTy actTy _sp) ->
        fname == "SEL_ANY"
          && (actTy == BOOL || actTy == INT)

    it ("PAIR2 " <> "allows different int types for different TVars") $
      expectUnitPassWithFuns
        funsAny
        "PROGRAM P\nVAR a: INT; b: DINT; ok: BOOL; END_VAR\nok := PAIR2(a, b);\n"

    it "rejects struct for ID_ANY_INT"
      $ expectUnitsFailWithDetail'' @ArgTypeMismatch
        funsAny
        [ "TYPE R : STRUCT x: INT; END_STRUCT; END_TYPE\n",
          "PROGRAM P\nVAR r: R; END_VAR\nr.x := ID_ANY_INT(r);\n"
        ]
      $ \(ArgTypeMismatch fname _ _ _ actTy _) ->
        fname == "ID_ANY_INT" && actTy /= INT

  describe "FuncEnv building" $ do
    it "rejects duplicate function names" $ do
      let srcs =
            [ "FUNCTION F : INT\n\
              \VAR_INPUT x : INT; END_VAR\n\
              \F := x;\n",
              "FUNCTION F : INT\n\
              \VAR_INPUT y : INT; END_VAR\n\
              \F := y;\n"
            ]
      expectUnitsFailWithDetail @DuplicateFunction srcs $
        \(DuplicateFunction n) -> n == "F"

  describe "FUNCTION body" $ do
    it "fails with MissingReturn when no assignment to function name (if Strict)" $ do
      let src =
            [ "FUNCTION F : INT\n\
              \VAR_INPUT x : INT; END_VAR\n"
            ]
      expectUnitsFailWithDetailWithMode @MissingReturn Strict M.empty src $
        \(MissingReturn n) -> n == "F"

    let funsAnyRet =
          M.fromList
            [ ( "G",
                FuncSig
                  { fsName = toIdent "G",
                    fsKind = FKFunction,
                    fsArgs = paramMap [("x", SigMono REAL)],
                    fsRet = Just $ SigGen GSTAnyInt (TV 0) -- ★ 具体型でない
                  }
              )
            ]

    it "fails with UnsupportedGenericReturn when callee returns ANY_*" $ do
      let src =
            [ "PROGRAM P\n\
              \VAR y : REAL; END_VAR\n\
              \y := G(1.0);\n"
            ]
      expectUnitsFailWithDetail'' @UnsupportedGenericReturn funsAnyRet src $
        \(UnsupportedGenericReturn n) -> n == "G"

  describe "NoReturnValue (semantics)" $ do
    it "errors when using a no-return function in expression position (RHS of assign)" $ do
      let srcs =
            [ "PROGRAM P\n\
              \VAR x : INT; END_VAR\n\
              \x := PROC(1);\n"
            ]

          funsNoRet :: FuncEnv
          funsNoRet =
            M.fromList
              [ ( "PROC",
                  FuncSig
                    { fsName = toIdent "PROC",
                      fsKind = FKFunction,
                      fsArgs = paramMap [("x", SigMono INT)],
                      fsRet = Nothing
                    }
                )
              ]

      expectUnitsFailWithDetail'' @NoReturnValue funsNoRet srcs $
        \case
          NoReturnValue _ name -> name == "PROC"

  describe "FUNCTION must-assign (mode)" $ do
    let prog =
          "PROGRAM P\nVAR x: INT; END_VAR\nx := F();\n"

    it "Strict: missing return fails" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR a: INT; END_VAR\n\
            \IF FALSE THEN F := 1; END_IF\n"
      expectUnitsFailWithDetailWithMode @MissingReturn
        Strict
        M.empty
        [fun, prog]
        (\(MissingReturn n) -> n == "F")

    it "CodesysLike: missing return passes" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR a: INT; END_VAR\n\
            \IF FALSE THEN F := 1; END_IF\n"
      expectUnitsPassWithMode CodesysLike M.empty [fun, prog]

  describe "FUNCTION must-assign per statement (Strict vs CodesysLike)" $ do
    let callProg =
          "PROGRAM P\nVAR x: INT; END_VAR\nx := F();\n"

    -- 1) WHILE：本体で代入しても 0 回実行の可能性 → Strict では NG
    it "WHILE-only assignment: Strict fails, CodesysLike passes" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR END_VAR\
            \WHILE FALSE DO F := 1; END_WHILE\n"
      expectUnitsFailWithDetailWithMode @MissingReturn
        Strict
        M.empty
        [fun, callProg]
        (\(MissingReturn n) -> n == "F")
      expectUnitsPassWithMode CodesysLike M.empty [fun, callProg]

    -- 2) REPEAT：少なくとも 1 回は実行されるが、ここでは代入しない → Strict では NG
    --    （※REPEAT 内で IF FALSE THEN F:=1; END_IF にしても同様に NG）
    it "REPEAT without assignment: Strict fails, CodesysLike passes" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR END_VAR\
            \REPEAT UNTIL TRUE END_REPEAT\n"
      expectUnitsFailWithDetailWithMode @MissingReturn
        Strict
        M.empty
        [fun, callProg]
        (\(MissingReturn n) -> n == "F")
      expectUnitsPassWithMode CodesysLike M.empty [fun, callProg]

    -- 3) CASE：アームに代入があっても、ELSE が無ければ網羅保証できない簡易実装 → Strict では NG
    it "CASE without ELSE (assignment in one arm): Strict fails, CodesysLike passes" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR END_VAR\
            \CASE 0 OF 1: F := 1; END_CASE\n"
      expectUnitsFailWithDetailWithMode @MissingReturn
        Strict
        M.empty
        [fun, callProg]
        (\(MissingReturn n) -> n == "F")
      expectUnitsPassWithMode CodesysLike M.empty [fun, callProg]

    -- 4) FOR：本体で代入しても 0 回実行の可能性があるとみなす保守的解析 → Strict では NG
    it "FOR-only assignment: Strict fails, CodesysLike passes" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR i:INT; END_VAR\
            \FOR i := 1 TO 10 DO F := 1; END_FOR\n"
      expectUnitsFailWithDetailWithMode @MissingReturn
        Strict
        M.empty
        [fun, callProg]
        (\(MissingReturn n) -> n == "F")
      expectUnitsPassWithMode CodesysLike M.empty [fun, callProg]

    -- 5) Skip（空文）：当然代入なし → Strict では NG
    it "Skip only: Strict fails, CodesysLike passes" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR END_VAR\
            \;\n"
      expectUnitsFailWithDetailWithMode @MissingReturn
        Strict
        M.empty
        [fun, callProg]
        (\(MissingReturn n) -> n == "F")
      expectUnitsPassWithMode CodesysLike M.empty [fun, callProg]

  describe "FUNCTION outputs must-assign (Strict vs CodesysLike)" $ do
    let callProg =
          "PROGRAM P\nVAR x: INT; END_VAR\nx := F();\n"

    -- ケースA: Fは代入するが、o1/o2 を一切代入しない
    it "Strict fails when any VAR_OUTPUT is never assigned (even if return is assigned)" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR_OUTPUT o1 : INT; o2 : INT; END_VAR\n\
            \F := 1;\n"
      expectUnitsFailWithDetailWithMode @MissingReturn
        Strict
        M.empty
        [fun, callProg]
        (\(MissingReturn n) -> n == "F")
      expectUnitsPassWithMode CodesysLike M.empty [fun, callProg]

    -- ケースB: 片方(o1)だけに代入、もう片方(o2)は未代入
    it "Strict fails when some VAR_OUTPUT is not assigned on all paths" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR_OUTPUT o1 : INT; o2 : INT; END_VAR\n\
            \o1 := 2; F := 1;\n"
      expectUnitsFailWithDetailWithMode @MissingReturn
        Strict
        M.empty
        [fun, callProg]
        (\(MissingReturn n) -> n == "F")
      expectUnitsPassWithMode CodesysLike M.empty [fun, callProg]

  describe "FUNCTION VAR_INPUT / VAR_IN_OUT Semantics (Strict vs CodesysLike)" $ do
    it "VAR_INPUT write: Strict fails, CodesysLike passes" $ do
      let fun =
            "FUNCTION F : INT\n\
            \VAR_INPUT a : INT; END_VAR\n\
            \a := 1; F := a;\n"
          prog =
            "PROGRAM P\nVAR x: INT; END_VAR\nx := F(0);\n"
      expectUnitsFailWithDetailWithMode @AssignToInput
        Strict
        M.empty
        [fun, prog]
        (\(AssignToInput _ n) -> n == "a")
      expectUnitsPassWithMode CodesysLike M.empty [fun, prog]

    it "VAR_IN_OUT literal: both modes fail (InOutArgNotLValue)" $ do
      let fun =
            "FUNCTION G : INT\n\
            \VAR_IN_OUT a : INT; END_VAR\n\
            \a := a + 1; G := a;\n"
          progBad =
            "PROGRAM P\nVAR y: INT; END_VAR\ny := G(1);\n"
          progGood =
            "PROGRAM P\nVAR v: INT; y: INT; END_VAR\ny := G(v);\n"
      -- literal → NG
      expectUnitsFailWithDetailWithMode @InOutArgNotLValue
        Strict
        M.empty
        [fun, progBad]
        (\(InOutArgNotLValue _ f v) -> f == "G" && v == "a")
      expectUnitsFailWithDetailWithMode @InOutArgNotLValue
        CodesysLike
        M.empty
        [fun, progBad]
        (\(InOutArgNotLValue _ f v) -> f == "G" && v == "a")
      -- 変数 → OK（両モード）
      expectUnitsPassWithMode Strict M.empty [fun, progGood]
      expectUnitsPassWithMode CodesysLike M.empty [fun, progGood]

  describe "ANY-family (gstMember) coverage" $ do
    let funsAnyFamilies :: FuncEnv
        funsAnyFamilies =
          let mk name gst =
                ( name,
                  FuncSig
                    { fsName = toIdent name,
                      fsKind = FKFunction,
                      fsArgs = paramMapDir [("x", SigGen gst (TV 0), ParamIn)],
                      fsRet = Just (SigMono INT)
                    }
                )
           in M.fromList
                [ mk "ID_ANY" GSTAny,
                  mk "ID_ANY_INT" GSTAnyInt,
                  mk "ID_ANY_NUM" GSTAnyNum,
                  mk "ID_ANY_REAL" GSTAnyReal,
                  mk "ID_ANY_BIT" GSTAnyBit,
                  mk "ID_ANY_STRING" GSTAnyString,
                  mk "ID_ANY_DATE" GSTAnyDate,
                  mk "ID_ANY_DURATION" GSTAnyDuration
                ]
    -- ANY: OK = INT, NG = STRUCT
    it "ANY accepts INT" $ do
      let srcs = ["PROGRAM P\nVAR x: INT; y: INT; END_VAR\ny := ID_ANY(x);\n"]
      expectUnitsPassWithMode CodesysLike funsAnyFamilies srcs

    it "ANY rejects STRUCT" $ do
      let srcs =
            [ "TYPE R : STRUCT a: INT; END_STRUCT; END_TYPE\n",
              "PROGRAM P\nVAR r: R; y: INT; END_VAR\ny := ID_ANY(r);\n"
            ]
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        funsAnyFamilies
        srcs
        (\(ArgTypeMismatch fname _ _ _ _ _) -> fname == "ID_ANY")

    -- ANY_INT: OK = SINT/INT/…、NG = REAL/BOOL
    it "ANY_INT accepts INT" $ do
      let srcs = ["PROGRAM P\nVAR x: INT; y: INT; END_VAR\ny := ID_ANY_INT(x);\n"]
      expectUnitsPassWithMode CodesysLike funsAnyFamilies srcs

    it "ANY_INT rejects REAL" $ do
      let srcs = ["PROGRAM P\nVAR r: REAL; y: INT; END_VAR\ny := ID_ANY_INT(r);\n"]
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        funsAnyFamilies
        srcs
        (\(ArgTypeMismatch fname _ _ _ _ _) -> fname == "ID_ANY_INT")

    -- ANY_NUM: OK = INT/LREAL、NG = BOOL
    it "ANY_NUM accepts LREAL" $ do
      let srcs = ["PROGRAM P\nVAR z: LREAL; y: INT; END_VAR\ny := ID_ANY_NUM(z);\n"]
      expectUnitsPassWithMode CodesysLike funsAnyFamilies srcs

    it "ANY_NUM rejects BOOL" $ do
      let srcs = ["PROGRAM P\nVAR b: BOOL; y: INT; END_VAR\ny := ID_ANY_NUM(b);\n"]
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        funsAnyFamilies
        srcs
        (\(ArgTypeMismatch fname _ _ _ _ _) -> fname == "ID_ANY_NUM")

    -- ANY_REAL: OK = REAL/LREAL、NG = INT
    it "ANY_REAL accepts REAL" $ do
      let srcs = ["PROGRAM P\nVAR r: REAL; y: INT; END_VAR\ny := ID_ANY_REAL(r);\n"]
      expectUnitsPassWithMode CodesysLike funsAnyFamilies srcs

    it "ANY_REAL rejects INT" $ do
      let srcs = ["PROGRAM P\nVAR i: INT; y: INT; END_VAR\ny := ID_ANY_REAL(i);\n"]
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        funsAnyFamilies
        srcs
        (\(ArgTypeMismatch fname _ _ _ _ _) -> fname == "ID_ANY_REAL")

    -- ANY_BIT: OK = BOOL/WORD…、NG = INT
    it "ANY_BIT accepts BOOL" $ do
      let srcs = ["PROGRAM P\nVAR b: BOOL; y: INT; END_VAR\ny := ID_ANY_BIT(b);\n"]
      expectUnitsPassWithMode CodesysLike funsAnyFamilies srcs

    it "ANY_BIT rejects INT" $ do
      let srcs = ["PROGRAM P\nVAR i: INT; y: INT; END_VAR\ny := ID_ANY_BIT(i);\n"]
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        funsAnyFamilies
        srcs
        (\(ArgTypeMismatch fname _ _ _ _ _) -> fname == "ID_ANY_BIT")

    -- ANY_STRING: OK = STRING/WSTRING/CHAR/WCHAR、NG = INT
    it "ANY_STRING accepts STRING" $ do
      let srcs = ["PROGRAM P\nVAR s: STRING[10]; y: INT; END_VAR\ny := ID_ANY_STRING(s);\n"]
      expectUnitsPassWithMode CodesysLike funsAnyFamilies srcs

    it "ANY_STRING rejects INT" $ do
      let srcs = ["PROGRAM P\nVAR i: INT; y: INT; END_VAR\ny := ID_ANY_STRING(i);\n"]
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        funsAnyFamilies
        srcs
        (\(ArgTypeMismatch fname _ _ _ _ _) -> fname == "ID_ANY_STRING")

    -- ANY_DATE: OK = DATE/TOD/DT、NG = TIME
    it "ANY_DATE accepts DATE" $ do
      let srcs = ["PROGRAM P\nVAR d: DATE; y: INT; END_VAR\ny := ID_ANY_DATE(d);\n"]
      expectUnitsPassWithMode CodesysLike funsAnyFamilies srcs

    it "ANY_DATE rejects TIME" $ do
      let srcs = ["PROGRAM P\nVAR t: TIME; y: INT; END_VAR\ny := ID_ANY_DATE(t);\n"]
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        funsAnyFamilies
        srcs
        (\(ArgTypeMismatch fname _ _ _ _ _) -> fname == "ID_ANY_DATE")

    -- ANY_DURATION: OK = TIME、NG = DATE
    it "ANY_DURATION accepts TIME" $ do
      let srcs = ["PROGRAM P\nVAR t: TIME; y: INT; END_VAR\ny := ID_ANY_DURATION(t);\n"]
      expectUnitsPassWithMode CodesysLike funsAnyFamilies srcs

    it "ANY_DURATION rejects DATE" $ do
      let srcs = ["PROGRAM P\nVAR d: DATE; y: INT; END_VAR\ny := ID_ANY_DURATION(d);\n"]
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        funsAnyFamilies
        srcs
        (\(ArgTypeMismatch fname _ _ _ _ _) -> fname == "ID_ANY_DURATION")

  describe "call-arg mixing (positional + named)" $ do
    -- 共通：3引数の関数F(x,y,z) : INT（本体は単純でOK）
    let funF =
          "FUNCTION F : INT\n\
          \VAR_INPUT x : INT; y : INT; z : INT; END_VAR\n\
          \F := x;\n"
        -- 正常系は CodesysLike で回す（Strictでも同じ挙動だが雑音を避ける）
        runOK :: [Text] -> Expectation
        runOK = expectUnitsPassWithMode CodesysLike M.empty
        -- 失敗系は CodesysLike で十分（モード非依存）
        runNG ::
          forall err.
          (err :| AllErrs, Typeable err) =>
          [Text] -> -- srcs
          (err -> Bool) -> -- predicate
          Expectation
        runNG = expectUnitsFailWithDetailWithMode @err CodesysLike M.empty

    -- 1) すべて位置引数 → OK
    it "accepts all positional" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(1, 2, 3);\n"
      runOK [funF, prog]

    -- 2) すべて名前付き（順不同） → OK
    it "accepts all named in any order" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(z := 3, x := 1, y := 2);\n"
      runOK [funF, prog]

    -- 3) 位置のあとに残りを名前付きで補完 → OK
    it "accepts mixed: positional first, then named for the rest" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(1, z := 3, y := 2);\n"
      runOK [funF, prog]

    -- 4) 名前付きのあとに位置 → NG (PositionalAfterNamed)
    it "rejects positional after any named" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(x := 1, 2, 3);\n"
      runNG @PositionalAfterNamed [funF, prog] (\_ -> True)

    -- 5) 同じ名前を2回（両方named）→ NG (DuplicateArgName)
    it "rejects duplicate named" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(x := 1, x := 2, z := 3);\n"
      runNG @DuplicateArgName
        [funF, prog]
        (\(DuplicateArgName fname nm _) -> fname == "F" && nm == "x")

    -- 6) 位置でxを埋めた後に namedでxを再指定 → NG (DuplicateArgName)
    it "rejects duplicate via positional + named for same param" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(1, y := 2, x := 9);\n"
      runNG @DuplicateArgName
        [funF, prog]
        (\(DuplicateArgName fname nm _) -> fname == "F" && nm == "x")

    -- 7) 未知の名前 → NG (UnknownArgName)
    it "rejects unknown arg name" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(foo := 1, y := 2, z := 3);\n"
      runNG @UnknownArgName
        [funF, prog]
        (\(UnknownArgName fname nm _) -> fname == "F" && nm == "foo")

    -- 8) 引数不足 → NG (BadArgCount)
    it "rejects too few args" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(1, 2);\n"
      runNG @BadArgCount
        [funF, prog]
        (\(BadArgCount fname _ expected actual) -> fname == "F" && expected == 3 && actual == 2)

    -- 9) 引数過多 → NG (BadArgCount)
    it "rejects too many args" $ do
      let prog = "PROGRAM P\nVAR r: INT; END_VAR\nr := F(1, 2, 3, 4);\n"
      runNG @BadArgCount
        [funF, prog]
        (\(BadArgCount fname _ expected actual) -> fname == "F" && expected == 3 && actual == 4)

  describe "FB misuse as expression" $ do
    let fb =
          "FUNCTION_BLOCK FB\n\
          \VAR_INPUT a : INT; END_VAR\n\
          \VAR_OUTPUT o : INT; END_VAR\n\
          \o := a;\n"

    it "rejects FB POU name used as expression call (FB(...))" $ do
      let prog =
            "PROGRAM P\nVAR x: INT; END_VAR\nx := FB(a := 1);\n"
      -- ここは「未インスタンス」エラーに期待を変更
      expectUnitsFailWithDetailWithMode @FBNotInstantiated
        CodesysLike
        M.empty
        [fb, prog]
        (\(FBNotInstantiated n _) -> n == "FB")

    it "rejects FB instance variable used like a function (f(...))" $ do
      let prog =
            "PROGRAM P\nVAR x: INT; f: FB; END_VAR\nx := f(a := 1);\n"
      expectUnitsFailWithDetailWithMode @FBUsedAsExpr
        CodesysLike
        M.empty
        [fb, prog]
        (\(FBUsedAsExpr t _) -> t == "f")

  describe "FB as a first-class type (Phase 1)" $ do
    it "allows declaring a variable of FB type when FB is defined" $ do
      let fb =
            "FUNCTION_BLOCK FB\n\
            \VAR_INPUT a : INT; END_VAR\n\
            \VAR_OUTPUT o : INT; END_VAR\n\
            \o := a;\n"
          prog =
            "PROGRAM P\n\
            \VAR f: FB; END_VAR\n"
      -- 両モードで通る（まだ呼び出しもフィールド参照もしない）
      expectUnitsPassWithMode Strict M.empty [fb, prog]
      expectUnitsPassWithMode CodesysLike M.empty [fb, prog]

    it "rejects FB type if FB is not defined" $ do
      let prog =
            "PROGRAM P\n\
            \VAR f: FB; END_VAR\n"
      -- FB 本体が無いので UnknownType を期待
      expectUnitsFailWithDetailWithMode @UnknownType Strict M.empty [prog] $
        \(UnknownType tname _) -> tname == "FB"

  describe "FB / TYPE name clash" $ do
    let fb =
          "FUNCTION_BLOCK MyFB\n\
          \VAR_INPUT a : INT; END_VAR\n\
          \VAR_OUTPUT o : INT; END_VAR\n\
          \o := a;\n"

        dut =
          "TYPE MyFB : INT; END_TYPE\n"

    it "rejects when TYPE then FB" $ do
      expectUnitsFailWithDetailWithMode @TypeFBNameClash
        Strict
        M.empty
        [dut, fb]
        (\(TypeFBNameClash n) -> n == "MyFB")
    it "rejects when FB then TYPE" $ do
      expectUnitsFailWithDetailWithMode @TypeFBNameClash
        Strict
        M.empty
        [fb, dut]
        (\(TypeFBNameClash n) -> n == "MyFB")

  describe "FB field read (expression side)" $ do
    let fb =
          "FUNCTION_BLOCK FB\n\
          \VAR_INPUT  a : INT;  END_VAR\n\
          \VAR_OUTPUT o : INT;  END_VAR\n\
          \VAR_IN_OUT r : INT;  END_VAR\n\
          \o := a;\n"

    it "allows reading VAR_INPUT as f.a" $ do
      let prog =
            "PROGRAM P\n\
            \VAR f: FB; x: INT; END_VAR\n\
            \x := f.a;\n"
      expectUnitsPassWithMode CodesysLike M.empty [fb, prog]

    it "allows reading VAR_OUTPUT as f.o" $ do
      let prog =
            "PROGRAM P\n\
            \VAR f: FB; x: INT; END_VAR\n\
            \x := f.o;\n"
      expectUnitsPassWithMode CodesysLike M.empty [fb, prog]

    it "rejects reading VAR_IN_OUT as f.r" $ do
      let prog =
            "PROGRAM P\n\
            \VAR f: FB; x: INT; END_VAR\n\
            \x := f.r;\n"
      expectUnitsFailWithDetailWithMode @UnknownFBMember
        CodesysLike
        M.empty
        [fb, prog]
        (\(UnknownFBMember _ ident _) -> ident == "r")

    it "rejects unknown FB field" $ do
      let prog =
            "PROGRAM P\n\
            \VAR f: FB; x: INT; END_VAR\n\
            \x := f.zz;\n"
      expectUnitsFailWithDetailWithMode @UnknownFBMember
        CodesysLike
        M.empty
        [fb, prog]
        (\(UnknownFBMember fbName mem _) -> fbName == "FB" && mem == "zz")

  describe "FB call statement (semantics)" $ do
    let fbDecl =
          "FUNCTION_BLOCK FB\n\
          \VAR_INPUT a : INT; END_VAR\n\
          \VAR_OUTPUT o : INT; END_VAR\n"

    it "accepts input-only bind" $ do
      let prog =
            "PROGRAM P\nVAR f: FB; END_VAR\n\
            \f(a := 1);\n"
      expectUnitsPassWithMode CodesysLike M.empty [fbDecl, prog]

    it "accepts input + output bind (order free)" $ do
      let prog =
            "PROGRAM P\nVAR f: FB; x: INT; END_VAR\n\
            \f(o => x, a := 2);\n"
      expectUnitsPassWithMode CodesysLike M.empty [fbDecl, prog]

    it "rejects unknown bind name" $ do
      let prog =
            "PROGRAM P\nVAR f: FB; END_VAR\n\
            \f(bogus := 1);\n"
      expectUnitsFailWithDetailWithMode @UnknownArgName
        CodesysLike
        M.empty
        [fbDecl, prog]
        (\(UnknownArgName pou nm _) -> pou == "FB" && nm == "bogus")

    it "rejects duplicate bind name" $ do
      let prog =
            "PROGRAM P\nVAR f: FB; END_VAR\n\
            \f(a := 1, a := 2);\n"
      expectUnitsFailWithDetailWithMode @DuplicateArgName
        CodesysLike
        M.empty
        [fbDecl, prog]
        (\(DuplicateArgName pou nm _) -> pou == "FB" && nm == "a")

    it "IN_OUT requires lvalue with := (not literal)" $ do
      let fbDeclInOut =
            "FUNCTION_BLOCK FB\n\
            \VAR_IN_OUT r : INT; END_VAR\n"
      let progBad =
            "PROGRAM P\nVAR f: FB; END_VAR\n\
            \f(r := 1);\n"
      expectUnitsFailWithDetailWithMode @InOutArgNotLValue
        CodesysLike
        M.empty
        [fbDeclInOut, progBad]
        (\(InOutArgNotLValue _ _ arg) -> arg == "r")

    it "type mismatch on input bind" $ do
      let prog =
            "PROGRAM P\nVAR f: FB; END_VAR\n\
            \f(a := TRUE);\n"
      expectUnitsFailWithDetailWithMode @ArgTypeMismatch
        CodesysLike
        M.empty
        [fbDecl, prog]
        (\(ArgTypeMismatch pou (Just "a") _ _ actTy _) -> pou == "FB" && actTy == BOOL)

  --------------------------------------------------------------------------------
  -- FB outputs must-assign (Strict vs CodesysLike)
  --------------------------------------------------------------------------------

  describe "FB outputs must-assign (Strict vs CodesysLike)" $ do
    -- FB 宣言の共通ヘッダ（END_FUNCTION_BLOCK は省略する方針）
    let fbHdr =
          "FUNCTION_BLOCK FB\n\
          \VAR_INPUT  a : INT; END_VAR\n\
          \VAR_OUTPUT o : INT; p : INT; END_VAR\n"

    -- 失敗 (Strict, MissingFBOutputs 期待）ヘルパ
    let expectStrictFail body missingNm =
          expectUnitsFailWithDetailWithMode @MissingFBOutputs
            Strict
            M.empty
            [fbHdr <> body]
            (\(MissingFBOutputs _ names) -> Set.member missingNm names)

    -- 成功ヘルパ
    let expectStrictPass body =
          expectUnitsPassWithMode Strict M.empty [fbHdr <> body]
    let expectCodesysPass body =
          expectUnitsPassWithMode CodesysLike M.empty [fbHdr <> body]

    it "Strict fails when any VAR_OUTPUT is never assigned (even if others are)" $ do
      -- o は代入されるが p は未代入
      let body = "o := a;\n"
      expectStrictFail body "p"
      expectCodesysPass body

    it "Strict passes when all outputs are assigned on all paths (IF: both branches)" $ do
      let body =
            "IF a > 0 THEN\n\
            \  o := a; p := a;\n\
            \ELSE\n\
            \  o := 0; p := 0;\n\
            \END_IF;\n"
      expectStrictPass body
      expectCodesysPass body

    it "Strict fails when an output is not assigned on some path (IF: only THEN assigns)" $ do
      let body =
            "IF a > 0 THEN\n\
            \  o := a; p := a;\n\
            \END_IF;\n"
      -- ELSE 無し → 未代入パスがある
      expectStrictFail body "o" -- どちらでもよいが一つは未達。両方見るなら2ケース作ってOK
      expectCodesysPass body

    it "Strict fails when only WHILE assigns (0-iteration may skip)" $ do
      let body =
            "WHILE a > 0 DO\n\
            \  o := a; p := a;\n\
            \END_WHILE;\n"
      expectStrictFail body "o"
      expectCodesysPass body

    it "Strict passes when REPEAT assigns (executes at least once)" $ do
      let body =
            "REPEAT\n\
            \  o := a; p := a;\n\
            \UNTIL a > 0 END_REPEAT;\n"
      expectStrictPass body
      expectCodesysPass body

    it "Strict fails for CASE without ELSE where some arms miss assignment" $ do
      let body =
            "CASE a OF\n\
            \  0: o := 0;              \n\
            \  1: o := 1; p := 1;      \n\
            \END_CASE;\n"
      -- a=0 の腕で p 未代入、ELSE も無し
      expectStrictFail body "p"
      expectCodesysPass body

    it "Strict passes for CASE with ELSE and all arms assign" $ do
      let body =
            "CASE a OF\n\
            \  0: o := 0; p := 0;      \n\
            \  1: o := 1; p := 1;      \n\
            \ELSE                      \n\
            \  o := 0; p := 0;         \n\
            \END_CASE;\n"
      expectStrictPass body
      expectCodesysPass body

    it "Strict fails when body has only Skip (no assignments)" $ do
      let body = ";\n" -- Skip 相当
      expectStrictFail body "o"
      expectCodesysPass body

  describe "FB call direction mismatch" $ do
    let fb =
          "FUNCTION_BLOCK FB\n\
          \VAR_INPUT  a : INT; END_VAR\n\
          \VAR_OUTPUT o : INT; END_VAR\n\
          \VAR_IN_OUT r : INT; END_VAR\n"

    it "reports mismatch: OUT with := (should use =>)" $ do
      let prog = "PROGRAM P\nVAR f: FB; x: INT; END_VAR\nf(o := 1);\n"
      expectUnitsFailWithDetailWithMode @ArgDirectionMismatch
        CodesysLike
        M.empty
        [fb, prog]
        ( \(ArgDirectionMismatch pou arg expected got _sp) ->
            pou == "FB" && arg == "o" && expected == ParamOut && got == ":="
        )

    it "reports mismatch: IN with => (should use :=)" $ do
      let prog = "PROGRAM P\nVAR f: FB; x: INT; END_VAR\nf(a => x);\n"
      expectUnitsFailWithDetailWithMode @ArgDirectionMismatch
        CodesysLike
        M.empty
        [fb, prog]
        ( \(ArgDirectionMismatch pou arg expected got _sp) ->
            pou == "FB" && arg == "a" && expected == ParamIn && got == "=>"
        )

    it "reports mismatch: IN_OUT with => (should use :=)" $ do
      let prog = "PROGRAM P\nVAR f: FB; x: INT; END_VAR\nf(r => x);\n"
      expectUnitsFailWithDetailWithMode @ArgDirectionMismatch
        CodesysLike
        M.empty
        [fb, prog]
        ( \(ArgDirectionMismatch _pou arg expected got _sp) ->
            arg == "r" && expected == ParamInOut && got == "=>"
        )

    it "still keeps InOutArgNotLValue precedence for IN_OUT with literal" $ do
      let prog = "PROGRAM P\nVAR f: FB; END_VAR\nf(r := 1);\n"
      expectUnitsFailWithDetailWithMode @InOutArgNotLValue
        CodesysLike
        M.empty
        [fb, prog]
        (const True)

  describe "FB body: VAR_INPUT write (Strict vs CodesysLike)" $ do
    -- 最小ケース：入力 a に代入
    let fb1 =
          "FUNCTION_BLOCK FB\n\
          \VAR_INPUT a : INT; END_VAR\n\
          \VAR_OUTPUT o : INT; END_VAR\n\
          \a := 1; o := a;\n"

        prog1 =
          "PROGRAM P\n\
          \VAR f: FB; x: INT; END_VAR\n\
          \f(a := 0, o => x);\n"

    it "Strict: writing to VAR_INPUT inside FB fails (AssignToInput)" $ do
      expectUnitsFailWithDetailWithMode @AssignToInput
        Strict
        M.empty
        [fb1, prog1]
        (\(AssignToInput _ v) -> v == "a")

    it "CodesysLike: writing to VAR_INPUT inside FB passes" $ do
      expectUnitsPassWithMode CodesysLike M.empty [fb1, prog1]

    -- 構造/添字でも「ベースがVAR_INPUT」なら禁止されること（Strict）
    let fb2 =
          "FUNCTION_BLOCK FB2\n\
          \VAR_INPUT arr : ARRAY[1..3] OF INT; END_VAR\n\
          \VAR_OUTPUT o  : INT; END_VAR\n\
          \arr[1] := 42; o := arr[1];\n"
        prog2 =
          "PROGRAM P\n\
          \VAR f: FB2; x: INT; END_VAR\n\
          \f(arr := [0,0,0], o => x);\n"

    it "Strict: writing to VAR_INPUT via index also fails (AssignToInput)" $ do
      expectUnitsFailWithDetailWithMode @AssignToInput
        Strict
        M.empty
        [fb2, prog2]
        (\(AssignToInput _ v) -> v == "arr")

    it "CodesysLike: writing to VAR_INPUT via index passes" $ do
      expectUnitsPassWithMode CodesysLike M.empty [fb2, prog2]
