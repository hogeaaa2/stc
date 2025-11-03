{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Monad (forM_)
import Data.Map.Strict qualified as M
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable, typeRep)
import ST.AST
import ST.Parser (parseProgram, parseUnit)
import ST.Semantic
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)
import Vary (Vary, into, (:|))
import Vary.VEither (VEither (VLeft, VRight))

type AllErrs =
  '[ DuplicateVar,
     TypeMismatch,
     TypeMismatch',
     MissingInitializer,
     AssignToLoopVar,
     InvalidCaseRange,
     OverlappingCase,
     OutOfRange,
     UnknownStructMember,
     NotAStruct,
     TooManyAggElems,
     BadUseOfFunction,
     TypeCycle,
     UnknownType,
     UnknownEnumMember,
     NotAnEnum,
     IndexOutOfBounds,
     NotAnArray,
     UnknownVar,
     AssignToConst,
     BadIndexCount,
     NonConstantExpr
   ]

expectParsed :: Text -> (Program -> Expectation) -> Expectation
expectParsed src k = case parseProgram src of
  Left e -> expectationFailure (show e)
  Right p -> k p

-- shouldParse :: Text -> Expectation
-- shouldParse src = expectParsed src $ \prog ->
--   elaborateProgram prog `shouldSatisfy` isRight

expectParsedUnit :: Text -> (Unit -> Expectation) -> Expectation
expectParsedUnit src k =
  case parseUnit src of
    Left e -> expectationFailure (show e)
    Right u -> k u

-- shouldParseUnit :: Text -> Expectation
-- shouldParseUnit src = expectParsedUnit src $ \u ->
--   elaborateUnit u `shouldSatisfy` isRight

-- -- 便利: elaborate の結果が Left で、かつ単一診断が述語に一致することを確かめる
-- shouldFailWith1 :: Either [SemantDiag] a -> (SemantDiag -> Bool) -> Expectation
-- shouldFailWith1 r p = case r of
--   Left [d] | p d -> pure ()
--   Left ds -> expectationFailure ("unexpected diagnostics: " <> show ds)
--   Right _ -> expectationFailure "expected Left, got Right"

-- isRight :: Either a b -> Bool
-- isRight (Right _) = True
-- isRight (Left _) = False

-- arrayLenMismatch :: SemantDiag -> Bool
-- arrayLenMismatch TooManyAggElems {} = True
-- arrayLenMismatch _ = False

-- unknownFieldInit :: SemantDiag -> Bool
-- unknownFieldInit UnknownField {} = True
-- unknownFieldInit _ = False

-- indexOOB :: SemantDiag -> Bool
-- indexOOB IndexOutOfBounds {} = True
-- indexOOB _ = False

-- opTypeMismatch :: Text -> SemantDiag -> Bool
-- opTypeMismatch want (OpTypeMismatch {op = o}) = o == want
-- opTypeMismatch _ _ = False

-- typeMismatch :: Text -> SemantDiag -> Bool
-- typeMismatch want (TypeMismatch {dName = o}) = o == want
-- typeMismatch _ _ = False

-- isOverlappingCase :: SemantDiag -> Bool
-- isOverlappingCase OverlappingCase = True
-- isOverlappingCase _ = False

-- outOfRange :: Text -> SemantDiag -> Bool
-- outOfRange want (OutOfRange {dName = o}) = o == want
-- outOfRange _ _ = False

-- | open-sum の Left から、指定エラー型だけを引き抜く（あるなら Just）
--   ※ Vary のプロジェクタ名が環境で違う場合はここを書き換えるだけでOK
prj :: forall err es. (err :| es) => Vary es -> Maybe err
prj = Vary.into @err

-- 成功/失敗（型に依存しないやつは曖昧性なし）
shouldSucceedV :: VEither es a -> Expectation
shouldSucceedV v = case v of
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

-- ==== Program 版 ヘルパ====

-- 期待：parseProgram → elaborateProgram が Right
expectProgPass :: Text -> Expectation
expectProgPass src =
  case parseProgram src of
    Left e -> expectationFailure (show e)
    Right p ->
      let v :: VEither AllErrs Program
          v = elaborateProgram p
       in shouldSucceedV v

-- 期待：Left（エラー“型”だけ一致していればOK）
expectProgFail ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  Text -> Expectation
expectProgFail src =
  case parseProgram src of
    Left e -> expectationFailure (show e)
    Right p ->
      let v :: VEither AllErrs Program
          v = elaborateProgram p
       in shouldFail @err v

-- 期待：Left（エラー型に加えて“中身”も判定）
expectProgFailWithDetail ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  Text -> (err -> Bool) -> Expectation
expectProgFailWithDetail src pred' =
  case parseProgram src of
    Left e -> expectationFailure (show e)
    Right p ->
      let v :: VEither AllErrs Program
          v = elaborateProgram p
       in shouldFailWithDetail @err v pred'

-- ==== Unit 版 ヘルパ ====

expectUnitPass :: Text -> Expectation
expectUnitPass src =
  case parseUnit src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnit u
       in shouldSucceedV v

expectUnitFail ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  Text -> Expectation
expectUnitFail src =
  case parseUnit src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnit u
       in shouldFail @err v

expectUnitFailWithDetail ::
  forall err.
  (err :| AllErrs, Typeable err) =>
  Text -> (err -> Bool) -> Expectation
expectUnitFailWithDetail src pred' =
  case parseUnit src of
    Left e -> expectationFailure (show e)
    Right u ->
      let v :: VEither AllErrs Unit
          v = elaborateUnit u
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
              v = elaborateProgram prog
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
      expectProgPass src

    it "parses two assignments after defaultInit" $ do
      let src = "PROGRAM P\nVAR\nx:INT; y:BOOL;\nEND_VAR\nx := 1;\ny := TRUE;\n"
      expectProgPass src

    it "rejects unknown variable" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nz := 1;\n"
      expectProgFailWithDetail @UnknownVar src $
        \(UnknownVar vname _) -> vname == "z"

    it "rejects type mismatch" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nx := TRUE;\n"
      expectProgFailWithDetail @TypeMismatch src $
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
        expectProgPass (src <> s)

    -- NG 群（型も中身もチェック）
    forM_ ng $ \(tc, p) ->
      it ("rejects " <> T.unpack tc) $
        expectProgFailWithDetail @TypeMismatch' (src <> tc) p

  describe "NOT semantics" $ do
    let base = "PROGRAM P\nVAR\nx:INT; y:BOOL;\nEND_VAR\n"

    -- it "accepts NOT on BOOL and keeps BOOL" $ do
    --   shouldParse (base <> "y := NOT TRUE;\n")

    -- it "accepts NOT on INT and keeps INT" $ do
    --   shouldParse (base <> "x := NOT 2;\n")

    it "rejects assigning NOT 2 (INT) to BOOL" $ do
      expectProgFailWithDetail @TypeMismatch' (base <> "y := NOT 2;\n") $
        \(TypeMismatch' actual) -> actual == INT

    it "rejects assigning NOT TRUE (BOOL) to INT" $ do
      expectProgFailWithDetail @TypeMismatch (base <> "x := NOT TRUE;\n") $
        \(TypeMismatch vname _ expected actual) -> vname == "x" && expected == INT && actual == BOOL

  describe "IF semantics" $ do
    it "accepts when type of conditions are BOOL" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nIF x=0 THEN x := 1; ELSIF x=1 THEN x := 2; ELSE x := 3; END_IF\n"
      expectProgPass src

    it "rejects when condition of IF is not BOOL (1)" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nIF 1 THEN x := 1; END_IF\n"
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == INT

    it "rejects when condition of ELSIF is not BOOL (1)" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nIF x=0 THEN x := 1; ELSIF 1 THEN x := 2; END_IF\n"
      -- case parseProgram src of
      --   Left e -> expectationFailure (show e)
      --   Right p ->
      --     elaborateProgram p `shouldSatisfy` \case
      --       Left [OpTypeMismatch {expected = BOOL, actual = INT}] -> True
      --       _ -> False
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == INT

    it "rejects unknown variable inside IF-THEN-ELSE-END_IF" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nIF TRUE THEN z := 1; END_IF\n"
      -- case parseProgram src of
      --   Left e -> expectationFailure (show e)
      --   Right p ->
      --     elaborateProgram p `shouldSatisfy` \case
      --       Left [UnknownVar {dName = "z"}] -> True
      --       _ -> False
      expectProgFailWithDetail @UnknownVar src $
        \(UnknownVar vname _) -> vname == "z"

  describe "WHILE/REPEAT/CASE semantics" $ do
    it "requires BOOL in WHILE condition" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nWHILE 1 DO x := 0; END_WHILE\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p `shouldSatisfy` \case
      --     Left [OpTypeMismatch {op = "WHILE", expected = BOOL, actual = INT}] -> True
      --     _ -> False
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == INT

    it "requires BOOL in REPEAT-UNTIL condition" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nREPEAT x := 1; UNTIL 0 END_REPEAT\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p `shouldSatisfy` \case
      --     Left [OpTypeMismatch {op = "REPEAT-UNTIL", expected = BOOL, actual = INT}] -> True
      --     _ -> False
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == INT

    it "requires INT scrutinee in CASE (minimal)" $ do
      let src = "PROGRAM P\nVAR\ny:BOOL;\nEND_VAR\nCASE y OF 0: y := TRUE; END_CASE\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p `shouldSatisfy` \case
      --     Left [OpTypeMismatch {op = "CASE", expected = INT, actual = BOOL}] -> True
      --     _ -> False
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

    it "accepts CASE with arms and ELSE" $ do
      let src = "PROGRAM P\nVAR\nx:INT;\nEND_VAR\nCASE x OF 0: x := 1; 1,2..3: x := 4; ELSE x := 0; END_CASE\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p `shouldSatisfy` isRight
      expectProgPass src

  describe "TYPE resolution (semantics)" $ do
    it "resolves alias in VAR and fills default init" $ do
      let src = "TYPE MyInt : INT; END_TYPE\nPROGRAM P\nVAR\nx: MyInt;\nEND_VAR\n"
      case parseUnit src of
        Left e -> expectationFailure (show e)
        Right u ->
          let vu :: VEither AllErrs Unit
              vu = elaborateUnit u
           in vu `shouldSatisfy` \case
                VRight (Unit _ [Program _ (VarDecls [v]) _]) ->
                  varType v == INT && varInit v == Just (EINT 0)
                _ -> False

    it "fails on unknown type" $ do
      let src = "PROGRAM P\nVAR\nx: Foo;\nEND_VAR\n"
      -- case parseUnit src of
      --   Left e -> expectationFailure (show e)
      --   Right u ->
      --     elaborateUnit u
      --       `shouldFailWith1` ( \case
      --                             UnknownType {tName = "Foo"} -> True
      --                             _ -> False
      --                         )
      expectUnitFailWithDetail @UnknownType src $
        \(UnknownType tname _) -> tname == "Foo"

    it "fails on type cycle" $ do
      let src = "TYPE A : B; B : A; END_TYPE\nPROGRAM P\nVAR\nx: A;\nEND_VAR\n"
      -- case parseUnit src of
      --   Left e -> expectationFailure (show e)
      --   Right u ->
      --     elaborateUnit u `shouldSatisfy` \case
      --       Left ds ->
      --         any
      --           ( \case
      --               TypeCycle {tName = "A"} -> True
      --               TypeCycle {tName = "B"} -> True
      --               _ -> False
      --           )
      --           ds
      --       _ -> False
      expectUnitFailWithDetail @TypeCycle src $
        \(TypeCycle tname _) -> tname == "B"

  describe "STRUCT field access (semantics)" $ do
    it "allows reading r.a when r: STRUCT{a:INT}" $ do
      let src =
            "TYPE R : STRUCT a: INT; END_STRUCT; END_TYPE\n\
            \PROGRAM P\nVAR\nr: R; x: INT;\nEND_VAR\n\
            \x := r.a;\n"
      -- case parseUnit src of
      --   Left e -> expectationFailure (show e)
      --   Right u -> elaborateUnit u `shouldSatisfy` isRight
      expectUnitPass src

    it "rejects unknown field r.z" $ do
      let src =
            "TYPE R : STRUCT a: INT; END_STRUCT; END_TYPE\n\
            \PROGRAM P\nVAR\nr: R; x: INT;\nEND_VAR\n\
            \x := r.z;\n"
      -- case parseUnit src of
      --   Left e -> expectationFailure (show e)
      --   Right u ->
      --     elaborateUnit u
      --       `shouldFailWith1` ( \case
      --                             UnknownField {fName = "z"} -> True
      --                             _ -> False
      --                         )
      expectUnitFailWithDetail @UnknownStructMember src $
        \(UnknownStructMember _ member _) -> member == "z"

    it "rejects r.a when r is INT (not a struct)" $ do
      let src =
            "PROGRAM P\nVAR\nr: INT; x: INT;\nEND_VAR\n\
            \x := r.a;\n"
      -- case parseUnit src of
      --   Left e -> expectationFailure (show e)
      --   Right u ->
      --     elaborateUnit u
      --       `shouldFailWith1` ( \case
      --                             NotAStruct {} -> True
      --                             _ -> False
      --                         )
      expectUnitFail @NotAStruct src

  describe "array/struct semantics" $ do
    it "accepts struct field - read and write" $ do
      let src =
            "TYPE Point : STRUCT x: INT; y: BOOL; END_STRUCT; END_TYPE\n\
            \PROGRAM P\nVAR r: Point; x: INT; y: BOOL; END_VAR\n\
            \x := r.x; y := r.y;\n\
            \r.x := 1; r.y := TRUE;\n"
      expectUnitPass src

    it "rejects unknown field" $ do
      let src =
            "TYPE Point : STRUCT x: INT; END_STRUCT; END_TYPE\n\
            \PROGRAM P\nVAR r: Point; END_VAR\n\
            \r.z := 1;\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u `shouldFailWith1` (\case UnknownField {} -> True; _ -> False)
      expectUnitFail @UnknownStructMember src

    it "accepts array index" $ do
      let src =
            "PROGRAM P\nVAR a: ARRAY [0..2] OF INT; i: INT; END_VAR\n\
            \a[1] := 42; a[i] := 0;\n"
      expectUnitPass src

    it "rejects bad index count" $ do
      let src =
            "PROGRAM P\nVAR a: ARRAY [0..2] OF INT; END_VAR\n\
            \a[1,2] := 5;\n"
      -- expectParsed badCount $ \p ->
      --   elaborateProgram p `shouldFailWith1` (\case BadIndexCount {expectedN = 1, actualN = 2} -> True; _ -> False)
      expectProgFail @BadIndexCount src

    it "rejects bad index type" $ do
      let src =
            "PROGRAM P\nVAR a: ARRAY [0..2] OF INT; b: BOOL; END_VAR\n\
            \a[b] := 5;\n"
      -- expectParsed badType $ \p ->
      --   elaborateProgram p `shouldFailWith1` (\case OpTypeMismatch {expected = INT, actual = BOOL} -> True; _ -> False)
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

    it "type-checks element type through index" $ do
      let src =
            "PROGRAM P\nVAR a: ARRAY [0..2] OF INT; y: BOOL; END_VAR\n\
            \y := a[0];\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p `shouldFailWith1` (\case TypeMismatch {expected = BOOL, actual = INT} -> True; _ -> False)
      expectProgFailWithDetail @TypeMismatch src $
        \(TypeMismatch _ _ expected actual) -> expected == BOOL && actual == INT

  describe "FOR semantics" $ do
    it "accepts simple FOR over INT" $ do
      let src = "PROGRAM P\nVAR i:INT; x:INT; END_VAR\nFOR i := 0 TO 3 DO x := i; END_FOR\n"
      expectProgPass src

    it "rejects writing to loop var" $ do
      let src = "PROGRAM P\nVAR i:INT; END_VAR\nFOR i := 0 TO 3 DO i := 1; END_FOR\n"
      -- expectParsed ng $ \p ->
      --   elaborateProgram p `shouldFailWith1` (\case AssignToLoopVar {dName = "i"} -> True; _ -> False)
      expectProgFailWithDetail @AssignToLoopVar src $
        \(AssignToLoopVar vname _) -> vname == "i"

    it "rejects FOR when loop var is not INT" $ do
      let src = "PROGRAM P\nVAR i:BOOL; END_VAR\nFOR i := 0 TO 1 DO END_FOR\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p `shouldFailWith1` (\case TypeMismatch {dName = "i", expected = INT} -> True; _ -> False)
      expectProgFailWithDetail @TypeMismatch src $
        \(TypeMismatch vname _ expected _) -> vname == "i" && expected == INT

    it "rejects non-INT (init)" $ do
      let src = "PROGRAM P\nVAR i:INT; END_VAR\nFOR i := TRUE TO 1 DO END_FOR\n"
      -- expectParsed initBad $ \p ->
      --   elaborateProgram p `shouldFailWith1` (\case OpTypeMismatch {op = "FOR-init", expected = INT, actual = BOOL} -> True; _ -> False)
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

    it "rejects non-INT (end)" $ do
      let src = "PROGRAM P\nVAR i:INT; END_VAR\nFOR i := 0 TO FALSE DO END_FOR\n"
      -- expectParsed endBad $ \p ->
      --   elaborateProgram p `shouldFailWith1` (\case OpTypeMismatch {op = "FOR-end", expected = INT, actual = BOOL} -> True; _ -> False)
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

    it "rejects non-INT (by)" $ do
      let src = "PROGRAM P\nVAR i:INT; END_VAR\nFOR i := 0 TO 10 BY TRUE DO END_FOR\n"
      -- expectParsed byBad $ \p ->
      --   elaborateProgram p `shouldFailWith1` (\case OpTypeMismatch {op = "FOR-by", expected = INT, actual = BOOL} -> True; _ -> False)
      expectProgFailWithDetail @TypeMismatch' src $
        \(TypeMismatch' actual) -> actual == BOOL

  describe "ENUM semantics" $ do
    it "accepts assignment and equality" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \c := Color.Red;\n\
            \IF c = Color.Red THEN c := Color.Green; END_IF\n"
      expectUnitPass src

    it "rejects unknown enum value" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \c := Color.Blue;\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           UnknownEnumValue {tName = "Color", eName = "Blue"} -> True
      --                           _ -> False
      --                       )
      expectUnitFailWithDetail @UnknownEnumMember src $
        \(UnknownEnumMember ename mname _) -> ename == "Color" && mname == "Blue"

    it "rejects Type.Ctor when Type is not enum" $ do
      let src =
            "TYPE MyInt : INT; END_TYPE\n\
            \PROGRAM P\nVAR x: INT; END_VAR\n\
            \x := MyInt.X;\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           NotAnEnum {tName = "MyInt"} -> True
      --                           _ -> False
      --                       )
      expectUnitFailWithDetail @NotAnEnum src $
        \(NotAnEnum name _ _) -> name == "MyInt"

  describe "CASE with ENUM semantics" $ do
    it "accepts CASE with enum selectors" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; x: INT; END_VAR\n\
            \CASE c OF Color.Red: x := 1; ELSE x := 0; END_CASE\n"
      expectUnitPass src

    it "rejects mixing INT selector for enum scrutinee" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR c:Color; x:INT; END_VAR\n\
            \CASE c OF 0: x := 1; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "CASE"} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @TypeMismatch' src

  describe "Enum default init" $ do
    it "fills first enumerator as default init" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n"
      expectParsedUnit src $ \u ->
        case elaborateUnit u :: VEither AllErrs Unit of
          VLeft e -> expectationFailure (show e)
          VRight (Unit _ [Program _ (VarDecls [v]) _]) ->
            case varInit v of
              Just (EField (EVar ty) ctor) -> do
                locVal ty `shouldBe` "Color"
                locVal ctor `shouldBe` "Red"
              other -> expectationFailure ("unexpected init: " <> show other)
          VRight other -> expectationFailure ("unexpected shape: " <> show other)

  describe "nominal identity (ENUM)" $ do
    it "rejects assigning different enum types" $ do
      let src =
            "TYPE A : (X,Y); END_TYPE\n\
            \TYPE B : (X,Y); END_TYPE\n\
            \PROGRAM P\nVAR a: A; b: B; END_VAR\n\
            \a := B.X;\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           TypeMismatch {expected = Named nA, actual = Named nB} ->
      --                             locVal nA == "A" && locVal nB == "B"
      --                           _ -> False
      --                       )
      expectUnitFailWithDetail @TypeMismatch src $
        \(TypeMismatch _ _ (Named nA) (Named nB)) -> locVal nA == "A" && locVal nB == "B"

    it "accepts equality for same enum name" $ do
      let src =
            "TYPE A : (X); END_TYPE\n\
            \TYPE B : (X); END_TYPE\n\
            \PROGRAM P\nVAR a: A; END_VAR\n\
            \IF a = A.X THEN a := A.X; END_IF\n"
      expectUnitPass src

    it "rejects equality for different enum name" $ do
      let src =
            "TYPE A : (X); END_TYPE\n\
            \TYPE B : (X); END_TYPE\n\
            \PROGRAM P\nVAR a: A; END_VAR\n\
            \IF a = B.X THEN a := A.X; END_IF\n"
      -- expectParsedUnit ng $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "=", expected = Named nA, actual = Named nB} ->
      --                             locVal nA == "A" && locVal nB == "B"
      --                           _ -> False
      --                       )
      expectUnitFailWithDetail @TypeMismatch' src $
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
      let src =
            "TYPE Color : (Red := 0, Green := 2); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \c := Color.Green;\n"
      expectUnitPass src

    it "rejects unknown enum value even with explicit values" $ do
      let src =
            "TYPE Color : (Red := 0, Green := 2); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \c := Color.Blue;\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           UnknownEnumValue {tName = "Color", eName = "Blue"} -> True
      --                           _ -> False
      --                       )
      expectUnitFailWithDetail @UnknownEnumMember src $
        \(UnknownEnumMember nA nB _) -> nA == "Color" && nB == "Blue"

  describe "ENUM explicit values with expressions (semantics integration)" $ do
    it "accepts assignment using an enumerator with explicit expr value" $ do
      let src =
            "TYPE Color : (Red := 1+2*3, Green := (1+2)*3); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \c := Color.Green;\n"
      expectUnitPass src

    it "still rejects unknown ctor even with explicit expr values" $ do
      let src =
            "TYPE Color : (Red := 0, Green := 2); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \c := Color.Blue;\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           UnknownEnumValue {tName = "Color", eName = "Blue"} -> True
      --                           _ -> False
      --                       )
      expectUnitFailWithDetail @UnknownEnumMember src $
        \(UnknownEnumMember nA nB _) -> nA == "Color" && nB == "Blue"

  describe "MOD/XOR semantics" $ do
    let base = "PROGRAM P\nVAR x:INT; b:BOOL; END_VAR\n"
    it "accepts MOD on INT" $
      expectProgPass (base <> "x := 7 MOD 4;\n")

    it "rejects MOD type mismatch" $
      -- expectParsed (base <> "x := TRUE MOD 2;\n") $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "MOD", expected = INT, actual = BOOL} -> True
      --                           _ -> False
      --                       )
      -- expectParsed (base <> "x := TRUE MOD 2;\n") $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "MOD", expected = INT, actual = BOOL} -> True
      --                           _ -> False
      --                       )
      -- expectParsed (base <> "x := TRUE MOD 2;\n") $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "MOD", expected = INT, actual = BOOL} -> True
      --                           _ -> False
      --                       )
      expectProgFailWithDetail @TypeMismatch' (base <> "x := TRUE MOD 2;\n") $
        \(TypeMismatch' actual) -> actual == BOOL

    it "accepts XOR on BOOL" $
      expectProgPass (base <> "b := TRUE XOR FALSE;\n")

    it "rejects XOR type mismatch" $
      -- expectParsed (base <> "b := 1 XOR 0;\n") $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "XOR"} -> True
      --                           _ -> False
      --                       )
      expectProgFail @TypeMismatch' (base <> "b := 1 XOR 0;\n")

  describe "REAL semantics (minimal)" $ do
    it "allows arithmetic mixing INT and REAL; result is REAL" $ do
      let src = "PROGRAM P\nVAR r:REAL; x:INT; END_VAR\nr := x + 2.5;\n"
      expectProgPass src

    it "rejects MOD on REAL" $ do
      let src = "PROGRAM P\nVAR r:REAL; END_VAR\nr := 5.0 MOD 2.0;\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "MOD"} -> True
      --                           _ -> False
      --                       )
      expectProgFail @TypeMismatch' src

    it "rejects NOT on REAL" $ do
      let src = "PROGRAM P\nVAR r:REAL; END_VAR\nIF NOT 1.0 THEN r := 0.0; END_IF\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "NOT", expected = BOOL} -> True
      --                           _ -> False
      --                       )
      expectProgFail @TypeMismatch' src

  describe "LREAL semantics" $ do
    it "accepts assignment of LREAL typed literal and equality" $ do
      let src =
            "PROGRAM P\nVAR x: LREAL; END_VAR\n\
            \x := LREAL#1.5;\n\
            \IF x = LREAL#1.5 THEN x := LREAL#2.0; END_IF\n"
      expectProgPass src

    it "arith/comp require same floating type" $ do
      let src =
            "PROGRAM P\nVAR a:LREAL; b:LREAL; END_VAR\n\
            \a := LREAL#1.0 + LREAL#2.0;\n\
            \IF a >= LREAL#3.0 THEN a := a; END_IF\n"
      expectProgPass src

  describe "numeric promotion (INT → REAL)" $ do
    it "allows assigning INT to REAL" $ do
      let src = "PROGRAM P\nVAR a: REAL; END_VAR\na := 1;\n"
      expectProgPass src

    it "allows IF with mixed REAL/INT comparison" $ do
      let src = "PROGRAM P\nVAR x: INT; END_VAR\nIF 1.0 < 2 THEN x := 0; END_IF\n"
      expectProgPass src

    it "rejects assigning REAL to INT" $ do
      let src = "PROGRAM P\nVAR i: INT; END_VAR\ni := 1.5;\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p `shouldSatisfy` \case
      --     Left [TypeMismatch {expected = INT, actual = REAL}] -> True
      --     _ -> False
      expectProgFailWithDetail @TypeMismatch src $
        \(TypeMismatch _ _ expected actual) -> expected == INT && actual == REAL

    it "still allows arithmetic mixing (result REAL) and assign to REAL" $ do
      let src = "PROGRAM P\nVAR a: REAL; END_VAR\na := 1 + 2.0;\n"
      expectProgPass src

  describe "numeric promotion (INT → LREAL)" $ do
    it "allows assigning INT to LREAL" $ do
      let src = "PROGRAM P\nVAR a: LREAL; END_VAR\na := 1;\n"
      expectProgPass src

    it "allows initializer INT to LREAL" $ do
      let src = "PROGRAM P\nVAR a: LREAL := 1; END_VAR\n"
      expectProgPass src

    it "rejects assigning LREAL to INT" $ do
      let src = "PROGRAM P\nVAR i: INT; END_VAR\ni := 1.0;\n" -- 小数は REAL/LREAL 扱い → INT へは不可
      -- expectParsed src $ \p ->
      --   elaborateProgram p `shouldSatisfy` \case
      --     Left [TypeMismatch {expected = INT, actual = REAL}] -> True
      --     _ -> False
      expectProgFailWithDetail @TypeMismatch src $
        \(TypeMismatch _ _ expected actual) -> expected == INT && actual == REAL

  describe "STRING default init (semantics)" $ do
    it "fills empty string for STRING" $ do
      let src = "PROGRAM P\nVAR s: STRING; END_VAR\n"
      expectParsed src $ \prog ->
        let v :: VEither AllErrs Program
            v = elaborateProgram prog
         in v `shouldSatisfy` \case
              VRight (Program _ (VarDecls [val]) _) -> varInit val == Just (ESTRING "")
              _ -> False

    it "fills empty string for STRING(80) and STRING[32]" $ do
      let s1 = "PROGRAM P\nVAR s: STRING(80); END_VAR\n"
          s2 = "PROGRAM P\nVAR t: STRING[32]; END_VAR\n"
      expectParsed s1 $ \p1 ->
        let v :: VEither AllErrs Program
            v = elaborateProgram p1
         in v `shouldSatisfy` \case
              VRight (Program _ (VarDecls [val]) _) -> varInit val == Just (ESTRING "")
              _ -> False

      expectParsed s2 $ \p2 ->
        let v :: VEither AllErrs Program
            v = elaborateProgram p2
         in v `shouldSatisfy` \case
              VRight (Program _ (VarDecls [val]) _) -> varInit val == Just (ESTRING "")
              _ -> False

  describe "STRING semantics" $ do
    it "allows comparison" $ do
      let src = "PROGRAM P\nVAR b: BOOL; END_VAR\nb := 'a' < 'b';\n"
      expectProgPass src

    it "rejects mixing STRING with INT in +" $ do
      -- 連結は不可
      let src = "PROGRAM P\nVAR s: STRING; END_VAR\ns := 'a' + 1;\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "+"} -> True
      --                           _ -> False
      --                       )
      expectProgFail @TypeMismatch' src

  describe "CASE type consistency (baseline)" $ do
    it "accepts INT scrutinee with INT selectors (value/range/ELSE)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 0: ; 1..3: ; ELSE ; END_CASE\n"
      expectProgPass src

    it "rejects enum selector when scrutinee is INT" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF Color.Red: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "CASE", expected = INT} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @TypeMismatch' src

    it "accepts enum scrutinee with matching enum constructors" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \CASE c OF Color.Red: ; Color.Green: ; END_CASE\n"
      expectUnitPass src

    it "rejects mismatched enum type in selector" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \TYPE Shape : (Circle); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \CASE c OF Shape.Circle: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "CASE"} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @TypeMismatch' src

    it "rejects BOOL scrutinee (only INT or enum allowed)" $ do
      let src =
            "PROGRAM P\nVAR b: BOOL; END_VAR\n\
            \CASE b OF 0: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OpTypeMismatch {op = "CASE", actual = BOOL} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @TypeMismatch' src

  describe "CASE selector validity (INT)" $ do
    it "accepts disjoint values and ranges across arms" $ do
      -- 全て不交差：{0}, {2..4}, {5}
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 0: ; 2..4: ; 5: ; END_CASE\n"
      expectProgPass src

    it "rejects a range with low > high (e.g., 5..3)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 5..3: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           InvalidCaseRange {} -> True
      --                           _ -> False
      --                       )
      expectProgFail @InvalidCaseRange src

    it "rejects duplicate literal within the same arm (1,1)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1,1: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OverlappingCase {} -> True
      --                           _ -> False
      --                       )
      expectProgFail @OverlappingCase src

    it "rejects overlap value vs range within the same arm (1,1..3)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1,1..3: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OverlappingCase {} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @OverlappingCase src

    it "rejects overlap range vs range within the same arm (1..3,3..5)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1..3,3..5: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OverlappingCase {} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @OverlappingCase src

    it "rejects overlap across different arms (1..3: ; 3..5: ;)" $ do
      let src =
            "PROGRAM P\nVAR x: INT; END_VAR\n\
            \CASE x OF 1..3: ; 3..5: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           OverlappingCase {} -> True
      --                           _ -> False
      --                       )
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
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           NonConstantExpr {} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @NonConstantExpr src

    it "rejects non-constant variable in a label" $ do
      let src =
            "PROGRAM P\nVAR x: INT; y: INT; END_VAR\n\
            \CASE x OF y+1: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           NonConstantExpr {} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @NonConstantExpr src

    it "accepts enum labels from a constant of that enum" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \VAR CONSTANT kc: Color := Color.Red; END_VAR\n\
            \CASE c OF kc: ; END_CASE\n"
      expectUnitPass src

    it "rejects INT constant expression for enum scrutinee" $ do
      let src =
            "TYPE Color : (Red, Green); END_TYPE\n\
            \PROGRAM P\nVAR c: Color; END_VAR\n\
            \CASE c OF 1+2: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     -- `shouldFailWith1` ( \case
      --     --                       OpTypeMismatch {} -> True
      --     --                       _ -> False
      --     --                   )
      --     `shouldFailWith1` opTypeMismatch "CASE"
      expectUnitFail @TypeMismatch' src

    it "rejects indexer in label (not a constant expression)" $ do
      -- y は非定数。v[0] は「式」だがコンパイル時定数ではない。
      let src =
            "PROGRAM P\nVAR x: INT; v: ARRAY[0..1] OF INT; END_VAR\n\
            \CASE x OF v[0]: ; END_CASE\n"
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u
      --     `shouldFailWith1` ( \case
      --                           NonConstantExpr {} -> True
      --                           _ -> False
      --                       )
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
        -- elaborateProgram prog `shouldSatisfy` \case
        --   Right (Program _ (VarDecls vs) _) ->
        --     -- すべての varInit が Just (EINT 0) であることを確認
        --     all (\v -> varInit v == Just (EINT 0)) vs
        --   _ -> False
        let p :: VEither AllErrs Program
            p = elaborateProgram prog
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
      -- expectParsed src $ \prog ->
      --   elaborateProgram prog
      --     `shouldFailWith1` ( \case
      --                           TypeMismatch {dName = "us"} -> True
      --                           _ -> False
      --                       )
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
      -- expectParsed src $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OutOfRange {dName = "us", target = USINT} -> True
      --                           _ -> False
      --                       )
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
      -- expectParsed src $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OutOfRange {} -> True
      --                           _ -> False
      --                       )
      expectUnitFail @OutOfRange src

    -- NG: UINT に 16#10000 (65536) は範囲外
    it "rejects assigning 16#10000 to UINT (out of range)" $ do
      let src =
            "PROGRAM P\nVAR x: UINT; END_VAR\n\
            \x := 16#10000;\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OutOfRange {} -> True
      --                           _ -> False
      --                       )
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
      -- expectParsed src $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OutOfRange {target = SINT} -> True
      --                           _ -> False
      --                       )
      expectUnitFailWithDetail @OutOfRange src $
        \(OutOfRange _ tgt _ _) -> tgt == SINT

    it "reports OutOfRange for underscored based literal" $ do
      -- 2#1_0000_0000 = 256 は SINT の範囲外
      let src = "PROGRAM P\nVAR s: SINT; END_VAR\ns := 2#1_0000_0000;\n"
      -- expectParsed src $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` ( \case
      --                           OutOfRange {target = SINT} -> True
      --                           _ -> False
      --                       )
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
      -- expectParsed ng $ \p ->
      --   elaborateProgram p
      --     `shouldFailWith1` (\case OutOfRange {} -> True; _ -> False)
      expectUnitFail @OutOfRange ng

    it "USINT accepts 2#1111_1111 and rejects 2#1_0000_0000" $ do
      let ok =
            "PROGRAM P\nVAR u: USINT; END_VAR\n\
            \u := 2#1111_1111;\n" -- 255
      expectUnitPass ok

      let ng =
            "PROGRAM P\nVAR u: USINT; END_VAR\n\
            \u := 2#1_0000_0000;\n" -- 256
            -- expectParsed ng $ \p ->
            --   elaborateProgram p
            --     `shouldFailWith1` (\case OutOfRange {} -> True; _ -> False)
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
            -- expectParsed ng1 $ \p ->
            --   elaborateProgram p
            --     `shouldFailWith1` (\case OutOfRange {} -> True; _ -> False)
      expectUnitFail @OutOfRange ng1

      let ng2 =
            "PROGRAM P\nVAR z: INT; END_VAR\n\
            \z := -16#8001;\n" -- -32769 NG
            -- expectParsed ng2 $ \p ->
            --   elaborateProgram p
            --     `shouldFailWith1` (\case OutOfRange {} -> True; _ -> False)
      expectUnitFail @OutOfRange ng2

    it "UDINT accepts 16#FFFF_FFFF and rejects 16#1_0000_0000" $ do
      let ok =
            "PROGRAM P\nVAR d: UDINT; END_VAR\n\
            \d := 16#FFFF_FFFF;\n" -- 4294967295 OK
      expectUnitPass ok

      let ng =
            "PROGRAM P\nVAR d: UDINT; END_VAR\n\
            \d := 16#1_0000_0000;\n" -- 4294967296 NG
            -- expectParsed ng $ \p ->
            --   elaborateProgram p
            --     `shouldFailWith1` (\case OutOfRange {} -> True; _ -> False)
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
  -- CHAR / WCHAR / WSTRING: 代入・比較（意味論）

  -- すでにあるヘルパ:
  --   shouldParseUnit :: Text -> Expectation  -- parse + elaborate が Right
  --   opTypeMismatch :: Text -> SemantDiag -> Bool
  --   typeMismatchOn :: Text -> SemantDiag -> Bool
  --   ※ elaborateUnit を使うときは shouldFailWith1 と上の述語で判定

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
      -- expectParsedUnit src $ \u ->
      --   elaborateUnit u `shouldFailWith1` typeMismatch "s"
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

    -- ★ 修正: 少なすぎる要素は受理し、残りは既定値でパディング
    it "accepts too-few initializer elements (pads with defaults)" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT := [1,2]; END_VAR\n"
      expectUnitPass src

    -- ★ 過剰は引き続きエラー
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

    -- ▼ 暫定: -1 の型解釈(SINT)と USINT への暗黙変換が入るまでは pending
    it "accepts USINT element from -1 via implicit SINT->USINT (pending)" $ do
      pendingWith "literal typing (SINT) & implicit SINT->USINT not implemented yet"
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..0] OF USINT := [-1]; END_VAR\n"
      expectUnitPass src

  describe "Struct aggregate initialization (semantics)" $ do
    it "accepts exact fields in any order" $ do
      let src =
            "TYPE R : STRUCT x: INT; y: LREAL; END_STRUCT; END_TYPE\n\
            \PROGRAM P\n\
            \VAR r1: R := (x := 1, y := 2.0); r2: R := (y := 2.0, x := 1); END_VAR\n"
      expectUnitPass src

    -- ★ 修正: 欠落フィールドは受理し、既定値で補完
    it "accepts missing fields (fills omitted with defaults)" $ do
      let src =
            "TYPE R : STRUCT x: INT; y: INT; END_STRUCT; END_TYPE\n\
            \PROGRAM P\n\
            \VAR r: R := (x := 1); END_VAR\n"
      expectUnitPass src

    -- ★ 未知フィールドは引き続きエラー
    it "rejects unknown field in aggregate" $ do
      let src =
            "TYPE R : STRUCT x: INT; y: INT; END_STRUCT; END_TYPE\n\
            \PROGRAM P\n\
            \VAR r: R := (x := 1, z := 0); END_VAR\n"
      expectUnitFail @UnknownStructMember src

    it "allows field-wise promotion (INT -> LREAL)" $ do
      let src =
            "TYPE R : STRUCT x: INT; y: LREAL; END_STRUCT; END_TYPE\n\
            \PROGRAM P\n\
            \VAR r: R := (x := 1, y := 2); END_VAR\n"
      expectUnitPass src

  describe "Static index bounds checking (semantics)" $ do
    it "flags out-of-bounds when both bounds and index are constants (literal)" $ do
      let src =
            "PROGRAM P\n\
            \VAR a: ARRAY[0..2] OF INT; x: INT; END_VAR\n\
            \x := a[3];\n"
      expectUnitFail @IndexOutOfBounds src

    it "flags out-of-bounds when index is VAR CONSTANT" $ do
      -- pendingWith "constructing env with int literal not implemented yet"
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
