{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Char
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import ST.AST
import ST.Parser
import Test.Hspec
import Text.Megaparsec

-- utils

shouldParseTo :: (Show a, Show b, Eq a, Eq b) => (t -> Either a b) -> t -> b -> Expectation
shouldParseTo run src want = run src `shouldBe` Right want

shouldParse :: (Show a, Show b) => (t -> Either a b) -> t -> Expectation
shouldParse run src = run src `shouldSatisfy` isRight

isLeft :: Either a b -> Bool
isLeft = not . isRight

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

toIdent :: Text -> Identifier
toIdent txt =
  let start = initialPos "<test>" -- 行=1, 列=1
      end = start {sourceColumn = mkPos (1 + T.length txt)}
   in Loc (Span start end) txt

varSig :: Variable -> (Text, STType, Maybe Expr, Bool)
varSig v =
  ( locVal (varName v),
    varType v,
    varInit v,
    varConst v
  )

varSigs :: VarDecls -> [(Text, STType, Maybe Expr, Bool)]
varSigs (VarDecls vs) = map varSig vs

expectRight :: (Show e) => Either e a -> (a -> Expectation) -> Expectation
expectRight (Left e) _ = expectationFailure (show e)
expectRight (Right x) k = k x

newlineToSpace :: Text -> Text
newlineToSpace = T.map (\c -> if c == '\n' then ' ' else c)

dummyPos :: SourcePos
dummyPos = initialPos "<no-span>"

dummySpan :: Span
dummySpan = Span dummyPos dummyPos

stripId :: Identifier -> Identifier
stripId (Loc _ v) = Loc dummySpan v

runInt :: Text -> Either (ParseErrorBundle Text Void) Int
runInt = parse (pInt <* eof) "<test>"

runReal :: Text -> Either (ParseErrorBundle Text Void) Double
runReal = parse (pReal <* eof) "<test>"

runE :: Text -> Either (ParseErrorBundle Text Void) Expr
runE = parse (pExpr <* eof) "<test>"

stripExpr :: Expr -> Expr
stripExpr =
  \case
    EINT n -> EINT n
    EBOOL b -> EBOOL b
    EREAL r -> EREAL r
    ELREAL lr -> ELREAL lr
    ESTRING s -> ESTRING s
    EWSTRING s -> EWSTRING s
    ECHAR c -> ECHAR c
    EWCHAR c -> EWCHAR c
    ENeg e -> ENeg (stripExpr e)
    EAdd a b -> EAdd (stripExpr a) (stripExpr b)
    ESub a b -> ESub (stripExpr a) (stripExpr b)
    EMul a b -> EMul (stripExpr a) (stripExpr b)
    EDiv a b -> EDiv (stripExpr a) (stripExpr b)
    EMod a b -> EMod (stripExpr a) (stripExpr b)
    EEq a b -> EEq (stripExpr a) (stripExpr b)
    ENe a b -> ENe (stripExpr a) (stripExpr b)
    ELt a b -> ELt (stripExpr a) (stripExpr b)
    ELe a b -> ELe (stripExpr a) (stripExpr b)
    EGt a b -> EGt (stripExpr a) (stripExpr b)
    EGe a b -> EGe (stripExpr a) (stripExpr b)
    ENot e -> ENot (stripExpr e)
    EAnd a b -> EAnd (stripExpr a) (stripExpr b)
    EXor a b -> EXor (stripExpr a) (stripExpr b)
    EOr a b -> EOr (stripExpr a) (stripExpr b)
    EVar i -> EVar (stripId i)
    EField e f -> EField (stripExpr e) (stripId f)
    EIndex e f -> EIndex (stripExpr e) (map stripExpr f)
    EEnum t c -> EEnum (stripId t) (stripId c)
    EArrayAgg es -> EArrayAgg $ map stripExpr es
    EStructAgg flds ->
      let norm = [(toIdent (locVal nm), stripExpr e) | (nm, e) <- flds]
       in EStructAgg (sortOn (locVal . fst) norm)

stripLValue :: LValue -> LValue
stripLValue =
  \case
    LVar i -> LVar (stripId i)
    LField lv f -> LField (stripLValue lv) (stripId f)
    LIndex lv f -> LIndex (stripLValue lv) (map stripExpr f)

stripStmt :: Statement -> Statement
stripStmt =
  \case
    Assign lv e ->
      Assign (stripLValue lv) (stripExpr e)
    If c th es el ->
      If
        (stripExpr c)
        (map stripStmt th)
        [(stripExpr ec, map stripStmt ss) | (ec, ss) <- es]
        (map stripStmt el)
    While c ss ->
      While
        (stripExpr c)
        (map stripStmt ss)
    Repeat ss c ->
      Repeat
        (map stripStmt ss)
        (stripExpr c)
    Case e arms els ->
      Case
        (stripExpr e)
        [CaseArm sels (map stripStmt ss) | CaseArm sels ss <- arms]
        (map stripStmt els)
    -- For Identifier Expr Expr (Maybe Expr) [Statement]
    For i e f mE ss ->
      For
        (stripId i)
        (stripExpr e)
        (stripExpr f)
        ( if isJust mE
            then do
              ex <- mE
              Just $ stripExpr ex
            else Nothing
        )
        (map stripStmt ss)
    Skip -> Skip

-- 位置無視で「式」を比較
shouldParseExprToNoLoc ::
  (Show e) =>
  (Text -> Either e Expr) ->
  Text ->
  Expr ->
  Expectation
shouldParseExprToNoLoc run src want =
  case run src of
    Left e -> expectationFailure (show e)
    Right x -> stripExpr x `shouldBe` stripExpr want

-- 位置無視で「文」を比較（LHS が Identifier の現在仕様用）
shouldParseStmtToNoLoc ::
  (Show e) =>
  (Text -> Either e Statement) ->
  Text ->
  Statement ->
  Expectation
shouldParseStmtToNoLoc run src want =
  case run src of
    Left e -> expectationFailure (show e)
    Right x -> stripStmt x `shouldBe` stripStmt want

-- 多相なヘルパ
shouldParseNumTo ::
  (Eq a, Show a) =>
  (Text -> Either (ParseErrorBundle Text Void) a) ->
  Text ->
  a ->
  Expectation
shouldParseNumTo run src want =
  case run src of
    Right x -> x `shouldBe` want
    Left e -> expectationFailure (show e)

shouldRejectNum ::
  (Text -> Either (ParseErrorBundle Text Void) a) ->
  Text ->
  Expectation
shouldRejectNum run src =
  case run src of
    Left _ -> pure ()
    Right _ -> expectationFailure "expected parse error (Left), but got Right"

-- tests

main :: IO ()
main = hspec $ do
  describe "Minimal ST parser" $ do
    it "parses a bare PROGRAM + empty VAR..END_VAR block" $ do
      let src = "PROGRAM PLC_PRG\nVAR\nEND_VAR\n" :: Text
      parseProgram src `shouldBe` Right (Program "PLC_PRG" (VarDecls []) [])

    it "fails when END_VAR is missing" $ do
      let src = "PROGRAM\nVAR\n"
      parseProgram src `shouldSatisfy` isLeft

    it "fails when identifier is invalid" $ do
      let src = "PROGRAM AB-\nVAR\nEND_VAR"
      parseProgram src `shouldSatisfy` isLeft
  describe "identifier parser" $ do
    let ok =
          [ "PLC_PRG",
            "_A9",
            "A_b0",
            "IW215",
            "IW215Z",
            "QX75",
            "IDENT",
            "LIM_SW_5",
            "LimSw5",
            "abcd",
            "ab_Cd",
            "_MAIN",
            "_12V7",
            T.pack $ map chr [0x41 .. 0x5A], -- [A-Z]
            T.pack $ map chr [0x61 .. 0x7A] -- [a-z]
          ]
        ng =
          [ "",
            "__LIM_SW5",
            "LIM__SW5",
            "LIM_SW5_",
            "9ABC",
            "A-B",
            "A.B",
            "AB-",
            "あ",
            "int",
            T.pack $
              map chr [0x20 .. 0x2F], -- " !\"#$%&'()*+,-./"
            T.pack $ map chr [0x3A .. 0x40], -- ":;<=>?@"
            -- 0x41 to 0x5A: [A-Z]
            T.pack $ map chr [0x5B .. 0x5E], -- "[\\]^"
            -- 0x5F: underscore "_"
            T.pack [chr 0x60], -- "^"
            -- 0x61 to 0x7A: [a-z]
            T.pack $ map chr [0x7B .. 0x7E] -- "{|}~"
          ]
        run = parse (identifier <* eof) "<test>"
    forM_ ok $ \tc ->
      it ("parses " <> T.unpack tc) $
        shouldParseTo run tc (toIdent tc)
    forM_ ng $ \tc ->
      it ("rejects " <> T.unpack tc) $
        run tc `shouldSatisfy` isLeft

  describe "Variable declaration" $ do
    let ok =
          [ ("_LIM_SW5:INT;", Variable (toIdent "_LIM_SW5") INT Nothing False),
            ("ab:BOOL;", Variable (toIdent "ab") BOOL Nothing False)
          ]
        run = parse (pVariable False <* eof) "<test>"
    forM_ ok $ \(tc, want) ->
      it ("parses " <> T.unpack tc) $
        shouldParseTo run tc want

  describe "Expressions" $ do
    let ok =
          [ ("1 + 2 * 3", EAdd (EINT 1) (EMul (EINT 2) (EINT 3))),
            ("(1 + 2) * 3", EMul (EAdd (EINT 1) (EINT 2)) (EINT 3)),
            ("-1", ENeg (EINT 1)),
            ("1 = 1", EEq (EINT 1) (EINT 1)),
            ("1 <> 2", ENe (EINT 1) (EINT 2)),
            ("1 < 2", ELt (EINT 1) (EINT 2)),
            ("1 <= 2", ELe (EINT 1) (EINT 2)),
            ("1 > 2", EGt (EINT 1) (EINT 2)),
            ("1 >= 2", EGe (EINT 1) (EINT 2)),
            ("1 + 2 * 3 = 7", EEq (EAdd (EINT 1) (EMul (EINT 2) (EINT 3))) (EINT 7)),
            ("(1 + 2) * 3 < 10", ELt (EMul (EAdd (EINT 1) (EINT 2)) (EINT 3)) (EINT 10)),
            ("-1 < 0", ELt (ENeg (EINT 1)) (EINT 0)),
            ("NOT TRUE", ENot (EBOOL True)),
            ("TRUE AND FALSE", EAnd (EBOOL True) (EBOOL False)),
            ("NOT TRUE OR FALSE", EOr (ENot (EBOOL True)) (EBOOL False)), -- NOT > OR
            ("NOT (TRUE OR FALSE)", ENot (EOr (EBOOL True) (EBOOL False))),
            ("1 < 2 AND 3 < 4", EAnd (ELt (EINT 1) (EINT 2)) (ELt (EINT 3) (EINT 4)))
          ]
        ng = ["1 < 2 < 3", "1 <= 2 <= 3", "1 = 2 = 3"]
        run = parse (pExpr <* eof) "<test>"
    forM_ ok $ \(tc, want) ->
      it ("parses " <> T.unpack tc) $
        shouldParseTo run tc want
    forM_ ng $ \tc ->
      it ("rejects " <> T.unpack tc) $
        run tc `shouldSatisfy` isLeft
  describe "VAR block" $ do
    it "parses variables without explicit init" $ do
      let src = "PROGRAM ABC\nVAR\n\t_LIM_SW5:INT;\n\tab:BOOL;\nEND_VAR\n"
          expected =
            [ ("_LIM_SW5", INT, Nothing, False),
              ("ab", BOOL, Nothing, False)
            ]
      expectRight (parseProgram src) $
        \(Program _ vds _) -> varSigs vds `shouldBe` expected

    it "parses INT with explicit init" $ do
      let src = "PROGRAM P\nVAR\n\tx:INT := 42;\nEND_VAR\n"
          expected =
            [("x", INT, Just (EINT 42), False)]
      expectRight (parseProgram src) $
        \(Program _ vds _) -> varSigs vds `shouldBe` expected

  describe "Simple Statement" $ do
    let ok =
          [ ("x:=1;", Assign (LVar (toIdent "x")) (EINT 1)),
            ("y:=TRUE;", Assign (LVar (toIdent "y")) (EBOOL True))
          ]
        run = parse (pStmt <* eof) "<test>"
    forM_ ok $ \(tc, want) ->
      it ("parses " <> T.unpack tc) $
        shouldParseTo run tc want
  describe "If Statement" $ do
    let ok =
          [ "IF x=0 THEN x := 1; ELSIF x=1 THEN x := 2; ELSE x := 3; END_IF"
          ]
        run = parse (pStmt <* eof) "<test>"
    forM_ ok $ \tc ->
      it ("parses " <> T.unpack tc) $
        shouldParse run tc
  describe "WHILE/REPEAT/CASE parsing" $ do
    let ok =
          [ "WHILE x < 10 DO x := x + 1; END_WHILE",
            "REPEAT x := x + 1; UNTIL x >= 10 END_REPEAT",
            "CASE x OF 0: x := 1; 1,2..4: x := 5; ELSE x := 0; END_CASE"
          ]
        run = parse (pStmt <* eof) "<test>"
    forM_ ok $ \tc ->
      it ("parses " <> T.unpack tc) $
        shouldParse run tc

  describe "TYPE block parsing" $ do
    let ok =
          [ "TYPE MyInt : INT; END_TYPE",
            "TYPE Point : STRUCT x: INT; y: INT; END_STRUCT; END_TYPE",
            "TYPE A : INT; B : BOOL; END_TYPE",
            "TYPE R : STRUCT a: INT; b: BOOL; END_STRUCT; Alias : R; END_TYPE",
            "TYPE R : STRUCT p: STRUCT x: INT; END_STRUCT; END_STRUCT; END_TYPE"
          ]
        run = parse (pTypeBlock <* eof) "<test>"
    forM_ ok $ \tc ->
      it ("parses TYPE block: " <> T.unpack tc) $
        shouldParse run tc

  describe "TYPE + VAR integration (parsing)" $ do
    let ok =
          [ "TYPE MyInt : INT; END_TYPE\nPROGRAM P\nVAR\nx: MyInt;\nEND_VAR\n"
          ]
        run = parse (pUnit <* eof) "<test>"
    forM_ ok $ \tc ->
      it ("parses unit: " <> T.unpack (newlineToSpace tc)) $
        shouldParse run tc

  describe "field access parsing" $ do
    let ok =
          [ ("r.a", EField (EVar (toIdent "r")) (toIdent "a")),
            ("r.a.b", EField (EField (EVar (toIdent "r")) (toIdent "a")) (toIdent "b")),
            ("(r).a", EField (EVar (toIdent "r")) (toIdent "a"))
          ]
        run = parse (pExpr <* eof) "<test>"
    forM_ ok $ \(tc, want) ->
      it ("parses " <> T.unpack tc) $
        shouldParseExprToNoLoc run tc want

  describe "Designator (field) parsing" $ do
    let shouldParseStmt src want =
          case parse (pStmt <* eof) "<test>" src of
            Left e -> expectationFailure (show e)
            Right s -> stripStmt s `shouldBe` stripStmt want
    it "parses field read in expr: r.a := 1;" $
      shouldParseStmt "r.a := 1;" (Assign (LField (LVar (toIdent "r")) (toIdent "a")) (EINT 1))
    -- 式だけのパースも

    it "parses r.a as EField" $
      shouldParseExprToNoLoc runE "r.a" (EField (EVar (toIdent "r")) (toIdent "a"))

  describe "ARRAY type parsing" $ do
    let run = parse (pTypeBlock <* eof) "<test>"
    it "parses 1D array type" $
      shouldParse run "TYPE A : ARRAY [1..10] OF INT; END_TYPE"
    it "parses 2D array of BOOL" $
      shouldParse run "TYPE M : ARRAY [0..1, 0..2] OF BOOL; END_TYPE"

  describe "indexing (expression)" $ do
    let ok =
          [ ("a[1]", EIndex (EVar (toIdent "a")) [EINT 1]),
            ("a[1,2]", EIndex (EVar (toIdent "a")) [EINT 1, EINT 2]),
            ("r.a[0]", EIndex (EField (EVar (toIdent "r")) (toIdent "a")) [EINT 0])
          ]
        run = parse (pExpr <* eof) "<test>"
    forM_ ok $ \(tc, want) ->
      it ("parses " <> T.unpack tc) $
        shouldParseExprToNoLoc run tc want

  describe "indexing (assignment LHS)" $ do
    let ok =
          [ ( "a[i] := 1;",
              Assign (LIndex (LVar (toIdent "a")) [EVar (toIdent "i")]) (EINT 1)
            ),
            ( "r.a[0] := 42;",
              Assign (LIndex (LField (LVar (toIdent "r")) (toIdent "a")) [EINT 0]) (EINT 42)
            )
          ]
        run = parse (pStmt <* eof) "<test>"
    forM_ ok $ \(tc, want) ->
      it ("parses " <> T.unpack tc) $
        shouldParseStmtToNoLoc run tc want

  describe "FOR parsing" $ do
    let ok =
          [ "FOR i := 0 TO 10 DO x := 1; END_FOR",
            "FOR i := 0 TO 10 BY 2 DO END_FOR"
          ]
        run = parse (pStmt <* eof) "<test>"
    forM_ ok $ \tc ->
      it ("parses " <> T.unpack tc) $
        shouldParse run tc

  -- TYPE block に enum
  describe "TYPE block parsing (enum)" $ do
    let ok =
          [ "TYPE Color : (Red, Green, Blue); END_TYPE",
            "TYPE Flag  : (Off, On); END_TYPE"
          ]
        run = parse (pTypeBlock <* eof) "<test>"
    forM_ ok $ \tc ->
      it ("parses enum TYPE: " <> T.unpack tc) $
        shouldParse run tc

  -- 列挙リテラル（式）
  describe "Enum literal parsing" $ do
    let run = parse (pExpr <* eof) "<test>"
    it "parses Color.Red" $
      shouldParseExprToNoLoc
        run
        "Color.Red"
        (EField (EVar (toIdent "Color")) (toIdent "Red"))

  describe "CASE parsing (case arm is enum)" $ do
    let ok = ["CASE c OF Color.Red: x := 1; ELSE x := 0; END_CASE"]
        run = parse (pStmt <* eof) "<test>"
    forM_ ok $ \tc ->
      it ("parses " <> T.unpack tc) $
        shouldParse run tc

  describe "CASE parser (ELSE placement)" $ do
    let run = parse (pStmt <* eof) "<test>"

    it "accepts ELSE before any arms" $
      run "CASE x OF ELSE x := 0; END_CASE" `shouldSatisfy` isRight

    it "rejects arms after ELSE" $
      run "CASE x OF 1: ; ELSE x := 0; 2: ; END_CASE" `shouldSatisfy` isLeft

    it "rejects multiple ELSE" $
      run "CASE x OF 1: ; ELSE x := 0; ELSE x := 1; END_CASE" `shouldSatisfy` isLeft

  describe "Empty statements" $ do
    it "parses ; as Skip" $
      parse (pStmt <* eof) "<test>" ";" `shouldBe` Right Skip

    it "parses many ;;;;;;" $
      parse (some pStmt <* eof) "<test>" ";;;;;;"
        `shouldBe` Right [Skip, Skip, Skip, Skip, Skip, Skip]

    -- CASE アーム内の空文
    let run = parse (pStmt <* eof) "<test>"
    it "parses CASE with empty arm body" $
      shouldParse run "CASE x OF 0: ; END_CASE"

  describe "ENUM explicit values (parsing)" $ do
    let run = parse (pTypeBlock <* eof) "<test>"

    it "parses TYPE Color : (Red := 0, Green := 2); END_TYPE" $ do
      shouldParse run "TYPE Color : (Red := 0, Green := 2); END_TYPE"

    it "parses negative explicit values" $ do
      shouldParse run "TYPE Sgn : (Pos := 1, Neg := -1); END_TYPE"

  describe "ENUM explicit values with expressions (parsing)" $ do
    let run = parse (pTypeBlock <* eof) "<test>"
    it "parses TYPE Color : (Red := 1 + 2*3, Green := (1+2)*(3-1)); END_TYPE" $
      shouldParse run "TYPE Color : (Red := 1 + 2*3, Green := (1+2)*(3-1)); END_TYPE"
    it "parses TYPE Sgn : (Pos := +1, Neg := -1); END_TYPE" $
      shouldParse run "TYPE Sgn : (Pos := +1, Neg := -1); END_TYPE"

  describe "Expressions (MOD/XOR)" $ do
    let run = parse (pExpr <* eof) "<test>"
    -- 構文形
    it "parses 7 MOD 4" $
      shouldParseTo run "7 MOD 4" (EMod (EINT 7) (EINT 4))
    it "parses TRUE XOR FALSE" $
      shouldParseTo run "TRUE XOR FALSE" (EXor (EBOOL True) (EBOOL False))
    -- 優先順位
    it "parses 1 + 2 MOD 3 as 1 + (2 MOD 3)" $
      shouldParseTo run "1 + 2 MOD 3" (EAdd (EINT 1) (EMod (EINT 2) (EINT 3)))
    it "parses TRUE AND FALSE XOR TRUE OR FALSE with precedence" $
      shouldParseTo
        run
        "TRUE AND FALSE XOR TRUE OR FALSE"
        (EOr (EXor (EAnd (EBOOL True) (EBOOL False)) (EBOOL True)) (EBOOL False))

  describe "Expressions" $ do
    let ok =
          [ ("1 < 2 AND 3 < 4", EAnd (ELt (EINT 1) (EINT 2)) (ELt (EINT 3) (EINT 4))),
            ("1.5", EREAL 1.5),
            ("1 + 2.5", EAdd (EINT 1) (EREAL 2.5)),
            ("1.0 < 2", ELt (EREAL 1.0) (EINT 2))
          ]
        run = parse (pExpr <* eof) "<test>"
    forM_ ok $ \(tc, want) ->
      it ("parses " <> T.unpack tc) $
        shouldParseTo run tc want

  describe "LREAL parsing" $ do
    let runExpr = parse (pExpr <* eof) "<test>"
        runUnit = parse (pUnit <* eof) "<test>"
    it "parses typed LREAL literal" $
      shouldParseExprToNoLoc runExpr "LREAL#1.25e+3" (ELREAL 1250.0)
    it "parses VAR x:LREAL;" $
      shouldParse runUnit "PROGRAM P\nVAR x: LREAL; END_VAR\n"

  --- 文字列（連結は削除、リテラルと比較だけ）
  describe "STRING literals & type (parsing)" $ do
    it "parses 'abc'" $
      shouldParseExprToNoLoc runE "'abc'" (ESTRING "abc")
    it "parses quote escape 'a$'b'" $
      shouldParseExprToNoLoc runE "'a$'b'" (ESTRING "a'b")

    -- STRING(n) ※デフォルト値はSemanticのほうで設定される
    it "parses STRING(80) in VAR block" $ do
      let src = "PROGRAM P\nVAR s: STRING(80); END_VAR\n"
      expectRight (parseProgram src) $ \case
        Program _ (VarDecls [v]) _ ->
          varSig v `shouldBe` ("s", STRING (Just 80), Nothing, False)
        other ->
          expectationFailure ("unexpected program shape: " <> show other)

    -- STRING[n] ※デフォルト値はSemanticのほうで設定される
    it "parses STRING[32] in VAR block" $ do
      let src = "PROGRAM P\nVAR s: STRING[32]; END_VAR\n"
      expectRight (parseProgram src) $ \case
        Program _ (VarDecls [v]) _ ->
          varSig v `shouldBe` ("s", STRING (Just 32), Nothing, False)
        other ->
          expectationFailure ("unexpected program shape: " <> show other)

  describe "STRING $-escapes" $ do
    it "parses $$ -> $" $
      shouldParseExprToNoLoc runE "'a$$b'" (ESTRING "a$b")
    it "parses $' in single-quoted string" $
      shouldParseExprToNoLoc runE "'a$'b'" (ESTRING "a'b")
    it "parses $N/$n and $L/$l as LF" $ do
      shouldParseExprToNoLoc runE "'x$Ny'" (ESTRING "x\ny")
      shouldParseExprToNoLoc runE "'x$ly'" (ESTRING "x\ny")
    it "parses $R as CR" $
      shouldParseExprToNoLoc runE "'x$Ry'" (ESTRING "x\ry")
    it "parses $T as TAB" $
      shouldParseExprToNoLoc runE "'x$Ty'" (ESTRING "x\ty")
  -- it "fails on $\" for STRING" $
  --   runE "\"a$\"b\"" `shouldSatisfy` isLeft

  -- パース段階ではselectorにexpressionがあるのをそのままパースする
  -- そうしておかないと、「constな配列をconst添え字で参照」をselectorで使うことができないため
  describe "CASE selectors with constant expressions (parsing)" $ do
    let run = parse (pStmt <* eof) "<test>"
    forM_
      [ "CASE x OF 1+2: x := 0; END_CASE",
        "CASE x OF (1 + 2) * 3: ; END_CASE",
        "CASE x OF -1: ; END_CASE",
        "CASE x OF 1, 2..1+3: ; END_CASE"
      ]
      $ \tc ->
        it ("parses " <> T.unpack tc) $
          shouldParse run tc

  describe "Extended integer & bitstring types (parsing)" $ do
    it "parses VAR block with extended int/bitstring types" $ do
      let src =
            "PROGRAM P\nVAR\n\
            \  s:SINT; i:INT; d:DINT; l:LINT;\n\
            \  us:USINT; ui:UINT; ud:UDINT; ul:ULINT;\n\
            \  b:BYTE; w:WORD; dw:DWORD; lw:LWORD;\n\
            \END_VAR\n"
          expected =
            [ ("s", SINT, Nothing, False),
              ("i", INT, Nothing, False),
              ("d", DINT, Nothing, False),
              ("l", LINT, Nothing, False),
              ("us", USINT, Nothing, False),
              ("ui", UINT, Nothing, False),
              ("ud", UDINT, Nothing, False),
              ("ul", ULINT, Nothing, False),
              ("b", BYTE, Nothing, False),
              ("w", WORD, Nothing, False),
              ("dw", DWORD, Nothing, False),
              ("lw", LWORD, Nothing, False)
            ]
      expectRight (parseProgram src) $ \(Program _ vds _) ->
        varSigs vds `shouldBe` expected

  describe "Based integer literals (parsing)" $ do
    let run = parse (pExpr <* eof) "<test>"

    -- 受理系
    it "parses 2#1011 as 11" $
      shouldParseExprToNoLoc run "2#1011" (EINT 11)

    it "parses 8#17 as 15" $
      shouldParseExprToNoLoc run "8#17" (EINT 15)

    it "parses 10#42 as 42" $
      shouldParseExprToNoLoc run "10#42" (EINT 42)

    it "parses 16#FF as 255" $
      shouldParseExprToNoLoc run "16#FF" (EINT 255)

    it "accepts underscores in digits: 16#DEAD_BEEF" $
      shouldParseExprToNoLoc run "16#DEAD_BEEF" (EINT 3735928559)

    it "accepts lowercase hex digits: 16#face" $
      shouldParseExprToNoLoc run "16#face" (EINT 64206)

    it "parses unary minus on based literal: -16#10 == -16" $
      shouldParseExprToNoLoc run "-16#10" (ENeg (EINT 16))

    -- 拒否系（基数と桁の不一致）
    it "rejects 2#2 (digit not allowed in base 2)" $
      run "2#2" `shouldSatisfy` isLeft

    it "rejects 8#89 (digit 9 not allowed in base 8)" $
      run "8#89" `shouldSatisfy` isLeft

    it "rejects 16#G (non-hex digit)" $
      run "16#G" `shouldSatisfy` isLeft

    -- 10# の桁は通常の十進桁のみ（下線はOK）
    it "rejects 10#1A (A not allowed in base 10)" $
      run "10#1A" `shouldSatisfy` isLeft

  describe "Numeric literals with underscores (parsing)" $ do
    -- 十進整数：アンダースコアは無視され同値
    it "parses decimal int with underscores" $
      shouldParseExprToNoLoc runE "1_2_3_4" (EINT 1234)

    -- 符号付き十進整数：符号は単項演算子、値は 10
    it "parses negative int with underscores" $
      shouldParseExprToNoLoc runE "-1_0" (ENeg (EINT 10))

    -- 実数：小数点の前後や指数部の桁にも '_' を許容
    it "parses real with underscores (around dot)" $
      shouldParseExprToNoLoc runE "3_._1_4_1_5" (EREAL 3.1415)

    it "parses real with underscores in exponent" $
      shouldParse runE "1.0e1_0" -- 1.0e10 と等価（AST の具体形までは拘らず受理確認）
    it "parses based ints without underscores (bin/oct/hex/dec)" $ do
      shouldParseNumTo runInt "2#1011" 11
      shouldParseNumTo runInt "8#77" 63
      shouldParseNumTo runInt "10#12345" 12345
      shouldParseNumTo runInt "16#DEADBEEF" 3735928559

  describe "Numeric literals without underscores (parsing)" $ do
    let ok :: [(Text, Int)]
        ok =
          [ ("2#1011", 11),
            ("8#77", 63),
            ("10#12345", 12345),
            ("16#DEADBEEF", 3735928559),
            ("2#1010_0101", 165), -- 0b10100101
            ("8#7_7", 63),
            ("16#FF_FF", 65535),
            ("10#1_000_000", 1000000),
            -- CODESYS 互換：基数付きは先頭/末尾 '_' も許容
            ("16#_FF", 255),
            ("16#FF_", 255),
            ("2#_1010", 10),
            ("2#1_0_1_0_0101", 165), -- 0b10100101（別表現）
            ("16#DEAD_BEEF", 3735928559),
            ("16#dead_beef", 3735928559), -- hex は大小無視
            ("-2#111", -7),
            ("-16#1F", -31),
            ("-10#1_000", -1000)
          ]
    forM_ ok $ \(src, want) ->
      it ("parses " <> T.unpack src) $
        shouldParseNumTo runInt src want

    it "rejects bad underscore forms for ints" $ do
      shouldRejectNum runInt "123_"
      shouldRejectNum runInt "2#10__10"

    it "parses real with underscores (around dot)" $
      shouldParseNumTo runReal "3_._1_4_1_5" (3.1415 :: Double)

    it "parses real with underscores in exponent" $
      shouldParseNumTo runReal "1.0e1_0" (1.0e10 :: Double)

    -- 先頭 '_' は identifier として正当なので、ここは pInt を使って拒否を確認
    it "rejects _123 as integer literal" $ do
      shouldRejectNum runInt "_123"

    it "rejects invalid digit for base" $ do
      shouldRejectNum runInt "2#102" -- 2 進に '2' はダメ
      shouldRejectNum runInt "8#89" -- 8 進に '8','9' はダメ
      shouldRejectNum runInt "16#G1" -- 16 進に 'G' はダメ

  -- CHAR / WCHAR / WSTRING: 型とリテラル（パース）

  describe "CHAR/WCHAR/WSTRING types & literals (parsing)" $ do
    it "parses CHAR/WCHAR in VAR block" $ do
      let src =
            "PROGRAM P\n\
            \VAR c: CHAR; wc: WCHAR; END_VAR\n"
          expected =
            [ ("c", CHAR, Nothing, False),
              ("wc", WCHAR, Nothing, False)
            ]
      expectRight (parseProgram src) $
        \(Program _ vds _) ->
          varSigs vds `shouldBe` expected

    it "parses WSTRING(80) and WSTRING[32] in VAR block" $ do
      let src =
            "PROGRAM P\n\
            \VAR ws1: WSTRING(80); ws2: WSTRING[32]; END_VAR\n"
          expected =
            [ ("ws1", WSTRING (Just 80), Nothing, False),
              ("ws2", WSTRING (Just 32), Nothing, False)
            ]
      expectRight (parseProgram src) $
        \(Program _ vds _) ->
          varSigs vds `shouldBe` expected

    -- リテラル：STRING は '...'、WSTRING は "..." とする前提
    it "parses STRING literal with $' escape (single-quoted)" $ do
      shouldParseExprToNoLoc runE "'a$'b'" (ESTRING "a'b")

    it "parses WSTRING literal with $\" escape (double-quoted)" $ do
      -- 期待ASTは EWSTRING を想定（未実装なら後で合わせる）
      shouldParseExprToNoLoc runE "\"a$\"b\"" (EWSTRING "a\"b")

    -- エスケープの相互排他（STRING側で$\"は不可 / WSTRING側で$'は不可）
    it "rejects $\" inside STRING literal" $ do
      runE "'a$\"b'" `shouldSatisfy` isLeft

    it "rejects $' inside WSTRING literal" $ do
      runE "\"a$'b\"" `shouldSatisfy` isLeft

  describe "Aggregate initializers (parsing)" $ do
    it "parses array aggregate in VAR block" $ do
      let src =
            "PROGRAM P\n\
            \VAR\n\
            \  a: ARRAY[0..2] OF INT := [1, 2, 3];\n\
            \END_VAR\n"
      expectRight (parseProgram src) $
        \(Program _ vds _) ->
          varSigs vds `shouldSatisfy` \case
            [("a", Array [ArrRange 0 2] INT, mi, False)] -> isJust mi
            _ -> False

    it "parses struct aggregate with named fields" $ do
      let src =
            "TYPE R : STRUCT x: INT; y: LREAL; END_STRUCT; END_TYPE\n\
            \PROGRAM P\n\
            \VAR\n\
            \  r: R := (x := 1, y := 2.0);\n\
            \END_VAR\n"
      expectRight (parseUnit src) $
        \(Unit _ ps) -> case ps of
          [Program _ vds _] ->
            varSigs vds `shouldSatisfy` \case
              [("r", Named i, mi, False)] -> isJust mi && locVal i == "R"
              _ -> False
          _ -> expectationFailure "unexpected pattern"

    it "parses struct aggregate in any field order" $ do
      let src =
            "TYPE R : STRUCT x: INT; y: LREAL; END_STRUCT; END_TYPE\n\
            \PROGRAM P\n\
            \VAR\n\
            \  r: R := (y := 2.0, x := 1);\n\
            \END_VAR\n"

      expectRight (parseUnit src) $
        \(Unit _ ps) -> case ps of
          [Program _ vds _] ->
            varSigs vds `shouldSatisfy` \case
              [("r", Named i, mi, False)] -> isJust mi && locVal i == "R"
              _ -> False
          _ -> expectationFailure "unexpected pattern"

    it "parses nested aggregate: array of structs" $ do
      let src =
            "TYPE R : STRUCT x: INT; y: INT; END_STRUCT; END_TYPE\n\
            \PROGRAM P\n\
            \VAR\n\
            \  a: ARRAY[0..1] OF R := [(x := 1, y := 2), (x := 3, y := 4)];\n\
            \END_VAR\n"
      expectRight (parseUnit src) $
        \(Unit _ ps) -> case ps of
          [Program _ vds _] ->
            varSigs vds `shouldSatisfy` \case
              [("a", Array [ArrRange 0 1] (Named i), mi, False)] -> isJust mi && locVal i == "R"
              _ -> False
          _ -> expectationFailure "unexpected pattern"

  describe "IEC 61131-3 Date/Time literals (parsing, spec examples)" $ do
    let mk :: Text -> Text -> Text
        mk decl assign =
          T.concat
            [ "PROGRAM P\nVAR ",
              decl,
              " END_VAR\n",
              assign,
              "\n"
            ]

    -- いまサポート前提で回す（TIME/TOD/DATE/DT、秒小数・区切りアンダースコアあり）
    let supportedNow :: [(Text, Text)]
        supportedNow =
          -- Duration (no underscore) short/long
          [ ("t: TIME;", "t := T#14ms;"),
            ("t: TIME;", "t := T#-14ms;"),
            ("t: TIME;", "t := T#14.7s;"),
            ("t: TIME;", "t := T#14.7m;"),
            ("t: TIME;", "t := T#14.7h;"),
            ("t: TIME;", "t := T#14.7d;"),
            ("t: TIME;", "t := T#25h15m;"),
            -- us/ns なし版（仕様例は us/ns ありだが、ここでは ms までに留める）
            ("t: TIME;", "t := T#12h4m34ms;"),
            -- long prefix
            ("t: TIME;", "t := TIME#14ms;"),
            ("t: TIME;", "t := TIME#-14ms;"),
            ("t: TIME;", "t := time#14.7s;"),
            -- Duration (with underscore)
            ("t: TIME;", "t := t#25h_15m;"),
            ("t: TIME;", "t := t#5d_14h_12m_18s_3.5ms;"),
            ("t: TIME;", "t := TIME#25h_15m;"),
            -- Date / Time of day / Date and time
            ("d: DATE;", "d := DATE#1984-06-25;"), -- 1a
            ("d: DATE;", "d := date#2010-09-22;"), -- 1a (lower-case)
            ("d: DATE;", "d := D#1984-06-25;"), -- 1b
            ("tod: TIME_OF_DAY;", "tod := TIME_OF_DAY#15:36:55.36;"), -- 3a
            ("tod: TOD;", "tod := TOD#15:36:55.36;"), -- 3b
            ("dt: DATE_AND_TIME;", "dt := DATE_AND_TIME#1984-06-25-15:36:55.360227400;"), -- 5a (ns 桁まで)
            ("dt: DT;", "dt := DT#1984-06-25-15:36:55.360_227_400;") -- 5b (下位桁にアンダースコア)
          ]

    let supported2 :: [(Text, Text, Text)] -- (理由, decl, assign)
        supported2 =
          [ ("LTIME literal (short)", "lt: LTIME;", "lt := LT#14.7s;"),
            ("LTIME literal (multi)", "lt: LTIME;", "lt := lt#5d14h12m18s3.5ms;"),
            ("LTIME with us/ns", "lt: LTIME;", "lt := t#12h4m34ms230us400ns;"),
            ("LTIME with underscore", "lt: LTIME;", "lt := t#5d_14h_12m_18s_3.5ms;"),
            ("LTIME long prefix underscore", "lt: LTIME;", "lt := LTIME#5m_30s_500ms_100.1us;"),
            ("ltime long prefix", "lt: LTIME;", "lt := ltime#5d_14h_12m_18s_3.5ms;"),
            ("LTIME ns", "lt: LTIME;", "lt := LTIME#34s_345ns;"),
            ("LDATE (long)", "ld: LDATE;", "ld := LDATE#2012-02-29;"), -- 2a
            ("LDATE (short)", "ld: LDATE;", "ld := LD#1984-06-25;"), -- 2b
            ("LTOD (short)", "ltod: LTOD;", "ltod := LTOD#15:36:55.36;"), -- 4a
            ("LTIME_OF_DAY (long)", "ltod: LTOD;", "ltod := LTIME_OF_DAY#15:36:55.36;"), -- 4b
            ("LDATE_AND_TIME (long)", "ldt: LDATE_AND_TIME;", "ldt := LDATE_AND_TIME#1984-06-25-15:36:55.360_227_400;"), -- 6a
            ("LDT (short)", "ldt: LDT;", "ldt := LDT#1984-06-25-15:36:55.360_227_400;") -- 6b
          ]

    forM_ supportedNow $ \(decl, stmt) ->
      it ("parses: " <> T.unpack stmt) $ do
        expectRight (parseProgram (mk decl stmt)) (const $ pure ())
    forM_ supported2 $ \(why, decl, stmt) ->
      it ("parses: " <> T.unpack why <> " - " <> T.unpack stmt) $ do
        expectRight (parseProgram (mk decl stmt)) (const $ pure ())

  describe "Function calls (parsing)" $ do
    let base = "PROGRAM P\nVAR x: INT; y: INT; b: BOOL; END_VAR\n"

        ok :: [Text]
        ok =
          [ "x := ADD(1, 2);",
            "x := ADD(y := 2, x := 1);", -- 名前付き引数（順不同）
            "x := ADD(1, y := 2);", -- 位置引数のあとに名前付きは OK
            "x := SUB( (1), 2 );", -- 空白・括弧あり
            "x := ADD(SUB(3, 1), 2);", -- ネスト呼び出し
            "x := NOP();" -- 引数なし
          ]

    forM_ ok $ \s ->
      it ("parses: " <> T.unpack s) $
        parseProgram (base <> s) `shouldSatisfy` isRight

    it "rejects trailing comma in call (parse error)" $ do
      -- 文法的に不正：末尾カンマ
      parseProgram (base <> "x := ADD(1, );\n") `shouldSatisfy` isLeft