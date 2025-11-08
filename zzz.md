# Next（着手候補 1〜3 件／1 行要約）

- [IN-PROGRESS] A: FUNCTION/FB の AST & Parser → tests: `test/Spec.hs` “parses FUNCTION … / FUNCTION_BLOCK …, rejects …” | run: `stack test :p --test-arguments "-m \"FUNCTION\|FUNCTION_BLOCK\""`
- [PENDING] B: POU シグネチャを FuncEnv へ反映 → tests: `test/SemanticSpec.hs` “accepts function call … / rejects bad args …” | run: `stack test :s --test-arguments "-m \"function call\""`
- [PENDING] C: ユーザー定義 FUNCTION 本体の型チェック → tests: `test/SemanticSpec.hs` “parses/accepts FUNCTION body … / rejects missing return …” | run: `stack test :s --test-arguments "-m \"FUNCTION body\""`

（着手時は [IN-PROGRESS]、完了時は [DONE] へ更新）

---

# Backlog（要約）

## (A) POU サポート: FUNCTION / FUNCTION_BLOCK の構文・AST

- 追加: `Unit` に `[FunctionDecl]`, `[FunctionBlockDecl]`
- FunctionDecl: 名前、戻り値型、`VAR_INPUT` / `VAR_OUTPUT` / `VAR`、本体文
- FunctionBlockDecl: 名前、各 VAR セクション（インスタンスメンバ）、本体
- Parser: `FUNCTION … END_FUNCTION`、`FUNCTION_BLOCK … END_FUNCTION_BLOCK`、まずは `VAR`/`VAR_INPUT` に限定

## (B) POU のシグネチャを関数環境へ

- elaboration で TYPE 群から `TypeEnv`、FUNCTION 群から `FuncEnv :: Map Text FuncSig`
- パラメータ型は `STType`（単相）と `GST`（ANY_*）の両方に対応
- FUNCTION_BLOCK は将来の FB 呼び出しに備えたインタフェース環境へ

## (C) FUNCTION 本体の意味解析

- `VAR_INPUT`/`VAR` を VarEnv に投入し `checkStmt`
- 戻り値の扱い（関数名への代入=戻り値 など）を規約化

## (D) VAR_INPUT / OUTPUT / IN_OUT の意味論

- `VAR_INPUT`: 呼び出しは式（値渡し）、本体では readonly（代入禁止）
- `VAR_OUTPUT`: 呼び出しは変数（lvalue）を渡す、本体では書込必須（警告/エラー方針は別途）
- `VAR_IN_OUT`: 参照渡し（lvalue 必須）、本体から読み書き可
- まずは INPUT readonly から着手、その後 OUTPUT/IN_OUT と call-site 検査（`ECall` 引数が LValue か等）

## (E) ANY / ANY_* を POU 定義でも許可（本命）

- ParamTy を単相/ジェネリックで表現（例: `PTMono STType` / `PTGen GST Tag`）
- `VAR_INPUT` で `ANY_INT` などをパース
- 既存の ECall のジェネリック束縛ロジックをユーザー定義 FUNCTION にも適用

## (F) 定数式評価 (evalConstExpr) の本格導入

- `ConstVal` ベースの `evalConstExpr` を仕上げて以下に適用:
  - `VAR CONSTANT` 初期値
  - CASE ラベル
  - 静的添字
  - 範囲リテラル 等

## (G) FB / PROGRAM 呼び出し（Statement call）と FB インスタンス

- `myFb();` 形式の呼び出しを `Statement` として扱う
- `myFb: MyFB;` を状態を持つ構造 + 実行ステップとして扱う
- 重めのため (A)(F) 後で着手

---

# Notes

- ANY 系（`ANY`, `ANY_INT`, …）は原則 FUNCTION / FUNCTION_BLOCK の `VAR_INPUT` 用の型クラス的な位置づけ。実体変数の型ではない。
- したがって ANY 対応は POU（FUNCTION/FB）のパース・AST が揃ってから適用するのが自然。

# TDD フロー（合意）

- まず該当 `test/*.hs` の末尾に `it` を追加（成功系: `parses …` / 失敗系: `rejects …`）。
- `stack test :p|:s --test-arguments "-m \"<pattern>\""` で Red を確認。
- 実装 → Green → ここ（zzz.md）の Next/Backlog を更新。

