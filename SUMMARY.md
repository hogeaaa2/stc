このチャットでは、IEC 61131-3 Structured Text 向け Haskell 製ツールチェーン「stc」の開発を継続します。

【開発環境 / 前提】
- 言語: Haskell（Stack プロジェクト）
- 主なコンポーネント:
  - 構文解析: ST.Parser（Megaparsec）
  - AST: ST.AST
  - セマンティクス: ST.Semantic（TypeEnv / FuncEnv / VarEnv 等）
  - テスト: test/Spec.hs, test/SemanticSpec.hs 他
- 対象: IEC 61131-3 Structured Text（PROGRAM, FUNCTION, FUNCTION_BLOCK 等）

【現在の状態（このスレ開始時点）】
- PROGRAM ブロックのパーサと基本的なセマンティック解析は概ね動作。
- Enum / Struct / Named 型などの型システムは実装済みで、nominalEq 等で型比較可能。
- TDD ベースで進行中（先にテスト書いてから実装）。

【次にやること（Next）】
- [IN-PROGRESS] A: FUNCTION / FUNCTION_BLOCK の AST & Parser 実装
  - tests: `test/Spec.hs` の "parses FUNCTION … / FUNCTION_BLOCK …, rejects …"
  - run: `stack test :p --test-arguments "-m \"FUNCTION\\|FUNCTION_BLOCK\""`
- [PENDING] B: 定義した POU シグネチャを FuncEnv に反映（関数呼び出し検査の土台）
  - tests: `test/SemanticSpec.hs` の "function call" 系
- [PENDING] C: ユーザー定義 FUNCTION 本体の型チェック
  - tests: `test/SemanticSpec.hs` の "FUNCTION body" 系

【中期タスク（Backlog 要約】
- POU サポート拡張: Unit に FunctionDecl / FunctionBlockDecl を追加。
- VAR_INPUT / OUTPUT / IN_OUT の意味論を整理（INPUT readonly から着手）。
- ANY / ANY_*（ジェネリック）を POU パラメータ型として許可。
- evalConstExpr の本格導入（定数式 / CASE ラベル / VAR CONSTANT 初期値など）。
- FB / PROGRAM 呼び出し構文と FB インスタンスの扱い（後ろ倒し）。

この前提を踏まえて、以降のやりとりでは「stc プロジェクトの継続開発」として回答してください。
