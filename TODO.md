1. **VAR_* セクション＆修飾子の網羅（基盤）**

  * ✅ ①-1: POU 内 VAR_* 基盤（LOCAL / INPUT / OUTPUT / IN_OUT / TEMP）
  * ✅ ①-2: VAR_GLOBAL / VAR_EXTERNAL / GlobalVarEnv
  * ⏸ ①-3: RETAIN / NON_RETAIN（パース＋ASTだけ or 後回し）
  * ⏸ ①-4: VAR_CONFIG（AT まわりとセットで後回し）
  * ⏸ ①-5: PUBLIC / PRIVATE / PROTECTED / INTERNAL（OOP 回で実装）
2. **ANY_BIT の部分アクセス（読み）**

   * `By.%X0` / `Bo := By.7` / `By := Do.%B3` 等
   * 後置演算子のパース、型付け（`BOOL`/`BYTE`）、範囲チェック。
3. **POINTER TO / REFERENCE TO（最小）**
    こんな感じでどうかな、けっこう細かめに割ってみた 👇

    ---

    ## 3. REF_TO / REF / `^`（最小）チェックリスト

    ### 3-0. 方針の固定（やる前に決めるだけ）

    * [ ] **POINTER TO は捨てる**（実装しない）
    * [ ] 採用するのは **IEC系の `REF_TO` / `REF()` / `^` だけ**
    * [ ] CODESYS 型の「暗黙デリファレンス」（`x := ref;` で勝手に `ref^` されるやつ）は **一切やらない**
    * [ ] stc 第一弾では「**読み中心**」で入り、`REF` の細かい禁止条件（文字列1文字 / const など）は後ろのサブタスクでやる

    ---

    ## ✅3-1. REF_TO 型 + `^`（参照外し）の最小対応

    ### ✅AST / パーサ

    * [ ] `STType` に新コンストラクタ追加
      例: `RefTo STType`（名前は `REF_TO` に対応するもの）
    * [ ] `reserved` に `REF_TO` を追加
    * [ ] `pSTType` に `REF_TO <型>` を読む分岐を追加

      * `REF_TO INT`
      * `REF_TO ARRAY [0..9] OF BYTE`
        などがパースできる
    * [ ] `Expr` にデリファレンスノード追加
      例: `EDeref Expr`  （`r^`）
    * [ ] `pExpr` のポストフィックス（`pPostfixE`）に `^` を追加

      * 任意の `Expr` に後置で `^` を付けられるようにする（`EDeref e`）

    ### ✅テスト（パース）

    * [ ] `TYPE` 内での `REF_TO` 宣言がパースできる
      例: `TYPE R : REF_TO INT; END_TYPE`
    * [ ] `PROGRAM` / `FUNCTION` / `FUNCTION_BLOCK` の `VAR` で `REF_TO` 型が書ける
    * [ ] 式として `r^` が `EDeref (EVar r)` になることを確認するテスト

    ---

    ## ✅3-2. `^` の型検査（REF_TO 以外はエラー）

    ### ✅セマンティク

    * [ ] 新しいエラー型（仮）を追加
      例: `data NotARef = NotARef Span ActualSTType`
    * [ ] `AllErrs` に `NotARef` を組み込む
    * [ ] `inferType env (EDeref e)` を実装：

      * [ ] `inferType env e` が `RefTo t` のとき → `t` を返す
      * [ ] それ以外の型（`INT`, `BYTE`, `ARRAY ...` 等）のとき → `NotARef` を投げる
    * [ ] `resolveType` が `RefTo` を中まで潜れるようにする（`REF_TO MyAlias` の MyAlias が Named の場合など）

    ### ✅テスト（セマンティク）

    * [ ] **成功系**

      * `VAR x : INT; r : REF_TO INT; END_VAR`
        本文で `x := r^;` のような式を書いても **型チェックは通る**（r 自体は未初期化でも、ここではとりあえず型だけ）
    * [ ] **失敗系**

      * `VAR x : INT; y : INT; END_VAR`
        本文で `x := y^;` と書くと `NotARef`（「REF_TO じゃないものに `^` 」）で落ちる

    ---

    ## 3-3. `REF(expr)` の構文（まだゆるめ）

    ### AST / パーサ

    * [ ] `reserved` に `REF` を追加
    * [ ] `Expr` に「参照取得」ノードを追加
      例: `ERef Expr` または `ERef LValue`（後で決める）
    * [ ] `pPrimaryE` に `REF(` … `)` の分岐を追加

      * とりあえずは `REF` をキーワード扱い（`ECall "REF"` にはしない）
      * 中身は一旦 `Expr` として読む（後で「LValue に限定する」方向に締める）

    ### テスト（パース）

    * [ ] `REF(x)` が `ERef (EVar x)` 的な AST になる
    * [ ] `REF(a.b[0])` がちゃんとネストした形で読めることを確認（とりあえず構文だけ）

    ---

    ## 3-4. `REF(expr)` の型検査（最初はかなり緩め）

    ### セマンティク（最小）

    * [ ] `inferType env (ERef e)` を実装

      * [ ] まず `inferType env e` で型 `T` を取る
      * [ ] いったん **何でも** `RefTo T` を返す（LValue制約などは次フェーズで締める）
    * [ ] `RefTo` のネストや Named 型経由の解決で変にならないか確認

    ### テスト（セマンティク・最小）

    * [ ] `VAR x : INT; r : REF_TO INT; END_VAR`
      本文で `r := REF(x);` が型チェックを通る（`REF(x)` の型が `REF_TO INT`）
    * [ ] `VAR a : ARRAY [0..9] OF INT; r : REF_TO INT; END_VAR`
      本文で `r := REF(a[0]);` が通る（この時点では「文字列1文字」などの禁止はまだ気にしない）

    ---

    ## 3-5. REF の「参照可能対象」制約（IEC 寄せ・あと回しでOKな部分）

    ここは「最小」が落ち着いてから着手でOKな拡張セット。

    * [ ] `REF(expr)` の **対象を LValue に限定**

      * `REF(1+2)` などはエラー（新しい `NonRefTarget` 的なエラー）
    * [ ] `VAR CONSTANT` など **定数の参照を禁止**

      * `VAR CONSTANT x : INT := 1; r : REF_TO INT; END_VAR`
      * `r := REF(x);` → エラー（`RefToConst` 的なもの）
    * [ ] **文字列内の一文字**を REF するのを禁止

      * `s : STRING; r : REF_TO CHAR;`
      * `r := REF(someString[1]);` → エラー
    * [ ] そのほか「規格が明示している NG ケース」があれば順次足していく

    （ここは 3.x 系の別タスクにして、実装コストと相談しながら増やすイメージ）

    ---

    ## 3-6. 将来タスク候補（今回の「最小」からは外す）

    * [ ] `REF_TO` 変数を LValue として扱う（`r^ := 10` のサポート）
      → LValue AST に `LDeref` を追加して、Assign 側のロジックを調整
    * [ ] NULL 参照 / 初期化済みチェック（コンパイル時にどこまで拾うか検討）
    * [ ] 他の機能との連携

      * `AT` 指定と REF_TO の絡み
      * FB インスタンス (`REF_TO` FB 型) への参照など

    ---

    こんな分解にしておくと、

    * **今すぐやるのは 3-1 / 3-2（REF_TO + `^`）**
    * そのあと **3-3 / 3-4（REF(expr) の構文＋型付け）**
    * さらに余裕が出てきたら 3-5（REF の制約強化）

    みたいに、いつもの「小さい塊ごとに：テスト → 実装」の流れで回しやすいと思う。

    この中で「まずは 3-1 から」で良さそうなら、次のターンで **3-1 用のパーステスト案** から出すね。

   * 型構文・`^` 参照外しの型付けだけ導入（ADR/REF 等は後段）。
4. **AT 指定（最小）**

   * `AT var` を型検査上のエイリアスとして扱う（実アドレスは保持のみ）。
5. **UNION（パース→最小セマンティクス）**

   * `u.f` でのみ型が確定、`u` 単体の式利用は禁止。
6. **METHOD / INTERFACE（パースだけ）**

   * AST 受け口を作って将来に接続（解決/呼出しは後回し）。
7. **ANY_BIT の書き込み**

   * `By.%X0 := TRUE` 等の LValue 解決・型/範囲チェック。
8. **可視性（PUBLIC/PROTECTED/PRIVATE/INTERNAL）の最小ガード**
