いい感じのところまで来てます ✨

質問2つに分けて答えるね。

---

## 1. ANY型 in VAR_INPUT について

認識あってます。

* IEC 61131-3 的には、`ANY`, `ANY_INT`, `ANY_NUM`, … は、
  **主に FUNCTION / FUNCTION_BLOCK の VAR_INPUT（引数）専用の「型クラス」** として使う想定。
* 「実体としての変数の型」ではなく、
  「呼び出し時に具体型でインスタンス化されるパラメータ」の制約。
* なので今やってる:

  * `GST`（Generic type family）
  * `FuncSig` に `mono` / `gen` 的なタグを入れて、「この引数は INT」「この引数は ANY_INT」等

  という設計ときれいに噛み合う。

ただしこれは

1. FUNCTION / FUNCTION_BLOCK の構文と AST がある
2. その interface から `FuncSig` / `FBインタフェース` を起こす
3. それを env に積んで呼び出し側の型チェックに使う

が揃ってからが本番なので、

> 「ANY型はFunction/FBのVAR_INPUTで使える」対応は、**POU（FUNCTION / FB）サポートを入れてからやる**

のが筋がいいです。

---

## 2. 次にやると気持ちよくハマる項目リスト

今の状態（PROGRAM + Unit + 型環境 + 関数呼び出し + ANY ファミリ(外部シグネチャ対応) + Vary エラー体系）を前提にすると、次の順番がきれいです。

### (A) 複数 POU サポート（FUNCTION / FUNCTION_BLOCK のパース＆AST）

ここからやるのがいちばんおすすめ。

1. AST 拡張

   * `Unit` に

     * `[FunctionDecl]`
     * `[FunctionBlockDecl]`
     * などを追加。
   * `FunctionDecl`

     * 名前
     * 戻り値型
     * `VAR_INPUT` / `VAR_OUTPUT` / `VAR` などの宣言群
     * 本体ステートメント
   * `FunctionBlockDecl`

     * 名前
     * `VAR_INPUT`, `VAR_OUTPUT`, `VAR` （インスタンスメンバ）
     * 本体（FB内部コード）

2. Parser:

   * `FUNCTION ... END_FUNCTION`
   * `FUNCTION_BLOCK ... END_FUNCTION_BLOCK`
   * それぞれで VAR セクションを読む（とりあえず `VAR` と `VAR_INPUT` くらいからでOK）

**ここまでやると、「ユーザー定義 FUNCTION に対して今の関数呼び出しロジックを適用する」道が開く**ので、その次がスムーズ。

---

### (B) 環境構築: POU のシグネチャを関数環境へ

(A) ができたら次:

* Unit elaboration の最初で:

  * TYPE 群から `TypeEnv` 構築（今どおり）
  * FUNCTION 群から

    * `FuncEnv`（`Text -> FuncSig`）を作る

      * パラメータ型が `STType`（+ 将来は `GST`）になる
  * FUNCTION_BLOCK から

    * 将来用 FB 環境（`Text -> FBInterface` みたいなやつ）を作る

* その `FuncEnv` を今の `Env` に差し込む（もうやってる構造にマージするだけ）。

→ これで

* 外部注入してた built-in シグネチャと
* 自前で定義した FUNCTION のシグネチャ

を同じ仕組みで扱えるようになる。

---

### (C) FUNCTION 本体の意味解析

環境がそろったら、各 FUNCTION 自体をチェック:

* `VAR_INPUT` / `VAR` 宣言を VarEnv に入れて `checkStmt`。
* 戻り値:

  * IEC流に「関数名そのものを変数として代入したらそれが戻り値」もサポートするなら、

    * その変数が必ずどこかで代入されているかチェック、などのルールも足せる。

ここまでで「普通の（非ジェネリック）ユーザー定義関数 + 呼び出しの型検査」が完成します。

---

### (D) VAR_INPUT/OUTPUT/IN_OUT の意味論

次にやると生きてくるやつ。

* AST に VarSection の種類を付けてある前提で:

  * `VAR_INPUT`:

    * 呼び出し側: 式（RValue）を渡す
    * 本体側: 読み取り専用（Assign 禁止）
  * `VAR_OUTPUT`:

    * 呼び出し側: 変数（LValue）を渡す必要がある
    * 本体側: 書き込む義務（未代入なら警告とか）
  * `VAR_IN_OUT`:

    * call-by-reference。呼び出し側でも LValue 必須、本体から読み書き可。

まずは `VAR_INPUT` read-only だけからでもOK。
そのあと output/inout の call-site チェック（`ECall` の引数が LValue かどうか）を追加。

この段階で「FB/PROGRAM 呼び出しの Statement 版」にも道がつながる。

---

### (E) ANY / ANY_* を POU 定義にも解禁（本命）

ここで最初に話してくれたやつ。

やること:

1. パラメータの型を `STType` だけでなく:

   ```haskell
   data ParamTy
     = PTMono STType
     | PTGen GST Tag  -- GST = ANY_*, Tag で「同じ型変数」管理
   ```

   みたいに表現。

2. `FunctionDecl` の VAR_INPUT で `ANY_INT` 等を書けるようにパース。

3. 既に ECall 用に作った
   「generic param を具体型に束縛してチェックするロジック」
   をユーザー定義関数にも流用。

ここまで来ると:

* ライブラリ関数
* ユーザー定義ジェネリック FUNCTION/FB

を同じコードで扱えるようになる。

---

### (F) 定数式評価 (evalConstExpr) の本格導入

すでに布石は打ってあるので、ここで:

* `ConstVal` ベースの `evalConstExpr` を完成させて

  * `VAR CONSTANT` の初期値評価
  * CASE ラベル
  * 静的添字
  * 範囲指定 etc.

に使う。

今はピンポイントでやっている定数判定が一箇所に集約されて、仕様追加もしやすくなる。

---

### (G) FB / PROGRAM 呼び出し (Statement call) と FB インスタンス

ここまで行けると、次の楽しいやつ:

* `myFb();` 形式の呼び出しを `Statement` として扱う。
* FB インスタンス（`myFb: MyFB;`）を「stateful struct + 実行ステップ」として扱う。

これはちょっと重いので、上の (A)〜(F) が落ち着いてからで十分。

---

## 結論（いま何からやるのが良いか）

今のあなたのコード状況だと、この順がバランス良いです：

1. **(A)** FUNCTION / FUNCTION_BLOCK の AST & Parser
2. **(B)** Unit elaboration で POU シグネチャを集約して `FuncEnv` に反映
3. **(C)** ユーザー定義 FUNCTION 本体の型チェック
4. **(D)** VAR_INPUT / OUTPUT / IN_OUT の意味論（少なくとも INPUT read-only）
5. そのあとに **(E)** ANY_* を VAR_INPUT に解禁してジェネリック POU 完成

この流れなら、いま構築した Env / ANY / Vary ベースの仕組みをほぼそのまま活かしつつ、段階的に強くしていけます。
