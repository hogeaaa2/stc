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

    ## ✅3-3. `REF(expr)` の構文（まだゆるめ）

    ### ✅AST / パーサ

    * [ ] `reserved` に `REF` を追加
    * [ ] `Expr` に「参照取得」ノードを追加
      例: `ERef Expr` または `ERef LValue`（後で決める）
    * [ ] `pPrimaryE` に `REF(` … `)` の分岐を追加

      * とりあえずは `REF` をキーワード扱い（`ECall "REF"` にはしない）
      * 中身は一旦 `Expr` として読む（後で「LValue に限定する」方向に締める）

    ### ✅テスト（パース）

    * [ ] `REF(x)` が `ERef (EVar x)` 的な AST になる
    * [ ] `REF(a.b[0])` がちゃんとネストした形で読めることを確認（とりあえず構文だけ）

    ---

    ## ✅3-4. `REF(expr)` の型検査（最初はかなり緩め）

    ### ✅セマンティク（最小）

    * [ ] `inferType env (ERef e)` を実装

      * [ ] まず `inferType env e` で型 `T` を取る
      * [ ] いったん **何でも** `RefTo T` を返す（LValue制約などは次フェーズで締める）
    * [ ] `RefTo` のネストや Named 型経由の解決で変にならないか確認

    ### ✅テスト（セマンティク・最小）

    * [ ] `VAR x : INT; r : REF_TO INT; END_VAR`
      本文で `r := REF(x);` が型チェックを通る（`REF(x)` の型が `REF_TO INT`）
    * [ ] `VAR a : ARRAY [0..9] OF INT; r : REF_TO INT; END_VAR`
      本文で `r := REF(a[0]);` が通る（この時点では「文字列1文字」などの禁止はまだ気にしない）

    ---

    ## ✅3-5. REF の「参照可能対象」制約（IEC 寄せ・あと回しでOKな部分）

    ここは「最小」が落ち着いてから着手でOKな拡張セット。

    * [ ] `REF(expr)` の **対象を LValue に限定**

      * `REF(1+2)` などはエラー（新しい `NonRefTarget` 的なエラー）
    * [ ] `VAR CONSTANT` など **定数の参照を禁止**

      * `VAR CONSTANT x : INT := 1; r : REF_TO INT; END_VAR`
      * `r := REF(x);` → エラー（`RefToConst` 的なもの）

    ---

    ## 3-6. REF の IEC ルール拡張
    * [ ] **文字列内の一文字**を REF するのを禁止

      * `s : STRING; r : REF_TO CHAR;`
      * `r := REF(someString[1]);` → エラー
    * [ ] そのほか「規格が明示している NG ケース」があれば順次足していく

    （ここは 3.x 系の別タスクにして、実装コストと相談しながら増やすイメージ）
    
    * [ ] `REF_TO` 変数を LValue として扱う（`r^ := 10` のサポート）
      → LValue AST に `LDeref` を追加して、Assign 側のロジックを調整
    * [ ] NULL 参照 / 初期化済みチェック（コンパイル時にどこまで拾うか検討）
    * [ ] 他の機能との連携

      * `AT` 指定と REF_TO の絡み
      * FB インスタンス (`REF_TO` FB 型) への参照など

4. **AT 指定（最小）**
  ### AT 実装まわりの課題メモ（stc TODO 用）

  1. **「AT の後ろに何を書くか」を決める必要がある**

    * IEC は「ロケーション（アドレス）」としか言ってない。
    * ベンダごとに書式バラバラ（CODESYS `%IX0.0`, Siemens 独自表記 など）。
    * **stc ではまず「変数エイリアス（`b AT a : INT;`）」に限定するのが楽そう。**

  2. **AST 設計をどうするか**

    * シンプル案：`varAt :: Maybe Identifier`（エイリアス先の変数名だけを持つ）。
    * 拡張可能案：`data AtTarget = AtAlias Identifier | AtAddress Text`
      → 将来 `%IX0.0` みたいなベタアドレスも扱いたくなった時用。

  3. **セマンティクでチェックしたいこと**

    * AT 先の変数が **同じスコープに存在するか**（なければ `UnknownVar` 的エラー）。
    * **型が一致しているか**（`a : INT`, `b AT a : BOOL` は NG）。
    * `VAR_GLOBAL`, `VAR_EXTERNAL`, `VAR` など **VarKind の組み合わせをどう許可するか**
      （最初は「とりあえず同一 Unit 内でだけ許可」でもよさそう）。

  4. **ハードウェアアドレスをどう扱うか（将来の宿題）**

    * `%IX0.0` などの **I/O アドレスは PLC 実機やターゲット依存**。
    * stc 側で真面目にやるには、中間表現やバックエンドとのインタフェース設計が必要。
    * いまは **「エイリアス専用 AT」だけに絞っておき、ハードアドレス対応は後回し**でよさそう。

  5. **REF / 寿命との絡み（さらに先送りでOK）**

    * 将来的に「AT 先の変数を REF するときの制約」や
      「寿命切れのエイリアス／REF をどう扱うか」を考える必要が出てくるかも。
    * これはデータフロー寄りの解析になるので、**現時点ではスコープ外として保留**。


5. **UNION（パース→最小セマンティクス）**

  ### UNION 実装まわりの課題メモ（stc TODO 用）

  1. **IEC 61131-3 には UNION は入っていない（ベンダ拡張）**

    * ABB / Schneider なども「IEC 標準 **には** union 無いよ」と明言。
    * CODESYS / TwinCAT / Schneider などが、独自拡張として `UNION ... END_UNION` を持っている。
      → **最初から「非ポータブル機能」と割り切る必要あり。**

  2. **ベンダごとに UNION の有無・ノリがバラバラ**

    * CODESYS / TwinCAT / Schneider: C の `union` っぽい「メモリ重ね合わせ型」。
    * Siemens 系など: 明示的な `UNION` キーワードはなく、UDT + AT などで似たことをやる文化。
      → 「**どのベンダに寄せるか**」を決めないと仕様が定まらない。

  3. **セマンティクが素直じゃない（C の union と同じ罠を持つ）**

    * 全メンバが同じオフセットを共有 → 「最後にどのフィールドを書いたか」に依存。
    * 型安全・初期化・データフロー解析が一気にややこしくなる。
    * stc の今の「ちゃんと型を追う」設計と、相性があまり良くない。

  4. **stc で UNION をやるかどうか自体を決める必要がある**

    * 選択肢イメージ：

      * **やらない**：IEC 準拠寄せ。UNION は将来の拡張 or 別モード扱い。
      * **CODESYS/TwinCAT 互換としてやる**：`TYPE U : UNION ... END_UNION END_TYPE` を vendor 拡張としてサポート。
      * **別表現でエミュレート**：`STRUCT + AT` やタグ付き union 風の安全な型で代替。
        → どれを選ぶかで AST / 型検査ポリシーが変わる。

  5. **やると決めた場合に必要になる設計タスク**（今はまだ着手しない前提）

    * AST: `STType` に `Union [(Identifier, STType)]` みたいなのを足すかどうか。
    * セマンティク:

      * 「`u.f` でのみ型が確定」「`u` 単体は式として使えない」など、どこまで制限するか。
      * 代入・初期化・ANY_* や REF/AT とどう絡めるか。
    * ユーザー向けには「**IEC 外の拡張であること**」をちゃんと明示したい。

  ---

  ひとまずは、

  > UNION は IEC 外のベンダ拡張で、
  > ちゃんと決めてから実装しないと stc の型システムが崩れる

  くらいのラベルを貼っておいて、
  「**OOP / AT / REF まわりが一段落した “その先の” 重めタスク**」にしておくのが良さそうだね。

6. **METHOD / INTERFACE（パースだけ）**

じゃあこれも TODO メモ用にギュッとまとめるね。
（※ちょい詳しめ vendor 状況付き）

---

### METHOD / INTERFACE 実装まわりの課題メモ（stc TODO 用）

  #### 1. 規格上のポジション（IEC 61131-3）

  * IEC 61131-3（第3版以降）には **OOP 拡張** が入っていて、

    * `INTERFACE ... END_INTERFACE`
    * `FUNCTION_BLOCK FB EXTENDS Base IMPLEMENTS IWhatever`
    * `METHOD M : INT`
      といった構文が「一応ちゃんと枠として」定義されている。
  * **METHOD** は IEC 的には「関数に似たもの」で、

    * 自分の FB のデータにアクセスできる
    * 入出力パラメータを持てる（普通の FUNCTION と同じノリ）
    * ローカル変数は呼び出しごとに初期化される
      という TwinCAT ドキュメントの説明が、ほぼそのまま IEC の意図をなぞっている。
  * **INTERFACE** は「メソッド群の型」として扱われていて、

    * FB に `IMPLEMENTS IFoo` させて多態性を実現
    * インターフェイスタイプを IN/IN_OUT パラメータとして渡してポリモーフィズム、
      という典型的な OOP パターン。

  > つまり：
  > UNION と違って **IEC 的な公式枠はある**。ただし解釈と拡張はベンダごとに盛ってる。

  ---

  #### 2. ベンダ別の雰囲気

  **(1) CODESYS**

  * IEC 61131-3 OOP のかなりフル装備実装：

    * FB が `IMPLEMENTS IWhatever` できる。
    * IFace を実装した FB 作成時に、IDE が自動でメソッド/属性をツリーに展開してくれる。
  * ライブラリ側も **インターフェイス推し**（Common Behaviour Model など）。
    `IActionProvider`, `IBehaviourModel` みたいな interface 型が普通に出てくる。
  * CODESYS 独自の `__QUERYINTERFACE` 演算子で、「オブジェクトから特定のインターフェイスを引き出す」みたいな COM 的ノリの拡張もある。

  **(2) Beckhoff TwinCAT 3**

  * ドキュメントに「**IEC 61131-3 の OOP 拡張**」と明記した上で、

    * FB の拡張（継承）
    * インターフェイスの実装
    * メソッド
    * プロパティ
      をサポートする、と書かれている。
  * METHOD については：

    * 「IEC 61131-3 によると、メソッドも通常の関数のように IN/OUT を持てる」
    * 「メソッド内のデータはすべて一時（スタック）で、呼び出しごとに初期化される」
      と説明されていて、**かなり規格準拠寄せの実装**になっている。

  **(3) Siemens TIA Portal**

  * TIA Portal は、FB まわりを中心に独自 UI で OOP を出してくる感じ。

    * 「InOut interface」など、**インターフェイスという用語は出てくる**が、
      CODESYS/TwinCAT のように ST で `INTERFACE ... END_INTERFACE` 書く形とは少し違って、
      エディタや通信機能（OPC UA クライアント等）に強く結びついている。
  * メソッドも「FB 内のメンバー」として GUI 上で管理されることが多く、
    ソース表現というより **プロジェクト構造・ライブラリ設計とセットの機能**になっている雰囲気。

  **ざっくり vendor まとめ**

  * CODESYS / TwinCAT:
    → 「**IEC OOP 拡張を “ガチの言語機能” としてフルサポート**＋独自機能」路線。
  * Siemens / その他:
    → 同じ概念はあるが、**エディタや通信機能とセットの設計が多く、構文レベルではベンダ色強め**。

  ---

  #### 3. stc にとっての「めんどうポイント」

  1. **AST と型システムのスコープが一気に広がる**

    * `INTERFACE`, `METHOD`, `EXTENDS`, `IMPLEMENTS` の追加で：

      * 型階層（継承）
      * インターフェイスと実装クラスの対応
      * メソッド解決（静的 / 動的ディスパッチ）
        が全部関わってくる。
    * 現在の stc は「PROGRAM / FUNCTION / FUNCTION_BLOCK + TYPE(STRUCT/ENUM)」を
      きれいに型検査するフェーズなので、ここに OOP を入れると **設計レイヤーが一段上がる**。

  2. **可視性・修飾子・ライフタイムまで芋づる式に来る**

    * `PUBLIC/PRIVATE/PROTECTED/INTERNAL` を「METHOD / INTERFACE の世界」に持ち込むタイミングとも絡む。
    * METHOD 内の Var は基本 TEMP 扱い（TwinCAT 仕様）なので、
      既存の `VAR/VAR_INPUT/...` ルールとどう折り合うかも設計が必要。

  3. **ベンダ差をどこまで吸収するか決めないとブレる**

    * 「CODESYS/TwinCAT 互換の ST 構文」をターゲットにするか、
    * 「IEC にかなり忠実な最小サブセット」を定義するか、
    * あるいは「stc 独自の素朴 OOP モデル + CODESYS 互換は別モード」にするか。

  ---

  #### 4. stc 的な当面の方針メモ

  * 今すぐやるのは重いので、当面は：

    1. **「パースだけ」の受け口候補として設計を温めておく**

      * 例:

        * `data Unit = ... | UInterface InterfaceDecl | UClass ClassDecl ...`
        * `data MethodDecl = MethodDecl { mdName :: Identifier, mdRetType :: STType, mdParams :: [Variable], ... }`
      * ただし実装は **OOP 回（別フェーズ）まで寝かせる**。

    2. 「CODESYS/TwinCAT 系 ST で一般的な最小サブセット」に寄せる案を有力候補としてメモ

      * `INTERFACE` / `METHOD` / `EXTENDS` / `IMPLEMENTS` 程度。
      * プロパティ (`PROPERTY`) や `__QUERYINTERFACE` はさらに後回し。

  * 優先度としては：

    * まず **VAR_*, ANY_*, REF_TO, AT などの「現行スコープ内の型まわり」**を固める。
    * そのあとで **「OOP 回」**を一つのまとまったマイルストーンとして METHOD / INTERFACE を扱う。

  ---

  こんな感じで TODO に貼っとけば、

  > METHOD / INTERFACE は「IEC に枠はあるけど、
  > ベンダ差と OOP 全体設計が絡むので **専用フェーズでやるべき大物**」

  って位置づけが思い出しやすいと思う。

7. ✅**ANY_BIT の書き込み**

   * `By.%X0 := TRUE` 等の LValue 解決・型/範囲チェック。
8. **可視性（PUBLIC/PROTECTED/PRIVATE/INTERNAL）の最小ガード**
