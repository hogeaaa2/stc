1. **VAR_* セクション＆修飾子の網羅（基盤）**

   * `VAR/INPUT/OUTPUT/IN_OUT/EXTERNAL/GLOBAL/TEMP/CONFIG/ACCESS`
   * `RETAIN/NON_RETAIN/CONSTANT/PROTECTED/PUBLIC/PRIVATE/INTERNAL`
   * まずはパースと AST を統一し、最小セマンティクス（書込み可否・可視性の基本）まで。

    ### ①-1: POU 内 VAR_* セクションの基盤

    対象：`PROGRAM` / `FUNCTION_BLOCK` / `FUNCTION` の中に書かれるやつ

    * `VarKind` の拡張

      * `VarLocal | VarInput | VarOutput | VarInOut | VarTemp | VarExternal?` など
    * `Variable` に載せるフィールドを整理

      * `varKind` 拡張、`varConst`/`varRetain` を将来 `VarAttrs` っぽくできる形に
    * パーサ

      * `VAR`, `VAR_INPUT`, `VAR_OUTPUT`, `VAR_IN_OUT`, `VAR_TEMP` を
        **いまの AST に落とす（= [Variable] にhttps://www.youtube.com/watch?v=asfONdPsnw8 flatten）**
      * セクション／宣言単位の `CONSTANT`, `RETAIN/NON_RETAIN` も読んで `Variable` に反映
    * セマンティクス（最小）

      * 既存の「`VAR_INPUT` への書込み禁止」ロジックを
        `isReadOnly` みたいなヘルパに寄せる
      * `CONSTANT` にも同じフラグを使って、書込み禁止を実装

    👉 ここまでは **完全に「POU 内だけの話」** なので、VAR_GLOBAL を気にせずサクサク進められる。

    ---

    ### ①-2: Unit 直下の VAR_GLOBAL（＆将来 VAR_CONFIG）基盤

    対象：`TYPE`/`PROGRAM` と同じ階層に書かれるやつ

    * AST

      * `UnitItem` に `UGlobalVar GlobalVarBlock` を追加
      * `GlobalVarBlock` の中身はひとまず `[Variable]`（`varKind = VarGlobal`）
    * パーサ

      * top-level で `VAR_GLOBAL ... END_VAR` を読んで `UGlobalVar` を生成
      * とりあえず visibility（PUBLIC/INTERNAL）は読み取るだけで OK（使わない）
    * セマンティクス（最小）

      * `GlobalVarEnv` を作る：

        * `elaborateUnit` で `UGlobalVar` を一周し、名前重複チェック＋ Env 化だけ
      * 「グローバル名の重複」エラーを定義してテスト書く

    👉 ここは **Unit レベルの構造だけ**作っておいて、
    visibility や `VAR_EXTERNAL` 連携は一旦後ろに回す。

    ---

    ### ①-3: VAR_EXTERNAL と Global のひも付け

    ここからやっと「グローバルと POU がつながる」フェーズ。

    * パーサ

      * POU 内で `VAR_EXTERNAL` セクションを読んで `Variable`（`varKind = VarExternal`）化
    * セマンティクス

      * `VarExternal` は、`GlobalVarEnv` に既にあるシンボルであることを要求
      * 型が一致しない／存在しない場合にエラー
      * 名前解決の優先順位（ローカル ＞ External ＞ Global）のルールを決める

    👉 ここまで行くと、**グローバル変数を参照する POU** が一通り動く。

    ---

    ### ①-4: visibility（PUBLIC/PRIVATE/PROTECTED/INTERNAL）は別タスクで

    visibility 系は

    * AST には乗せておく（`Variable` に `varVisibility :: Maybe VarVisibility` 的な）
    * でも **チェックロジックはこの後のタスク**として切り出す

    ぐらいにしておくと、「とりあえず全部見えていいモード」でテストを動かせるので楽。

    ---

    ## どう進めるのがよさそうか

    順番としては：

    1. **①-1: POU 内 VAR_* 基盤**

      * いま触ってる `Variable`/`VarKind` の話と完全に同じレイヤーなので、ここからやるのが自然
    2. **①-2: UGlobalVar 追加 & パーサ**

      * UnitItem に 1 コンストラクタ足すだけなので、割とサクッといける
    3. **①-3: VAR_EXTERNAL 連携**

      * ここで初めて GlobalVarEnv をちゃんと使い始める

    という流れが、実装スコープも小さくて進めやすいかなと思う。

    ---

    もしよければ、次の一手としては **①-1 の「Variable/VarKind の拡張」と POU 内 VAR_* パース**から一緒に具体コード詰めようか。
    いまの `VarKind` 定義と、POU の Var 部分の AST/パーサ周りを貼ってもらえれば、それに合わせたパッチ案出せるよ。

2. **ANY_BIT の部分アクセス（読み）**

   * `By.%X0` / `Bo := By.7` / `By := Do.%B3` 等
   * 後置演算子のパース、型付け（`BOOL`/`BYTE`）、範囲チェック。
3. **POINTER TO / REFERENCE TO（最小）**

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
