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
