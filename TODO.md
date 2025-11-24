いい感じに積み上がってきたので、現状まとめ＋この先の道筋をコンパクトに整理するね。

# いま出来ていること（要点）

* #AST / Parser

  * `Unit` は和型（`UType | UProgram | UFunction | UFunctionBlock`）。
  * **複数ファイル**対応：`parseUnit :: FilePath -> Text -> Unit`、`parseUnits :: [(FilePath,Text)] -> [Unit]`。
  * テスト用ヘルパ整備：`expectParsedUnits` などで “TYPEとPROGRAMは別ファイル” 前提に統一。

* #TypeEnv / FuncEnv 構築

  * `typeEnvFromUnits`：全 TYPE を一括解決。
  * `funcEnvFromUnit(s)`：FUNCTION/FB から `FuncEnv` 構築。

    * **相互再帰バグ**（`funcEnvFromUnit`→`funcEnvFromUnits`→…）を解消。
    * `DuplicateFunction` を **位置情報つき**で検出（テストも Pass）。

* #Semantics（式・文の検査）

  * `elaborateUnits`：`tenv` / `fenv` を作って各 `Unit` を検査するプロジェクト入口。
  * 既定初期化：`INT/BOOL` や Enum の **先頭列挙子**など（該当テスト Pass）。
  * 代入・式の型推論（`inferType`）

    * **ANY-family**の単一化：`assignArgs` → 引数型推論 → `subst` 構築（`gstMember`/`nominalEq`）。
    * **戻り値の具体化は厳格**に：`instantiateSigTyRet`（未束縛なら `UnsupportedGenericReturn`）。
    * **戻り値なし**を式で使ったら `NoReturnValue`（テスト Pass）。
  * 関数本体検査（`elaborateFunction`）

    * 返り型の解決は **自分の宣言を resolve**。
      （“戻り ANY_* 禁止”は**呼び出し側**で扱う方針に整理済み）

* #テスト整備

  * 既存の単一文字列テストを **複数ファイル**化して順次修正・緑。
  * `DuplicateFunction` / `UnsupportedGenericReturn` / `NoReturnValue` などの新規系も緑。

---

# この先やること（推奨順）

1. ✅ **FUNCTION 本体の完了判定を堅くする**（MissingReturn 系）

   * いまの `hasReturnAssign` を Statement 仕様（`Assign/If/While/Repeat/Case/For/Skip`）で**全到達パス**判定に仕上げる。
   * テスト：

     * 正常系：全パスで `F := ...` 済み。
     * 異常系：どこかの分岐で未代入 → `MissingReturn`。

2. ✅ **パラメータ方向（VAR_INPUT/OUTPUT/IN_OUT）ルール**

   * Parser は OK なので Semantics で適用：

     * `INPUT` は**読み取り専用**（LHS 代入禁止）。
     * `OUTPUT` は**少なくとも一度は代入**（未代入なら警告/エラーの方針を決める）。
     * `IN_OUT` は**参照渡し**的：実引数は LValue 必須・式不可。
   * VarEnv へ投入時に `VarKind` を保持（`VarInfo.viKind` 既存）。
   * テスト：INPUT書き換えNG、IN_OUTにリテラル渡しNG、OUTPUT未代入NG。

3. ✅ **ANY-family の網羅性拡張**

   * `gstMember` を `ANY_BIT/ANY_STRING/ANY_DATE/ANY_DURATION` まで拡充。
   * 代表値（デフォルト）と等価判定の整理（`nominalEq` で十分か確認）。
   * テスト：受理/拒否の組合せを追加。

4. ✅ **呼び出し引数の“名前付き＋位置”ミックスの完成度**

   * 既に `assignArgs` あり。

     * 二重指定・未指定・順序違反などのエラーを詰めてテスト追加。

5. ✅ **エラーメッセージの表現統一** のかわりに、C#からテキスト受け取ってC#にパース結果返す処理

   * すべて `Span` 付与済みか確認（`UnknownFunction`/`ArgTypeMismatch`/…）。
   * `ArgTypeMismatch` の expected に家族代表（`repForGST`）を使う等、読みやすさ改善。

6. **（先取りの安全策）FBを式に使った誤用の専用エラー**

   * いまは `UnknownFunction`→`NoReturnValue` になりがち。
     VarEnv に FB インスタンスが見つかったら `FBUsedAsExpr` を返すプリチェックを `inferType ECall` 頭に入れておく。

7. **（中期）FB 本体/実行モデル**

   * 文としての呼び出しノード（例：`FBInvoke`）の追加、入出力の意味論（後ろ倒しでOK）。

---

# 小さな運用Tips

* **単体Unit検査**は `elaborateUnitWithDecls`（UProgram短絡）を “自己完結テスト”に限定。
  ファイル跨ぎ（TYPE/PROGRAM別）は**必ず** `elaborateUnits` を使う。
* `funcEnvFromUnit` は**絶対に** `elaborate*` を呼ばない（再発防止）。
* `shouldSatisfy` で落ちるときは `force`＋`case`＋`shouldBe` に寄せるとデバッグが楽。

---

この順でいけば、A（Parser/AST）は完了、B（シグネチャ反映＆呼び出し検査の土台）は**ほぼ完了**、C（FUNCTION本体の型検査）は**未決の“全パス代入”と param dir ルール**を押さえればゴール、って感じ。
次は 1) と 2) を片付けよう。テスト雛形が要ればすぐ出すよ。
