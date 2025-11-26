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

6. ✅ **（先取りの安全策）FBを式に使った誤用の専用エラー**

   * いまは `UnknownFunction`→`NoReturnValue` になりがち。
     VarEnv に FB インスタンスが見つかったら `FBUsedAsExpr` を返すプリチェックを `inferType ECall` 頭に入れておく。

7. (in progress) **（中期）FB 本体/実行モデル**

   * 文としての呼び出しノード（例：`FBInvoke`）の追加、入出力の意味論（後ろ倒しでOK）。


いいね、ここからは **「FB 本体／実行モデル」** を“コンパイラ的に破綻しない最小構成→拡張しやすい”順で固めよう。下はそのまま差し込める **最小パッチ案＋段階計画**。まずは“静的意味付け”と“IRの受け皿”を用意して、ランタイム挙動（状態遷移）をあとから載せられる形にする。

---

  # 進め方（段階計画）

  1. ✅ **型系と環境の土台を作る（FB を第一級の“型メタ”として扱う）**

    * ✅ `TypeEnv` に FB 名を登録できるように、`STType` に **FB メタ型**を追加。
    * ✅ これで `f : FB` みたいな変数宣言が **UnknownType にならず**通る。
    * ✅ 名目同値・フィールド参照（`f.o` 読み）時の型解決フックもここで整備。

  2. ✅ **FB 呼び出しを“文”として表現する**

    * ✅ `Statement` に **`CallFB`** を追加（`ECall` は引き続き FUNCTION 用）。
    * ✅ 右記の専用引数 `FBArg`（`in :=` / `out =>` / `inout :=`）で“方向”を保持。
    * ✅ 既存の `collectPOUParams` を **FB でも流用**（`VKOutput` を ParamOut に含める方針で OK）。

  3. (in prgress) **静的検査（elaborate）**

    * `CallFB` の型検査：

      * ① 対象 `LValue` が **FB メタ型**のインスタンスか確認
      * ② `assignArgs` の FB 版（“名前付きのみ”＋方向一致＋`IN_OUT` は LValue 必須）
      * ③ 型適合（`assignCoerce` / `gstMember` など既存ロジックを流用）

    * まだのところ（静的検査で残っている最小セット）

    1. ✅ **FB 本体の「出力の必須代入（Strict）」**

      * FUNCTION でやったのと同等を **FB にも**入れる。
      * 具体的には、`FB` 本体に対して

        * `req = {VAR_OUTPUT の全名前}`
        * `mustAssignAll req fbBody` を Strict で要求、CodesysLike はスキップ（デフォルト初期化で逃がす想定）。
      * 既存の `lvalueWritesTo` / `stmtAssignsTo` / `caseArmAssignsTo` をそのまま使えます。
      * エラーは `MissingReturn` と別にするなら、例えば `MissingFBOutputs FBName (Set VarName)` のような型を新設。

    2. ✅ **方向不一致の明示的エラー**（任意だが推し）

      * いま `CallOut` で ParamOut 以外だった場合にプレースホルダ的なエラーを返している箇所を、
        専用エラー（例：`BindDirectionMismatch POU ArgName ExpectedDir ActualDir Span`）に差し替え。
      * 仕様として **IN_OUT への `=>` は不許可**で OK（Codesys の挙動にも沿う）。

    3. ✅ **FB 本体内：VAR_INPUT 書き込み禁止（Strict）**の確認

      * これは既存の「AssignToInput（Strict でNG / CodesysLike OK）」ロジックが **FB 本体にも効く**状態か要確認。
        もし関数本体だけを対象にしていたら、FB にも同じチェックを掛ける（すぐ入れられます）。

    4. （オプション）**関数名 vs FB名の衝突**のテスト追加

      * 型名 vs FB 名は済。関数名 vs FB 名も一応網羅テストを足しておくと安心。


      4. **IR（将来の実行モデル）に落とす足場**

        * FB を **`fb_step_FBNAME : State × Inputs -> (State', Outputs)`** に下げる想定で IR ノード（ダミー）を用意。
        * まずは **“文としての副作用あり呼び出し”を IR に載せる**だけ（実際の実行は後続タスク）。

      5. **フィールドアクセスの最小許容**

        * `EField (EVar f) out` の **“読み”**は FB の `VAR_OUTPUT` に限り許可（型は出力の型）。
        * **外部からの “書き”**（`LField (LVar f) out := ...`）は **エラー**にする（`AssignToFBOutputOutside` など）。

---

# 小さな運用Tips

* **単体Unit検査**は `elaborateUnitWithDecls`（UProgram短絡）を “自己完結テスト”に限定。
  ファイル跨ぎ（TYPE/PROGRAM別）は**必ず** `elaborateUnits` を使う。
* `funcEnvFromUnit` は**絶対に** `elaborate*` を呼ばない（再発防止）。
* `shouldSatisfy` で落ちるときは `force`＋`case`＋`shouldBe` に寄せるとデバッグが楽。

---

この順でいけば、A（Parser/AST）は完了、B（シグネチャ反映＆呼び出し検査の土台）は**ほぼ完了**、C（FUNCTION本体の型検査）は**未決の“全パス代入”と param dir ルール**を押さえればゴール、って感じ。
次は 1) と 2) を片付けよう。テスト雛形が要ればすぐ出すよ。
