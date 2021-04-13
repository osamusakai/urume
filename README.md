# ウルメイワシのVPAと将来予測

## 使用パッケージ

- frasyr
- tidyverse
- knitr 

## ファイル構成

#### .Rprofile

作業の一番初めに実行させるファイル、パッケージの保存場所、作業ディレクトリの指定、パッケージの呼び出しを設定(この辺はお好みで。。。)

#### uVPA.r

 VPA計算のための関数群を記述したコード。VPAの計算手順は、uvpa()に記述されています（step1～12）。それぞれのstepでやっていることを、VPA手順.xlsxに記載しました。

#### ufuture.r

将来予測計算のための関数群を記述したコード。prefuture（）で将来予測の下準備をして、ufuture（）で将来予測を実行する.　

- ufuture_check.rはufuture.rがきちんと計算されているか確認するため追加した。ufuture_checkのコードを順次実行していき、301行まで実行したときに生成されているnaafが、urume_vpa&future_forshare.xlsxの「VPA&future (残差固定 計算確認用)」シートの年齢別漁獲尾数（Y列44行から始まる）に一致するはず。ufuture_check.rは、「#チェック時以外はコメントアウト」とある行をコメントアウトし、「#チェック時以外はコメントアウトを外す」とある行のコメントアウト(#)をはずせば、ufuture.rと同じものになる。

#### 1.vpa_check.rmd

　VPA、レトロスペクティブ計算、caaのブートストラップを行う

#### 2.SRreport_urume.rmd

 再生産関係の検討を行う

#### 3.uestMSY.rmd

MSY等の推定、Yield curve,　Kobe plotの作図

#### vpa_dataフォルダ

　 vpaに使用するデータを保存　

####  obj フォルダ

　　計算結果の保存先

#### urume_vpa&future_forshare.xlsx

- 「VPA」シート： 1.vpa_check.rmdの計算結果に合う。
- 「VPA＆future（計算確認用）」シートufuture_check.rの結果に合う（はず）
- 「VPA&future」:乱数を動かしたときの変化をみれる
- 「YPR SPR(pope)」：YPR等の検算用

#### VPA手順.xlsx

​    uVPA.rの　uvpa()の計算手順を図にした

　



## 計算手順

1.vpa_check.rmd →2.SRreport_urume.rmd →3.uestMSY.rmd　の順番で実行

1. 1.vpa_check.rmd

   1.  uvpa.rを呼び出し
   2.  データを呼び出してdata.handler()で整形
   3.  calcuVPA() でVPA計算
   4.  結果をres_urume_vpa.objとして保存(あとで使用)

2. 2.SRreport_urume.rmd

   res_urume_vpa.objを呼び出して、再生産関係の検討をする。

   - SR exhaustive　チャンクで当てはめ結果の一覧表を作成、保存（SRmodel-list.csv)
   - SR_exhaustive_estimateチャンクで当てはめ結果を個別に取り出せるリスト（fitSR.reslist）を作成, 保存（fitSR_reslist.obj、あとで使用) 。

3. 3.uestMSY.rmd

   - res_urume_vpa.objとfitSR_reslist.objを呼び出す
   - prefuture()で将来予測のための下準備をしたオブジェクト(parepare)を作成
   - ufuture()を使って将来予測
     - 最適化関数（opt_MSY()など)を使い、MSY等を実現するfbeta（Fcurrentに掛ける係数）を求める。
     - 求められたfbetaを引数にしてufuture（）を実行し、MSY等を計算する



