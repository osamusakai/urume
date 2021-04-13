
#ウルメイワシの将来予測関数

# 関数の定義----

## 1 一般的な関数----

#1-1. 再生産モデルに応じた加入量計算関数 ----
pred.RI <- function(SSB,a,b){a*SSB*exp(-b*SSB)}
pred.BH <- function(SSB,a,b){ a*SSB/(1+b*SSB)}
pred.HS <- function(SSB,a,b){ifelse(SSB > b,a*b,a*SSB)}
pred.SL <- function(SSB,a) {a*SSB} #aはRPS中央値（ufutureでは使わない）

#1-2. Fを計算する関数：将来予測用----　
### CとNから計算 ----
F_cn_futur = function(n,c,m){-log(1-c*exp(m/2)/n)}

#1-3.  Nを計算する関数:将来予測用 ----
## CとFから
N_cf_futur = function(c,f,m){ c*exp(m/2)/(1-exp(-f))}
## CとNから
N_cn_futur = function(c,n,m){n*exp(m)+c*exp(m/2)}
## fとNから
N_fn_futur = function(n,f,m){n*exp(-m-f)}

#1-4. Cを計算する関数、NとFから：将来予測用 ----
C_nf_futur = function(n,f,m){n*(1-exp(-f))*exp(-m/2)}

#1-5. deviance（自己相関ありln加入量残差)を与える関数 ----
## T3+2年(2021年: 資源量推定最終年（2019年）の2年後、ABC算定年）の残差
give_dev_ini = function(rand_res, p_dev, rho, bias_factor){rand_res + p_dev*rho - bias_factor}
## >=T3+3年（2022年）以降の加入量残差
give_dev  = function(rand_res, p_dev, rho, bias_factor) {rand_res + (p_dev + bias_factor)*rho - bias_factor}


#1-6. fcurrentの比を引数にし,任意のFに対するspr、ypr、%sprを出力する関数 ----

calc_uYPRSPR= function(fmulti, fcur, m, wspr){
  nspr =  fspr =rep(NA, 4)
  multi = c(0, fmulti)
  sprs  = rep(NA, 2)
  yprs  = rep(NA, 2)
  
  for(k in 1:2){
    nspr[1] = 1
    for(i in 2:4){
      nspr[i] = N_fn_futur(nspr[i-1], multi[k]*fcur[i-1], m) 
    }
    cspr = C_nf_futur(nspr, multi*fcur,m)
    yprs[k] = sum(cspr*wspr)
    sprs[k] = nspr[3]*wspr[3]
  }
  resspr =  c(multi[2], yprs[2],sprs[2], sprs[2]/sprs[1])  
  names(resspr) = c("f/fcur","ypr","spr","%SPR")
  return(resspr)
}
#example: calc_YPRSPR(1, fcur= Fcurrent, m = M, wspr = waa_future)


#1-7. urume仕様のget.SPR ----
#2020年評価の場合、2019年までのSPRをとるように、ncol(res$faa)から1を引く
uget.SPR = function (dres, target.SPR = 30, Fmax = 10, max.age = Inf) {
  dres$ysdata <- matrix(0, ncol(dres$faa)-1, 5)
  dimnames(dres$ysdata) <- list(colnames(dres$faa[,1:(ncol(dres$faa)-1)]), c("perSPR", "YPR", "SPR", "SPR0", "F/Ftarget"))
  
  for (i in 1:(ncol(dres$faa)-1)) {#ウルメ仕様に計算年限を最大値-1にした
    dres$Fc.at.age <- dres$faa[, i]
    if (all(dres$Fc.at.age > 0, na.rm = T)) {
      (byear <- colnames(dres$faa)[i])
      #Fmax = 10 #
      #max.age = 3 #
      #target.SPR=30 #
      a <- ref.F(dres, 
                 waa.year = byear, maa.year = byear, 
                 M.year = byear,
                 #rps.year = 2000:2011,#なくていい
                 pSPR = round(target.SPR), 
                 F.range = c(seq(from = 0, to = ceiling(max(dres$Fc.at.age,na.rm = T) * Fmax), length = 301),
                             max(dres$Fc.at.age, na.rm = T)), plot = FALSE, max.age = max.age)
      
      dres$ysdata[i, 1:2] <- (as.numeric(rev(a$ypr.spr[which(a$ypr.spr$Frange2Fcurrent == 1)[1], 2:3])))
      dres$ysdata[i, 3] <- a$spr0 * dres$ysdata[i, 1]/100
      dres$ysdata[i, 4] <- a$spr0
      dres$ysdata[i, 5] <- 1/a$summary[3, grep("SPR", colnames(a$summary))][1]
    }
    else {
      break
    }
  }
  dres$ysdata <- as.data.frame(dres$ysdata)
  dres$target.SPR <- target.SPR
  return(dres)
}



## 2. 将来予測本体の関数 ----
#prefuture:将来予測に必要なオブジェクトを整える
#ufuture: 将来予測を実行する

### check ----
### 計算がエクセルと一致しているかどうか確認するための処理
### チェック時以外はコメントアウトする
#

# setwd("C:/RWD/ASSESS/urume/for_share")                 #データを呼び出すためディレクトリを設定
# res_vpa_urume = get(load( "./obj/res_urume_vpa.rda"))　#vpaデータの取り出し
# fitSR.reslist = readRDS("./obj/fitSR_reslist.obj")     #再生産モデルの当てはめ結果の等の取り出し　
# res_SR_MSY    = fitSR.reslist$HS_L2$AR1inner　         #ホッケースティックL2内側を選択
#
#計算チェックのための乱数
#check_rand_resid = c(0.318776839,0.076398708,-0.40992155,0.251194213,-0.480688271,0.161022006,0.465073503,
#                     0.067484357,0.058161107,0.32922759,0.147012277,0.190556751,-0.018898123,
#                     -0.045920335,	0.228067925,	-0.313433658,	0.083548616,	-0.173446685,	-0.108284626,
#                     0.492720211,	0.005059113,	0.032745216,	0.436532383,	-0.264884831,	0.733889497,
#                     -0.154276223,	-0.513341691,	0.120168544,	-0.0450614)



#2-1. prefuture:将来予測の準備 ----

prefuture = function(res_vpa, res_SR_MSY, fyear){
  # 中の計算を確認するための入力
  # res_SR_MSY = res_SR_MSY     #check時以外はコメントアウト
  # res_vpa    = res_vpa_urume  #check時以外はコメントアウト
  # fyear      = 50             #check時以外はコメントアウト
  
  #選択した再生産関係に応じて加入量予測関数を選択
  whichSR = as.character(res_SR_MSY$input$SR)
  switch(whichSR,               # switch(文字列,
         "RI" = pred.f<-pred.RI,      #  "1" のときに実行
         "HS" = pred.f<-pred.HS,      #  "2" のときに実行
         "BH" = pred.f<-pred.BH
  )          
  
  # 再生産パラメータ と残差 ----
  # 係数の取り出し
  a   = res_SR_MSY$pars$a
  b   = res_SR_MSY$pars$b
  sd  = res_SR_MSY$pars$sd
  rho = res_SR_MSY$pars$rho
  
  #自己相関係数をとりいれた標準偏差
  sd_with_AR = (sd^2/(1-rho^2))^(1/2)	
  
  #バイアス調整の係数
  bias_factor =(sd_with_AR^2)/2	
  
  #前処理 ----
  years = as.numeric(colnames(res_vpa$naa)) #1999-2020 
  ages  = as.numeric(rownames(res_vpa$naa)) #1:0歳前半　2：0歳後半　3：1歳前半　4：1歳後半
  ny    = length(years)  #既存のデータ年数；1999-2020 まで22年
  na    = length(ages)   #齢成数；4
  
  # 何年から将来予測を始めるか。2020年から開始
  startyr = max(years)
  Fcur    = res_vpa$Fc.at.age #Fcurrent , VPAの仮定に合わせる
  waa_future = apply(res_vpa$input$dat$waa[,(ny-9):ny], 1, mean) #将来の体重、過去10年平均
  M       = res_vpa$input$dat$M[1,1]
  
  # 結果を入れる容器、年ループ用。matrix、期間2019―2070
  naaf <- faaf <-  caaf <- baaf <- waaf <- wcaaf <- matrix(NA, nrow = length(ages), 
                                                           ncol = fyear+2,
                                                           dimnames = list(ages,seq(startyr-1, startyr+fyear, by = 1)))
  ## 将来予測まえに2019年と2020年の値を埋める
  #N
  naaf[,1:2] = res_vpa$naa[,(ny-1):ny] #2020年の前半資源尾数を入力
  #C
  caaf[,1:2] = as.matrix(res_vpa$input$dat$caa[,(ny-1):ny]) #2020年の前半漁獲尾数まで入力
  #F
  faaf[,1] = res_vpa$faa[,ny-1] #2019年のF
  faaf[, 2]  = t(Fcur)          #2020年のF＝Fcurrent
  
  #W.at.age
  #2019、2020年の前半は実測、2020年0歳後期Fは過去10年平均、2020年1歳後期Fは同年1歳前期と同じ
  waaf[,1:2] = as.matrix(res_vpa$input$dat$waa[,(ny-1):ny])
  
  #2021年以降は2020年からさかのぼって過去10年平均に同じ
  waaf[,3:(fyear+2)] = t(waa_future)
  
  ##2020年後半資源尾数を計算
  naaf[c(2,4),2] = N_fn_futur(naaf[c(1,3),2], faaf[c(1,3),2], M) 
  ##2020年後半漁獲尾数を計算
  caaf[c(2,4),2] = C_nf_futur(naaf[c(2,4),2], faaf[c(2,4),2], M) 
  #biomass
  baaf = naaf*waaf
  
  #加入量計算の準備 ----
  ## 加入量期待値、ランダム残差、ln加入量残差をいれるオブジェクト。2020年から50年間（2070年まで）
  ### est_r     :加入量期待値=再生産関係から計算される加入量
  ### rand_resid:ランダム残差：平均ゼロ、標準偏差が再生産関係のsdに従う一様乱数（-1～１）
  ### deviance  :ln加入量残差. est_rにexp(deviance)を掛けると誤差つき加入量になる
  set_rec <- matrix(NA, nrow = 3, ncol = fyear+2, 
                    dimnames = list(c("est_r","rand_resid","deviance"),seq(startyr-1, startyr+fyear, by = 1)))
  
  #加入量期待値と加入量残差の初期値＝2020年の値を埋める
  set_rec["est_r",2]    = pred.f(baaf[3, 2], a = a, b = b)
  set_rec["deviance",2] = log(naaf[1, 2]/set_rec["est_r", 2])
    #前準備終わり----
  
  #ufutureにわたす情報
  input        = list(startyr, Fcur, waa_future, M, a, b, sd, rho, sd_with_AR, bias_factor, fyear)
  names(input) = c("startyr", "Fcur", "waa_future","M","a","b","sd","rho","sd_with_AR","bias_factor","fyear")
  
  prelist        = list(pred.f, input, set_rec, naaf, faaf, caaf, baaf, waaf, wcaaf)
  names(prelist) = c("SRmodel","input", "set_rec", "naaf","faaf", "caaf", "baaf", "waaf", "wcaaf")
  return(prelist)
}#prefuture終わり


#2-2. ufuture: Fを与え、rep回の施行 を実行する関数----

ufuture = function(prepare,rep,fbeta){
  
  #prepare = prelist #チェック時以外はコメントアウト
  #fbeta = 0.6       #チェック時以外はコメントアウト
  #rep=5             #チェック時以外はコメントアウト
  
  #prefutureで準備した情報の取り出し----
  #再生産式 
  (pred.f  = prepare$SRmodel)
  #再生産式のパラメータ
  a           = prepare$input$a
  b           = prepare$input$b
  sd          = prepare$input$sd
  rho         = prepare$input$rho
  sd_with_AR  = prepare$input$sd_with_AR
  bias_factor = prepare$input$bias_factor
  
  #その他の基本情報
  Fcur       = prepare$input$Fcur
  M          = prepare$input$M
  startyr    = prepare$input$startyr
  waa_future = prepare$input$waa_future
  
  #prepareで準備した将来予測計算を格納するオブジェクトを取り出す
  set_rec = prepare$set_rec
  faaf    = prepare$faaf
  naaf    = prepare$naaf
  caaf    = prepare$caaf
  baaf    = prepare$baaf
  waaf    = prepare$waaf
  wcaaf   = prepare$wcaaf
  fyear   = prepare$input$fyear
  
  #Fmultiの計算 ----
  #HCR適用時にはここでHCRに基づきFmultiを決める：2021年2月の会議では対応しない
  Fmulti = fbeta    
  
  #加入量の誤差乱数rand_residを発生
  set.seed(1) 
  rand_resid = matrix(rnorm((fyear+1)*rep, 0,sd), ncol = fyear+1, nrow =rep, byrow = T)
  
  #試行ごとの結果をいれるオブジェクトを用意
  catchl = bioml = ssbl  = recl  = favel = res_aal =  cratiol = list()
  wcaa1  = wcaa2 = wcaa3 = wcaa4 = list()
  
  #計算開始 ----
  for(iter in 1:rep){ #試行回数のループ ----
   # iter=1 　#iter:何回目の試行かを示す変数 #チェック時以外はコメントアウト　
    
    #第iter回の試行に使うrand_resid
    set_rec["rand_resid", 2:ncol(set_rec)] = rand_resid[iter,1:(fyear+1)]
    #set_rec["rand_resid", 2:(length(check_rand_resid)+1)] = check_rand_resid #チェック時以外はコメントアウト
    
    set_rec["deviance", 3] = give_dev_ini(set_rec["rand_resid", 3], set_rec["deviance",2], rho, bias_factor)
    
    #2021年(T3+2)以降の年ループ ----
    for(i in 3:(fyear+2)){
      #i=3#:テスト用のi #チェック時以外はコメントアウト
      
      #step1: Fの計算（HCR適用時にはFmultiを決める関数が必要）
      faaf[, i]  = t(Fcur) * Fmulti
      
      #step2: 1歳前半尾数を前年0歳後半尾数から計算
      naaf[3, i] = N_fn_futur(naaf[2, i-1], faaf[2, i-1], M) 
      
      #step3: 1歳前半尾数＊体重＝親魚量を計算
      ssb_cur    = naaf[3, i]*waaf[3,i]
      
      #step4: 加入量計算 
      
      ##step4-1: 再生産モデルから誤差なしの加入量を計算
      set_rec["est_r", i] = pred.f(ssb_cur, a = a, b = b)
      
      ##step4-2: 加入量にあたえる誤差を計算（ここ、効率悪い）
      set_rec["deviance", i] = ifelse(i == 3,
                                  give_dev_ini(set_rec["rand_resid",i], set_rec["deviance",i-1], rho, bias_factor),
                                  give_dev(set_rec["rand_resid",i], set_rec["deviance",i-1], rho, bias_factor)
                                  )
      
      ##step4-3: 0歳前半尾数＝誤差あり加入量を計算
      naaf[1, i] = set_rec["est_r",i]*exp(set_rec["deviance",i])
      
      #step5: 0,1歳後半尾数を、0,1歳前半尾数から計算
      naaf[c(2, 4), i] = N_fn_futur(naaf[c(1,3), i], faaf[c(1,3), i], M) 
      
      #step6:バイオマス計算
      baaf[, i] = naaf[, i]*waaf[,i]
      
    }#2021年以降の年ループ終わり
    
    #漁獲尾数
    caaf = C_nf_futur(naaf, faaf,M)
    #年齢別資源量、漁獲量
    baaf = naaf*waaf
    wcaaf = caaf*waaf
    
    #出力 ----
    #年齢別の結果をまとめてリストに格納:出力させるかどうか？
    #res_aal[[iter]] = list(naaf, caaf, baaf, faaf, wcaaf)
    
    #合計値
    catchl[[iter]] = colSums(wcaaf)
    bioml[[iter]]  = colSums(baaf[c(1,3),])
    favel[[iter]]  = colMeans(faaf)
    ssbl[[iter]]   = baaf[3,]
    recl[[iter]]   = naaf[1,]
    wcaa1[[iter]]  = wcaaf[1,]
    wcaa2[[iter]]  = wcaaf[2,]
    wcaa3[[iter]]  = wcaaf[3,]
    wcaa4[[iter]]  = wcaaf[4,]
    
    #漁獲割合の計算
    cratiol[[iter]] = colMeans(rbind(colSums(wcaaf[c(1,3),])/colSums(baaf[c(1,3),]),
                                     colSums(wcaaf[c(2,4),])/colSums(baaf[c(2,4),])))
  } #試行回数のループ終わり
  
  
  restemp = list(Fmulti, do.call(rbind, bioml),do.call(rbind,ssbl), do.call(rbind,catchl), do.call(rbind,cratiol),
                 do.call(rbind,wcaa1),   do.call(rbind,wcaa2),
                 do.call(rbind,wcaa3),   do.call(rbind,wcaa4))
  names(restemp) = c("Fmulti", "bioml","ssbl", "catchl","cratiol","wcaa1", "wcaa2","wcaa3","wcaa4")
  
  return(restemp) 
}#ufuture終わり
