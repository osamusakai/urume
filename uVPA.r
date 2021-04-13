options(digits=15)
   ### uVPA関数の中で使う関数の定義 ---- 

   ## 1. Fを計算する関数----
   ### CとNから
    F_cn = function(n,c,m){-log(1-c*exp(m/2)/n)}

   ## 2. Nを計算する関数----
   ### CとFから
    N_cf = function(c,f,m){ c*exp(m/2)/(1-exp(-f))}

   ### CとNから
    N_cn = function(c,n,m){n*exp(m)+c*exp(m/2)}

   #3. uVPA関数 ----
    ## 引数
    #### finit:直近年最高齢Fの初期値
    #### dat  :年齢別漁獲尾数などのデータ、frasyrのdata_handler() で作る
    #### lagyr:直近年の翌年の
    
    uvpa= function(finit, dat, lagyr){
      caa = dat$caa
      M   = dat$M
      
      years = dimnames(dat$caa)[[2]]
      ages  = dimnames(dat$caa)[[1]]
      na = nrow(dat$caa)
      ny = ncol(dat$caa)
      tyr = max(ny)-1
      faa <- naa <- matrix(NA, nrow = max(na), ncol = ny, dimnames = list(ages, years))
     
     #VPA計算：直近年＝2019年、評価年＝2020年の場合
     
     # step1.----
     ##F[4, 2019] 1歳後半/直近年（2019年）のFに初期値を与える
      faa[na, tyr] = finit 
     ##F[4, 1999:2018] 直近年以前に同じ値を入れる
      faa[na, 1:(tyr-1)] = faa[na, tyr]

     # step2.----
     ##N[4, 1999:2019] 1歳後半のNをFと漁獲尾数Cから計算
      for(i in 1:(tyr)){
       naa[na,i] = N_cf(caa[na, i], faa[na, i], m = M[na, i])}
    
     # step3.----
     ##N[3, 1999:2019] 1歳前半のNを同年1歳後半のNと1歳前半のCから計算
      for(i in 1:(tyr)){
       naa[na-1,i] = N_cn(caa[na-1, i], naa[na, i], m = M[na-1, i])
      }
      
     # step4.-----
     ##N[2, 1999:2018] 0歳後半のNを、同年0歳後半のCと、翌年1歳前半のNから計算
      for(i in 1:(tyr-1)){
       naa[na-2,i] = N_cn(caa[na-2, i], naa[na-1, i+1], m = M[na-3, i])
      }
     
     #step5.----
     ##N[1, 1999:2018] 0歳前半のNを同年0歳後半のCとNから計算
      for(i in 1:(tyr-1)){
       naa[na-3,i] = N_cn(caa[na-3, i], naa[na-2, i], m = M[na-3, i])
      }

     # step6.----
     ##F[1:3, 1999:2018] 計算されたNとCから、0歳前後～1歳前半のFを計算（直近年の前年2018年まで）
      for(j in (na-1):1){
        for(i in 1:(tyr-1)){
           faa[j,i] = F_cn(naa[j,i],caa[j,i], M[j,i])
        }
      }

     ##F[3,2019] 1歳前半直近年のFを同年のNとCから計算
      faa[(na-1),tyr]= F_cn(naa[(na-1),tyr],caa[(na-1),tyr], M[(na-1),tyr])

     # step7.----
     ##N,F[3,2020],1歳前半の評価年Fに、1歳前半の過去lagyr年平均値を入力して、1歳前半評価年のNを計算
      faa[(na-1), tyr+1] = mean(faa[(na-1), (tyr-lagyr+1):tyr]) 
      naa[(na-1), tyr+1] = N_cf(caa[(na-1),tyr+1],faa[(na-1),tyr+1],M[(na-1),tyr+1])

     #step8.----
     ##N[2,2019] 0歳後半直近年Nを、同年0歳後半のCと評価年1歳後半のNから計算
      naa[(na-2),tyr] = N_cn(caa[(na-2), tyr], naa[(na-1), tyr+1], m = M[(na-2), tyr])
 
     #step9.----
     ##N[1,2019] 0歳前半直近年Nを同年のNとCから計算 
      naa[(na-3),tyr] = N_cn(caa[(na-3), tyr], naa[(na-2), tyr], m = M[(na-2), tyr])

     #step10.----
     ##F[1:2, 2019] ０歳前・後半直近年Fを同年NとCから計算
       for(j in (na-3):(na-2)){faa[j, tyr] = F_cn(naa[j, tyr],caa[j, tyr],M[j, tyr])}

     #step11.----
     ##F[1,2020]　０歳前半・評価年のFに、同齢の過去lagyr年の平均を入力して、０歳前半評価年Nを計算
       faa[(na-3), tyr+1] = mean(faa[(na-3),(tyr-lagyr+1):(tyr)]) 
       naa[(na-3), tyr+1] = N_cf(caa[(na-3), tyr+1], faa[(na-3),tyr+1], M[(na-3),tyr+1])

     #step12.----
     ## 目的関数:最初に仮定した直近年・１歳後半のFと０歳後半のFの全期間平均の差
     ## これを最小化するような値を探索する
      objfun = (log(mean(faa[na-2, 1:tyr]))-log(faa[na,tyr]))^2
      return(list(naa, faa, objfun))
    }
    
  #4. 最適化のための関数 ----
    ## 引数：F　Fを与えてVPAを計算し、objfunの数値を返す。
    do_opt <- function(finit){
             res = uvpa(finit/10^6, dat, lagyr)
             return(res[[3]])
     }


  #5. 出力関数 ----
    ## frasyrのVPA出力に合わせた形で結果を返す
    out_uvpa = function(resvpa, dat, lagyr){ 
      naa = resvpa[[1]]
      faa = resvpa[[2]]
      caa = dat$caa 
      
      fc.at.age = apply(faa[,(max(ncol(faa))-1):(max(ncol(faa))-lagyr)]*10^6, 1, mean)/10^6
 
      waa = dat$waa
      waa.catch = waa
      maa = dat$maa
      Pope = TRUE
      last.catch.zero = FALSE

      ssb = naa*waa*maa
      baa =naa*waa
      wcaa = caa * waa
 
      temp  = list(dat$maa, dat$caa, dat$M, dat$waa, waa.catch)
      input = list(temp, Pope, last.catch.zero)

      names(input) = c("dat", "Pope", "last.catch.zero")
      names(input$dat) = c("maa","caa","M","waa","waa.catch")

      res = list(naa,faa,fc.at.age,input,ssb,baa,wcaa)
      names(res) = c("naa", "faa", "Fc.at.age", "input", "ssb", "baa","wcaa")
      return(res)
    }

  #6. 計算本体の関数 ----
  ## 引数 dat:データ
  ## 引数 lagyr :  評価年０歳前半と１歳前半に与えるFとして、過去何年のFを平均するか
  ##  1.目的関数do_optに1/10^6～3000000/10^6の範囲でfinitを与え、
  ##    do_optのres[[3]]=resobjが最小になるようなfinit(resF[[1]]を返す。
  ##  2.得られたfinitをout_uvpaに与えて得られた結果を返す
    
    calcuVPA = function(dat, lagyr){
      resF   = optimize(do_opt, lower = 1, upper = 3000000)
      res = out_uvpa(uvpa(resF[[1]]/10^6,dat, lagyr), dat, lagyr)
      return(res)
      }

### 関数おわり
