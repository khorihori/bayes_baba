# multi-lm.R
# suumoの住宅データでベイズ統計モデリング(重回帰)をやってみよう

# 解析の準備 -----------------------------------------------
# データの読み込み
library(rstan)
library(brms)
library(bayesplot)
library(ggplot2)


# 分析対象のデータ
DataRent.All <- read.csv("data/suumo.csv")
sel <- sample(1:nrow(DataRent.All),nrow(DataRent.All)*0.7)
DataRent <- DataRent.All[sel,]
DataRent.test <- DataRent.All[-sel,]

# 重回帰モデルを作る --------------------------------------
lm_brms <- brm(
  formula = SumValue ~ Area + Age,           # モデルの構造を決定
  family  = gaussian(),                      # 正規分布を使う
  data    = DataRent,                        # データ
  seed    = 1,                               # 乱数の種
  prior   = c(set_prior("", class="Intercept"),
              set_prior("", class="sigma"))
)

# 結果の確認
lm_brms

# traceplot
plot(lm_brms)

# 事後分布の図示
stanplot(lm_brms, pars=c("b_Area","b_Age"))
mcmc_areas(lm_brms,pars=c("b_Area","b_Age"))


# 回帰直線の信用区間付きの予測
eff <- marginal_effects(lm_brms)
plot(eff, points=TRUE)

# 95%予測区間付きのグラフ
eff_pre <- marginal_effects(lm_brms, method="predict")
plot(eff_pre, points=TRUE)


# 実測vs予測 ------------------------------------------
# 関数の用意
fRMSE = function(y, pred){
  f = (!is.na(y) & !is.na(pred))
  sqrt(mean((pred[f]-y[f])^2))
}
fR2 = function(y,pred){
  f = (!is.na(y) & !is.na(pred))
  zansa = sum((y-pred)^2)
  bunsan = sum((y-mean(y))^2)
  1-zansa/bunsan
}

f_ggplot <- function(pred,actual,col="black",title=""){
  errors <- aes(ymax=pred[,4],ymin=pred[,3])
  RMSE <- fRMSE(y=actual,pred=pred[,1])
  R2   <- fR2(y=actual,pred=pred[,1])
  data <- data.frame(pred,actual=actual)
  p <- ggplot(data,aes(x=actual,y=Estimate)) +
    geom_point(alpha=0.3,colour=col) +
    ggtitle(title) +
    scale_x_continuous(limits=c(0,25)) +
    scale_y_continuous(limits=c(0,25)) +
    labs(x="実測値",y="予測値") +
    geom_errorbar(errors,alpha=0.1,col=col) +
    annotate("text",label=paste0("RMSE=",round(RMSE,2)),x=3,y=24,size=6) +
    annotate("text",label=paste0("R2=",round(R2,2)),x=3,y=22,size=6) +
    geom_abline(intercept=0,slope=1,linetype="dotted",colour="grey40") +
    theme_bw()
                         
  return(p)
}
f_ggplot(pred   = predict(lm_brms,DataRent[,c("Area","Age")]),
         actual = DataRent[,"SumValue"],
         col    = "darkblue",
         title  = "重回帰_トレーニングデータ")

f_ggplot(pred   = predict(lm_brms,DataRent.test[,c("Area","Age")]),
         actual = DataRent.test[,"SumValue"],
         col    = "darkred",
         title  = "重回帰_テストデータ")


# 単回帰(面積だけ)との比較
simple_lm_brms <- brm(
    formula = SumValue ~ Area,                 # モデルの構造を決定
    family  = gaussian(),                      # 正規分布を使う
    data    = DataRent,                        # データ
    seed    = 1,                               # 乱数の種
    prior   = c(set_prior("", class="Intercept"),
                set_prior("", class="sigma"))
)

f_ggplot(pred   = predict(simple_lm_brms,DataRent[,c("Area","Age")]),
         actual = DataRent[,"SumValue"],
         col    = "darkblue",
         title  = "単回帰_トレーニングデータ")

f_ggplot(pred   = predict(simple_lm_brms,DataRent.test[,c("Area","Age")]),
         actual = DataRent.test[,"SumValue"],
         col    = "darkred",
         title  = "単回帰_テストデータ")


