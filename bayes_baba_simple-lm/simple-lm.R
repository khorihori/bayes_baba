# simple-lm.R
# suumoの住宅データでベイズ統計モデリング(単回帰)をやってみよう

# 解析の準備 =====================================

# loading library 
library(rstan)
library(bayesplot)
library(ggplot2)

# loading data 
DataRent <- read.csv("data/suumo.csv")
head(DataRent)

# 図示
pairs.panels(DataRent[,c("Area","SumValue")])
ggplot(DataRent, aes(x=Area,y=SumValue)) +
  geom_point() + 
  labs(title = "専有面積と賃料の関係",x="面積[m2]",y="賃料[万円]")
  

# 解析実行 =======================================
# mu_i = beta_0 + beta_1 * x_i
# y_i ~ Normal(mu_i, sigma^2)
# 線形予測子：beta_0 + beta_1 * x_i
# リンク関数：恒等関数
# 確率分布：正規分布

sample_size <- nrow(DataRent)

data_list <- list(
  N = sample_size,
  SumValue = DataRent$SumValue,
  Area = DataRent$Area
)

mcmc_result <- stan(
  file   = "simple-lm.stan",        #stanファイル
  data   = data_list,               #対象データ
  seed   = 1,                       #乱数の種
  chains = 4,                       #チェーン数
  iter   = 2000,                    #乱数生成の繰り返し数
  warmup = 1000,                    #バーンイン期間
  thin   = 1                        #間引き数(1なら間引き無し)
)

# 結果の確認　====================================


