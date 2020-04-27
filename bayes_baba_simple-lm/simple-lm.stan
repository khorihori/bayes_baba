
data {
  int N;                  //サンプルサイズ
  vector[N] Area;         //住宅面積
  vector[N] SumValue;     //月々の賃料
}

parameters {
  real Intercept;         //切片
  real beta;              //係数
  real<lower=0> sigma;    //標準偏差
}

model {
  for(i in 1:N){
    SumValue[i] ~ normal(Intercept + beta*Area[i], sigma);
  } 
}

generated quantities{
  //事後予測分布を得る
  vector[N] pred;
  for(i in 1:N){
    pred[i] = normal_rng(Intercept + beta*Area[i],sigma);
  }
}