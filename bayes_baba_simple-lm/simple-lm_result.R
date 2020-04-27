# 結果の確認
# mcmc_sampleの結果を確認する

library(ggplot2)

# 実測 vs 予測プロット ==========================================
# アヒル本 P.49 fig4.8を参考

# 必要な自作関数(by アヒル本) ----
my_theme <- function(...) {
  theme_bw(base_size=18) + theme(legend.position='none', ...)
}

my_theme0 <- function(...) {
  my_theme(
    axis.ticks=element_blank(),
    axis.text.x=element_blank(),  axis.text.y=element_blank(),
    axis.title.x=element_blank(), axis.title.y=element_blank(),
    ...
  )
}

data.frame.quantile.mcmc <- function(x, y_mcmc, probs=c(2.5, 25, 50, 75, 97.5)/100) {
  qua <- apply(y_mcmc, 2, quantile, probs=probs)
  d <- data.frame(X=x, t(qua))
  colnames(d) <- c('X', paste0('p', probs*100))
  return(d)
}

ggplot.5quantile <- function(data, size=1) {
  qn <- colnames(data)[-1]
  p <- ggplot(data=data, aes(x=X, y=p50))
  p <- p + my_theme()
  p <- p + geom_ribbon(aes_string(ymin=qn[1], ymax=qn[5]), fill='black', alpha=1/6)
  p <- p + geom_ribbon(aes_string(ymin=qn[2], ymax=qn[4]), fill='black', alpha=2/6)
  p <- p + geom_line(size=size)
  return(p)
}
customize.ggplot.axis <- function(p){
  p <- p + labs(x='X', y='Y')
  p <- p + scale_y_continuous(breaks=seq(from=0, to=25, by=5))
  p <- p + coord_cartesian(xlim=c(0, 100), ylim=c(0, 25))
  return(p)
}

# ここから
X_new <- 1:100 #住宅面積の範囲領域
N_X   <- length(X_new)
N_mcmc <- length(ms$lp__)
y_base_mcmc <- as.data.frame(matrix(nrow=N_mcmc, ncol=N_X))
y_mcmc <- as.data.frame(matrix(nrow=N_mcmc, ncol=N_X))

for (i in 1:N_X) {
  y_base_mcmc[,i] <- ms$Intercept + ms$beta * X_new[i]
  y_mcmc[,i] <- rnorm(n=N_mcmc, mean=y_base_mcmc[,i], sd=ms$sigma)
}

d_est <- data.frame.quantile.mcmc(x=X_new, y_mcmc=y_mcmc)

# ggplotによる図示
p <- ggplot.5quantile(data=d_est)
p <- p + geom_point(data=DataRent, aes(x=Area, y=SumValue), shape=1, size=3)
p <- customize.ggplot.axis(p)
p <- p + labs()
p
