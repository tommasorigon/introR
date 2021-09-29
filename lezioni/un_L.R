rm(list = ls())
ciliegi <- read.csv("https://tommasorigon.github.io/introR/data/ciliegi.csv", header = TRUE)
head(ciliegi)


loss <- function(par, y, x) {
  mean((y - par[1] * x^par[2])^2)
}

loss(c(1, 1), ciliegi$volume, ciliegi$diametro)

fit_ls <- nlminb(start = c(1, 1), function(param) loss(param, ciliegi$volume, ciliegi$diametro), lower = c(1e-6, 1e-6))
fit_ls

param_hat_ls <- fit_ls$par

beta_hat_ols <- cov(log(ciliegi$volume), log(ciliegi$diametro)) / var(log(ciliegi$diametro))
alpha_hat_ols <- mean(log(ciliegi$volume)) - mean(log(ciliegi$diametro)) * beta_hat_ols

param_hat_ols <- c(exp(alpha_hat_ols), beta_hat_ols)

cbind(param_hat_ls, param_hat_ols)

loss(param_hat_ls, ciliegi$volume, ciliegi$diametro)
loss(param_hat_ols, ciliegi$volume, ciliegi$diametro)

plot(ciliegi)
curve(param_hat_ls[1] * x^param_hat_ls[2], add = TRUE, lty = "dashed")
curve(param_hat_ols[1] * x^param_hat_ols[2], add = TRUE, lty = "dashed", col = "red")
