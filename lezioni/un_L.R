rm(list = ls())
ciliegi <- read.csv("https://tommasorigon.github.io/introR/data/ciliegi.csv", header = TRUE)
head(ciliegi)


loss <- function(par, y, x) {
  mean((y - par[1] * x^par[2])^2)
}

loss(c(12, 1e-4), ciliegi$volume, ciliegi$diametro)


eta <- seq(0.015, 0.4, length = 200)
lambda <- seq(1.5, 3, length = 200)
parvalues <- expand.grid(eta, lambda)

lossvalues <- apply(parvalues, 1, loss, y = ciliegi$volume, x = ciliegi$diametro)
lossvalues <- matrix(lossvalues, nrow = length(eta), ncol = length(lambda), byrow = F)

filled.contour(eta, lambda, lossvalues,
               xlab = expression(eta), ylab = expression(lambda),
               levels = seq(from = 0, to = 25, by = 2),
               #col = terrain.colors(20),
               main = "Funzione di perdita"
)



fit_ls <- nlminb(start = c(1, 1), function(param) loss(param, ciliegi$volume, ciliegi$diametro),
                 lower = c(1e-6, 1e-6))
fit_ls

param_hat_ls <- fit_ls$par

z <- log(ciliegi$volume)
w <- log(ciliegi$diametro)

beta_hat_ols <- cov(z, log(ciliegi$diametro)) / var(w)
alpha_hat_ols <- mean(z) - mean(w) * beta_hat_ols

param_hat_ols <- c(exp(alpha_hat_ols), beta_hat_ols)

cbind(param_hat_ls, param_hat_ols)

loss(param_hat_ls, ciliegi$volume, ciliegi$diametro)
loss(param_hat_ols, ciliegi$volume, ciliegi$diametro)

plot(ciliegi)
curve(param_hat_ls[1] * x^param_hat_ls[2], add = TRUE, lty = "dashed")
curve(param_hat_ols[1] * x^param_hat_ols[2], add = TRUE, lty = "dashed", col = "red")
