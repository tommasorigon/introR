rm(list = ls())
library(boot)
data("aircondit")

y <- aircondit7$hours


loglik <- function(lambda, y) {
  length(y) * log(lambda) - lambda * sum(y)
}

loglik2 <- function(lambda, y) {
  sum(dexp(y, rate = lambda, log = TRUE))
}

loglik3 <- function(lambda, y) {
  sum(log(lambda) - lambda * y)
}

loglik(0.001, y)
loglik2(0.001, y)
loglik3(0.001, y)

par(mfrow = c(1, 1))
curve(loglik(x, y), 1e-4, 0.06)

# Funzioni vettorizzabili
loglik(1:4 / 10, y)
loglik2(1:4 / 10, y)
loglik3(1:4 / 10, y)

loglik2 <- Vectorize(loglik2, vectorize.args = "lambda")
loglik3 <- Vectorize(loglik3, vectorize.args = "lambda")

# Funzioni vettorizzabili
loglik(1:4 / 10, y)
loglik2(1:4 / 10, y)
loglik3(1:4 / 10, y)

# Benchmarking
library(microbenchmark)
microbenchmark(
  L1 = loglik(1:4 / 10, y),
  L2 = loglik2(1:4 / 10, y),
  L3 = loglik3(1:4 / 10, y)
)


lambda_hat <- 1 / mean(y)
lambda_hat
loglik(lambda_hat, y)

optim(par = 1, fn = function(lambda) - loglik(lambda, y), lower = 1e-5, method = "L-BFGS-B")

nlminb(start = 1, objective =  function(lambda) - loglik(lambda, y), lower = 1e-5)
