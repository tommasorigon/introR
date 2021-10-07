loglik <- function(lambda, y) {
  length(y) * log(lambda) - lambda * sum(y)
}

loglik2 <- function(lambda, y) {
  sum(dexp(y, rate = lambda, log = TRUE))
}

loglik3 <- function(lambda, y) {
  sum(log(lambda) - lambda * y)
}

library(boot)
data("aircondit7")
y <- aircondit7$hours

curve(loglik(x, y), 1e-4, 0.06) # log-verosimiglianza nell'intervallo (0.001, 0.06)

loglik(0.01, y)
loglik2(0.01, y)
loglik3(0.01, y)

# Risultato corretto
loglik(c(0.01, 0.02, 0.03), y)

# I seguenti comandi producono invece dei risultati errati
loglik2(c(0.01, 0.02, 0.03), y)
loglik3(c(0.01, 0.02, 0.03), y)

loglik2 <- Vectorize(loglik2, vectorize.args = "lambda")
loglik3 <- Vectorize(loglik3, vectorize.args = "lambda")

loglik(c(0.01, 0.02, 0.03), y)
loglik2(c(0.01, 0.02, 0.03), y)
loglik3(c(0.01, 0.02, 0.03), y)

library(microbenchmark) # Se assente, va installata
microbenchmark(
  L1 = loglik(c(0.01, 0.02, 0.03), y),
  L2 = loglik2(c(0.01, 0.02, 0.03), y),
  L3 = loglik3(c(0.01, 0.02, 0.03), y)
)

lambda <- numeric(10)
lambda[1] <- 0.005 # Punto iniziale (cosa succede cambiandolo?)
n <- length(y)
sum_y <- sum(y)
# Algorithmo Newton-Raphson
for (k in 1:5) {
  score <- n / lambda[k] - sum_y # Funzione score
  obs_info <- n / lambda[k]^2 # Informazione osservata
  lambda[k + 1] <- lambda[k] + score / obs_info # Passo iterativo
  print(c(lambda[k + 1], loglik(lambda[k + 1], y)), digits = 3) # Mostro i risultati
}

lambda_hat <- 1 / mean(y)
c(lambda_hat, loglik(lambda_hat, y))

library(numDeriv) # Libreria per il calcolo di derivate numeriche
hessian(func = function(lambda) loglik(lambda, y), x = lambda_hat) # Derivata seconda

fit_exp <- nlminb(start = 1, objective = function(lambda) -loglik(lambda, y), lower = 1e-5)
fit_exp

lambda_hat <- fit_exp$par # Stima di massima verosimiglianza
curve(loglik(x, y), 0.001, 0.05)
abline(v = lambda_hat, lty = "dotted")

obs_info <- length(y) / lambda_hat^2
obs_info

optim(par = 1, fn = function(lambda) -loglik(lambda, y), lower = 1e-5, method = "L-BFGS-B", hessian = TRUE)

fit_exp_reparam <- nlminb(start = 1, objective = function(psi) -loglik(exp(psi), y))
exp(fit_exp_reparam$par) # Trasformazione inversa

plot(ecdf(y))
curve(pexp(x, lambda_hat), col = "red", add = TRUE)

loglik <- function(par, y) {
  sum(dweibull(y, shape = par[1], scale = par[2], log = TRUE))
}

library(boot)
data("aircondit7")
y <- aircondit7$hours

# Definizione degli intervalli di punti
gamma <- seq(0.1, 2.25, length = 200)
beta <- seq(0.5, 400, length = 200)

# Ottenimento della griglia tramite prodotto cartesiano
parvalues <- expand.grid(gamma, beta)

# Calcolo valori di verosimiglianza
llikvalues <- apply(parvalues, 1, loglik, y = y)

# Ri-organizzazione dei valori della log-verosimiglianza in forma matriciale
llikvalues <- matrix(llikvalues, nrow = length(gamma), ncol = length(beta), byrow = F)

contour(gamma, beta, llikvalues,
  xlab = expression(gamma), ylab = expression(beta), # Produce le lettere greche nel grafico
  levels = seq(from = -140, to = -120, by = 2),
  main = "Log-verosimiglianza (Modello Weibull)"
)

filled.contour(gamma, beta, llikvalues,
  xlab = expression(gamma), ylab = expression(beta),
  levels = seq(from = -140, to = -120, by = 1),
  col = terrain.colors(20), main = "Log-verosimiglianza (Modello Weibull)"
)

fit_weibull <- nlminb(start = c(1, 1), function(par) -loglik(par, y), lower = c(1e-7, 1e-7))
fit_weibull

obs_info <- -hessian(func = function(par) loglik(par, y), x = fit_weibull$par)
solve(obs_info)

plot(ecdf(y))
curve(pexp(x, fit_exp$par), col = "red", add = TRUE)
curve(pweibull(x, fit_weibull$par[1], fit_weibull$par[2]), col = "blue", add = TRUE)
