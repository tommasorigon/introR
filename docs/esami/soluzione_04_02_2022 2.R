# Problema 1 ----------------------------------------------------

library(MASS)
data("birthwt")

# 1.1
birthwt$age_class <- cut(birthwt$age, c(13, 22.5, 28.5, 50))
levels(birthwt$age_class) <- c("low", "medium", "high")
table(birthwt$age_class)

# 1.2
birthwt$bwt <- birthwt$bwt / 1000
hist(birthwt$bwt)

# 1.3
boxplot(birthwt$bwt ~ birthwt$age_class)

# 1.4
birthwt_no_smoke <- birthwt[birthwt$smoke == 0, ]
boxplot(birthwt_no_smoke$bwt ~ birthwt_no_smoke$age_class)

# 1.5
X <- cbind(1, birthwt$age, birthwt$ftv)
y <- birthwt$bwt
beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta
# Ovviamente, il simbolo (-1) indica la matrice inversa.

# Problema 2 ----------------------------------------

# 2.1
n <- 20

# Questa soluzione è accettabile, anche se un po' instabile numericamente per n elevato
kl <- sum(dbinom(0:n, n, 0.5) * log(dbinom(0:n, n, 0.5) / dbinom(0:n, n, 0.2)))
kl # 4.462871

# 2.2
set.seed(123)
R <- 10^5
X <- rbinom(R, n, 0.5)
sim_kl <- log(dbinom(X, n, 0.5) / dbinom(X, n, 0.2))

# Stima Montecarlo
mean(sim_kl) # 4.460029

# Deviazione standard della stima
sd(sim_kl) / sqrt(R) # 0.009778728

# 2.3

# Gli approcci possibili sono almeno due, ovvero quelli dei punti 2.1 e 2.2. L'approccio Monte Carlo sembra essere il più semplice. In alternativa, si poteva procedere come in 2.1, troncando opportunamente la serie ad un valore "grande".

set.seed(123)
R <- 10^5
X <- rpois(R, 1)
sim_kl <- log(dpois(X, 1) / dpois(X, 4))

# Stima Montecarlo
mean(sim_kl) # 1.616048

# Problema 3 ----------------------------------

# 3.1
loglik <- function(theta, y) {
  log(theta) * sum(y) + log(1 - theta) * sum(1 - y)
}

# Non serve vettorizzare se il codice è stato scritto come nelle righe precedenti

# Se necessario (dipende dall'implementazione), si poteva aggiungere:
# loglik <- Vectorize(loglik, vectorize.args = "theta")

# 3.2
y <- c(0, 0, 1, 0, 0, 0, 0, 1)
curve(loglik(x, y), 0, 1)

# 3.3
theta_hat <- nlminb(start = 0.5, function(theta) -loglik(theta, y), lower = 1e-10, upper = 1 - 1e-10)$par
theta_hat # 0.25

# La verosimiglianza è definita tra (0,1). Per cui è necessario usare le opzioni "lower" ed "upper". Inoltre, la procedura di ottimizzazione va inizializzata ad un valore interno a (0,1), come ad esempio start = 0.5.

# 3.4
loglik_approx <- function(theta, y) {
  theta_hat <- mean(y)
  n <- length(y)
  loglik(theta_hat, y) - n / (2 * theta_hat * (1 - theta_hat)) * (theta - theta_hat)^2
}

curve(loglik(x, y), 1 / 10, 1 / 2)
curve(loglik_approx(x, y), add = TRUE, lty = "dotted", col = "red")

# 3.5

y <- c(0, 0, 0, 0, 0, 0, 0, 0)

# Calcolo la stima di massima verosimiglianza usando la formula. Anche la procedura con nlminb è stata considerata corretta.
mean(y) # 0

curve(loglik(x, y), 0, 1)
# Il seguente comando non produce alcun risultato. La stima di massima verosimiglianza è infatti al bordo dello spazio parametrico e le approssimazioni quadratiche non sono ben definite.
curve(loglik_approx(x, y), add = TRUE, lty = "dotted", col = "red")
