# Esercizio A --------------------------------

# A.1

media <- 5
scarto <- sqrt(12)

# Numerosità campionarie
nn <- c(50, 100, 1000, 5000, 10000, 50000, 100000)

# Mediane
set.seed(150)
median_hat <- c(
  median(rnorm(nn[1], mean = media, sd = scarto)),
  median(rnorm(nn[2], mean = media, sd = scarto)),
  median(rnorm(nn[3], mean = media, sd = scarto)),
  median(rnorm(nn[4], mean = media, sd = scarto)),
  median(rnorm(nn[5], mean = media, sd = scarto)),
  median(rnorm(nn[6], mean = media, sd = scarto)),
  median(rnorm(nn[7], mean = media, sd = scarto))
)

plot(nn, median_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Mediana"
)
abline(h = media, lty = 2) # Lo stimatore sembra essere consistente.

# A.2

R <- 10^5
n <- 30

# Esecuzione della simulazione
set.seed(520)
stimatore1 <- replicate(R, mean(rnorm(n = n, mean = media, sd = scarto)))
stimatore2 <- replicate(R, median(rnorm(n = n, mean = media, sd = scarto)))

# Errore quadratico
mean((stimatore1 - media)^2)
mean((stimatore2 - media)^2)

# La media sembra essere uno stimatore migliore in termini di errore quadratico medio

# A.3

hist(stimatore2, breaks = 50)
qqnorm(stimatore2)
qqline(stimatore2)

# L'approssimazione gaussiana sembra essere molto buona.

# Esercizio B -----------------------------------------

# B.1

# Numerosità campionarie
nn <- c(50, 100, 1000, 5000, 10000, 50000, 100000)

# Varianza campionaria
var2 <- function(x) mean(x^2) - mean(x)^2

# Parametri della simulazione
media <- 0
varianza <- 10
scarto <- sqrt(varianza)

set.seed(150)
var2_hat <- c(
  var2(rnorm(nn[1], mean = media, sd = scarto)),
  var2(rnorm(nn[2], mean = media, sd = scarto)),
  var2(rnorm(nn[3], mean = media, sd = scarto)),
  var2(rnorm(nn[4], mean = media, sd = scarto)),
  var2(rnorm(nn[5], mean = media, sd = scarto)),
  var2(rnorm(nn[6], mean = media, sd = scarto)),
  var2(rnorm(nn[7], mean = media, sd = scarto))
)

set.seed(150)
var_hat <- c(
  var(rnorm(nn[1], mean = media, sd = scarto)),
  var(rnorm(nn[2], mean = media, sd = scarto)),
  var(rnorm(nn[3], mean = media, sd = scarto)),
  var(rnorm(nn[4], mean = media, sd = scarto)),
  var(rnorm(nn[5], mean = media, sd = scarto)),
  var(rnorm(nn[6], mean = media, sd = scarto)),
  var(rnorm(nn[7], mean = media, sd = scarto))
)

plot(nn, var2_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Varianzia campionaria"
)
abline(h = varianza, lty = 2) # Lo stimatore sembra essere consistente.

plot(nn, var_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Varianzia campionaria corretta"
)
abline(h = varianza, lty = 2) # Lo stimatore sembra essere consistente.

# Esercizio C ---------------------------------------

# C.1

pearson_sym <- function(x) {
  xbar <- mean(x)
  s <- sqrt(mean(x^2) - xbar^2)
  mean(((x - xbar) / s)^3)
}

media <- 3
scarto <- sqrt(10)

# Numerosità campionarie
nn <- c(50, 100, 1000, 5000, 10000, 50000, 100000)

# Mediane
set.seed(150)
gamma_hat <- c(
  pearson_sym(rnorm(nn[1], mean = media, sd = scarto)),
  pearson_sym(rnorm(nn[2], mean = media, sd = scarto)),
  pearson_sym(rnorm(nn[3], mean = media, sd = scarto)),
  pearson_sym(rnorm(nn[4], mean = media, sd = scarto)),
  pearson_sym(rnorm(nn[5], mean = media, sd = scarto)),
  pearson_sym(rnorm(nn[6], mean = media, sd = scarto)),
  pearson_sym(rnorm(nn[7], mean = media, sd = scarto))
)

plot(nn, gamma_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Asimmetria di Pearson"
)
abline(h = 0, lty = 2) # Lo stimatore sembra essere consistente.

# C.2

R <- 10^5
n <- 10

# Esecuzione della simulazione
set.seed(520)
stimatore <- replicate(R, pearson_sym(rnorm(n = n, mean = media, sd = scarto)))

# Distorsione
mean(stimatore - 0) # Lo stimatore presenta una leggerissima distorsione

# C.3

n <- 100
set.seed(520)
stimatore <- replicate(R, pearson_sym(rnorm(n = n, mean = media, sd = scarto)))

# Errore quadratico medio
mean((stimatore - 0)^2)

# C.4

hist(stimatore, breaks = 50, freq = F)
curve(dnorm(x, mean(stimatore), sd(stimatore)), add = T)

# Comando alternativo
qqnorm(stimatore)
qqline(stimatore) # La distribuzione NON sembra essere gaussiana

# C.5

# L'esercizio è identico, sostituendo alla funzione rnorm le funzion rgamma e rlogis.

# Esercizio D ----------------------------------------------------------

x <- c(225, 171, 198, 189, 189, 135, 162, 135, 117, 162)

# D.1

loglik <- function(alpha, lambda, x) {
  sum(dweibull(x = x, shape = alpha, scale = lambda, log = TRUE))
}

# D.2

loglik(alpha = 6, lambda = 200, x = x) # -1026.19
# -50.17214

# D.3

fit <- nlminb(start = c(1, 1), function(param) -loglik(param[1], param[2], x), lower = c(1e-10, 1e-10))
theta_hat <- fit$par
# 5.976923 181.405558

# D.4

hist(x, breaks = 8, freq = FALSE)
curve(dweibull(x, shape = theta_hat[1], scale = theta_hat[2]), add = TRUE)

# Esercizio E --------------------------------

# E.1

lambda <- 3

# Numerosità campionarie
nn <- c(50, 100, 1000, 5000, 10000, 50000, 100000)

# simulazioni
set.seed(150)
T1_hat <- c(
  sum(rpois(nn[1], lambda = lambda)),
  sum(rpois(nn[2], lambda = lambda)),
  sum(rpois(nn[3], lambda = lambda)),
  sum(rpois(nn[4], lambda = lambda)),
  sum(rpois(nn[5], lambda = lambda)),
  sum(rpois(nn[6], lambda = lambda)),
  sum(rpois(nn[7], lambda = lambda))
)

T2_hat <- c(
  sum(1:nn[1] * rpois(nn[1], lambda = lambda)),
  sum(1:nn[2] * rpois(nn[2], lambda = lambda)),
  sum(1:nn[3] * rpois(nn[3], lambda = lambda)),
  sum(1:nn[4] * rpois(nn[4], lambda = lambda)),
  sum(1:nn[5] * rpois(nn[5], lambda = lambda)),
  sum(1:nn[6] * rpois(nn[6], lambda = lambda)),
  sum(1:nn[7] * rpois(nn[7], lambda = lambda))
)

T3_hat <- c(
  mean(rpois(nn[1], lambda = lambda)),
  mean(rpois(nn[2], lambda = lambda)),
  mean(rpois(nn[3], lambda = lambda)),
  mean(rpois(nn[4], lambda = lambda)),
  mean(rpois(nn[5], lambda = lambda)),
  mean(rpois(nn[6], lambda = lambda)),
  mean(rpois(nn[7], lambda = lambda))
)

plot(nn, T1_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Stimatori"
)
abline(h = lambda, lty = 2) # Lo stimatore NON sembra essere consistente.

plot(nn, T2_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Stimatori"
)
abline(h = lambda, lty = 2) # Lo stimatore NON sembra essere consistente.


plot(nn, T3_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Stimatori"
)
abline(h = lambda, lty = 2) # Lo stimatore sembra essere consistente.


# E.2

R <- 10^5
n <- 50

# Esecuzione della simulazione
set.seed(520)
stimatore1 <- replicate(R, sum(rpois(n, lambda = lambda)))
stimatore2 <- replicate(R, sum(1:n * rpois(n = n, lambda = lambda)))
stimatore3 <- replicate(R, mean(rpois(n = n, lambda = lambda)))

# Errore quadratico
mean((stimatore1 - lambda)^2)
mean((stimatore2 - lambda)^2)
mean((stimatore3 - lambda)^2)


# La media sembra essere uno stimatore migliore in termini di errore quadratico medio

# E.3

hist(stimatore1, breaks = 50, freq = FALSE)
curve(dnorm(x, mean(stimatore1), sd(stimatore1)), add = T)

# Comando alternativo
qqnorm(stimatore1)
qqline(stimatore1)

hist(stimatore2, breaks = 50, freq = FALSE)
curve(dnorm(x, mean(stimatore2), sd(stimatore2)), add = T)

# Comando alternativo
qqnorm(stimatore2)
qqline(stimatore2)

hist(stimatore3, breaks = 50, freq = FALSE)
curve(dnorm(x, mean(stimatore3), sd(stimatore3)), add = T)

# Comando alternativo
qqnorm(stimatore3)
qqline(stimatore3)

# L'approssimazione gaussiana sembra essere buona.


# Esercizio F -------------------------------------------------------------

y <- c(2.52, 0.76, 1.55, 0.98, 4.03, 0.09, -2.27, 1.67, -0.54, -0.27)

# D.1
loglik <- function(theta, y) {
  n <- length(y)
  - n / 2 * log(theta) - sum(y^2) / (2 * theta)
}

# D.2
loglik(theta = 3, y = y) # -11.30076
loglik(theta = 5, y = y) # -11.53181

# E' piú verosimile theta = 3

# D.3

fit <- nlminb(start = 1, function(param) -loglik(param, y), lower = 1e-10)
theta_hat <- fit$par
theta_hat 
# 3.48462

# D.4

hist(y, breaks = 8, freq = FALSE)
curve(dnorm(x, mean = 0, sd = sqrt(theta_hat)), add = TRUE)

# Esercizio G -----------------------------------------

# G.1

# Numerosità campionarie
nn <- c(50, 100, 1000, 5000, 10000, 50000, 100000)

# Media troncata
alpha <- 0.05

trunc_mean <- function(x, alpha) {
  mean(variabile, trim = alpha)
}

# Parametri della simulazione
media <- 5
varianza <- 15
scarto <- sqrt(varianza)

set.seed(150)
media_camp <- c(
  mean(rnorm(nn[1], mean = media, sd = scarto)),
  mean(rnorm(nn[2], mean = media, sd = scarto)),
  mean(rnorm(nn[3], mean = media, sd = scarto)),
  mean(rnorm(nn[4], mean = media, sd = scarto)),
  mean(rnorm(nn[5], mean = media, sd = scarto)),
  mean(rnorm(nn[6], mean = media, sd = scarto)),
  mean(rnorm(nn[7], mean = media, sd = scarto))
)

set.seed(150)
trunc_mean_hat <- c(
  trunc_mean(rnorm(nn[1], mean = media, sd = scarto), alpha = alpha),
  trunc_mean(rnorm(nn[2], mean = media, sd = scarto), alpha = alpha),
  trunc_mean(rnorm(nn[3], mean = media, sd = scarto), alpha = alpha),
  trunc_mean(rnorm(nn[4], mean = media, sd = scarto), alpha = alpha),
  trunc_mean(rnorm(nn[5], mean = media, sd = scarto), alpha = alpha),
  trunc_mean(rnorm(nn[6], mean = media, sd = scarto), alpha = alpha),
  trunc_mean(rnorm(nn[7], mean = media, sd = scarto), alpha = alpha)
)

plot(nn, media_camp,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Media campionaria"
)
# lines(nn, trunc_mean_hat, col=2)
abline(h = media, lty = 2) # Lo stimatore sembra essere consistente.

plot(nn, trunc_mean_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Media troncata campionaria"
)
abline(h = media, lty = 2) # Lo stimatore sembra essere consistente.
