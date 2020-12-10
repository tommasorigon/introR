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
media <- 10
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

# Errore quadratico medio
mean((stimatore - 0)^2)

# C.4

hist(stimatore, breaks = 50)
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

# D.3

fit <- nlminb(start = c(1, 1), function(param) -loglik(param[1], param[2], x), lower = c(1e-10, 1e-10))
theta_hat <- fit$par

# D.4

hist(x, breaks = 8, freq = FALSE, xlim = c(0, 2))
curve(dweibull(x, shape = theta_hat[1], scale = theta_hat[2]), add = TRUE)
