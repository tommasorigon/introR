rm(list = ls())

# Problema 1 ----------------------------------------------------

# 1.1
data(swiss)
swiss$logEducation <- log(swiss$Education)

# 1.2
plot(swiss$logEducation, swiss$Agriculture, xlab = "Istruzione", ylab = "Agricoltura")

# 1.3
cov(subset(swiss, select = c(Agriculture, Fertility, logEducation)))
#              Agriculture Fertility logEducation
# Agriculture    515.79942 100.16915  -12.1791394
# Fertility      100.16915 156.04250   -5.1961902
# logEducation   -12.17914  -5.19619    0.6294627

cor(subset(swiss, select = c(Agriculture, Fertility, logEducation)))
#              Agriculture  Fertility logEducation
# Agriculture    1.0000000  0.3530792   -0.6759136
# Fertility      0.3530792  1.0000000   -0.5242985
# logEducation  -0.6759136 -0.5242985    1.0000000


# 1.4
resid <- function(x, y) {
  beta_hat <- cov(x, y) / var(x)
  alpha_hat <- mean(y) - beta_hat * mean(x)
  fitted <- alpha_hat + beta_hat * x
  y - fitted
}

cor(resid(swiss$logEducation, swiss$Agriculture), resid(swiss$logEducation, swiss$Fertility))
# [1] -0.002073525

# Problema 2 ------------------------------------------------------

# 2.1
dkum <- function(x, alpha, beta) {
  alpha * beta * x^(alpha - 1) * (1 - x^alpha)^(beta - 1)
}

pkum <- function(x, alpha, beta) {
  1 - (1 - x^alpha)^beta
}

qkum <- function(p, alpha, beta) {
  (1 - (1 - p)^(1 / beta))^(1 / alpha)
}

# 2.2
par(mfrow = c(1, 3))
curve(dkum(x, 2, 2), 0, 1)
curve(pkum(x, 2, 2), 0, 1)
curve(qkum(x, 2, 2), 0, 1)

# 2.3
rkum <- function(R, alpha, beta) {
  qkum(runif(R), alpha, beta)
}

# 2.4
set.seed(500)
R <- 10^5
par(mfrow = c(1, 1))
hist(rkum(R, 2, 2), freq = FALSE, breaks = 100)
curve(dkum(x, 2, 2), add = TRUE)

# 2.5
set.seed(500)
X_sim <- rkum(R, 2, 2)
mean(X_sim) # 0.5328233
sd(X_sim) / sqrt(R)

# Problema 3 ---------------------------------------------------

# Definisco le funzioni che calcolano gli stimatori
T1 <- function(x) exp(-mean(x))
T2 <- function(x) mean(x == 0)

# 3.1
lambda0 <- 2
psi0 <- dpois(0, lambda0)

nn <- c(10, 100, 300, 500, 1000)
set.seed(123)
T1_seq <- c(
  T1(rpois(nn[1], lambda0)),
  T1(rpois(nn[2], lambda0)),
  T1(rpois(nn[3], lambda0)),
  T1(rpois(nn[4], lambda0)),
  T1(rpois(nn[5], lambda0))
)

T2_seq <- c(
  T2(rpois(nn[1], lambda0)),
  T2(rpois(nn[2], lambda0)),
  T2(rpois(nn[3], lambda0)),
  T2(rpois(nn[4], lambda0)),
  T2(rpois(nn[5], lambda0))
)

plot(nn, T1_seq, type = "b")
abline(h = psi0, lty = "dotted")

plot(nn, T2_seq, type = "b")
abline(h = psi0, lty = "dotted")

# 3.2 - 3.3
set.seed(520)
R <- 10^5
n <- 20
lambda0 <- 2
psi0 <- dpois(0, lambda0)

# Esecuzione della simulazione
T1_sim <- replicate(R, T1(rpois(n = n, lambda0)))
T2_sim <- replicate(R, T2(rpois(n = n, lambda0)))

# Distorsioni (approssimate)
round(mean(T1_sim - psi0), 2)
round(mean(T2_sim - psi0), 2)

# Errore quadratico medio (approssimato)
mean((T1_sim - psi0)^2)
mean((T2_sim - psi0)^2)
