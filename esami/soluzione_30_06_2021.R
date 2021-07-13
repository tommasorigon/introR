# ----------------------------------------
# Domanda 1
# ----------------------------------------

# 1.1 (3pt)
cohenD <- function(x, y){
  xbar <- mean(x)
  ybar <- mean(y)
  s2 <- (sum((x - xbar)^2) + sum((y - ybar)^2)) / (length(x) + length(y) - 2)
  abs(xbar - ybar) / sqrt(s2)
}

# 1.2 (1pt)
x <- c(7.19, 8.27, 6.77, 6.65, 8.56, 6.56, 6.09)
y <- c(6.37, 6.27, 7.95, 6.52, 7.72, 6.92, 6.34, 7.51, 6.07, 8.09, 6.03)
cohenD(x, y) # 0.3168269

# 1.3 e 1.4 (1pt + 1pt)
set.seed(123)
cohenD(rnorm(500, 0, 10), rnorm(2000, 0, 10)) # circa 0.03028573, debole
cohenD(rnorm(500, 0, 10), rnorm(2000, 10, 10)) # circa 0.9551635, forte
# Attenzione che rnorm chiede la deviazione standard e non la varianza come input.


# 1.5 (4pt)
set.seed(123)
sim <- replicate(5000, cohenD(rnorm(500, 0, 10), rnorm(2000, 10, 10)))
hist(sim)

# ----------------------------------------
# Domanda 2
# ----------------------------------------

# Le variabili aleatorie binarie coinvolte sono indipendenti tra loro ma NON identicamente distribuite
# Di conseguenza, la loro somma NON segue una distribuzione binomiale!

# 2.1 (4pt)
rS1 <- function(n, alpha){
  probs <- alpha / (alpha + (2:n) - 1)
  1 + sum(rbinom(n = n - 1, size = 1, prob = probs))
}

rS <- function(R, n, alpha){
  replicate(R, rS1(n, alpha))
}

# 2.2 (2pt)
set.seed(123)
mean(rS(R = 10^4, n = 100, alpha = 1)) # 5.1723

# 2.3 (2pt)
set.seed(123)
sim <- rS(R = 10^4, n = 500, alpha = pi / 4)
mean((sim >= 3) & (sim <= 5)) # 0.4393

# 2.4 (2pt)
set.seed(123)
sim <- rS(R = 10^4, n = 500, alpha = sqrt(2))
plot(prop.table(table(sim)), xlab = "s", ylab = "Probabilità empirica")

# ----------------------------------------
# Domanda 3
# ----------------------------------------

library(MASS)
data(Animals)
? Animals # Accede alla documentazione, in cui è spiegato cosa rappresentano le variabili.

# Punto 3.1 (1pt)
lbody <- log(Animals$body)
lbrain <- log(Animals$brain)

# Punto 3.2 (1pt)
summary(lbody)
boxplot.stats(lbody)

summary(lbrain)
boxplot.stats(lbrain)

# Dall'output di questi ultimi comandi si evince che NON sono presenti outlier.
# In alternativa si poteva fare un grafico tramite il comando boxplot

# Punto 3.3 (2pt)
par(mfrow = c(1, 2))
hist(lbrain) # Senza specificare niente, il numero di intervalli viene automaticamente selezionato in maniera corretta
# Ulteriori abbellimenti grafici sono apprezzati ma non necessari
hist(lbody)

# Punto 3.4 (1pt)
par(mfrow = c(1, 1))
plot(lbody, lbrain) # Ulteriori abbellimenti grafici sono apprezzati ma non necessari

# Punto 3.5 (1pt)
cor(lbody, lbrain)

# Punto 3.6 (4pt)
ols <- function(x, y) {
  beta_hat <- cov(x, y) / var(x)
  alpha_hat <- mean(y) - mean(x) * beta_hat
  c(alpha_hat, beta_hat)
}
ols_est <- ols(lbody, lbrain)
ols_est

# Nota: non è necessario creare una funzione, l'importante è ottenere i valori contenuti in ols_est
