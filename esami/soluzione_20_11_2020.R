# ----------------------------------------
# Domanda 6
# ----------------------------------------

# Punto 1.1
library(MASS)
data(forbes)
? forbes # Accede alla documentazione, in cui è spiegato cosa rappresentano le variabili.

# Punto 1.2
mean(forbes$bp)
median(forbes$bp)
quantile(forbes$bp)

mean(forbes$pres)
median(forbes$pres)
quantile(forbes$pres)

boxplot.stats(forbes$bp)
boxplot.stats(forbes$pres)

# Dall'output di questi ultimi comandi si evince che NON sono presenti outlier.
# In alternativa si poteva fare un grafico tramite il comando boxplot

# Punto 1.3
par(mfrow = c(1, 2))
hist(forbes$bp) # Senza specificare niente, il numero di intervalli viene automaticamente selezionato in maniera corretta
# Ulteriori abbellimenti grafici sono apprezzati ma non necessari
hist(forbes$pres)

# Punto 1.4
par(mfrow = c(1, 1))
plot(forbes$bp, forbes$pres) # Ulteriori abbellimenti grafici sono apprezzati ma non necessari

# Punto 1.5
cor(forbes$bp, forbes$pres)

# Punto 1.6
ols <- function(x, y) {
  beta_hat <- cov(x, y) / var(x)
  alpha_hat <- mean(y) - mean(x) * beta_hat
  c(alpha_hat, beta_hat)
}
ols_est <- ols(forbes$bp, forbes$pres)
ols_est

# Nota: non è necessario creare una funzione, l'importante è ottenere i valori contenuti in ols_est

# Punto 1.7
abline(ols_est) # Questo è il comando più semplice in assoluto, ma esistono varie alternative più complesse

# Il modello sembra adattarsi estremamente bene ai dati.

# ----------------------------------------
# Domanda 7
# ----------------------------------------

# Punto 2.1
dburr <- function(x, alpha, beta) {
  alpha * beta * x^(alpha - 1) / (1 + x^alpha)^(beta + 1)
}

pburr <- function(x, alpha, beta) {
  1 - 1 / (1 + x^alpha)^beta
}

qburr <- function(p, alpha, beta) {
  (1 / (1 - p)^1 / beta - 1)^(1 / alpha)
}

# Controllo che sia tutto corretto: qburr(pburr(10,2,1),2,1)

# Punto 2.2
curve(dburr(x, 2, 1), 0, 5)

# Punto 2.3
rburr <- function(R, alpha, beta) {
  qburr(runif(R), alpha, beta)
}

# Punto 2.4
x <- rburr(10^4, 2, 1)
mean(x)
var(x) # Alta variabilità nella stima della varianza
mean(x > 2 & x < 3)
# Ultimo comando equivalente a: pburr(3,2,1) - pburr(2,2,1)

# Punto 2.5
hist(x, breaks = 1000, freq = FALSE, xlim = c(0, 5)) # Ci sono dei valori anomali, è necessario restringere il range
curve(dburr(x, 2, 1), add = TRUE)

# Punto 2.6
plot(ecdf(x), xlim = c(0, 5)) # Ci sono dei valori anomali, è necessario restringere il range
curve(pburr(x, 2, 1), add = TRUE, col = "red")
