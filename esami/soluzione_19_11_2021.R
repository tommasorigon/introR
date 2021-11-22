# Esercizio 1 -------------------------------------------------

library(MASS)
data(Boston)

# 1.1
x <- Boston$crim
summary(x) # Era corretto anche usare i comandi mean, median

# 1.2
x_river <- x[Boston$chas == "1"]
x_no_river <- x[Boston$chas == "0"]

# 1.3 (Soluzione diretta)
delta <- function(x) {
  n <- length(x)
  S <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      S <- S + abs(x[i] - x[j])
    }
  }
  S / (n * (n - 1))
}
delta(x_river) # 2.484511
delta(x_no_river) # 6.02453

# 1.3 (Soluzione alternativa "elegante")
delta <- function(x) {
  n <- length(x)
  sum(abs(outer(x, x, "-"))) / (n * (n - 1))
}
delta(x_river) # 2.484511
delta(x_no_river) # 6.02453

# 1.4
delta2 <- function(x) {
  n <- length(x)
  4 * sum(sort(x) * 1:n) / (n * (n - 1)) - 2 * mean(x) * (n + 1) / (n - 1)
}

# NOTA: attenzione non dimenticarsi del comando "sort"!

delta2(x_river) # 2.484511
delta2(x_no_river) # 6.02453

# Esercizio 2 -------------------------------------------------

# 2.1 (2pt)
r_log_sin <- function(R) {
  U1 <- runif(R)
  U2 <- runif(R)
  sqrt(-2 * log(U1)) * sin(2 * pi * U2)
}

# 2.2 (2pt)
R <- 10^5

X <- r_log_sin(R)
est1 <- mean(X)
est1 # Circa 0

est2 <- mean(X^2)
est2 # Circa 1

# 2.3 (2pt)
std1 <- sd(X) / sqrt(R)
std1

std2 <- sd(X^2) / sqrt(R)
std2

# 2.4 (2pt)
mean(X < -1)
mean(X < 0)
mean(X < 1)

# 2.5 (2pt)
hist(X, freq = FALSE) 
curve(dnorm(x), add = T)

# La distribuzione assomiglia una gaussiana. Non è un caso: si può dimostrare che X è in effetti distribuita come una distribuzione normale standard.

# Per chi fosse interessato, si veda in proposito: https://it.wikipedia.org/wiki/Trasformazione_di_Box-Muller

# Esercizio 3 -----------------------------------------------

# 3.1
loglik <- function(theta, y){
  - sum(abs(y - theta))
}
loglik <- Vectorize(loglik, vectorize.args = "theta")

# 3.2 (Senza vettorizzazione il seguente codice non funziona)
y <- c(10, 3, 12, 20, 32, 8)
curve(loglik(x, y), 0, 30)

# 3.3
theta_hat <- nlminb(start = 1, function(theta) - loglik(theta, y))$par

# NOTA: non è necessario (anzi, è errato) specificare lower = 1e-5 o in generale lower = "numero vicino a zero". Il parametro theta infatti è un numero reale che, in linea di principio, potrebbe anche essere negativo. 

# 3.4
loglik(theta_hat, y) # - 43
loglik(10 + pi / 4, y) # -43

# La stima di massima verosimiglianza, come si evince dal grafico e dal risultato del punto precedente, non è unica. Infatti, la massimizzazione della funzione di log-verosimiglianza coincide con la minimizzazione della somma degli scarti in valore assoluto. Qualsiasi valore compreso nell'intervallo [10,12] produce il valore -43, ovvero l'insieme delle possibili mediane.

# Si noti infatti (non richiesto) che:
loglik(median(y), y) # - 43
