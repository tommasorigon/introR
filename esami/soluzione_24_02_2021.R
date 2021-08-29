# Esame 24 Febbraio 2021

# Il seguente file costituisce uno SCHEMA di una possibile soluzione.

# ----------------------------------------
# Domanda 1
# ----------------------------------------

# 1.1 (6pt)
stirling2 <- function(n, k) {
  idx <- 0:k
  1 / factorial(k) * sum((-1)^(k - idx) * choose(k, idx) * idx^n)
}

# Questa soluzione basata sui "cicli for" è accettabile
Bell <- function(n, k) {
  stirlings2 <- numeric(n)
  for (k in 1:n) {
    stirlings2[k] <- stirling2(n, k)
  }
  sum(stirlings2)
}

# Soluzione "elegante", sebbene NON richiesta
stirling2 <- Vectorize(stirling2, vectorize.args = "k") # Questo comando è cruciale, altrimenti si ottiene un valore negativo (privo di ogni senso)
Bell <- function(n){
  sum(stirling2(n, 1:n))
}

# 1.2 (1pt)
stirling2(10, 5) # 42525

# 1.3 (1pt)
Bell(10) # 115975

# Si veda anche https://it.wikipedia.org/wiki/Numeri_di_Stirling per una lista di valori fino a n = 9.

# ----------------------------------------
# Domanda 2
# ----------------------------------------

# 2.1 (2pt)
H <- function(n, alpha) {
  sum(1 / (1:n)^alpha)
}
H(10, 1) # 2.928968

# 2.2 (2pt)
dzipf <- function(k, n, alpha) {
  k^(-alpha) / H(n, alpha)
}
dzipf(5, 10, 2) # 0.02581032

# 2.3 (2pt)
n <- 50
alpha <- 1
plot(1:n, dzipf(1:n, n, alpha), type = "h") # Diagramma a bastoncini

# Nota: per "funzione di probabilità si intendeva la probabilità P(X = k), ovvero la funzione "dzipf". Qualche studente ha rappresentato invece la funzione di ripartizione (pzipf), che è stata accettata come corretta.

# 2.4 (2pt)
n <- 100
alpha <- 2
sum(dzipf(10:n, n, alpha)) # 0.05823676

# 2.5 (3pt)
rzipf <- function(R, n, alpha) {
  sample(1:n, size = R, prob = dzipf(1:n, n, alpha), replace = TRUE)
}

# Non richiesto, ma per verificare che tutto funzioni si potevano usare i comandi:
set.seed(123)
rzipf(R = 10, n, alpha) # 1 3 1 5 9 1 1 5 1 1

# 2.6 (2pt)
n <- 50
alpha <- 1
set.seed(123)
mean(rzipf(R = 10^5, n, alpha)) # Circa 11.08. A seconda del seed scelto, il risultato può variare

# Non richiesto, ma per verificare che la correttezza dei propri risultati, si poteva calcolare anche il valore atteso teorico
sum(1:n * dzipf(1:n, n, alpha)) # 11.11307

# ----------------------------------------------
# Domanda 3
# ----------------------------------------------

library(MASS)

data(bacteria)

# 3.1
Gini <- function(x) {
  freq <- as.numeric(table(x))
  freq_rel <- freq / sum(freq)
  1 - sum(freq_rel^2)
}

Gini(bacteria$trt) # 0.6507438

# 3.2
bacteria2 <- bacteria[bacteria$week == 6, ]
nrow(bacteria2) # 40

# 3.3
tab <- table(bacteria2$y, bacteria2$trt)
tab

#     placebo drug drug+
#  n       1    5     5
#  y      16    6     7

# 3.4
prop.table(tab, 2)

#       placebo       drug      drug+
#  n 0.05882353 0.45454545 0.41666667
#  y 0.94117647 0.54545455 0.58333333

# I suggeriscono che la somministrazione del farmaco ha un effetto sui pazienti.

# 3.5
levels(bacteria2$trt) <- c("placebo", "drug_and_drug+", "drug_and_drug+")
tab <- table(bacteria2$y, bacteria2$trt)
tab

#    placebo drug_and_drug+
#  n       1             10
#  y      16             13

prop.table(tab, 2)

#       placebo drug_and_drug+
#  n 0.05882353     0.43478261
#  y 0.94117647     0.56521739

# I risultati confermano che la sommonistrazione del farmaco ha un effetto sui pazienti.
