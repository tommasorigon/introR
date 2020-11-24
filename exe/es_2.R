# Esercizio A ----------------------

# A.1

var2 <- function(x) {
  mean((x - mean(x))^2)
}

# A.2

var3 <- function(x) {
  n <- length(x) # Ottengo la lunghezza del vettore x
  D <- matrix(0, nrow = n, ncol = n) # Creazione matrice vuota
  for (i in 1:n) {
    for (j in 1:n) {
      D[i, j] <- (x[i] - x[j])^2
    }
  }
  sum(D) / (2 * n^2)
}

# A.3 

x <- c(1, 4, 2, 2, 10)
c(var2(x), var3(x), var(x))

# A.4

x <- 1:3000
var2(x)
var3(x) # Molto più lenta rispetto a var2(x)

# A.5
var(x) # Viene diviso per n - 1 e non per n
