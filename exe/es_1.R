# Esercizio A ----------------------

f <- function(x) {
  abs(sin(pi / (4 * x^2))) / x
}

# A.1

f(1)
f(10^8)

# A.2

round(f(1), 2)
round(f(10), 2)

# A.3

curve(f(x), 1, 3)

# A.4

K <- 500
sum(f(1:K))

# A.5

? integrate
integrate(f, lower = 1, upper = 3)

# Esercizio B ----------------------

# B.1

n <- 10
sum((-1)^(0:n) * choose(n, 0:n))

n <- 100
sum((-1)^(0:n) * choose(n, 0:n)) # ERRORE NUMERICO!

n <- 1000
sum((-1)^(0:n) * choose(n, 0:n)) # ERRORE NUMERICO!

# B.2

n <- 10
sum((1:n) * choose(n, 1:n))
n * 2^(n - 1)

n <- 100
sum((1:n) * choose(n, 1:n))
n * 2^(n - 1)

n <- 1000
sum((1:n) * choose(n, 1:n))
n * 2^(n - 1)

# B.3

n <- 10
sum((-1)^(1:n) * (1:n) * choose(n, 1:n))

n <- 100
sum((-1)^(1:n) * (1:n) * choose(n, 1:n)) # ERRORE NUMERICO!

n <- 1000
sum((-1)^(1:n) * (1:n) * choose(n, 1:n)) # ERRORE NUMERICO!

# Esercizio C ----------------------

# La soluzione di un'equazione di secondo grado è ovviamente disponibile in forma chiusa

roots <- function(a, b, c) {
  Delta <- b^2 - 4 * a * c
  if (Delta < 0) stop("Il delta deve essere non negativo")
  (-b + c(-1, 1) * sqrt(Delta)) / (2 * a)
}

# C.1

roots(1, 5, 2)

# C.2

roots(1, 2, 1)

# C.3

roots(1, 1, 1) # Non esistono soluzioni reali

# Esercizio D -------------------------------

A <- rbind(
  c(26, 22, 17, 22, 23),
  c(22, 18, 14, 23, 27),
  c(17, 14, 14, 20, 24),
  c(22, 23, 20, 26, 23),
  c(23, 27, 24, 23, 12)
)

# D.1

det(solve(A)) # 0.0006493506
1 / det(A) # Stesso valore

# D.2

det(A %*% A) # 2371600
det(A)^2 # Stesso valore

# D.3

sum(diag(A)) # 96

# D.4

det(A) # 1540
prod(eigen(A)$values) # Stesso valore

# Esercizio E -----------------------

# E.1

dist_euclid <- function(x, y) {
  sqrt(crossprod(x - y))
}

# E.2

x <- c(1, 4, 2, 2, 10)
y <- c(8, 1, 8, 3, 6)
dist_euclid(x, y) # 10.53565

# Esercizio F ------------------------

# F.1

frobenius_norm <- function(A) {
  sqrt(sum(A^2))
}

A <- cbind(1:5, 6:10, 11:15)
frobenius_norm(A) # 35.21363



# Esercizio G ------------------------------

data(mtcars)

# G.1

dim(mtcars) # 32 righe, 11 colonne

# G.2

mtcars[1:20] # Bisogna aggiungere una virgola
mtcars[1:20, ] # Comando corretto

# G.3

# mtcars[mtcars$cyl = 4, ] # COMANDO ERRATO
mtcars[mtcars$cyl == 4, ] # COMANDO CORRETTO

# mtcars[-1:4, ] # COMANDO ERRATO
mtcars[-(1:4), ] # COMANDO CORRETTO (elimina le prima 4 righe)

# mtcars[mtcars$cyl <= 5] # COMANDO ERRATO
mtcars[mtcars$cyl <= 5, ] # COMANDO CORRETTO

# mtcars[mtcars$cyl == 4 | 6, ] # COMANDO ERRATO
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ] # COMANDO CORRETTO


# Esercizio H -------------------------------

diag2 <- function(A) {
  n <- nrow(A)
  diagonal <- numeric(n)
  for (i in 1:n) {
    diagonal[i] <- A[i, i]
  }
  diagonal
}

A <- rbind(
  c(26, 22, 17, 22, 23),
  c(22, 18, 14, 23, 27),
  c(17, 14, 14, 20, 24),
  c(22, 23, 20, 26, 23),
  c(23, 27, 24, 23, 12)
)

diag(A)
diag2(A)

# Esercizio I ----------------------------------

# La consegna chiedeva di indovinare il risultato senza eseguire il codice.
# Se eseguito, si ottiene 202.
