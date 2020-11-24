# Esercizio A ----------------------

x <- c(10, 20, NA, 20.5, pi, "banana", "mela", "pera", NA, NA, NA)

is.na(x) # TRUE/FALSE a seconda che il valore sia mancante o meno

? is.na

# Esercizio B ----------------------

f <- function(x) {
  abs(sin(pi / (4 * x^2))) / x
}

# B.1

f(1)
f(10^8)

# B.2

round(f(1), 2)
round(f(10), 2)

# B.3

curve(f(x), 1, 3)

# B.4

K <- 500
sum(f(1:K))

# B.5

? integrate
integrate(f, lower = 1, upper = 3)

# Esercizio C ----------------------

# C.1

n <- 10
sum((-1)^(0:n) * choose(n, 0:n))

n <- 100
sum((-1)^(0:n) * choose(n, 0:n)) # ERRORE NUMERICO!

n <- 1000
sum((-1)^(0:n) * choose(n, 0:n)) # ERRORE NUMERICO!

# C.2

n <- 10
sum((1:n) * choose(n, 1:n))
n * 2^(n - 1)

n <- 100
sum((1:n) * choose(n, 1:n))
n * 2^(n - 1)

n <- 1000
sum((1:n) * choose(n, 1:n))
n * 2^(n - 1)

# C.3

n <- 10
sum((-1)^(1:n) * (1:n) * choose(n, 1:n))

n <- 100
sum((-1)^(1:n) * (1:n) * choose(n, 1:n)) # ERRORE NUMERICO!

n <- 1000
sum((-1)^(1:n) * (1:n) * choose(n, 1:n)) # ERRORE NUMERICO!

# Esercizio D ----------------------

# La soluzione di un'equazione di secondo grado è ovviamente disponibile in forma chiusa

roots <- function(a, b, c) {
  Delta <- b^2 - 4 * a * c
  if (Delta < 0) stop("Il delta deve essere non negativo")
  (-b + c(-1, 1) * sqrt(Delta)) / (2 * a)
}

# D.1

roots(1, 5, 2)

# D.2

roots(1, 2, 1)

# D.3

roots(1, 1, 1) # Non esistono soluzioni reali

# Esercizio E -------------------------------

A <- rbind(
  c(26, 22, 17, 22, 23),
  c(22, 18, 14, 23, 27),
  c(17, 14, 14, 20, 24),
  c(22, 23, 20, 26, 23),
  c(23, 27, 24, 23, 12)
)

# E.1

det(solve(A)) # 0.0006493506
1/det(A) # Stesso valore

# E.2

det(A%*%A) # 2371600
det(A)^2 # Stesso valore

# E.3

sum(diag(A))

# E.4

det(A)
prod(eigen(A)$values)

# Esercizio F -----------------------