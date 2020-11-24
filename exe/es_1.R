# Esercizio A ----------------------

x <- c(10, 20, NA, 20.5, pi, "banana", "mela", "pera", NA, NA, NA)

is.na(x)

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
