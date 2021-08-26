n <- 10 # Numero di tentativi
p <- 0.6 # Probabilità di successo
k <- 4 # Numero di successi

choose(n, k) * p^k * (1 - p)^(n - k)

dbinom(k, size = n, prob = p)


sum(dbinom(0:5, size = n, prob = p))
pbinom(5, size = n, prob = p)

1 - pbinom(5, size = n, prob = p)
pbinom(5, size = n, prob = p, lower.tail = FALSE)

n * p # Valore atteso ottenuto tramite calcoli analitici
sum(0:n * dbinom(0:n, size = n, prob = 0.6)) # Calcolo numerico

sum(sqrt(0:n) * dbinom(0:n, size = n, prob = 0.6)) # Calcolo numerico

qbinom(0.25, size = n, prob = p) # Primo quartile

pbinom(4, size = n, prob = p) # Il valore è minore di 0.25
pbinom(5, size = n, prob = p) # Il valore è maggiore di 0.25

x <- 1 # Punto in cui calcolare f(x)
mu <- 3 # Media
sigma2 <- 10 # Varianza

# Calcolo diretto
1 / sqrt(2 * pi * sigma2) * exp(-1 / (2 * sigma2) * (x - mu)^2)

# Nota: il comando dnorm chiede come input lo scarto quadratico medio, non la varianza
dnorm(x, mean = 3, sd = sqrt(sigma2))


integrate(dnorm, lower = -1, upper = 1)
pnorm(1) - pnorm(-1)

qnorm(0.75)

pnorm(qnorm(0.75))

curve(dnorm(x, mean = 0, sd = 1), from = -3, to = 3, ylab = "f(x)", main = "N(0,1)")

R <- 5
rnorm(R, mean = 0, sd = 1)

runif.wh <- function(n) {
  a <- c(171, 172, 170)
  b <- c(30269, 30307, 30323)
  s <- .current.seed
  u <- rep(0, n)
  for (i in 1:n) {
    s <- (a * s) %% b
    u[i] <- sum(s / b) %% 1
  }
  .current.seed <<- s 
  u
}

.current.seed <- c(123, 456, 789)
runif.wh(5)

.current.seed

rbinom.wh <- function(n, size, prob){
  u <- runif.wh(n)
  probs <- dbinom(0:size, size = size, prob = prob)
  breaks <- cumsum(c(0, probs))
  as.numeric(cut(u, breaks)) - 1 # Converte la variabile "factor" in numeri interi
}

.current.seed <- c(100, 200, 300)
rbinom.wh(20, size = 5, prob = 0.5)

rnorm.wh <- function(n, mean, sd){
  u <- runif.wh(n)
  qnorm(u, mean = mean, sd = sd)
}

.current.seed <- c(100, 200, 300)
rnorm.wh(10, mean = 0, sd = 1)


rbinom.wh2 <- function(n, size, prob){
  u <- runif.wh(n)
  qbinom(u, size = size, prob = prob)
}

.current.seed <- c(100, 200, 300)
rbinom.wh(n = 10, size = 5, prob = 0.5)
.current.seed <- c(100, 200, 300)
rbinom.wh2(n = 10, size = 5, prob = 0.5)

