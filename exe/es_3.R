# Esercizio A ----------------------

media <- 500 # Media
scarto <- sqrt(625) # Deviazione standard

# A.1

pnorm(540, media, scarto)

# A.2

pnorm(510, media, scarto, lower.tail = F)

# A.3

pnorm(520, media, scarto) - pnorm(470, media, scarto)

# Esercizio B --------------------------------

# B.1

erf_approx <- function(x, N) {
  indexes <- 0:N
  values <- (-1)^indexes * x^(2 * indexes + 1) / (factorial(indexes) * (2 * indexes + 1))
  2 / sqrt(pi) * sum(values)
}

pnorm_approx <- function(x, N) {
  0.5 * (1 + erf_approx(x / sqrt(2), N))
}

# B.2 - B.3

# Nota: rbind è usato solamente per migliorare la resa estetica

rbind(
  c(erf_approx(x = 0, N = 10), pnorm_approx(x = 0, N = 10), pnorm(0)),
  c(erf_approx(x = 1, N = 10), pnorm_approx(x = 1, N = 10), pnorm(1)),
  c(erf_approx(x = 2, N = 10), pnorm_approx(x = 2, N = 10), pnorm(2))
)

c(pnorm_approx(x = -3, N = 10), pnorm(-3))

# Commento: l'approssimazione sembra funzioare molto bene per x = 0 e x = 1, mentre sembra avere dei problemi per valori maggiori (ad es. x = 2). Nel punto x = -3 si ottengono addirittura probabilità negative.

# B.4

set.seed(100)
mean(rnorm(10^5) < -3)

# Esercizio C ----------------------------------------

# C.1

set.seed(100)
n <- 10^5 # Numero di simulazioni da effettuare
x <- sample(1:20, size = n, replace = TRUE) # Campiono n volte il dado da 20.
mean(x + 5 >= 17) # Stimo la probabilità dell'evento

# C.2

set.seed(100)
# La funzione pmax seleziona il massimo di ciascuna coppia
z <- pmax(sample(1:20, size = n, replace = TRUE), sample(1:20, size = n, replace = TRUE))

freq_rel <- table(z) / n
plot(freq_rel) # Rappresentazione grafica

# C.3

mean(z + 5 >= 17) # La probabilità è più alta rispetto a quella ottenuta al punto C.1

# Esercizio D -----------------------------------------

# D.1

curve(dnorm(x), -4, 4, col = "red") # Curva della densità gaussiana, in rosso
curve(dcauchy(x), -4, 4, add = TRUE) # Curva della densità di Cauchy

# Si nota fin da subito che la densità di Cauchy ha le code più "pesanti".

# D.2

set.seed(100)
mean(rcauchy(10^5) > 3) # Valore tramite simulazione
1 - pcauchy(3) # Valore teorico
# I due valori sono molto simili tra loro

# D.3

set.seed(100) # Esperimento 1
mean(rcauchy(10^2))
mean(rcauchy(10^3))
mean(rcauchy(10^4))
mean(rcauchy(10^5))

set.seed(150) # Esperimento 2
mean(rcauchy(10^2))
mean(rcauchy(10^3))
mean(rcauchy(10^4))
mean(rcauchy(10^5))

# In entrambi i casi, le medie campionarie sembrano non convergere. Questo è dovuto al fatto che il valore atteso della distribuzione di Cauchy NON ESISTE. Pertanto, le ipotesi della legge dei grandi numeri non sono verificate ed il comportamento della media aritmetica è erratico.

# Esercizio E -----------------------------------

# E.1

Q1 <- qnorm(0.25, 5, 5)
Q2 <- qnorm(0.5, 5, 5)
Q3 <- qnorm(0.75, 5, 5)

(Q3 - 2 * Q2 + Q1) / (Q3 - Q1) # Circa pari a zero

# E.2

Q1 <- qgamma(0.25, 2, 4)
Q2 <- qgamma(0.5, 2, 4)
Q3 <- qgamma(0.75, 2, 4)

(Q3 - 2 * Q2 + Q1) / (Q3 - Q1) # Asimmetria positiva

# Non richiesto, ma si poteva anche fare il grafico per averne conferma:
curve(dgamma(x, 2, 4), 0, 3)

# E.3

Q1 <- qgamma(0.25, 200, 400)
Q2 <- qgamma(0.5, 200, 400)
Q3 <- qgamma(0.75, 200, 400)

(Q3 - 2 * Q2 + Q1) / (Q3 - Q1) # La distribuzione è sostanzialmente simmetrica

# Non richiesto, ma si poteva anche fare il grafico per averne conferma:
curve(dgamma(x, 200, 400), 0.3, 0.7)
# La distribuzione tende ad assomigliare sempre più ad una gaussiana, poichè la Gamma di parametro 200 è la somma di 200 esponenziali indipendenti. Pertanto, vale il teorema del limite centrale.

# Esercizio F -----------------------------------------------

# F.1

# Implementazione "diretta", numericamente un po' instabile
dbetabinom <- function(k, n, alpha, beta) {
  choose(n, k) * beta(k + alpha, n - k + beta) / beta(alpha, beta)
}

# Implementazione numericamente stabile sfruttando i logaritmi
dbetabinom <- function(k, n, alpha, beta) {
  exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
}

pbetabinom <- function(k, n, alpha, beta) {
  sum(dbetabinom(0:k, n = n, alpha = alpha, beta = beta))
}

# F.2

n <- 40
alpha <- beta <- 2

sum(dbetabinom(0:n, n, alpha, beta))
pbetabinom(n, n, alpha, beta)

plot(0:n, dbetabinom(0:n, n, alpha, beta), type = "h")

# F.3

media <- sum(0:n * dbetabinom(0:n, n, alpha, beta))
varianza <- sum((0:n)^2 * dbetabinom(0:n, n, alpha, beta)) - media^2

media
varianza

# Esercizio G -----------------------------------------------

mean(exp(-abs(rnorm(10^5))^3))

# Esercizio H -----------------------------------------------

# H.1

x <- rnorm(10^5)
g <- function(x) x^2

k <- 1
mean(g(x) >= k) <= mean(g(x)) / k
k <- 2
mean(g(x) >= k) <= mean(g(x)) / k
k <- 3
mean(g(x) >= k) <= mean(g(x)) / k

# H.2

x <- rgamma(10^5, 10, 0.1)
g <- function(x) exp(x / 100)
hist(g(x))

k <- 1
mean(g(x) >= k) <= mean(g(x)) / k
k <- 2
mean(g(x) >= k) <= mean(g(x)) / k
k <- 3
mean(g(x) >= k) <= mean(g(x)) / k

# Esercizio I ---------------------------------

# I.1

R <- 10^4 # Numero di simulazioni montecarlo
set.seed(1500)

# Soluzione usando cicli for (inefficiente)
y <- numeric(R)
n <- 50
for (r in 1:R) {
  y[r] <- sum(duplicated(sample(365, size = n, replace = TRUE))) > 0 # Ci sono dei valori uguali?
}
mean(y)

# I.2

y <- numeric(R)
n <- 25 # Cambia la numerosità campionaria
for (r in 1:R) {
  y[r] <- sum(duplicated(sample(365, size = n, replace = TRUE))) > 0
}
mean(y)

# I.3

# Soluzione usando replicate: più efficiente e concisa
n <- 50
y <- replicate(R, sum(duplicated(sample(365, size = n, replace = TRUE))) > 0)
mean(y)

n <- 25
y <- replicate(R, sum(duplicated(sample(365, size = n, replace = TRUE))) > 0)
mean(y)

# Esercizio L ---------------------------------

# L.1
dsn <- function(x, alpha) {
  2 * dnorm(x) * pnorm(x * alpha)
}

# L.2
par(mfrow = c(1, 3))

curve(dsn(x, -5), -4, 4)
curve(dsn(x, 0), -4, 4)
curve(dsn(x, 5), -4, 4)

# Il parametro alpha sembra regolare il grado di asimmetria della distribuzione. Si noti che per alpha = 0 si ottiene la gaussiana standard come caso particolare.

# L.3

rsn <- function(n, alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  sqrt(1 - delta^2) * rnorm(n) + delta * abs(rnorm(n))
}

# L.4

par(mfrow = c(1, 1))
x <- rsn(10^5, 7)
hist(x, freq = FALSE, breaks = 100)
curve(dsn(x, 7), add = TRUE)

# Esercizio M ---------------------------------------

R <- 10^4 # Numero di simulazioni Monte Carlo
Xbar <- replicate(R, mean(runif(30)))

hist(Xbar) # Istogramma
qqnorm(Xbar) # QQplot
qqline(Xbar)

# L'approssimazione Gaussiana sembra essere buona