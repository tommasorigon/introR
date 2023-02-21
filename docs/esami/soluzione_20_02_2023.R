# Problema 1 --------------------------------

# 1.1
set.seed(123)
R <- 10^6
x <- runif(R)
y <- runif(R)
mean(exp(sqrt(x * y))) # 1.601412

# Non richiesto. Verifico il risultato tramite integrazione numerica
library(cubature)
f <- function(z) exp(sqrt(z[1] * z[2]))
hcubature(f, lowerLimit = c(0, 0), upperLimit = c(1, 1))$integral # 1.60152

# 1.2
x <- runif(R, 0, 2)
# Attenzione al fatto che bisogna moltiplicare per 2 la seguente funzione, per tenere conto della costante di normalizzazione di una distribuzione uniforme in (0, 2)
g <- function(x) 2 * exp(sqrt(x)) 
est <- mean(g(x))
std_error <- sd(g(x)) / sqrt(R)
c(est, std_error) # 5.406536437 0.001640879

# Non richiesto. Verifico il risultato tramite integrazione numerica:
integrate(function(x) exp(sqrt(x)), lower = 0, upper = 2)
# 5.407529 with absolute error < 0.00053

# 1.3

# Le funzioni f(x) e g(x) devono essere valutate sullo stesso insieme di valori. La soluzione corretta è quindi:
my_integral <- function(f, g, R = 1000) {
  x <- rnorm(R)
  mean(f(x) / g(x))
}

# Problema 2 --------------------------------
library(MASS)
data(faithful)

# 2.1
hist(faithful$waiting)
plot(faithful$eruptions, faithful$waiting)

# 2.2
long_waiting <- faithful$waiting[faithful$eruptions > 3]
short_waiting <- faithful$waiting[faithful$eruptions <= 3]

# 2.3

# Quanto segue è la soluzione più elegante, che consente di confrontare le due funzioni di ripartizione
# Anche due grafici sono stati valutati positivamente (anche se di più difficile lettura)
plot(ecdf(long_waiting), xlim = c(40, 100)) # Il parametro xlim deve essere manualmente aggiustato, altrimenti il grafico risulta illeggibile
plot(ecdf(short_waiting), add = TRUE, col = "red")

# Anche calcolare mean e median separatamente era corretto
summary(long_waiting)
summary(short_waiting)

# 2.4
asym <- function(x) {
  Q <- quantile(x, probs = c(0.25, 0.5, 0.75))
  B <- (Q[1] + Q[3] - 2 * Q[2]) / (Q[3] - Q[1])
  B
}

asym(long_waiting)
asym(short_waiting)

# Problema 3

# 2.1
JB <- function(x) {
  n <- length(x)
  gamma <- mean((x - mean(x))^3) / sd(x)^3
  kappa <- mean((x - mean(x))^4) / sd(x)^4
  n / 6 * (gamma^2 + (kappa - 3)^2 / 4)
}

# Un possibile modo di ragionare era basato sulla funzione replicate (usata nel modo giusto!)
# Soluzioni corrette basate su "cicli for" sono state valutate positivamente.
qJB <- function(p, n, R = 1000) {
  sim <- replicate(R, JB(rnorm(n)))
  quantile(sim, probs = p)
}

# 2.2

# Queste stime sono molto imprecise anche per valori di R moderatamente elevati
set.seed(123)
qJB(p = 0.95, n = 50, R = 10000) # 4.288469 

# Quanto segue non è materiale d'esame, ma potrebbe esservi utile in futuro ------------

# In realtà, il "vero" test JB non utilizza sd bensì la sua versione in cui la somma degli scarti al quadrato viene divisa per n. La funzione JB pertanto si dovrebbe calcola come segue:
JB <- function(x) {
  n <- length(x)
  sigma <- sqrt(mean(x^2) - mean(x)^2)
  gamma <- mean((x - mean(x))^3) / sigma^3
  kappa <- mean((x - mean(x))^4) / sigma^4
  n / 6 * (gamma^2 + (kappa - 3)^2 / 4)
}

# Il test di Jarque-Bera è implementato in R dentro il pacchetto tseries:
library(tseries)

x <- rnorm(100)
JB(x)
jarque.bera.test(x)

# Se mai vi capitasse di usare questo test in econometria, ricordatevi di questo esercizio e confrontate il valore della funzione qJB...
qJB(p = 0.95, n = 50, R = 10000)

# ...con l'approssimazione asintotica chi-quadrato, che è pari a
qchisq(0.95, df = 2)
