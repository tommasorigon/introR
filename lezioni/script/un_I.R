R <- 5000 # Numero di repliche

set.seed(123)
X <- rnorm(R, 0, 1) # Ottengo R copie da una distribuzione gaussiana
Y <- cos(X) # Ottengo R copie dalla distribuzione di Y
Z <- Y > 0 # Vettore logico che verifica se Y > 0 o meno
Z[1:10]

prop.table(table(Z)) # Considero la frequenza relativa
mean(Z) # Oppure, più semplicemente

set.seed(100) # Imposto un seed diverso da prima
Z <- cos(rnorm(R, 0, 1)) > 0 # Calcolo gli indicatori (codice in forma compatta)
mean(Z)

MonteCarlo <- function(R) {
  X <- rnorm(R)
  Z <- (X > 1) & (X < 2)
  estimate <- mean(Z)
  std.error <- sqrt(estimate * (1 - estimate) / R)
  out <- c(estimate, std.error)
  names(out) <- c("estimate", "std.error")
  out
}
# Vero valore
pnorm(2) - pnorm(1)

MonteCarlo(100) # R = 100 conduce a std.error elevato
MonteCarlo(5000) # R = 5000 conduce a std.error ragionevole
MonteCarlo(10^6) # R = 10^6 conduce a std.error basso

U <- runif(10^6)
I_hat <- mean((cos(50 * U) + sin(20 * U))^2)
I_hat

MonteCarlo <- function(R) {
  U <- runif(R)
  hU <- (cos(50 * U) + sin(20 * U))^2
  estimate <- mean(hU)
  std.error <- sd(hU) / sqrt(R)
  out <- c(estimate, std.error)
  names(out) <- c("estimate", "std.error") # Aggiungo solo per ragioni estetiche
  out
}

MonteCarlo(100) # R = 100 conduce a uno std.error elevato
MonteCarlo(5000) # R = 5000 conduce a uno std.error ragionevole
MonteCarlo(10^6) # R = 10^6 conduce a uno std.error basso

X <- rnorm(10^5)
hX <- sin(X)^2
mean(hX) # Estimate
sd(hX) / sqrt(10^5) # Std.error

# Integrazione numerica
integrate(function(x) sin(x)^2 * dnorm(x), -Inf, Inf)

mean(rgamma(10^5, 3, 3) * rgamma(10^5, 3, 3))

X <- rgamma(10^5, 3, 3)
mean(X * X)

X <- rnorm(10^5)
hist(X, freq = FALSE, breaks = 100)
curve(dnorm(x), add = TRUE) # add = TRUE Aggiunge la curva al grafico precedente

X <- rpois(10^5, 10) # Simulazione di R variabili Poisson con media 10
freq_rel <- table(X) / n # Calcolo delle frequenze relative

par(mfrow = c(1, 2)) # Grafici
plot(freq_rel, ylab = "P(X = k)", xlab = "k", main = "Distribuzione empirica")
plot(0:28, dpois(0:28, 10),
  type = "h", ylab = "P(X = k)",
  xlab = "k", main = "Distribuzione teorica"
)
