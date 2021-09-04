R <- 5000 # Numero di repliche

set.seed(123)
X <- rnorm(R, 0, 1) # Ottengo R copie da una gaussiana
Y <- cos(X) # Ottengo R copie dalla distribuzione di Y
Z <- Y > 0 # Vettore logico
Z[1:10]

prop.table(table(Z))
mean(Z)

set.seed(100) # Imposto un seed diverso da prima
Z <- cos(rnorm(R, 0, 1)) > 0 
mean(Z)

MonteCarlo <- function(R){
  Z <- cos(rnorm(R, 0, 1)) > 0 
  estimate <- mean(Z)
  std.error <- sqrt(estimate * (1 - estimate) / R)
  out <- c(estimate, std.error)
  names(out) <- c("estimate", "std.error")
  out
}

set.seed(123)
MonteCarlo(100) # R = 100 conduce a std.error elevato
MonteCarlo(5000) # R = 5000 conduce a std.error ragionevole
MonteCarlo(10^6) # R = 10^6 conduce a std.error basso



MonteCarlo <- function(R){
  X <- rnorm(R)
  Z <- (X > 1) & (X < 2)
  estimate <- mean(Z)
  std.error <- sqrt(estimate * (1 - estimate) / R)
  out <- c(estimate, std.error)
  names(out) <- c("estimate", "std.error")
  out
}

set.seed(123)
MonteCarlo(100) # R = 100 conduce a std.error elevato
MonteCarlo(5000) # R = 5000 conduce a std.error ragionevole
MonteCarlo(10^6) # R = 10^6 conduce a std.error basso

pnorm(2) - pnorm(1)




U <- runif(10^6)
mean((cos(50 * U) + sin(20 * U))^2)

MonteCarlo <- function(R){
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
mean(hX)
sd(hX) / sqrt(10^5)

X <- rnorm(10^5)
hist(X, freq=FALSE, breaks = 100)
curve(dnorm(x), add=TRUE)


R <- 10^5
set.seed(100)
X <- rpois(R, 10) # Simulazione di n variabili Poisson
freq_rel <- table(X) / n # Calcolo delle frequenze relative

par(mfrow = c(1, 2))
plot(freq_rel, ylab = "P(X = k)", xlab = "k", main = "Distribuzione empirica")
plot(0:28, dpois(0:28, 10), type = "h", ylab = "P(X = k)", 
     xlab = "k", main = "Distribuzione teorica")