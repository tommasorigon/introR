## -----------------------------------------------------------------------------------------
set.seed(100)
n <- 20 # Numerosità campionaria
mu <- 10 # Media teorica (solitamente ignota)

# Campione y_1,...,y_n
y <- rnorm(n, mean = mu, sd = sqrt(16))

# Vero valore è mu = 10
mean(y) 
median(y) 


## -----------------------------------------------------------------------------------------
set.seed(156)
R <- 10^5
# Ottengo R estrazioni della mediana campionaria Me_1,...Me_R
median_hat <- replicate(R, median(rnorm(n = n, mean = mu, sd = sqrt(16))))


## -----------------------------------------------------------------------------------------
mean((median_hat - mu)^2) # Stima dello scarto quadratico medio (MSE) della mediana


## -----------------------------------------------------------------------------------------
set.seed(156)
R <- 10^5

mu_hat <- replicate(R, mean(rnorm(n = n, mean = mu, sd = sqrt(16))))
median_hat <- replicate(R, median(rnorm(n = n, mean = mu, sd = sqrt(16))))

mean(mu_hat) - mu # Distorsione dello stimatore; valore teorico: 0
mean((mu_hat - mu)^2) # Scarto quadratico medio dello stimatore; valore teorico: 0.8

mean(median_hat) - mu # Distorsione dello stimatore; valore teorico: ??
mean((median_hat - mu)^2) # Scarto quadratico medio dello stimatore; valore teorico: ??


## -----------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
hist(mu_hat, breaks = 100, freq = F)
hist(median_hat, breaks = 100, freq = F)


## -----------------------------------------------------------------------------------------
set.seed(520)
R <- 10^5; n <- 20
mu <- 0; sigma2 <- 16

# Definisco le funzioni che calcolano gli stimatori
var1 <- function(x) mean(x^2) - mean(x)^2
var2 <- function(x) var(x) # Coincide con la definizione di R
var3 <- function(x) mean(x^2)
var4 <- function(x) (length(x) - 1) / (length(x) + 1) * var(x)

# Esecuzione della simulazione
S2_1 <- replicate(R, var1(rnorm(n = n, mean = mu, sd = sqrt(sigma2))))
S2_2 <- replicate(R, var2(rnorm(n = n, mean = mu, sd = sqrt(sigma2))))
S2_3 <- replicate(R, var3(rnorm(n = n, mean = mu, sd = sqrt(sigma2))))
S2_4 <- replicate(R, var4(rnorm(n = n, mean = mu, sd = sqrt(sigma2))))

# Distorsioni (approssimate)
round(mean(S2_1 - sigma2), 2)
round(mean(S2_2 - sigma2), 2)
round(mean(S2_3 - sigma2), 2)
round(mean(S2_4 - sigma2), 2)

# Errore quadratico medio (approssimato)
mean((S2_1 - sigma2)^2)
mean((S2_2 - sigma2)^2)
mean((S2_3 - sigma2)^2)
mean((S2_4 - sigma2)^2)


## -----------------------------------------------------------------------------------------
theta0 <- 40

# Numerosità campionarie
nn <- c(10, 100, 300, 500, 1000)

# Stime di massima verosimiglianza
set.seed(123)
theta_hat <- c(
  max(runif(nn[1], min = 0, max = theta0)),
  max(runif(nn[2], min = 0, max = theta0)),
  max(runif(nn[3], min = 0, max = theta0)),
  max(runif(nn[4], min = 0, max = theta0)),
  max(runif(nn[5], min = 0, max = theta0))
)
theta_hat


## -----------------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(nn, theta_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Massima verosimiglianza"
)

