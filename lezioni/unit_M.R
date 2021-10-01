set.seed(100)

n <- 20 # Numerosità campionaria
sigma2 <- 16 # Varianza
mu <- 10  # Media (si suppone che sia ignota!)

x <- rnorm(n, mean = mu, sd = sqrt(sigma2))
x

mean(x)
median(x)

set.seed(156)
R <- 10^5
mu_hat <- replicate(R, mean(rnorm(n = n, mean = mu, sd = sqrt(sigma2))))

mean(mu_hat) # Media dello stimatore; valore teorico: 10
var(mu_hat) # Varianza dello stimatore; valore teorico: 16 / n = 0.8
mean(mu_hat - mu) # Distorsione dello stimatore; valore teorico: 0
mean((mu_hat - mu)^2) # Scarto quadratico medio dello stimatore; valore teorico: 16 / n = 0.8

# Rappresentazione grafica
hist(mu_hat, breaks = 100, freq = F)
curve(dnorm(x, mean = mu, sd = sqrt(sigma2 / n)), add = T)


set.seed(156)
R <- 10^5
median_hat <- replicate(R, median(rnorm(n = n, mean = mu, sd = sqrt(sigma2))))

mean(median_hat) # Media dello stimatore; valore teorico: ??
var(mu_hat) # Varianza dello stimatore; valore teorico: ??
mean(mu_hat - mu) # Distorsione dello stimatore; valore teorico: ??
mean((mu_hat - mu)^2) # Scarto quadratico medio dello stimatore; valore teorico: ??

# Rappresentazione grafica
par(mfrow=c(1,2))
hist(mu_hat, breaks = 100, freq = F)
hist(median_hat, breaks = 100, freq = F)

