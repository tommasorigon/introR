set.seed(100)
n <- 20 # Numerosità campionaria
mu <- 10 # Media teorica (solitamente ignota)

# Campione y_1,...,y_n
y <- rnorm(n, mean = mu, sd = sqrt(16))

mean(x)
median(x)

set.seed(156)
R <- 10^5

mu_hat <- replicate(R, mean(rnorm(n = n, mean = mu, sd = sqrt(16))))
median_hat <- replicate(R, median(rnorm(n = n, mean = mu, sd = sqrt(16))))

mean(mu_hat) - mu # Distorsione dello stimatore; valore teorico: 0
mean((mu_hat - mu)^2) # Scarto quadratico medio dello stimatore; valore teorico: 16 / n = 0.8

mean(median_hat) - mu # Distorsione dello stimatore; valore teorico: ??
mean((median_hat - mu)^2) # Scarto quadratico medio dello stimatore; valore teorico: ??

# Rappresentazione grafica
hist(mu_hat, breaks = 100, freq = F)
curve(dnorm(x, mean = mu, sd = sqrt(sigma2 / n)), add = T)

# Rappresentazione grafica
par(mfrow = c(1, 2))
hist(mu_hat, breaks = 100, freq = F)
hist(median_hat, breaks = 100, freq = F)
