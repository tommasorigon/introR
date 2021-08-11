# Per $n = `r nrow(forbes)`$ luoghi nelle Alpi viene misurata la pressione atmosferica (inHg) e la temperatura di ebollizione dell'acqua (in gradi Celsius). I dati provengono da un esperimento condotto da Forbes nel 1857. I dati sono riportati nella seguente tabella.
#
# ```{r, results="asis", echo=F}
# kable(forbes)
# ```
#
# 1. Si disegni un istogramma della variabile `temperatura`, scegliendo un numero appropriato di classi equispaziate e giustificandone la scelta.
#
# 1. Si ottengano la media aritmetica di entrambe le variabili `temperatura` e `pressione`. La relazione tra gradi Celsius e gradi Fahrenheit è la seguente:
# $$
# (\text{Fahrenheit}) = 32 + \frac{9}{5}(\text{Celsius}).
# $$
# Quanto vale la temperatura di ebollizione media espressa in gradi Fahrenheit? Si risponda senza ricalcolare tutti i valori della variabile `temperatura`.
#
# 1. Si ottenga la varianza delle variabili `temperatura` e `pressione`.
#
# 1. Si disegni un opportuno grafico che aiuti a comprendere la relazione tra le due variabili. Si calcoli quindi la correlazione: il valore ottenuto è coerente col grafico?
#
# 1. Si ottenga la retta ai minimi quadrati per la relazione tra `temperatura` (variabile esplicativa $x$) e `pressione` (variabile risposta $y$), e la si disegni nel grafico ottenuto in precedenza. Si fornisca quindi un'interpretazione per i coefficienti.
#
# 1. In base al modello stimato, se la temperatura di ebollizione dell'acqua è pari 97 gradi Celsius, a quanto è pari la pressione?
#
# 1. Si ottenga un indice di bontà di adattamento ai dati della curva ottenuta e lo si interpreti nel contesto del problema.

rm(list = ls())
forbes <- read.table("https://tommasorigon.github.io/introR/data/forbes.csv",
  header = TRUE, sep = ","
)
str(forbes)

colnames(forbes) <- c("TempF", "Pressione")

# Creazione variabile in celsius
forbes$TempC <- round((forbes$TempF - 32) * 5 / 9, 2) # Da Farenheit a Celsius

summary(forbes)

par(mfrow = c(1, 2))
hist(forbes$TempC, breaks = "sturges")

breaks <- c(90, 92.5, 95, 97.5, 100, 102.5)
hist(forbes$TempC, breaks = breaks)

mean(forbes$TempC)
mean(forbes$Pressione)

32 + 9 / 5 * mean(forbes$TempC)
mean(forbes$TempF)


my_var <- function(x) mean(x^2) - mean(x)^2

my_var(forbes$TempC)
my_var(forbes$Pressione)

par(mfrow = c(1, 1))
plot(forbes$TempC, forbes$Pressione)
plot(forbes$TempC, forbes$Pressione, pch = 16, xlab = "Temperatura", ylab = "Pressione")

my_cov <- function(x, y) mean(x * y) - mean(x) * mean(y)
my_cov(forbes$TempC, forbes$Pressione)
my_cov(forbes$Pressione, forbes$TempC)
cov(forbes$Pressione, forbes$TempC)


my_cov(forbes$TempC, forbes$Pressione) / sqrt(my_var(forbes$TempC) * my_var(forbes$Pressione))
cov(forbes$TempC, forbes$Pressione) / sqrt(var(forbes$TempC) * var(forbes$Pressione))

correlation <- cor(forbes$TempC, forbes$Pressione)
correlation

beta_hat <- my_cov(forbes$TempC, forbes$Pressione) / my_var(forbes$TempC)
alpha_hat <- mean(forbes$Pressione) - mean(forbes$TempC) * beta_hat

c(alpha_hat, beta_hat)

plot(forbes$TempC, forbes$Pressione, pch = 16, xlab = "Temperatura", ylab = "Pressione")
abline(a = alpha_hat, b = beta_hat)

x <- seq(from = 90, to = 100, length = 20)
alpha_hat + beta_hat * x
alpha_hat + beta_hat * 97

residuals <- forbes$Pressione - (alpha_hat + beta_hat * forbes$TempC)
residuals

mean(residuals)
correlation^2
1 - var(residuals) / var(forbes$Pressione)

lPres <- log(forbes$Pressione)
Temp <- forbes$TempC

lambda_hat <- cov(lPres, Temp) / var(Temp)
gamma_hat <- exp(mean(lPres) - mean(Temp) * lambda_hat)

c(gamma_hat, lambda_hat)

gamma_hat * exp(lambda_hat * Temp)

plot(forbes$TempC, forbes$Pressione, pch = 16, xlab = "Temperatura", ylab = "Pressione")
abline(a = alpha_hat, b = beta_hat)
curve(gamma_hat * exp(lambda_hat * x), add = TRUE, col = "red")
