## --------------------------------------------------------------------------------
forbes <- read.table("../dataset/forbes.csv", header = TRUE, sep = ",")


## --------------------------------------------------------------------------------
## path <- "https://tommasorigon.github.io/introR/data/forbes.csv"
## forbes <- read.table(path, header = TRUE, sep = ",")


## --------------------------------------------------------------------------------
str(forbes)


## --------------------------------------------------------------------------------
colnames(forbes) <- c("TempF", "Pressione") # Cambio i nomi alle variabili

forbes$TempC <- round((forbes$TempF - 32) * 5 / 9, 2) # Da Fahrenheit a Celsius
summary(forbes)


## --------------------------------------------------------------------------------
par(mfrow = c(1, 2)) # Divido la finestra grafica in 2 parti

# Opzione 1, per un totale di 6 classi equispaziate
hist(forbes$TempC) # Equivalente a: hist(forbes$TempC, breaks = "sturges") 

# Opzione 2, definisco manualmente 5 classi equispaziate
breaks <- c(90, 92.5, 95, 97.5, 100, 102.5)
hist(forbes$TempC, breaks = breaks)


## --------------------------------------------------------------------------------
# Prima parte della domanda
mean(forbes$TempC)
mean(forbes$Pressione)


## --------------------------------------------------------------------------------
# Seconda parte della domanda 
32 + 9 / 5 * mean(forbes$TempC) # Utilizzo proprietà della media
mean(forbes$TempF) # Non richiesto, calcola la media a partire dai dai dati trasformati


## --------------------------------------------------------------------------------
# Si, la funzione è definita in un'unica riga e non c'è nulla di male in questo
my_var <- function(x) mean(x^2) - mean(x)^2 

# Calcolo delle due varianze
my_var(forbes$TempC)
my_var(forbes$Pressione)


## --------------------------------------------------------------------------------
par(mfrow = c(1, 1)) # Vogliamo mostrare un grafico alla volta
plot(forbes$TempC, forbes$Pressione)
plot(forbes$TempC, forbes$Pressione, pch = 16, xlab = "Temperatura", ylab = "Pressione")


## --------------------------------------------------------------------------------
my_cov <- function(x, y) mean(x * y) - mean(x) * mean(y)
my_cov(forbes$TempC, forbes$Pressione) # = my_cov(forbes$Pressione, forbes$TempC)


## --------------------------------------------------------------------------------
cov(forbes$Pressione, forbes$TempC) # = 17 / 16 * my_cov(forbes$TempC, forbes$Pressione)


## --------------------------------------------------------------------------------
my_cov(forbes$TempC, forbes$Pressione) / sqrt(my_var(forbes$TempC) * my_var(forbes$Pressione))

cov(forbes$TempC, forbes$Pressione) / sqrt(var(forbes$TempC) * var(forbes$Pressione))


## --------------------------------------------------------------------------------
correlation <- cor(forbes$TempC, forbes$Pressione)
correlation


## --------------------------------------------------------------------------------
# Coefficiente angolare
beta_hat <- my_cov(forbes$TempC, forbes$Pressione) / my_var(forbes$TempC)
# Intercetta
alpha_hat <- mean(forbes$Pressione) - mean(forbes$TempC) * beta_hat

c(alpha_hat, beta_hat)


## --------------------------------------------------------------------------------
plot(forbes$TempC, forbes$Pressione, pch = 16, xlab = "Temperatura", ylab = "Pressione")
abline(a = alpha_hat, b = beta_hat)


## --------------------------------------------------------------------------------
x <- seq(from = 90, to = 100, length = 20)
alpha_hat + beta_hat * x


## --------------------------------------------------------------------------------
alpha_hat + beta_hat * 97


## --------------------------------------------------------------------------------
residuals <- forbes$Pressione - (alpha_hat + beta_hat * forbes$TempC)


## --------------------------------------------------------------------------------
correlation^2
1 - my_var(residuals) / my_var(forbes$Pressione)

