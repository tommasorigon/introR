# Esercizio A ----------------------

# A.1

var2 <- function(x) {
  mean((x - mean(x))^2)
}

# A.2

var3 <- function(x) {
  n <- length(x) # Ottengo la lunghezza del vettore x
  D <- matrix(0, nrow = n, ncol = n) # Creazione matrice vuota
  for (i in 1:n) {
    for (j in 1:n) {
      D[i, j] <- (x[i] - x[j])^2
    }
  }
  sum(D) / (2 * n^2)
}

# A.3 

x <- c(1, 4, 2, 2, 10)
c(var2(x), var3(x), var(x))

# A.4

x <- 1:3000
var2(x)
var3(x) # Molto più lenta rispetto a var2(x)

# A.5
var(x) # Viene diviso per n - 1 e non per n

# Esercizio B ----------------------------------------------------

# B.1

# Supponiamo che di aver scaricato il file nel proprio computer e che il "path" sia quello corretto, quindi:
load("../data/elezioni.RData")

# B.2

str(elez)

# B.3

table(elez$Lista, elez$Municipio)

# B.4

# Numero di voti
table(elez$Lista)

# Barplot (i nomi potrebbero non comparire a seconda delle dimensioni della finestra del grafico)
barplot((table(elez$Lista)))

# Percentuale di voti (approssimata)
round(100*table(elez$Lista) / nrow(elez),2)

# B.5

Gini <- function(x) {
  freq <- as.numeric(table(x))
  freq_rel <- freq / sum(freq)
  1 - sum(freq_rel^2)
}

# B.6

Gini_norm <- function(x) {
  freq <- as.numeric(table(x))
  freq_rel <- freq / sum(freq)
  k <- length(freq)
  k / (k - 1) * (1 - sum(freq_rel^2))
}

# B.7

tapply(elez$Lista, elez$Municipio, Gini_norm) # Il Municipio 1 sembra essere quello più polarizzato.

# Esercizio C --------------------------------------

# C.1

# Assumendo di aver scaricato il file nel proprio computer e che il "path" sia quello corretto,
province <- read.table("../data/province.csv", header = TRUE)

# C.2

str(province)

# C.3

hist(province$istruzione)
summary(province$istruzione)
var(province$istruzione) # Tralasciando la questione del denominatore n vs n-1

# C.4

plot(ecdf(province$istruzione))

# C.5

plot(province$fertilità, province$agricoltura) # Si, è presente una lieve associazione positiva

# C.6

cov(province) # Tralasciando la questione del denominatore n vs n-1
# Tra agricoltura ed istruzione è presente una relazione negativa abbastanza accentuata

# C.7

cor(province)

# Esercizio D --------------------------------------

# D.1

# Supponiamo che di aver scaricato il file nel proprio computer e che il "path" sia quello corretto, quindi:
load("../data/calcio.RData")

# D.2

str(calcio)

# D.3

summary(calcio)
calcio <- na.omit(calcio)

# D.4

summary(calcio$B365H)

par(mfrow = (c(1, 2)))
hist(calcio$B365H, breaks = 20)
boxplot(calcio$B365H)