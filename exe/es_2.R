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
c(var2(x), var3(x))

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

str(elez) # Due variabili di tipo "factor" chiamate "Lista" e "Municipio".

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

str(province) # Sono presenti tre variabili numeriche: "fertilità", "agricoltura" e "istruzione". 

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

summary(calcio) # Sono presenti 3 dati mancanti
calcio <- na.omit(calcio) # I dati mancanti vengono esclusi e il dataset sovrascritto

# D.4

summary(calcio$B365H)

par(mfrow = (c(1, 2)))
hist(calcio$B365H, breaks = 20)
boxplot(calcio$B365H) # Dal grafico è possibile capire che sono presenti vari outliers

# D.5

summary(log(calcio$B365H))
par(mfrow = (c(1, 2)))
hist(log(calcio$B365H), breaks = 20)
boxplot(log(calcio$B365H)) # Sono ancora presenti valori anomali, ma il fenomeno si è ridotto

# D.6

cor(calcio$B365H, calcio$B365A) # Il commento è lasciato allo studente

# D.7

par(mfrow=c(1,1))
plot(log(calcio$B365H), log(calcio$B365A)) # Il commento è lasciato allo studente

# D.8

tapply(calcio$B365H, calcio$FTR, mean) # Il commento è lasciato allo studente

# D.9

plot(calcio$B365H ~ calcio$FTR) # Il commento è lasciato allo studente

# Esercizio E ------------------------------------

# E.1

# Supponiamo che di aver scaricato il file nel proprio computer e che il "path" sia quello corretto, quindi:
load("../data/imdb.RData")

# E.2

nrow(imdb)

# E.3

mean(imdb$duration)
mean(exp(imdb$lgross))

# E.4

tapply(imdb$duration, imdb$Drama, mean)
tapply(imdb$duration, imdb$Drama, median)
plot(imdb$duration ~ imdb$Drama) # Boxplot

# E.5

imdb$movie_title[order(imdb$lgross, decreasing = TRUE)][1:5]

# E.6

par(mfrow = c(1, 3))
hist(imdb$lbudget)
hist(imdb$lgross)
plot(imdb$lbudget, imdb$lgross)
par(mfrow = c(1, 1))

# E.7

cor(imdb$lbudget, imdb$lgross) # Il commento è lasciato allo studente

# E.8

cor(imdb$duration, imdb$lgross) # Il commento è lasciato allo studente

