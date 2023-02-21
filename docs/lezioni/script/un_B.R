## -------------------------------------------------------------------------------------------------------------------------------
class(sum) # Identifica la tipologia di oggetto


## -------------------------------------------------------------------------------------------------------------------------------
class(log) # Secondo esempio


## -------------------------------------------------------------------------------------------------------------------------------
cube <- function(x) {
  out <- x^3
  out
}


## -------------------------------------------------------------------------------------------------------------------------------
cube(4) # Calcola il cubo del valore 4


## -------------------------------------------------------------------------------------------------------------------------------
cube <- function(x) {
  out <- x^3
  return(out) # Esplicita che il valore da dover restituire è out
}
cube(8) # Calcola il cubo del valore 8


## -------------------------------------------------------------------------------------------------------------------------------
power <- function(x, p = 2) {
  out <- x^p
  out
}


## -------------------------------------------------------------------------------------------------------------------------------
power(x = 4) # Calcola il quadrato del valore 4


## -------------------------------------------------------------------------------------------------------------------------------
power(4) # Sintassi alternativa: non è necessario specificare i nomi degli argomenti


## -------------------------------------------------------------------------------------------------------------------------------
power(x = 4, p = 3) # Calcola il cubo del valore 4
power(4, 3) # Sintassi alternativa: non è necessario specificare i nomi degli argomenti


## -------------------------------------------------------------------------------------------------------------------------------
condizione <- pi^2 < 10 # Valore booleano (in questo caso la condizione è TRUE)

if (condizione) {
  print("La condizione è vera")
  # Alcuni comandi da eseguire
  # ...
} else {
  print("La condizione è falsa")
  # Altri comandi da eseguire
  # ...
}


## -------------------------------------------------------------------------------------------------------------------------------
square_root <- function(x) {
  if (x < 0) {
    # Messaggio di avvertimento; in realtà sarebbe più appropriato usare il comando "warning"
    print("Il valore di x deve essere positivo")
    out <- NaN # Restituisco Not A Number
  } else {
    out <- sqrt(x)
  }
  out
}

square_root(-2) # La condizione x < 0 è verificata
square_root(36) # La condizione x < 0 NON è verificata


## -------------------------------------------------------------------------------------------------------------------------------
i <- 5 # Partiamo con i = 5

while (i <= 25) { # Ripete l'operazione fintanto che i non è minore o uguale di 25
  print(i) # Mostra a schermo il valore di i
  i <- i + 5 # Incrementa il valore di i

  # In contesti reali, qui ovviamente ci sono altre operazioni da eseguire
  # ...
}


## -------------------------------------------------------------------------------------------------------------------------------
## # NON ESEGUIRE IL SEGUENTE CODICE!
## #
## # La condizione i <= 25 è sempre vera perché i non viene aggiornato
## i <- 5
## while (i <= 25) {
##   print(i)
## 
##   # Altre operazioni da eseguire
##   # ...
## }


## -------------------------------------------------------------------------------------------------------------------------------
values <- seq(from = 5, to = 25, by = 5)
values

for (i in values) {
  print(i + 2) # Mostra a schermo il valore di i + 2

  # Altre operazioni da eseguire
  # ...
}


## -------------------------------------------------------------------------------------------------------------------------------
distances <- function(x) {
  n <- length(x) # Ottengo la lunghezza del vettore x
  D <- matrix(0, nrow = n, ncol = n) # Creazione matrice vuota

  for (i in 1:n) {
    for (j in 1:n) {
      D[i, j] <- (x[i] - x[j])^2
    }
  }
  D # Valore da restituire
}

x <- c(5, 2, 1, 24) # Esempio per verificare che sia corretto
distances(x)


## -------------------------------------------------------------------------------------------------------------------------------
## for (i in 1:100) {
##   condA <- (i %% 3) == 0 # Il numero è un multiplo di 3?
##   condB <- (i %% 5) == 0 # Il numero è un multiplo di 5?
## 
##   if (condA & condB) {
##     print("fizzbuzz")
##   } else if (condA) {
##     print("fizz")
##   } else if (condB) {
##     print("buzz")
##   } else {
##     print(i)
##   }
## }


## -------------------------------------------------------------------------------------------------------------------------------
library(MASS) # Carica in memoria il pacchetto MASS
library(knitr) # Carica in memoria il pacchetto knitr


## -------------------------------------------------------------------------------------------------------------------------------
## install.packages("knitr") # Installa il pacchetto knitr

