2 + 2
4 * (3 + 5) # La somma entro parentesi viene eseguita per prima
pi / 4 # Pi greco quarti

2^5 # Sintassi alternativa: 2**5

sqrt(2)
sin(pi / 4)

# Assegna il valore 5 all'oggetto x
x <- sqrt(5) # Sintassi alternativa (sconsigliata): x = sqrt(5)

y <- x + pi # ovvero pi greco + radice quadrata di 5
y
# [1] 5.377661

rm(x) # x non è più presente nel "workspace"

ls() # Nel workspace è presente l'oggetto y

rm(list = ls())

x <- 1/2 # Esempio di numero reale

exp(x) # Esponenziale e logaritmo naturale
log(x)

abs(x) # Valore assoluto
sign(x) # Funzione segno

sin(x) # Funzioni trigonometriche (seno, coseno, tangente)
cos(x)
tan(x)

asin(x) # Funzioni trigonometriche inverse
acos(x)
atan(x)

x <- 1 / 2; y <- 1 / 3 # Numeri reali 
n <- 5; k <- 2 # Numeri naturali

factorial(n) # n!
choose(n, k) # Coefficiente binomiale

round(x, digits = 2) # Arrotonda x usando 2 cifre decimali
floor(x) # Arrotonda x all'intero più vicino, per difetto
ceiling(x) # Arrotonda x all'intero più vicino, per eccesso

curve(sin(x) / x, from = 0, to = 15)

? log # Documentazione della funzione log

10^15
10^(-15)

10^1000 # Numero molto grande, anche se finito

log(-1) # Questo comando genera inoltre un avviso

sin(pi)

cos(pi)

x <- 5
x < 0 # Il valore di x è minore di 0?

a <- (x == -3) # Il valore di x è uguale a -3?
a

x >= y # x è maggiore o uguale a y? (Si usa "<=" per minore uguale)
x != y # x è diverso da y?
b <- TRUE
a & b # a AND b. I valori booleani a e b sono entrambi veri?
a | b # a OR b. Almeno uno tra a ed b è vero?

x <- c(4, 2, 2, 8, 10) 
x

x <- c("A", "B", 2, 8, 10)
x

x <- 5:10 # Equivalente a: x <- c(5, 6, 7, 8, 9, 10)
x

x <- seq(from = 0, to = 1, by = 0.1)
x

x <- rep(10, 7) # Vettore in cui il numero 10 è ripetuto 7 volte
x

exp(1:6) + (1:6) / 2 + 1 # Esempio 1

x <- c(10, 10^2, 10^3, 10^4, 10^5, 10^6) # Esempio 2
log(x, base = 10)

1:8 > 4 # Esempio 3

x <- c(2, 3, 1, 3, 10, 5)
length(x) # Lunghezza del vettore
sum(x) # Somma degli elementi del vettore
cumsum(x) # Somme cumulate

x <- c(2, 3, 1, 3, 10, 5)
prod(x) # Prodotto degli elementi del vettore
cumprod(x) # Prodotti cumulati
sort(x, decreasing = FALSE) # Vettore ordinato in ordine crescente
min(x) # Valore minimo
which.min(x) # Posizione del valore corrispondente al minimo

max(x) # Valore massimo
which.max(x) # Posizione del valore corrispondente al massimo
range(x) # Equivalente a: c(min(x), max(x))

# Concatenazione di vettori
x <- c(rep(pi, 2), sqrt(2), c(10, 7))

x[3] # Estrae il terzo elemento dal vettore x, ovvero sqrt(2)
x[c(1, 3, 5)] # Estrae il primo, il terzo ed il quinto elemento
x[-c(1, 3, 5)] # Elimina il primo, il terzo ed il quinto elemento

x[x > 3.5] # Estrae gli elementi maggiori di 3.5

# Concatenazione di vettori
x <- 1:5
y <- 1:6
x + y # Equivalente a: c(x, x[1]) + y

x <- 1:3
y <- 1:6
x + y # Equivalente a: c(x, x) + y

A <- matrix(c(5, 1, 2, 4), nrow = 2, ncol = 2)
A


# Definizione equivalente
A <- matrix(c(5, 2, 1, 4), nrow = 2, ncol = 2, byrow = TRUE)

x_col <- matrix(c(1, 10, 3, 5), ncol = 1)
x_col


x_row <- matrix(c(1, 10, 3, 5), nrow = 1)
x_row

x_row <- matrix(c(1, 10, 3, 5), nrow = 1)
x <- c(1, 10, 3, 5) # Simile, ma non identico, a x_row

dim(x_row)
dim(x)

A[1, 2] # Estrazione di elemento in posizione (1,2)
A[, 2] # Estrazione seconda colonna
A[1, ] # Estrazione prima riga

dim(A) # Restituisce la dimensione della matrice
a <- c(A) # Converte la matrice in un vettore
a

diag(A) # Restituisce la diagonale della matrice
t(A) # Calcola la matrice trasposta A'
sum(A) # Somma di tutti gli elementi di A

exp(A)

B <- A # Creo una matrice B identica ad A, per semplicità
C <- rbind(A, B)

C <- cbind(A, B)

x <- matrix(c(-4, 2, 6, 10, 22), ncol = 1)
y <- matrix(c(3, 2, 2, 7, 9), ncol = 1)
crossprod(x, y) # Equivalente a: sum(x * y)

A <- rbind(c(1, 2, 3), c(4, 9, 2), c(2, 2, 2))
B <- rbind(c(5, 2, 5), c(3, 3, 7), c(-2, -8, 10))

A %*% B # Prodotto righe per colonne AB

A <- rbind(c(1, 2, 3), c(4, 9, 2), c(2, 2, 2))
A1 <- solve(A) # Matrice inversa di A
A1

round(A %*% A1, digits = 5) # Operazione di controllo

det(A) # Calcola il determinante della matrice A

# Esempio di matrice NON invertibile
A <- rbind(c(1, 2, 3), c(2, 4, 6), c(2, 2, 2))
det(A) # Deteminante pari a 0, solve(A) produce un errore

A <- matrix(c(4, 1, 1, 8), ncol = 2)
chol(A) # Scomposizione di Cholesky
qr(A) # Scomposizione QR
eigen(A) # Scomposizione spettrale

# Creazione di una lista
new_list <- list(
  A = matrix(c(4, 1, 1, 8), ncol = 2),
  x = c(1, 2, 6, 6, 9)
)

new_list

Spec_A <- eigen(A) # Scomposizione spettrale della matrice A
Spec_A

Spec_A$values # Estrazione degli autovalori
