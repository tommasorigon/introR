# Il seguente file costituisce uno schema di una possibile soluzione.

# ----------------------------------------
# Domanda 6
# ----------------------------------------

# 1.1
f <- function(x) {
  (exp(x) - 1) / (1 + exp(x))
}

# 1.2
f(4) # 0.9640276

# 1.3
round(f(4), 2) # 0.96

# 1.4
curve(f, -2, 4)

# 1.5
sum(f(5:10)) # 5.978839
sum(f(5:100)) # 95.97879

# Qualche studente ha utilizzato dei cicli "for" per ottenere la risposta. Sono stati assegnati alcuni punti ugualmente, ma non il punteggio pieno. 

# 1.6
integrate(f, 1, 4) # 2.409776 with absolute error < 2.7e-14

# ----------------------------------------
# Domanda 7
# ----------------------------------------

# Caricamento in memoria del dataset
library(MASS)
data(Pima.te)

# Per accedere alla documentazione
# ? Pima.te

# 2.1
str(Pima.te) # 'data.frame':	332 obs. of  8 variables:
# In realtà, può essere sufficiente guardare nella casella inferiore del "global environment"

# 2.2
mean(Pima.te$bp) # 71.65361

# 2.3
bmi <- Pima.te$bmi
bmi_log <- log(Pima.te$bmi)

hist(bmi_log) # Utilizzo il numero di intervalli di default

# 2.4
asym <- function(x) {
  mean((x - mean(x))^3) / sd(x)^3
}
# Questa è l'implementazione più concisa. Variazioni sul tema sono state accettate ugualmente.


# 2.5
asym(bmi) # 0.7980727
asym(bmi_log) # 0.06305837
# La variabile bmi risulta pertanto maggiormente asimmetrica

# 2.6
qqnorm(bmi)
qqline(bmi)
# L'assunzione di normalità per la variabile bmi sembra NON essere verificata

qqnorm(bmi_log)
qqline(bmi_log)
# L'assunzione di normalità per la variabile bmi_log sembra essere ragionevole

# Nota: alcuni studenti hanno sovrapposto la densità di una gaussiana (con opportuna media e varianza) all'istogramma dei dati. Tale strategia è stata considerata valida (= punteggio pieno), sebbene leggermente meno appropriata di qqplot, il cui scopo esplicito è verificare la somiglianza tra i dati ed una specifica distribuzione. 

# 2.7
bmi_yes <- bmi[Pima.te$type == "Yes"]
bmi_no <- bmi[Pima.te$type == "No"]

# 2.8

# Funzioni di ripartizione empiriche
plot(ecdf(bmi_yes))
lines(ecdf(bmi_no), col = "red")

# Media e mediana. Anche i comandi mean / median sono corretti.
summary(bmi_yes)
summary(bmi_no)

# ----------------------------------------
# Domanda 8
# ----------------------------------------

# Caricamento dei dati in memoria
x <- c(2.1499496, 5.0539201, 3.1207749, 1.4512639, 3.8040806, 1.6647759)

# Nel testo dell'esame era erroneamente scritto n = 5 mentre, ovviamente, si ha che n = 6. Nei rari casi in cui questo ha causato confusione, ho corretto manualmente il codice inviatomi e assegnato il punteggio pieno. 

# 3.1
alpha <- 2
lambda_hat <- alpha / mean(x)
lambda_hat # 0.6958634

# 3.2
loglik <- function(x, alpha, lambda) {
  sum(dgamma(x, alpha, lambda, log = TRUE))
}

# 3.3
loglik(x, 2, 1) # -11.50288
loglik(x, 2, lambda_hat) # -10.60933
# Per definizione, la stima di massima verosimiglianza è il valore che rende massima la funzione di verosimiglianza.

# 3.4

# Nota: alcuni studenti hanno fornito soluzioni "alternative" a quella seguente, basandosi sulla convergenza dell'MSE. Quest'ultimo approccio è stato considerato come corretto, ma non è riportato nel seguito. 

# Numerosità campionarie
nn <- c(100, 500, 1000, 10000)
2
# Stime di massima verosimiglianza
set.seed(123)
lambda_hat <- c(
  alpha / mean(rgamma(nn[1], 2, 1)),
  alpha / mean(rgamma(nn[2], 2, 1)),
  alpha / mean(rgamma(nn[3], 2, 1)),
  alpha / mean(rgamma(nn[4], 2, 1))
)

# Grafico per la verifica
plot(nn, lambda_hat,
  type = "b",
  xlab = "Numerosità campionaria",
  ylab = "Massima verosimiglianza"
)
