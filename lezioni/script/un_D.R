dde <- read.table("https://tommasorigon.github.io/introR/data/dde.csv",
  header = TRUE,
  sep = ","
) # Scarica il file da internet
str(dde)

dde_preterm <- dde$DDE[dde$GAD < 37 * 7] # Gruppo parto prematuro
dde_non_preterm <- dde$DDE[dde$GAD >= 37 * 7] # Gruppo parto non prematuro

length(dde_preterm) # Numerosità campionaria gruppo 1
length(dde_non_preterm) # Numerosità campionaria gruppo 2

breaks <- 18 * (0:10) # Definizione degli intervalli. Usiamo 10 intervalli di lunghezza 18.

dde_preterm_class <- cut(dde_preterm, breaks = breaks)
dde_non_preterm_class <- cut(dde_non_preterm, breaks = breaks)

head(dde_preterm_class)

freq_abs_preterm <- table(dde_preterm_class)
freq_abs_preterm

freq_abs_non_preterm <- table(dde_non_preterm_class)
freq_abs_non_preterm

freq_rel_preterm <- freq_abs_preterm / sum(freq_abs_preterm)
round(freq_rel_preterm, digits = 3)

freq_rel_non_preterm <- freq_abs_non_preterm / sum(freq_abs_non_preterm)
round(freq_rel_non_preterm, digits = 3)

tab_summary <- cbind(
  freq_abs_preterm, freq_abs_non_preterm,
  freq_rel_preterm, freq_rel_non_preterm
)
colnames(tab_summary) <- c(
  "n_j prematura",
  "n_j non prematura",
  "f_j prematura",
  "f_j non prematura"
)

round(tab_summary, 3) # Visualizzazione dei risultati

par(mfrow = c(1, 2)) # Divide il grafico in due parti

# Primo grafico, frequenze assolute
hist(dde_non_preterm,
  freq = TRUE,
  breaks = 10, # Utilizzo 10 sotto-intervalli
  # Da qui in poi stiamo solo aggiungendo dettagli estetici
  main = "Istogramma di DDE (nascite non premature)",
  xlab = "DDE",
  ylab = "Frequenze assolute"
)

# Secondo grafico, densità
hist(dde_non_preterm,
  freq = FALSE, # NON vengono usate le frequenze
  breaks = 10, # Utilizzo 10 sotto-intervalli
  # Da qui in poi stiamo solo aggiungendo dettagli estetici
  main = "Istogramma di DDE (nascite non premature)",
  xlab = "DDE",
  ylab = "Densità"
)

# Definizione di intervalli NON equispaziati
breaks <- c(5 * 0:10, 70, 90, 110, 130, 150, 170, 190)

# GRAFICO CORRETTO
hist(dde_non_preterm,
  freq = FALSE,
  breaks = breaks,
  # Da qui in poi stiamo solo aggiungendo dettagli estetici
  main = "GRAFICO CORRETTO", xlab = "DDE", ylab = "Densità"
)

# GRAFICO ERRATO
hist(dde_non_preterm,
  freq = TRUE, # NON vengono usate le frequenze
  breaks = breaks,
  # Da qui in poi stiamo solo aggiungendo dettagli estetici
  main = "GRAFICO ERRATO", xlab = "DDE", ylab = "Frequenze assolute"
)

nclass.Sturges(dde_non_preterm) # Regola di Sturges
nclass.FD(dde_non_preterm) # Regola di Freedman e Diaconis

hist(dde_non_preterm, # Gruppo nascite non-premature
  freq = FALSE,
  main = "Istogramma di DDE (nascite non premature)"
)

hist(dde_preterm, # Gruppo nascite premature
  freq = FALSE,
  main = "Istogramma di DDE (nascite premature)"
)

ecdf(dde_non_preterm)

F_non_preterm <- ecdf(dde_non_preterm)
F_non_preterm(c(20, 40, 60)) # Calcola la funzione di ripartizione empirica in 20, 40 e 60
sum(dde_non_preterm <= 40) / length(dde_non_preterm) # Comando "manuale" alternativo

par(mfrow = c(1, 1))
plot(ecdf(dde_non_preterm))

plot(ecdf(dde_preterm),
  do.points = FALSE, col = "blue",
  main = "Blu: parto prematuro. Rosso: parto non prematuro",
  xlab = "DDE",
  ylab = "F(DDE)"
)

plot(ecdf(dde_non_preterm), col = "red", add = TRUE) # Aggiungo un secondo gruppo

mean(dde_preterm) # Media aritmetica di DDE per donne con parto prematuro
mean(dde_non_preterm) # Media aritmetica di DDE per donne con parto regolare

n <- length(dde_preterm) # Numerosità campionaria di dde_preterm (=361)
sum(dde_preterm) / n # Media aritmetica della variabile dde_preterm

median(dde_preterm) # Mediana di DDE per donne con parto prematuro
median(dde_non_preterm) # Mediana di DDE per donne con parto regolare

F_non_preterm(median(dde_non_preterm))

x <- c(10, 20, 25, 3.5, 28, 62)
n <- length(x) # Numerosità campionaria
n # Si noti che n = 6 è pari

x_sort <- sort(x) # Vettore ordinato dei valori di x
x_sort

pos_med_1 <- n / 2 # Elemento in posizione n/2
pos_med_2 <- n / 2 + 1 # Elemento in posizione n/2+1

(x_sort[pos_med_1] + x_sort[pos_med_2]) / 2 # Mediana di x

median(x) # Ovviamente, il risultato deve coincidere con:

quantile(dde_preterm, probs = c(0.1, 0.25, 0.75, 0.9), type = 1)
quantile(dde_non_preterm, probs = c(0.1, 0.25, 0.75, 0.9), type = 1)

min(dde_non_preterm[F_non_preterm(dde_non_preterm) >= 0.25])
min(dde_non_preterm[F_non_preterm(dde_non_preterm) >= 0.75])

tab <- rbind(
  quantile(dde_preterm, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 1),
  quantile(dde_preterm, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 6),
  quantile(dde_preterm, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 7),
  quantile(dde_preterm, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 9)
)
rownames(tab) <- c(1, 6, 7, 9) # Cambia i nomi alle righe della tabella
tab

summary(dde_preterm)
summary(dde_non_preterm)

summary(dde)

boxplot.stats(dde_preterm)

boxplot(dde_preterm, dde_non_preterm) # Produce il grafico
