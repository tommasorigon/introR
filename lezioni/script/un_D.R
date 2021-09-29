dde <- read.table("https://tommasorigon.github.io/introR/data/dde.csv", header = TRUE, 
                  sep = ",") # Scarica il file da internet
str(dde)

dde_preterm <- dde$DDE[dde$GAD < 37 * 7] # Gruppo parto prematuro
dde_non_preterm <- dde$DDE[dde$GAD >= 37 * 7] # Gruppo parto non prematuro

# Numerosità campionarie
length(dde_preterm)
length(dde_non_preterm)


breaks <- 18 * (0:10) # Definizione degli intervalli. Usiamo 10 intervalli di lunghezza 18.
dde_preterm_class <- cut(dde_preterm, breaks = breaks) # Creazione di una variabile divisa per intervalli
dde_non_preterm_class <- cut(dde_non_preterm, breaks = breaks) # Creazione di una variabile divisa per intervalli

head(dde_preterm_class)

# Frequenze assolute
freq_abs_preterm <- table(dde_preterm_class)
freq_abs_preterm

freq_abs_non_preterm <- table(dde_non_preterm_class)
freq_abs_non_preterm

# Frequenze relative
freq_rel_preterm <- freq_abs_preterm / sum(freq_abs_preterm)
round(freq_rel_preterm, digits = 3)

freq_rel_non_preterm <- freq_abs_non_preterm / sum(freq_abs_non_preterm)
round(freq_rel_non_preterm, digits = 3)


tab_summary <- cbind(freq_abs_preterm, freq_abs_non_preterm, 
                     freq_rel_preterm, freq_rel_non_preterm)
colnames(tab_summary) <- c(
  "n_j prematura",
  "n_j non prematura",
  "f_j prematura",
  "f_j non prematura"
)
round(tab_summary, 3)

par(mfrow = c(1, 2)) # Divide il grafico in due parti

# Primo grafico, frequenze assolute
hist(dde_non_preterm,
     freq = TRUE,
     breaks = 10,
     # Da qui in poi stiamo solo aggiungendo dettagli estetici
     main = "Istogramma di DDE (nascite non premature)",
     xlab = "DDE",
     ylab = "Frequenze assolute"
)

# Secondo grafico, densità
hist(dde_non_preterm,
     freq = FALSE, # NON vengono usate le frequenze 
     breaks = 10,
     # Da qui in poi stiamo solo aggiungendo dettagli estetici
     main = "Istogramma di DDE (nascite non premature)",
     xlab = "DDE",
     ylab = "Densità"
)



par(mfrow = c(1, 2))

# GRAFICO CORRETTO
breaks <- c(5 * 0:10, 70, 90, 110, 130, 150, 170, 190) # Definizione degli intervalli
hist(dde_non_preterm,
     freq = FALSE,
     breaks = breaks,
     # Da qui in poi stiamo solo aggiungendo dettagli estetici
     main = "GRAFICO CORRETTO",
     xlab = "DDE",
     ylab = "Densità"
)

# GRAFICO ERRATO
hist(dde_non_preterm,
     freq = TRUE, # NON vengono usate le frequenze 
     breaks = breaks,
     # Da qui in poi stiamo solo aggiungendo dettagli estetici
     main = "GRAFICO ERRATO",
     xlab = "DDE",
     ylab = "Frequenze assolute"
)


par(mfrow = c(1, 2))

# POCHI INTERVALLI
hist(dde_non_preterm,
     freq = TRUE,
     breaks = 4,
     # Da qui in poi stiamo solo aggiungendo dettagli estetici
     main = "Istogramma di DDE (nascite non premature)",
     xlab = "DDE",
     ylab = "Frequenze assolute"
)

# TROPPI INTERVALLI
hist(dde_non_preterm,
     freq = TRUE,
     breaks = 180,
     # Da qui in poi stiamo solo aggiungendo dettagli estetici
     main = "Istogramma di DDE (nascite non premature)",
     xlab = "DDE",
     ylab = "Frequenze assolute"
)


nclass.Sturges(dde_non_preterm)
nclass.FD(dde_non_preterm)


hist(dde_non_preterm,
     freq = FALSE,
     # Da qui in poi stiamo solo aggiungendo dettagli estetici
     main = "Istogramma di DDE (nascite non premature)"
)

hist(dde_preterm,
     freq = FALSE,
     breaks = "sturges",
     # Da qui in poi stiamo solo aggiungendo dettagli estetici
     main = "Istogramma di DDE (nascite premature)"
)

ecdf(dde_non_preterm)

# Definisco la funzione di ripartizione di DDE
F_non_preterm <- ecdf(dde_non_preterm)

F_non_preterm(c(20, 40, 60)) # Calcola la funzione di ripartizione empirica in 40
sum(dde_non_preterm <= 40) / length(dde_non_preterm) # Calcola la funzione di ripartizione empirica in 40

par(mfrow=c(1,1))
plot(ecdf(dde_non_preterm))

plot(ecdf(dde_preterm), do.points = FALSE, col = "blue",
     main = "Blu: parto prematuro. Rosso: parto non prematuro",
     xlab = "DDE",
     ylab = "F(DDE)"
)
plot(ecdf(dde_non_preterm), col = "red", add=T)


mean(dde_preterm) # Media aritmetica di DDE per donne con parto prematuro
mean(dde_non_preterm) # Media aritmetica di DDE per donne con parto regolare



n <- length(dde_preterm) # Numerosità campionaria
sum(dde_preterm) / n # Media aritmetica di dde_preterm


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
median(x)


### Quantili


quantile(dde_preterm, probs = c(0.1, 0.25, 0.75, 0.9), type = 1)
quantile(dde_non_preterm, probs = c(0.1, 0.25, 0.75, 0.9), type = 1)

# Primo quartile di DDE
min(dde_non_preterm[F_non_preterm(dde_non_preterm) >= 0.25])
# Terzo quartile di DDE
min(dde_non_preterm[F_non_preterm(dde_non_preterm) >= 0.75])



### Ambiguità nel calcolo dei quantili

tab <- rbind(
  quantile(dde_preterm, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 1),
  quantile(dde_preterm, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 6),
  quantile(dde_preterm, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 7),
  quantile(dde_preterm, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 9)
)
rownames(tab) <- c(1, 6, 7, 9)
tab


summary(dde_preterm)
summary(dde_non_preterm)

summary(dde)

par(mfrow=c(1,1))
boxplot.stats(dde_preterm)
boxplot(dde_preterm, dde_non_preterm)
