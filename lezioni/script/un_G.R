## -------------------------------------------------------------------------------------------------------------------------------
titanic <- read.table("../dataset/titanic.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
## path <- "https://tommasorigon.github.io/introR/dataset/titanic.csv"
## titanic <- read.table(path, header = TRUE, sep = ",", stringsAsFactors = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
str(titanic)


## -------------------------------------------------------------------------------------------------------------------------------
summary(titanic)


## -------------------------------------------------------------------------------------------------------------------------------
freq_abs_classe <- table(titanic$Classe)
freq_rel_classe <- freq_abs_classe / sum(freq_abs_classe)
tab_summary <- cbind(freq_abs_classe, freq_rel_classe)
tab_summary


## -------------------------------------------------------------------------------------------------------------------------------
tab <- table(titanic$Salvato, titanic$Classe)
tab


## -------------------------------------------------------------------------------------------------------------------------------
addmargins(tab) # Aggiunge le distribuzioni marginali (assolute)


## -------------------------------------------------------------------------------------------------------------------------------
tab_rel <- prop.table(tab) # Comando alternativo: table(tab) / sum(tab)
tab_rel


## -------------------------------------------------------------------------------------------------------------------------------
addmargins(tab_rel) # Aggiunge le distribuzioni marginali relative


## -------------------------------------------------------------------------------------------------------------------------------
prop.table(tab, 1)


## -------------------------------------------------------------------------------------------------------------------------------
prop.table(tab, 2)


## -------------------------------------------------------------------------------------------------------------------------------
barplot(tab, beside = TRUE, legend.text = TRUE) # Beside = TRUE "affianca" i rettangoli.


## -------------------------------------------------------------------------------------------------------------------------------
barplot(prop.table(tab, 2),
  beside = FALSE,
  xlab = "Classe",
  legend.text = TRUE
) # Beside = FALSE "mette in colonna" i rettangoli.


## -------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)


## -------------------------------------------------------------------------------------------------------------------------------
ggplot(data = titanic, aes(x = Classe, fill = Salvato)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Dark2") +
  ylab("Frequenze assolute")


## -------------------------------------------------------------------------------------------------------------------------------
ggplot(data = titanic, aes(x = Classe, fill = Salvato)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Dark2") +
  ylab("Frequenze relative")


## -------------------------------------------------------------------------------------------------------------------------------
chi_squared <- function(x, y) {
  nn <- table(x, y)
  n <- sum(nn)
  ff <- nn / n # Frequenze relative congiunte
  f_x <- table(x) / n # Frequenze relative marginali di x
  f_y <- table(y) / n # Frequenze relative marginali di y
  S <- 0
  for (i in 1:length(f_x)) {
    for (j in 1:length(f_y)) {
      S <- S + ff[i, j]^2 / (f_x[i] * f_y[j])
    }
  }
  n * (S - 1)
}
chi_squared(titanic$Salvato, titanic$Classe)


## -------------------------------------------------------------------------------------------------------------------------------
chi_squared <- function(x, y) {
  nn <- table(x, y)
  n <- sum(nn)
  ff <- nn / n
  f_x <- apply(ff, 1, sum)
  f_y <- apply(ff, 2, sum)
  f_e <- outer(f_x, f_y) # Prodotto "esterno" tra vettori
  n * (sum(ff^2 / f_e) - 1)
}
chi_squared(titanic$Salvato, titanic$Classe)


## -------------------------------------------------------------------------------------------------------------------------------
chisq.test(table(titanic$Salvato, titanic$Classe))

