path <- "https://tommasorigon.github.io/introR/data/titanic.csv"
titanic <- read.table(path, header = TRUE, sep = ",", stringsAsFactors = TRUE) # Scarica il file da internet

str(titanic)

summary(titanic)

freq_abs_classe <- table(titanic$Classe)
freq_rel_classe <- freq_abs_classe / sum(freq_abs_classe)
tab_summary <- cbind(freq_abs_classe, freq_rel_classe)
tab_summary

## Tabella a doppia entrata

# La  **tabella a doppia entrata** viene utilizzata per rappresentare una **distribuzione di frequenza congiunta** di due variabili.

# Rappresentiamo qui di seguito la tabella che mette i relazioni si sopravvissuti `Salvato`, con la classe di appartenenza `Classe`, del dataset `titanic`.

tab <- table(titanic$Salvato, titanic$Classe)
tab

# Possiamo anche aggiungere le frequenze assolute marginali come segue:
tab_margins <- addmargins(tab)
tab_margins

### Frequenze relative congiunte e relative marginali

# Le **frequenze relative congiunte**  si ottengono dividendo le frequenze congiunte assolute  per il totale delle osservazioni. Pertanto, in **R** eseguiamo

tab_rel <- prop.table(tab) # Comando alternativo: table(tab) / sum(tab)
tab_rel

# Esattamente come nel caso precedente, possiamo aggiungere le frequenze relative marginali.

tab_rel_margins <- addmargins(tab_rel)
tab_rel_margins

## Frequenze condizionate

# Le **frequenze subordinate (o condizionate)** per riga o per colonna rappresentano la distribuzione di frequenze relative di una variabile per una fissata modalità dell'altra variabile. Esse permettono di identificare facilmente eventuali relazioni tra le variabili.

# Frequenze condizionate 1
prop.table(tab, 1)

# Frequenze condizionate 2
prop.table(tab, 2)


chi_squared <- function(x, y) {
  nn <- table(x, y)
  n <- sum(nn)
  ff <- nn / n
  f_x <- apply(ff, 1, sum)
  f_y <- apply(ff, 2, sum)
  f_e <- outer(f_x, f_y)
  n * (sum(ff^2 / f_e) - 1)
}
