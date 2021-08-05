# Per $n = `r nrow(forbes)`$ luoghi nelle Alpi viene misurata la pressione atmosferica (inHg) e la temperatura di ebollizione dell'acqua (in gradi Celsius). I dati provengono da un esperimento condotto da Forbes nel 1857. I dati sono riportati nella seguente tabella.
# 
# ```{r, results="asis", echo=F}
# kable(forbes)
# ```
# 
# 1. Si disegni un istogramma della variabile `temperatura`, scegliendo un numero appropriato di classi equispaziate e giustificandone la scelta.
# 
# 1. Si ottengano la media aritmetica di entrambe le variabili `temperatura` e `pressione`. La relazione tra gradi Celsius e gradi Fahrenheit è la seguente:
# $$
# (\text{Fahrenheit}) = 32 + \frac{9}{5}(\text{Celsius}).
# $$
# Quanto vale la temperatura di ebollizione media espressa in gradi Fahrenheit? Si risponda senza ricalcolare tutti i valori della variabile `temperatura`.
# 
# 1. Si ottenga la varianza delle variabili `temperatura` e `pressione`.
# 
# 1. Si disegni un opportuno grafico che aiuti a comprendere la relazione tra le due variabili. Si calcoli quindi la correlazione: il valore ottenuto è coerente col grafico?
# 
# 1. Si ottenga la retta ai minimi quadrati per la relazione tra `temperatura` (variabile esplicativa $x$) e `pressione` (variabile risposta $y$), e la si disegni nel grafico ottenuto in precedenza. Si fornisca quindi un'interpretazione per i coefficienti.
# 
# 1. In base al modello stimato, se la temperatura di ebollizione dell'acqua è pari 97 gradi Celsius, a quanto è pari la pressione?
# 
# 1. Si ottenga un indice di bontà di adattamento ai dati della curva ottenuta e lo si interpreti nel contesto del problema.

rm(list=ls())
forbes <- read.table("https://tommasorigon.github.io/introR/data/forbes2.csv", header = TRUE, sep = ",")
head(forbes)
dim(forbes)
colnames(forbes) <- c("TempF", "Pressione")

forbes$TempC <- (forbes$TempF - 32) * 5 / 9 # Da Farenheit a Celsius


