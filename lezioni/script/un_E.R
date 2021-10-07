path <- "https://tommasorigon.github.io/introR/data/emoglobina.csv"
emoglobina <- read.table(path, header = TRUE, sep = ",", stringsAsFactors = TRUE)

str(emoglobina)

emo_A <- emoglobina$emoglobina[emoglobina$Metodologia == "A"] # Emoglobina gruppo A
emo_B <- emoglobina$emoglobina[emoglobina$Metodologia == "B"] # Emoglobina gruppo B

summary(emo_A)
summary(emo_B)

boxplot(emoglobina$emoglobina ~ emoglobina$Metodologia,
  ylab = "Emoglobina",
  xlab = "Metodologia"
)

mean((emo_A - mean(emo_A))^2) # Varianza del gruppo A
mean((emo_B - mean(emo_B))^2) # Varianza del gruppo B

mean(emo_A^2) - mean(emo_A)^2 # Varianza del gruppo A, formula alternativa
mean(emo_B^2) - mean(emo_B)^2 # Varianza del gruppo B, formula alternativa

my_var <- function(x) {
  mean(x^2) - mean(x)^2
}

my_var(emo_A)
my_var(emo_B)

var(emo_A)
var(emo_B)

my_sd <- function(x) {
  sqrt(my_var(x))
}

my_sd(emo_A)
my_sd(emo_B)

sd(emo_A)
sd(emo_B)

max(emo_A) - min(emo_A)
max(emo_B) - min(emo_B)

diff(range(emo_A))
diff(range(emo_B))

interquartile_range <- function(x) {
  diff(quantile(x, probs = c(0.25, 0.75)))
}

interquartile_range(emo_A)
interquartile_range(emo_B)

MAD <- function(x) {
  median(abs(x - median(x)))
}

MAD(emo_A)
MAD(emo_B)

tapply(emoglobina$emoglobina, emoglobina$Metodologia, mean)
with(emoglobina, tapply(emoglobina, Metodologia, mean)) # In maniera ancora più compatta

tab <- rbind(
  with(emoglobina, tapply(emoglobina, Metodologia, my_var)),
  with(emoglobina, tapply(emoglobina, Metodologia, my_sd)),
  with(emoglobina, tapply(emoglobina, Metodologia, interquartile_range)),
  with(emoglobina, tapply(emoglobina, Metodologia, MAD))
)
rownames(tab) <- c("Varianza", "Deviazione Standard", "Scarto interquartile", "MAD")
tab

# Prima possibile implementazione
asym <- function(x) {
  sqm <- sqrt(mean(x^2) - mean(x)^2)
  mean((x - mean(x))^3) / sqm^3
}

# Implementazione (leggermente) alternativa
asym <- function(x) {
  sqm <- sqrt(mean(x^2) - mean(x)^2)
  z <- (x - mean(x)) / sqm
  mean(z^3)
}
