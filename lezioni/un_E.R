path <- "https://tommasorigon.github.io/introR/data/emoglobina.csv"
emoglobina <- read.table(path, header = TRUE, sep = ",", stringsAsFactors = TRUE) # Scarica il file da internet

str(emoglobina)

emo_A <- emoglobina$emoglobina[emoglobina$Metodologia == "A"] # Emoglobina gruppo A
emo_B <- emoglobina$emoglobina[emoglobina$Metodologia == "B"] # Emoglobina gruppo B

summary(emo_A)
summary(emo_B)

boxplot(emoglobina$emoglobina ~ emoglobina$Metodologia,
        ylab = "Emoglobina",
        xlab = "Metodologia"
)

mean((emo_A - mean(emo_A))^2)
mean((emo_B - mean(emo_B))^2)

my_var <- function(x) {
  mean(x^2) - mean(x)^2
}

my_var(emo_A)
my_var(emo_B)
