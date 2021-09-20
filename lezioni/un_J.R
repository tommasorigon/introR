
# Gioco 50 volte ai dadi contro il banco
set.seed(123)
sample(c(-1, 1), 50, replace = TRUE)

# Calcolo il mio budget cumulato
set.seed(123)
cumsum(sample(c(-1, 1), 50, replace = TRUE))

# Un paio di traiettorie di partite
par(mfrow=c(1,2))
set.seed(250)
plot(1:50, cumsum(sample(c(-1, 1), 50, replace = TRUE)), type = "b", xlab = "# Partita", ylab = "Budget")
abline(h = 0, lty = "dotted")
plot(1:50, cumsum(sample(c(-1, 1), 50, replace = TRUE)), type = "b", xlab = "# Partita", ylab = "Budget")
abline(h = 0, lty = "dotted")

# Ottengo il valore complessivo (una volta sola)
sum(sample(c(-1, 1), 50, replace = TRUE))

# Ripeto questa operazione R volte
set.seed(123)
R <- 10^5
final_budget <- replicate(R, sum(sample(c(-1, 1), 50, replace = TRUE)))

# Distribuzione budget finale
par(mfrow = c(1, 1))
plot(table(final_budget) / R, xlab = "Budget finale (50 partite)", ylab = "Funzione di probabilità")

# Media del budget finale
mean(final_budget)

# Deviazione standard del budget finale
sd(final_budget)

# Probabilità di vincere, ovvero P(S > 0)
mean(final_budget > 0)

# ---------------------------------------
# Secondo scenario, probabilità non eque (roulette americana )
# ---------------------------------------

p_winning <- 18 / 38
p_winning

# Ottengo il valore complessivo (una volta sola)
sum(sample(c(-1, 1), 50, replace = TRUE, prob = c(1 - p_winning, p_winning)))

# Ripeto questa operazione R volte
set.seed(123)
R <- 10^5
final_budget <- replicate(R, sum(sample(c(-1, 1), 50, replace = TRUE, prob = c(1 - p_winning, p_winning))))

# Distribuzione budget finale
par(mfrow = c(1, 1))
plot(table(final_budget) / R, xlab = "Budget finale (50 partite)", ylab = "Funzione di probabilità")

# Media del budget finale
mean(final_budget)

# Deviazione standard del budget finale
sd(final_budget)

# Probabilità di vincere, ovvero P(S > 0)
mean(final_budget > 0)

# Supponiamo inoltre che il banco abbia un budget pari a 1000, mentre il giocatore abbia un budget pari a 10
