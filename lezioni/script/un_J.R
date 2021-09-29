rm(list=ls())
# Gioco 50 volte ai dadi contro il banco
set.seed(123)
sample(c(-10, 10), 50, replace = TRUE)

# Calcolo il mio guadagno (o perdita) cumulato
set.seed(123)
cumsum(sample(c(-10, 10), 50, replace = TRUE))

# Un paio di traiettorie di partite
par(mfrow=c(1,2))
set.seed(250)
plot(1:50, cumsum(sample(c(-10, 10), 50, replace = TRUE)), type = "b", xlab = "# Lancio", ylab = "Guadagno")
abline(h = 0, lty = "dotted")
plot(1:50, cumsum(sample(c(-10, 10), 50, replace = TRUE)), type = "b", xlab = "# Lancio", ylab = "Guadagno")
abline(h = 0, lty = "dotted")

# Ottengo il valore complessivo (una volta sola)
set.seed(123)
sum(sample(c(-10, 10), 50, replace = TRUE))

# Ripeto questa operazione R volte
set.seed(123)
R <- 10^5
final_earning <- replicate(R, sum(sample(c(-10, 10), 50, replace = TRUE)))
final_earning[1:5]

# Distribuzione guadagno finale
par(mfrow = c(1, 1))
plot(table(final_earning) / R, xlab = "guadagno finale (50 partite)", ylab = "Funzione di probabilità")

# Media del guadagno finale
mean(final_earning)

# Deviazione standard del guadagno finale
sd(final_earning)

# Probabilità di vincere, ovvero P(Z > 0)
mean(final_earning > 0)

mean(final_earning == 0)

# ---------------------------------------
# Secondo scenario, probabilità non eque (roulette americana )
# ---------------------------------------

p_winning <- 18 / 38
p_winning

# Ottengo il valore complessivo (una volta sola)
set.seed(123)
sum(sample(c(-10, 10), 50, replace = TRUE, prob = c(1 - p_winning, p_winning)))

# Ripeto questa operazione R volte
set.seed(123)
R <- 10^5
final_earning <- replicate(R, sum(sample(c(-10, 10), 50, replace = TRUE, 
                                         prob = c(1 - p_winning, p_winning))))

# Distribuzione guadagno finale
par(mfrow = c(1, 1))
plot(table(final_earning) / R, xlab = "Guadagno finale (50 partite)", ylab = "Funzione di probabilità")

# Media del guadagno finale
mean(final_earning)

# Deviazione standard del guadagno finale
sd(final_earning)

# Probabilità di vincere, ovvero P(X > 0)
mean(final_earning > 0)

# Probabilità di vincere, ovvero P(X == 0)
mean(final_earning == 0)

par(mfrow = c(1, 1))
plot(table(final_earning) / R, xlab = "Guadagno finale (50 partite)", ylab = "Funzione di probabilità")

# Supponiamo inoltre che il banco abbia un budget pari a 1000, mentre il giocatore abbia un budget pari a 10

sim_match <- function(player_budget, casino_budget, p_winning){
  
  player_money <- player_budget
  casino_money <- casino_budget
  
  for(r in 1:50){
    outcome <- sample(c(-10, 10), 1, prob = c(1 - p_winning, p_winning))
    player_money <- player_money + outcome 
    casino_money <- casino_money - outcome
    if(player_money <= 0){
      # Giocatore in rovina: il giocatore perde tutto il budget
      return(- player_budget)
    }
    if(casino_money <= 0){
      # Casinò in rovina: il casinò perde tutto il budget
      return(- casino_budget)
    }
  }
  player_money - player_budget # guadagno rispetto al valore iniziale
}

set.seed(220)
sim_match(100, 100000, p_winning)

set.seed(123)
final_earning <- replicate(10^5, sim_match(100, 100000, p_winning))

# Media del guadagno finale
mean(final_earning)

# Deviazione standard del guadagno finale
sd(final_earning)

# Probabilità di vittoria, ovvero P(X > 0)
mean(final_earning > 0)

# Probabilità di pareggio, ovvero P(X == 0)
mean(final_earning == 0)
# [1] 0.10483

plot(table(final_earning) / R, xlab = "Guadagno finale (50 partite)", ylab = "Funzione di probabilità")
