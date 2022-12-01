## -----------------------------------------------------------------------------------------
set.seed(123)
sample(c(-10, 10), 50, replace = TRUE) # Gioco 50 volte ai dadi contro B


## -----------------------------------------------------------------------------------------
set.seed(123)
cumsum(sample(c(-10, 10), 50, replace = TRUE))


## -----------------------------------------------------------------------------------------
par(mfrow=c(1,2))
set.seed(250)

plot(1:50, cumsum(sample(c(-10, 10), 50, replace = TRUE)), type = "b", xlab = "# Lancio", ylab = "Guadagno")
abline(h = 0, lty = "dotted")

plot(1:50, cumsum(sample(c(-10, 10), 50, replace = TRUE)), type = "b", xlab = "# Lancio", ylab = "Guadagno")
abline(h = 0, lty = "dotted")


## -----------------------------------------------------------------------------------------
# Ottengo il guadagno finale (una volta sola)
set.seed(123)
sum(sample(c(-10, 10), 50, replace = TRUE))


## -----------------------------------------------------------------------------------------
# Ripeto questa operazione R volte per ottenere dei campioni
set.seed(123)
R <- 10^5
# Otteniamo 10^5 copie iid aventi la stessa distribuzione di X
final_earning <- replicate(R, sum(sample(c(-10, 10), 50, replace = TRUE)))
final_earning[1:5]


## -----------------------------------------------------------------------------------------
mean(final_earning) # Media del guadagno finale


## -----------------------------------------------------------------------------------------
sd(final_earning) # Deviazione standard del guadagno finale


## -----------------------------------------------------------------------------------------
par(mfrow = c(1, 1))
plot(table(final_earning) / R, xlab = "Guadagno finale (50 partite)", ylab = "Funzione di probabilità")


## -----------------------------------------------------------------------------------------
mean(final_earning > 0) # Probabilità di vincere, ovvero P(X > 0)


## -----------------------------------------------------------------------------------------
mean(final_earning == 0) # Probabilità di pareggiare, ovvero P(X = 0)


## -----------------------------------------------------------------------------------------
p_winning <- 18 / 38
p_winning

# Ottengo il guadagno finale (una volta sola)
set.seed(123)
sum(sample(c(-10, 10), 50, replace = TRUE, prob = c(1 - p_winning, p_winning)))


## -----------------------------------------------------------------------------------------
set.seed(123)
R <- 10^5
final_earning <- replicate(R, sum(sample(c(-10, 10), 50, replace = TRUE, 
                                         prob = c(1 - p_winning, p_winning))))


## -----------------------------------------------------------------------------------------
# Media del guadagno finale
mean(final_earning)


## -----------------------------------------------------------------------------------------
# Deviazione standard del guadagno finale
sd(final_earning)


## -----------------------------------------------------------------------------------------
# Probabilità di vittoria, ovvero P(X > 0)
mean(final_earning > 0)


## -----------------------------------------------------------------------------------------
# Probabilità di pareggio, ovvero P(X == 0)
mean(final_earning == 0)


## -----------------------------------------------------------------------------------------
par(mfrow = c(1, 1))
plot(table(final_earning) / R, xlab = "Guadagno finale (50 partite)", ylab = "Funzione di probabilità")


## -----------------------------------------------------------------------------------------
# Funzione che simula il guadagno finale dopo 50 scommesse
sim_match <- function(player_budget, casino_budget, p_winning){
  
  player_money <- player_budget
  casino_money <- casino_budget
  
  for(r in 1:50){
    outcome <- sample(c(-10, 10), 1, prob = c(1 - p_winning, p_winning))
    player_money <- player_money + outcome # Aggiorno il budget giocatore
    casino_money <- casino_money - outcome # Aggiorno il budget casinò
    if(player_money <= 0){
      # Giocatore in rovina: il giocatore perde tutto il budget
      return(- player_budget)
    }
    if(casino_money <= 0){
      # Casinò in rovina: il casinò perde tutto il budget
      return(casino_budget)
    }
  }
  player_money - player_budget # guadagno rispetto al valore iniziale
}

set.seed(220)
sim_match(100, 100000, p_winning)


## -----------------------------------------------------------------------------------------
set.seed(123)
R <- 10^5
final_earning <- replicate(10^5, sim_match(100, 100000, p_winning))


## -----------------------------------------------------------------------------------------
# Media del guadagno finale
mean(final_earning)


## -----------------------------------------------------------------------------------------
# Deviazione standard del guadagno finale
sd(final_earning)


## -----------------------------------------------------------------------------------------
# Probabilità di vittoria, ovvero P(X > 0)
mean(final_earning > 0)


## -----------------------------------------------------------------------------------------
# Probabilità di pareggio, ovvero P(X == 0)
mean(final_earning == 0)


## -----------------------------------------------------------------------------------------
plot(table(final_earning) / R, xlab = "Guadagno finale (50 partite)", ylab = "Funzione di probabilità")

