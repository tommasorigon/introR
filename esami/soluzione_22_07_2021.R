# ----------------------------------------
# Domanda 1
# ----------------------------------------

# 1.1 (5pt)
set.seed(123)
R <- 10^6 # In questo caso l'esecuzione è talmente rapida che posso usare un numero R elevato, per aumentare la precisione
X <- rexp(R, 2) # Attenzione che il parametro della funzione rexp è "rate", non la media.
Y <- rpois(R, exp(X / 4))
mean(Y^2) # 2.476926

# 1.2 (5pt)
set.seed(123)
R <- 10^6
X <- rexp(R, 2)
mean(exp(X / 4) + exp(X / 2)) # 2.476027

# ----------------------------------------
# Domanda 2
# ----------------------------------------

# 2.1-2.2 (2pt + 2pt)
theta <- 1
n <- 20
set.seed(123)
R <- 50000

T1_sim <- function(n, theta) {
  4 / 3 * mean(runif(n, theta / 2, theta))
}

T2_sim <- function(n, theta) {
  sim <- runif(n, theta / 2, theta)
  2 / 3 * (min(sim) + max(sim))
}

# Distorsione
mean(replicate(R, T1_sim(n, theta)) - theta) # -0.000314001
mean(replicate(R, T2_sim(n, theta)) - theta) # 0.0001052971

# Errore quadratico medio
mean((replicate(R, T1_sim(n, theta)) - theta)^2) # 0.001847496
mean((replicate(R, T2_sim(n, theta)) - theta)^2) # 0.0004829104

# Pertanto T_2 è più efficiente di T_1

# 2.3 (6pt)

# Questo esercizio poteva essere svolto in vari modi. Nel seguito è presentata una possibile (ma non l'unica) soluzione
set.seed(140)
nn <- 1:10000
theta_hat <- 4 / 3 * cumsum(runif(10000, theta / 2, theta)) / nn

plot(nn, theta_hat,
  type = "l",
  xlab = "Numerosità campionaria",
  ylab = "Stimatore"
)
abline(h = theta)

# ----------------------------------------
# Domanda 3
# ----------------------------------------

# In alternativa al comando seguente, era possibile scaricare il file in locale.
dataset <- read.table("https://tommasorigon.github.io/introR/data/province.csv", header = TRUE)

# 3.1 (1pt)
dim(dataset)

# 3.2 (1pt)
cor(dataset)

# 3.3 (8pt)
ols_res <- function(x, y) {
  beta_hat <- cov(x, y) / var(x)
  alpha_hat <- mean(y) - mean(x) * beta_hat
  y - (alpha_hat + beta_hat * x)
}

r1 <- ols_res(dataset$istruzione, dataset$agricoltura)
r2 <- ols_res(dataset$istruzione, dataset$fertilità)
cor(r1, r2) # -0.002065755

# 3.3 - Soluzione alternativa, basato sulle nozioni di ASM-MS
r1 <- residuals(lm(agricoltura ~ istruzione, data = dataset))
r2 <- residuals(lm(fertilità ~ istruzione, data = dataset))
cor(r1, r2) # -0.002065755
