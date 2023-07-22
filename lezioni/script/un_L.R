## -------------------------------------------------------------------------------------------------------------------------------
ciliegi <- read.table("../dataset/ciliegi.csv", header = TRUE, sep = ",")


## -------------------------------------------------------------------------------------------------------------------------------
## path <- "https://tommasorigon.github.io/introR/data/ciliegi.csv"
## ciliegi <- read.table(path, header = TRUE, sep = ",")


## -------------------------------------------------------------------------------------------------------------------------------
head(ciliegi)


## -------------------------------------------------------------------------------------------------------------------------------
plot(ciliegi)


## -------------------------------------------------------------------------------------------------------------------------------
# Funzione di perdita che vogliamo minimizzare
loss <- function(par, y, x) {
  mean((y - par[1] * x^par[2])^2)
}


## -------------------------------------------------------------------------------------------------------------------------------
loss(c(1, 1), ciliegi$volume, ciliegi$diametro)


## -------------------------------------------------------------------------------------------------------------------------------
fit_ls <- nlminb(start = c(1, 1), function(param) loss(param, ciliegi$volume, ciliegi$diametro),
                 lower = c(1e-6, 1e-6))
fit_ls


## -------------------------------------------------------------------------------------------------------------------------------
# Salvo i risultati
param_hat_ls <- fit_ls$par
param_hat_ls


## -------------------------------------------------------------------------------------------------------------------------------
fit_ls$objective


## -------------------------------------------------------------------------------------------------------------------------------
z <- log(ciliegi$volume)
w <- log(ciliegi$diametro)

beta_hat_ols <- cov(w, z) / var(w)
alpha_hat_ols <- mean(z) - mean(w) * beta_hat_ols

# Stima ai minimi quadrati, scala trasformata
param_hat_ols <- c(exp(alpha_hat_ols), beta_hat_ols)
param_hat_ols

# Varianza residuale
loss(param_hat_ols, ciliegi$volume, ciliegi$diametro)


## -------------------------------------------------------------------------------------------------------------------------------
plot(ciliegi)
curve(param_hat_ls[1] * x^param_hat_ls[2], 
      add = TRUE, lty = "dashed") # Stime numeriche non-lineari
curve(param_hat_ols[1] * x^param_hat_ols[2], 
      add = TRUE, lty = "dashed", col = "red") # Modello linearizzato

