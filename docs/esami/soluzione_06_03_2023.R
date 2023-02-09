# Problema 1 --------------------------------------------------------
rm(list=ls())
data(trees)

# 1.a -----------
Ftree <- ecdf(trees$Girth)
plot(Ftree) # Grafico
Ftree(15) # 0.7419355

# 1.b -----------
quantile(trees$Volume, probs = c(0.25, 0.75))
#   25%  75% 
#  19.4 37.3

hist(trees$Volume) # Numero di classi di default è accettabile

# 1.c -----------
trees$Volume_class <- cut(trees$Volume, breaks = c(10, 21, 37, 80))

# 1.d -----------
levels(trees$Volume_class) <- c("low", "medium", "high")

# 1.e -----------
boxplot(trees$Girth ~ trees$Volume_class, xlab = "Class of volume", ylab = "Girth")

# 1.f -----------
trees2 <- subset(trees, select = c(Girth, Volume))
colnames(trees2) <- c("Diameter", "Volume")

# Problema 2 -----------------------------------------

# 2.a -----------
rprop <- function(R){
  Y1 <- rexp(R, rate = 1) # rate = 1 è il default
  Y2 <- rexp(R, rate = 1)
  Y1 / (Y1 + Y2)
}

# 2.b -----------
R <- 10^4
set.seed(100)

sim <- rprop(R) 
mean_est <- mean(sim)
mean_est # 0.503371
var_est <- var(sim) 
var_est # 0.08243298

# 2.c -----------

# Parte 1. Per quel che riguarda la media, si poteva procedere come al solito, riportando lo std. error dello stimatore, ovvero:
se_mean <- sd(sim) / sqrt(R)
se_mean # 0.002871114

# (Per la lode). Per quel che riguarda la varianza, lo svolgimento era più complesso.

# Opzione 1: si tratta "mean_est" come fosse un valore noto e fissato. Funziona ragionevolmente bene quando mean_est è una buona approssimazione della media teorica. È una soluzione approssimata, ma efficace. Si ottiene:
se_var <- sd( (sim - mean_est)^2) / sqrt(R)
se_var # 0.0007478013

# Opzione 2: si procede interamente tramite simulazione:
R2 <- 10^4 
set.seed(123)
sim_var <- replicate(R2, var(rprop(R))) # Questo comando richiede un po' di pazienza
sd(sim_var) # 0.0007508882

# Opzione 3 richiedeva troppo lavoro per poter essere svolta all'esame. Viene riportata in questo file solo per completezza. Consultando un libro di statistica inferenziale, si approssima la varianza della varianza campionaria come segue:
se_var2 <- sqrt((mean( (sim - mean_est)^4) - (R - 3) / (R - 1) * var(sim)^2 ) / R)
se_var2 # 0.000747764

# Inoltre, poichè X è una variabile aleatoria uniforme in (0,1), allora si può dimostrare che il valore teorico cercato è:
sqrt((1/80 - (R - 3) / (R - 1) * (1/12)^2 ) / R) # 0.0007454492

# 2.d -----------
mean(sim <= 0.1)
mean(sim <= 0.5)
mean(sim <= 0.9)

# 2.e -----------
hist(sim, freq = FALSE)
curve(dunif(x), add = TRUE)
# La variabile X segue una distribuzione uniforme

# Problema 3 --------------------------------------

data(stackloss)

# 3.a -----------

# Un algoritmo di ottimizzazione, ovvero una procedura iterativa che mira a minimizzare una determinata funzione, giunge a convergenza quando due valori successivi della procedura sono praticamente indistinguibili tra loro. Ad esempio, quando la funzione assume valori reali, questo può voler dire che la differenza in valore assoluto tra due valori consecutivi dell'algoritmo è prossima a zero.

# 3.b -----------
beta_ols <- cov(stackloss$Water.Temp, stackloss$stack.loss) / var(stackloss$Water.Temp)
alpha_ols <- mean(stackloss$stack.loss) - mean(stackloss$Water.Temp) * beta_ols

# 3.c -----------
plot(stackloss$Water.Temp, stackloss$stack.loss)
abline(a = alpha_ols, b = beta_ols)

# 3.d -----------
loss <- function(par, x, y){
  sum(abs(y - par[1] - par[2]*x))
}

fit <- nlminb(start  = c(alpha_ols, beta_ols), function(param) loss(param, stackloss$Water.Temp, stackloss$stack.loss))

plot(stackloss$Water.Temp, stackloss$stack.loss)
abline(a = alpha_ols, b = beta_ols, col = "red")
abline(fit$par)

# NOTA 1. Questo problema di minimizzazione era caratterizzato da numerosi punti di discontinuità, per cui la funzione nlminb purtroppo spesso identificava una soluzione sub-ottimale. Ciò significa che il punto di ottimo (locale) individuato dipendeva fortemente dall'inizializzazione. 

# Ai fini dell'esame, sono stati valutati come  corretti anche eventuali minimi "locali" ed è stato molto apprezzato un eventuale commento critico al problema. 

# NOTA 2. Un miglior risultato (non richiesto all'esame) si ottiene tramite la funzione optim, che risulta molto più stabile di nlminb in questo specifico caso, dato che, dalla documentazione:

# The default method is an implementation of that of Nelder and Mead (1965), that uses only function values and is robust but relatively slow. It will work reasonably well for non-differentiable functions."

fit_optim <- optim(par = c(alpha_ols, beta_ols), function(param) loss(param, stackloss$Water.Temp, stackloss$stack.loss))

plot(stackloss$Water.Temp, stackloss$stack.loss)
abline(a = alpha_ols, b = beta_ols, col = "red")
abline(fit_optim$par, col = "blue")
abline(fit$par)
