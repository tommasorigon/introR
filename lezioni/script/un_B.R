class(sum) # Identifica la tipologia di oggetto

class(log) # Secondo esempio

cube <- function(x) {
  out <- x^3
  out
}

cube(4) # Calcola il cubo del valore 4

cube <- function(x) {
  out <- x^3
  return(out) # Esplicita che il valore da dover restituire è out
}
cube(8) # Calcola il cubo del valore 8

power <- function(x, p = 2) {
  out <- x^p
  out
}

power(x = 4) # Calcola il quadrato del valore 4

power(4) # Sintassi alternativa: non è necessario specificare i nomi degli argomenti

power(x = 4, p = 3) # Calcola il cubo del valore 4

power(4, 3) # Sintassi alternativa: non è necessario specificare i nomi degli argomenti

if (condizione) {
  print("La condizione è vera")
  # Alcuni comandi da eseguire
  # ...
} else {
  print("La condizione è falsa")
  # Altri comandi da eseguire
  # ...
}

square_root <- function(x) {
  if (x < 0) {
    # Messaggio di avvertimento; in realtà sarebbe più appropriato usare il comando "warning"
    print("Il valore di x deve essere positivo")
    out <- NaN # Restituisco Not A Number
  } else {
    out <- sqrt(x)
  }
  out
}

square_root(-2) # La condizione x < 0 è verificata

square_root(36) # La condizione x < 0 NON è verificata
