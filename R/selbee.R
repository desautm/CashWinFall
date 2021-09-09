library(tidyverse)

p <- choose(6,5)*choose(40,1)/choose(46,6)
n <- 217833

pselbee <- tribble(
  ~Résultat, ~Probabilité,
  "Aucun 5 dans 6", dbinom(0,n,p),
  "1 billet 5 dans 6", dbinom(1,n,p),
  "2 billets 5 dans 6", dbinom(2,n,p),
  "3 billets 5 dans 6", dbinom(3,n,p),
  "4 billets 5 dans 6", dbinom(4,n,p),
  "5 billets 5 dans 6", dbinom(5,n,p),
  "6 billets 5 dans 6", dbinom(6,n,p),
  "Plus de 6 billets de 5 dans 6", pbinom(6,n,p,lower.tail = FALSE)
)
