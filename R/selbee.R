library(tidyverse)

p <- choose(6,5)*choose(40,1)/choose(46,6)

selbee <- tribble(
  ~Résultat, ~Probabilité,
  "Aucun 5 dans 6", dbinom(0,217833,p),
  "1 billet 5 dans 6", dbinom(1,217833,p),
  "2 billet 5 dans 6", dbinom(2,217833,p),
  "3 billet 5 dans 6", dbinom(3,217833,p),
  "4 billet 5 dans 6", dbinom(4,217833,p),
  "5 billet 5 dans 6", dbinom(5,217833,p),
  "6 billet 5 dans 6", dbinom(6,217833,p),
  "Plus de 6 billets de 5 dans 6", pbinom(6,217833,p,lower.tail = FALSE)
)
