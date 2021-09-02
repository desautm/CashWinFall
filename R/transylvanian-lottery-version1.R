library(tidyverse)
library(scales)

pdeuce <- function(number) {
  
  p <- choose(3,2)*choose(4,1)/choose(7,3)
  proba <- dbinom(number,7,p)
  
  return(proba)
  
}

pthree <- function(number) {
  
  p <- choose(3,3)/choose(7,3)
  proba <- dbinom(number,7,p)
  
  return(proba)
  
}

transylvanian <- tibble (
  number = seq(from=0,to=7,by=1)
) %>% 
  mutate(
    p_deux = map_dbl(number,pdeuce),
    p_trois = map_dbl(number,pthree),
    prix_deux = 2*number*p_deux,
    prix_trois = 6*number*p_trois
  )

total_prix_deux = sum(transylvanian$prix_deux)
total_prix_trois = sum(transylvanian$prix_trois)

deuce <- transylvanian %>% 
  mutate(
    "Vous obtenez" = ifelse(number<2,sprintf("%.0f duo", number),sprintf("%.0f duos", number)),
    "Avec une probabilité de" = p_deux,
    "Et un prix de" = dollar(prix_deux)
  ) %>% 
  select(
    `Vous obtenez`,
    `Avec une probabilité de`,
    `Et un prix de`
  )

thrice <- transylvanian %>% 
  mutate(
    "Vous obtenez" = ifelse(number<2,sprintf("%.0f trio", number),sprintf("%.0f trios", number)),
    "Avec une probabilité de" = p_trois,
    "Et un prix de" = dollar(prix_trois)
  ) %>% 
  select(
    `Vous obtenez`,
    `Avec une probabilité de`,
    `Et un prix de`
  )
