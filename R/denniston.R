library(tidyverse)
library(here)
library(gtools)

# Données trouvées ici: https://www.dmgordon.org/steiner/
# https://ljcr.dmgordon.org/cover/LARGE/C_48_6_5.html

denniston <- read_csv(here("data","denniston.csv"), col_select = c(1,2,3,4,5,6), col_types = "iiiiii")
cashwinfall <- denniston %>% 
  filter(col1 < 47,
         col2 < 47,
         col3 < 47,
         col4 < 47,
         col5 < 47,
         col6 < 47)

reject <- denniston %>% 
  filter( col1 >= 47 | 
          col2 >= 47 | 
          col3 >= 47 | 
          col4 >= 47 | 
          col5 >= 47 | 
          col6 >= 47)

reject47 <- reject %>% 
  filter( col1 == 47 | 
          col2 == 47 | 
          col3 == 47 | 
          col4 == 47 | 
          col5 == 47 | 
          col6 == 47)

reject48 <- reject %>% 
  filter( col1 == 48 | 
          col2 == 48 | 
          col3 == 48 | 
          col4 == 48 | 
          col5 == 48 | 
          col6 == 48)

# On calcule le nombre de cas où le nombre 47 est présent
# On calcule le nombre de cas où le nombre 48 est présent
# On calcule le nombre de cas où 47 ET 48 sont présents
# en additionnant n47 et n48 et en soustrayant le nombre
# de cas rejetés de denniston
n47 <- nrow(reject47)
n48 <- nrow(reject48)
n47_48 <- n47+n48-nrow(reject)

ndenniston <- nrow(denniston)
ncashwinfall <- nrow(cashwinfall)
nreject <- nrow(reject)

p <- (n47+n48-n47_48)/(ndenniston-n47_48)

# allcombinations <-combinations(46,6,1:46)
# colnames(allcombinations) <- c("Numéro 1", "Numéro 2", "Numéro 3", "Numéro 4", "Numéro 5", "Numéro 6")
# loto <- as_tibble(allcombinations)
# save(loto, file="loto.Rdata")

load("loto.Rdata")

result <- rep(0,nrow(loto))
for (i in 1:100){
  num <- loto[i,]
  n <- 0
  for (j in 1:ncashwinfall){
    test <- sum(num %in% cashwinfall[j,])
    if (test == 5) n <- n+1
    if (n > 5) break
  }
  result[i] <- n
}

n <- 0
num <- loto[56,]
ncashwinfall <- nrow(cashwinfall)
for (j in 1:ncashwinfall){
  test <- sum(num %in% cashwinfall[j,])
  if (test == 5) n <- n+1
  if (n > 5) break
}

num <- loto[789562,]
cashwinfall %>% 
  rowwise() %>% 
  mutate(test = sum(num %in% c(col1,col2,col3,col4,col5,col6))) %>% 
  group_by(test) %>% 
  summarise(n())





