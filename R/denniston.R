library(tidyverse)
library(here)

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
