library(tidyverse)

p <- function(){
  
  n <- choose(46,6)
  temp <- rep(NA, n)
  temp[1] <- 1
  for (i in 2:n){
    temp[i] <- (n-i+1)/n*temp[i-1]
  }
  return(1-temp)
  
}

id <- 1:choose(46,6)
cashwinfall <- tibble(n=id, `Probabilité de coïncidence`=p()) %>% 
  filter(`Probabilité de coïncidence` < 1) %>% 
  mutate(`Probabilité de non-coïncidence` = 1-`Probabilité de coïncidence`) %>% 
  pivot_longer(!n,names_to="Probabilités", values_to="proba")

cashwinfall %>% ggplot(aes(x=n))+
  geom_line(aes(y=proba,color=`Probabilités`))+
  scale_x_continuous(breaks = seq(from=0,to=30000,by=5000))+
  labs(
    x = "Nombre de billets",
    y = "Probabilité"
  )+
  theme(legend.position = "bottom")

