library(tidyverse)

p <- function(){
  
  n <- 365
  temp <- rep(NA, n)
  temp[1] <- 1
  for (i in 2:n){
    temp[i] <- (n-i+1)/n*temp[i-1]
  }
  return(1-temp)
  
}

id <- 1:365
birthday <- tibble(n=id, `Probabilité de coïncidence`=p()) %>% 
  mutate(`Probabilité de non-coïncidence` = 1-`Probabilité de coïncidence`) %>% 
  pivot_longer(!n,names_to="Probabilités", values_to="proba")

birthday %>% ggplot(aes(x=n))+
  geom_line(aes(y=proba,color=`Probabilités`))+
  scale_x_continuous(breaks = seq(from=0,to=365,by=50))+
  labs(
    x = "Nombre de personnes",
    y = "Probabilité"
  )+
  theme(legend.position = "bottom")

birthday_table <- birthday %>% 
  filter(Probabilités=="Probabilité de coïncidence") %>% 
  filter(n %in% c(5,10,15,20,23,25,30,40,50,60,80,100,200,300,365)) %>% 
  select(n,proba)
