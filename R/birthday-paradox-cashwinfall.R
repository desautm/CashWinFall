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
  geom_line(aes(y=proba,color=`Probabilités`),size=1)+
  scale_x_continuous(breaks = seq(from=0,to=30000,by=5000))+
  labs(
    x = "Nombre de billets",
    y = "Probabilité"
  )+
  theme(legend.position = "bottom")+
  geom_hline(yintercept=0.5, color="orange", size=1)+
  geom_vline(xintercept=3604, color="orange", size=1)+
  annotate("text",x=4500,y=0,label = c("3604") , color="orange",size=5,fontface="bold")

cashwinfall_table <- cashwinfall %>% 
  filter(`Probabilités`=="Probabilité de coïncidence") %>% 
  filter(abs(proba-0.5)<0.0001)
