fitRV <- stan_glm(P ~ offset(log(Conferiti)) , 
                    family="poisson", data = RV, seed = 123, control = list(adapt_delta = 0.99),
                    cores = 8)

 
RV2 <- RV %>% 
   group_by(Ageclass,YPeriod) %>% 
   summarise(P = sum(P), 
             C = sum(Conferiti))  %>% 
  ungroup()
   
   
 
  fitRV0<- stan_glmer(P ~  (1|codaz)+offset(log(Conferiti)), 
                     family="poisson", data = RV, seed = 123, control = list(adapt_delta = 0.99),
                     cores = 8)
 
 
 RV %>%
   #group_by(month) %>% 
   summarise(sP = sum(P),
             sC = sum(Conferiti)) %>% View()
View(RV)


sum(RV$P)
sum(RV$Conferiti)
773/999
fitRV0
exp(-0.3)


fitrv2 <- glm(P ~ Yperiod+ Ageclass+offset(log(C)), family = poisson(link = "log"), data = RV2)
## Using the offset option


fitRV2 <- stan_glm(P ~YPeriod+ Ageclass+offset(log(C)), 
                    family="poisson", data = RV2, seed = 123, control = list(adapt_delta = 0.99),
                    cores = 8)


tRV2 <- describe_posterior(
  fitRV2,
  centrality = "median",
  test = c("rope", "p_direction")
)

plot(tRV2)


