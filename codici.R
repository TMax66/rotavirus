source("librerie.R")
source("preparazione dati.R")



##Modelli di poisson-----
DT <- dt %>% 
  group_by(codaz, year, month, ageclass,  RV) %>% count() %>% 
  pivot_wider(names_from = "RV", values_from ="n", values_fill = 0 ) %>% 
  mutate(Conferiti = P+N, 
         YPeriod = recode(month, 
                          "01" = "Winter", 
                          "02" = "Winter",
                          "03" = "Spring",
                          "04" = "Spring",
                          "05" = "Spring",
                          "06" = "Summer",
                          "07" = "Summer",
                          "08" = "Summer",
                          "09" = "Autumn", 
                          "10" = "Autumn",
                          "11" = "Autumn",
                          "12" = "Autumn"), 
         prov = substr(codaz, 4,5)) %>% 
  select(-N)

DT <- DT %>% 
  filter(prov!= "FE")

fit0p <- stan_glmer(P ~ 1+(1|codaz)+offset(log(Conferiti)), 
                    family="poisson", data = DT, seed = 123, control = list(adapt_delta = 0.99),
                    cores = 8)







library(rstanarm)



fit0 <- stan_glmer(P ~ 1+(1|codaz)+offset(log(Conferiti)), 
                      family="poisson", data = DT, seed = 123, control = list(adapt_delta = 0.99),
                      cores = 8)
 

###modello per rate province
fit1 <- stan_glmer(P ~ 0+prov+(1|codaz)+offset(log(Conferiti)), 
                   family="poisson", data = DT, seed = 123, control = list(adapt_delta = 0.99),
                   cores = 8)

rate <- fit1$coefficients %>% as.data.frame()  

rate <-  rownames_to_column(rate, "prov")
rate <- rate[1:29,]

rate <- rate %>% 
  mutate(prov= recode(prov, 
                      "provAP" = "Ascoli Piceno", 
                      "provBG" = "Bergamo", 
                      "provBI" ="Biella",
                      "provBL" = "Belluno",
                      "provBO" ="Bologna",
                      "provBS" ="Brescia",
                      "provCN" = "Cuneo",
                      "provCR" = "Cremona",
                      "provCS" ="Cosenza",
                      "provEN" = "Enna", 
                      "provLO" =  "Lodi",
                      "provMC" = "Macerata",
                      "provMI" =  "Milano",
                      "provMN" =    "Mantova",
                      "provMO" = "Modena",
                      "provNO" =  "Novara",
                      "provPC" = "Piacenza",
                      "provPD" = "Padova",
                      "provPN" =  "Pordenone",
                      "provPR" ="Parma",
                      "provPV" ="Pavia", 
                      "provPZ" ="Potenza",
                      "provRC" = "Reggio Calabria",
                      "provRO" ="Rovigo",
                      "provTO" =  "Torino",
                      "provTV" = "Treviso", 
                      "provVC" ="Vercelli",
                      "provVI" ="Viterbo",
                      "provVR" ="Verona"  
  ), 
  rate = exp(.)) 



library(tmap)
library(GADMTools)
library(tmaptools)

ITA <- GADMTools::gadm_sf_loadCountries("ITA", level=0,basefile = "data/")$sf
REG <- GADMTools::gadm_sf_loadCountries("ITA", level=1,basefile = "data/")$sf
province   <-GADMTools::gadm_sf_loadCountries("ITA", level=2,basefile = "data/")$sf

pr <- c("Ascoli Piceno", "Bergamo",  "Biella",  "Belluno",  "Bologna",  "Brescia",  "Cuneo", 
        "Cremona",  "Cosenza",  "Enna",  "Lodi",  "Macerata", "Milano", "Mantova",  "Modena",  
        "Novara", "Piacenza",  "Padova",  "Pordenone",  "Parma",  "Pavia",  "Potenza", 
        "Reggio Calabria", "Rovigo", "Torino",  "Treviso",  "Vercelli",  "Viterbo",  "Verona")

mapPr<- province %>%  
  filter(NAME_2 %in%pr) %>% 
  left_join(rate, by = c("NAME_2" = "prov"))


RV <- tm_shape(ITA)+tm_fill("white")+tm_borders("gray")+
  tm_shape(REG)+tm_fill("white")+tm_borders("black")+
  tm_shape(mapPr, id= "Name_2")+tm_fill("rate", palette = "Blues" )+tm_borders("black")+
  tm_layout(main.title = "Rotavirus positive rate of pigs enteric cases ",
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)+
  tm_scale_bar(breaks = c(0, 50, 100), text.size = .5,position = "left")+
  tm_compass(type = "8star", position = c("right", "bottom")) 
  
  
  








library(sjPlot)
###RV----

fit2 <- stan_glmer(P ~  YPeriod+ ageclass+(1|codaz)+offset(log(Conferiti)), 
                   family="poisson", data = DT, seed = 123, control = list(adapt_delta = 0.99),
                   cores = 8)


plot_model(fit2,   show.values = TRUE, value.offset = .3) +
  theme_ipsum_rc()


tab_model(fit2)

plot(p_direction(fit2))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()
  


###RVA----

RVA <- dt %>% 
  group_by(codaz, year, month, ageclass,  RVA) %>% count() %>%  
  pivot_wider(names_from = "RVA", values_from ="n", values_fill = 0 ) %>% 
  mutate(Conferiti = P+N, 
         YPeriod = recode(month, 
                          "01" = "Winter", 
                          "02" = "Winter",
                          "03" = "Spring",
                          "04" = "Spring",
                          "05" = "Spring",
                          "06" = "Summer",
                          "07" = "Summer",
                          "08" = "Summer",
                          "09" = "Autumn", 
                          "10" = "Autumn",
                          "11" = "Autumn",
                          "12" = "Autumn"), 
         prov = substr(codaz, 4,5)) %>% 
  select(-N)

RVA <- RVA %>% 
  filter(prov!= "FE")

fitRVA <- stan_glmer(P ~  YPeriod+ ageclass+(1|codaz)+offset(log(Conferiti)), 
                 family="poisson", data = RVA, seed = 123, control = list(adapt_delta = 0.99),
                 cores = 8)

plot_model(fitRVA, type = "est",show.values = TRUE,  value.offset = .3,show.intercept = T) +
  theme_ipsum_rc()
plot(p_direction(fit2))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()


#RVA modello per mappa

RVAmp <- stan_glmer(P ~ 0+prov+(1|codaz)+offset(log(Conferiti)), 
                   family="poisson", data = RVA, seed = 123, control = list(adapt_delta = 0.99),
                   cores = 8)

rate <- RVAmp$coefficients %>% as.data.frame()  

rate <-  rownames_to_column(rate, "prov")
rate <- rate[1:29,]

rate <- rate %>% 
  mutate(prov= recode(prov, 
                      "provAP" = "Ascoli Piceno", 
                      "provBG" = "Bergamo", 
                      "provBI" ="Biella",
                      "provBL" = "Belluno",
                      "provBO" ="Bologna",
                      "provBS" ="Brescia",
                      "provCN" = "Cuneo",
                      "provCR" = "Cremona",
                      "provCS" ="Cosenza",
                      "provEN" = "Enna", 
                      "provLO" =  "Lodi",
                      "provMC" = "Macerata",
                      "provMI" =  "Milano",
                      "provMN" =    "Mantova",
                      "provMO" = "Modena",
                      "provNO" =  "Novara",
                      "provPC" = "Piacenza",
                      "provPD" = "Padova",
                      "provPN" =  "Pordenone",
                      "provPR" ="Parma",
                      "provPV" ="Pavia", 
                      "provPZ" ="Potenza",
                      "provRC" = "Reggio Calabria",
                      "provRO" ="Rovigo",
                      "provTO" =  "Torino",
                      "provTV" = "Treviso", 
                      "provVC" ="Vercelli",
                      "provVI" ="Viterbo",
                      "provVR" ="Verona"  
  ), 
  rate = exp(.)) 


mapPr<- province %>%  
  filter(NAME_2 %in%pr) %>% 
  left_join(rate, by = c("NAME_2" = "prov"))


RVA <- tm_shape(ITA)+tm_fill("white")+tm_borders("gray")+
  tm_shape(REG)+tm_fill("white")+tm_borders("black")+
  tm_shape(mapPr, id= "Name_2")+tm_fill("rate", palette = "Blues" )+tm_borders("black")+
  tm_layout(main.title = "Rotavirus A positive rate of pigs enteric cases ",
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)+
  tm_scale_bar(breaks = c(0, 50, 100), text.size = .5,position = "left")+
  tm_compass(type = "8star", position = c("right", "bottom")) 
  






###RVB---
RVB <- dt %>% 
  group_by(codaz, year, month, ageclass,  RVB) %>% count() %>%  
  pivot_wider(names_from = "RVB", values_from ="n", values_fill = 0 ) %>% 
  mutate(Conferiti = P+N, 
         YPeriod = recode(month, 
                          "01" = "Winter", 
                          "02" = "Winter",
                          "03" = "Spring",
                          "04" = "Spring",
                          "05" = "Spring",
                          "06" = "Summer",
                          "07" = "Summer",
                          "08" = "Summer",
                          "09" = "Autumn", 
                          "10" = "Autumn",
                          "11" = "Autumn",
                          "12" = "Autumn"), 
         prov = substr(codaz, 4,5)) %>% 
  select(-N)

RVB <- RVB %>% 
  filter(prov!= "FE")
  
RVBmp <- stan_glmer(P ~ 0+prov+(1|codaz)+offset(log(Conferiti)), 
                    family="poisson", data = RVB, seed = 123, control = list(adapt_delta = 0.99),
                    cores = 8)

rate <- RVBmp$coefficients %>% as.data.frame()  

rate <-  rownames_to_column(rate, "prov")
rate <- rate[1:29,]

rate <- rate %>% 
  mutate(prov= recode(prov, 
                      "provAP" = "Ascoli Piceno", 
                      "provBG" = "Bergamo", 
                      "provBI" ="Biella",
                      "provBL" = "Belluno",
                      "provBO" ="Bologna",
                      "provBS" ="Brescia",
                      "provCN" = "Cuneo",
                      "provCR" = "Cremona",
                      "provCS" ="Cosenza",
                      "provEN" = "Enna", 
                      "provLO" =  "Lodi",
                      "provMC" = "Macerata",
                      "provMI" =  "Milano",
                      "provMN" =    "Mantova",
                      "provMO" = "Modena",
                      "provNO" =  "Novara",
                      "provPC" = "Piacenza",
                      "provPD" = "Padova",
                      "provPN" =  "Pordenone",
                      "provPR" ="Parma",
                      "provPV" ="Pavia", 
                      "provPZ" ="Potenza",
                      "provRC" = "Reggio Calabria",
                      "provRO" ="Rovigo",
                      "provTO" =  "Torino",
                      "provTV" = "Treviso", 
                      "provVC" ="Vercelli",
                      "provVI" ="Viterbo",
                      "provVR" ="Verona"  
  ), 
  rate = exp(.)) 


mapPr<- province %>%  
  filter(NAME_2 %in%pr) %>% 
  left_join(rate, by = c("NAME_2" = "prov"))


RVB <- tm_shape(ITA)+tm_fill("white")+tm_borders("gray")+
  tm_shape(REG)+tm_fill("white")+tm_borders("black")+
  tm_shape(mapPr, id= "Name_2")+tm_fill("rate", palette = "Blues" )+tm_borders("black")+
  tm_layout(main.title = "Rotavirus B positive rate of pigs enteric cases ",
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)+
  tm_scale_bar(breaks = c(0, 50, 100), text.size = .5,position = "left")+
  tm_compass(type = "8star", position = c("right", "bottom")) 



###RVC---
RVC <- dt %>% 
  group_by(codaz, year, month, ageclass,  RVC) %>% count() %>%  
  pivot_wider(names_from = "RVC", values_from ="n", values_fill = 0 ) %>% 
  mutate(Conferiti = P+N, 
         YPeriod = recode(month, 
                          "01" = "Winter", 
                          "02" = "Winter",
                          "03" = "Spring",
                          "04" = "Spring",
                          "05" = "Spring",
                          "06" = "Summer",
                          "07" = "Summer",
                          "08" = "Summer",
                          "09" = "Autumn", 
                          "10" = "Autumn",
                          "11" = "Autumn",
                          "12" = "Autumn"), 
         prov = substr(codaz, 4,5)) %>% 
  select(-N)

RVC <- RVC %>% 
  filter(prov!= "FE")

RVCmp <- stan_glmer(P ~ 0+prov+(1|codaz)+offset(log(Conferiti)), 
                    family="poisson", data = RVC, seed = 123, control = list(adapt_delta = 0.99),
                    cores = 8)


rate <- RVCmp$coefficients %>% as.data.frame()  

rate <-  rownames_to_column(rate, "prov")
rate <- rate[1:29,]

rate <- rate %>% 
  mutate(prov= recode(prov, 
                      "provAP" = "Ascoli Piceno", 
                      "provBG" = "Bergamo", 
                      "provBI" ="Biella",
                      "provBL" = "Belluno",
                      "provBO" ="Bologna",
                      "provBS" ="Brescia",
                      "provCN" = "Cuneo",
                      "provCR" = "Cremona",
                      "provCS" ="Cosenza",
                      "provEN" = "Enna", 
                      "provLO" =  "Lodi",
                      "provMC" = "Macerata",
                      "provMI" =  "Milano",
                      "provMN" =    "Mantova",
                      "provMO" = "Modena",
                      "provNO" =  "Novara",
                      "provPC" = "Piacenza",
                      "provPD" = "Padova",
                      "provPN" =  "Pordenone",
                      "provPR" ="Parma",
                      "provPV" ="Pavia", 
                      "provPZ" ="Potenza",
                      "provRC" = "Reggio Calabria",
                      "provRO" ="Rovigo",
                      "provTO" =  "Torino",
                      "provTV" = "Treviso", 
                      "provVC" ="Vercelli",
                      "provVI" ="Viterbo",
                      "provVR" ="Verona"  
  ), 
  rate = exp(.)) 


mapPr<- province %>%  
  filter(NAME_2 %in%pr) %>% 
  left_join(rate, by = c("NAME_2" = "prov"))


RVC <- tm_shape(ITA)+tm_fill("white")+tm_borders("gray")+
  tm_shape(REG)+tm_fill("white")+tm_borders("black")+
  tm_shape(mapPr, id= "Name_2")+tm_fill("rate", palette = "Blues" )+tm_borders("black")+
  tm_layout(main.title = "Rotavirus C  positive rate of pigs enteric cases ",
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)+
  tm_scale_bar(breaks = c(0, 50, 100), text.size = .5,position = "left")+
  tm_compass(type = "8star", position = c("right", "bottom")) 




###RVH---
RVH <- dt %>% 
  group_by(codaz, year, month, ageclass,  RVH) %>% count() %>%  
  pivot_wider(names_from = "RVH", values_from ="n", values_fill = 0 ) %>% 
  mutate(Conferiti = P+N, 
         YPeriod = recode(month, 
                          "01" = "Winter", 
                          "02" = "Winter",
                          "03" = "Spring",
                          "04" = "Spring",
                          "05" = "Spring",
                          "06" = "Summer",
                          "07" = "Summer",
                          "08" = "Summer",
                          "09" = "Autumn", 
                          "10" = "Autumn",
                          "11" = "Autumn",
                          "12" = "Autumn"), 
         prov = substr(codaz, 4,5)) %>% 
  select(-N)

RVH <- RVH %>% 
  filter(prov!= "FE")

RVHmp <- stan_glmer(P ~ 0+prov+(1|codaz)+offset(log(Conferiti)), 
                    family="poisson", data = RVH, seed = 123, control = list(adapt_delta = 0.99),
                    cores = 8)

rate <- RVHmp$coefficients %>% as.data.frame()  

rate <-  rownames_to_column(rate, "prov")
rate <- rate[1:29,]

rate <- rate %>% 
  mutate(prov= recode(prov, 
                      "provAP" = "Ascoli Piceno", 
                      "provBG" = "Bergamo", 
                      "provBI" ="Biella",
                      "provBL" = "Belluno",
                      "provBO" ="Bologna",
                      "provBS" ="Brescia",
                      "provCN" = "Cuneo",
                      "provCR" = "Cremona",
                      "provCS" ="Cosenza",
                      "provEN" = "Enna", 
                      "provLO" =  "Lodi",
                      "provMC" = "Macerata",
                      "provMI" =  "Milano",
                      "provMN" =    "Mantova",
                      "provMO" = "Modena",
                      "provNO" =  "Novara",
                      "provPC" = "Piacenza",
                      "provPD" = "Padova",
                      "provPN" =  "Pordenone",
                      "provPR" ="Parma",
                      "provPV" ="Pavia", 
                      "provPZ" ="Potenza",
                      "provRC" = "Reggio Calabria",
                      "provRO" ="Rovigo",
                      "provTO" =  "Torino",
                      "provTV" = "Treviso", 
                      "provVC" ="Vercelli",
                      "provVI" ="Viterbo",
                      "provVR" ="Verona"  
  ), 
  rate = exp(.)) 


mapPr<- province %>%  
  filter(NAME_2 %in%pr) %>% 
  left_join(rate, by = c("NAME_2" = "prov"))


RVH <- tm_shape(ITA)+tm_fill("white")+tm_borders("gray")+
  tm_shape(REG)+tm_fill("white")+tm_borders("black")+
  tm_shape(mapPr, id= "Name_2")+tm_fill("rate", palette = "Blues" )+tm_borders("black")+
  tm_layout(main.title = "Rotavirus H  positive rate of pigs enteric cases ",
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)+
  tm_scale_bar(breaks = c(0, 50, 100), text.size = .5,position = "left")+
  tm_compass(type = "8star", position = c("right", "bottom")) 






###Influenza di RV su infezioni batteriche---
##il tasso di positività a brachyspira cambia in caso di positività a RV e come?

###Brachy---

Brachy <- dt %>% 
  filter(!is.na(brachyod) | !is.na(brachypil)) %>% 
  mutate(Brachyspira = ifelse(brachyod=="Brachyod", "Pos", 
                              ifelse(brachypil== "Brachypil", "Pos", "Neg"))) %>%  
  select(Brachyspira, RVA,RVB, RVC, RVH, RV, ageclass,year, month, codaz) %>%  
  group_by(codaz, year, month, ageclass,RVA,RVB, RVC, RVH, RV, Brachyspira) %>% count()  %>% 
  pivot_wider(names_from = "Brachyspira", values_from ="n", values_fill = 0 ) %>% 
  mutate(Conferiti = Pos+Neg, 
         YPeriod = recode(month, 
                          "01" = "Winter", 
                          "02" = "Winter",
                          "03" = "Spring",
                          "04" = "Spring",
                          "05" = "Spring",
                          "06" = "Summer",
                          "07" = "Summer",
                          "08" = "Summer",
                          "09" = "Autumn", 
                          "10" = "Autumn",
                          "11" = "Autumn",
                          "12" = "Autumn"), 
         prov = substr(codaz, 4,5)) %>% 
  select(-Neg)

Brachy <- Brachy %>% 
  filter(prov!= "FE" & ageclass!= "svezzamento")  
  

fitBrachy1 <- stan_glmer(Pos ~  YPeriod+ ageclass+ RV + (1|codaz)+offset(log(Conferiti)), 
                     family="poisson", data = Brachy, seed = 123, control = list(adapt_delta = 0.99),
                     cores = 8)
  
plot_model(fitBrachy1, type = "est",show.values = TRUE,  value.offset = .3,show.intercept = T) +
  theme_ipsum_rc()
plot(p_direction(fitBrachy1))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()



fitBrachy12 <- stan_glmer(Pos ~  YPeriod+ ageclass+ RVA+RVB+RVC+RVH + (1|codaz)+offset(log(Conferiti)), 
                         family="poisson", data = Brachy, seed = 123, control = list(adapt_delta = 0.99),
                         cores = 8)

plot_model(fitBrachy12, type = "est",show.values = TRUE,  value.offset = .3,show.intercept = T) +
  theme_ipsum_rc()
plot(p_direction(fitBrachy12))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()


##modello binomiale---

modbrachy <- stan_glmer(formula = Brachyspira ~   stagione + ageclass+(1|codaz)
                        + RVA + RVB + RVC + RVH,
                        data=Brachybinom,
                        family = binomial(link = logit), 
                        cores=8,
                        seed = 1966)



plot_model(modbrachy, type = "est",show.values = TRUE, value.offset = .3) +
  theme_ipsum_rc()

plot(p_direction(modbrachy))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()



modclost <- stan_glmer(formula = Clostridi ~   stagione + ageclass+(1|codaz)
                       + RVA + RVB + RVC + RVH,
                       data=Clostr,
                       family = binomial(link = logit), 
                       cores=8,
                       seed = 1966)
plot_model(modclost, type = "est",show.values = TRUE, value.offset = .3) +
  theme_ipsum_rc()

plot(p_direction(modclost))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()



modlaws <- stan_glmer(formula = Lawsonia ~   stagione + ageclass+(1|codaz)
                      + RVA + RVB + RVC + RVH,
                      data=Lawsonia,
                      family = binomial(link = logit), 
                      cores=8,
                      seed = 1966)

plot_model(modlaws, type = "est",show.values = TRUE, value.offset = .3) +
  theme_ipsum_rc()
plot(p_direction(modlaws))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()

 

#maps

library(tmap)
library(GADMTools)
library(tmaptools)

ITA <- GADMTools::gadm_sf_loadCountries("ITA", level=0,basefile = "data/")$sf
REG <- GADMTools::gadm_sf_loadCountries("ITA", level=1,basefile = "data/")$sf
province   <-GADMTools::gadm_sf_loadCountries("ITA", level=2,basefile = "data/")$sf
 
pr <- c("Ascoli Piceno", "Bergamo",  "Biella",  "Belluno",  "Bologna",  "Brescia",  "Cuneo", 
        "Cremona",  "Cosenza",  "Enna",  "Lodi",  "Macerata", "Milano", "Mantova",  "Modena",  
        "Novara", "Piacenza",  "Padova",  "Pordenone",  "Parma",  "Pavia",  "Potenza", 
        "Reggio Calabria", "Rovigo", "Torino",  "Treviso",  "Vercelli",  "Viterbo",  "Verona")

mapPr<- province %>%  
  filter(NAME_2 %in%pr) %>% 
  left_join (rate, by = c("NAME_2" = "prov"))# usa il data.frame rate per prendere i tassi per provincia stimati col modello


tmap_mode("plot")
tm_shape(ITA)+tm_fill(col = "white")+tm_borders("gray")+
  tm_shape(REG)+tm_fill(col = "white")+tm_borders("black")+
  tm_shape(mapPr)+tm_fill("rate", id = "NAME_2", alpha = 0.5, palette = "Blues")+tm_borders("black")+
  tm_scale_bar(breaks = c(0, 100, 200), text.size = .8,position = "left")+
  tm_layout(main.title = "Rotavirus A positive rate of pigs enteric cases ",
            title.size = 0.6,
            legend.title.size = 0.6,
            legend.text.size = 0.6,
            legend.position = c("right", "top"),
            legend.bg.color = "white",
             
            legend.bg.alpha = 1)
  
 

 
 



##Modelli di regressione rotavirus-----

# RV <- stan_glmer(RV~0+as.factor(stagione)+as.factor(ageclass)+(1|codaz), 
#            data = Rota, 
#            family = binomial(link = logit), 
#            # prior = student_t(df=7, location = 0, scale = 2.5), 
#            # prior_intercept = student_t(df=7, location = 0, scale = 2.5), 
#            cores = 8, seed = 1966)
# RVloo <- loo(RV)
# 
# RVint <- stan_glmer(RV~stagione*ageclass+(1|codaz), 
#                  data = Rota, 
#                  family = binomial(link = logit), 
#                  # prior = student_t(df=7, location = 0, scale = 2.5), 
#                  # prior_intercept = student_t(df=7, location = 0, scale = 2.5), 
#                  cores = 8, seed = 1966)
# 
# RVintloo <- loo(RVint)

library(sjPlot)

 plot_model(RV, type = "est", show.values = TRUE, value.offset = .3) +
   theme_ipsum_rc()

RVA<- stan_glmer(RVA~stagione+ageclass+(1|codaz), 
                 data = Rota, 
                 family = binomial(link = logit), 
                 prior = student_t(df=7, location = 0, scale = 2.5), 
                 prior_intercept = student_t(df=7, location = 0, scale = 2.5), 
                 cores = 8, seed = 1966)
plot_model(RVA, type = "est",show.values = TRUE,transform = NULL, value.offset = .3,show.intercept = T) +
  theme_ipsum_rc()

RVB <- stan_glmer(RVB~stagione+ageclass+(1|codaz), 
                 data = Rota, 
                 family = binomial(link = logit), 
                 prior = student_t(df=7, location = 0, scale = 2.5), 
                 prior_intercept = student_t(df=7, location = 0, scale = 2.5), 
                 cores = 8, seed = 1966)
plot_model(RVB, type = "est",show.values = TRUE, value.offset = .3) +
  theme_ipsum_rc()

RVC <- stan_glmer(RVC~stagione+ageclass+(1|codaz), 
                 data = Rota, 
                 family = binomial(link = logit), 
                 prior = student_t(df=7, location = 0, scale = 2.5), 
                 prior_intercept = student_t(df=7, location = 0, scale = 2.5), 
                 cores = 8, seed = 1966)
plot_model(RVC, type = "est",show.values = TRUE, value.offset = .3) +
  theme_ipsum_rc()


RVH <- stan_glmer(RVH~stagione+ageclass+(1|codaz), 
                 data = Rota, 
                 family = binomial(link = logit), 
                 prior = student_t(df=7, location = 0, scale = 2.5), 
                 prior_intercept = student_t(df=7, location = 0, scale = 2.5), 
                 cores = 8, seed = 1966)

plot_model(RVH, type = "est",show.values = TRUE, value.offset = .3) +
  theme_ipsum_rc()

##Modelli di regressione predizione infezioni batteriche----
modbrachy <- stan_glmer(formula = Brachyspira ~   stagione +(1|codaz)
                 + RVA + RVB + RVC + RVH,
                 data=Brachy,
                 family = binomial(link = logit), 
                 cores=8,
                 seed = 1966)



plot_model(modbrachy, type = "est",show.values = TRUE, value.offset = .3) +
  theme_ipsum_rc()

plot(p_direction(modbrachy))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()


modbrachy <- brm(formula = Brachyspira ~  0+ stagione +(1|codaz)
                           + RVA + RVB + RVC + RVH,
           data=Brachy,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 4000,
           chains = 4,
           inits= "0",
           cores=8,
           seed = 123)



# modclst <- brm(formula = Clostridi ~   stagione +(1|codaz)
#                  + RVA + RVB + RVC + RVH + ageclass,
#                  data=Clostr,
#                  family = bernoulli(link = "logit"),
#                  warmup = 1000,
#                  iter = 4000,
#                  chains = 4,
#                  inits= "0",
#                  cores=8,
#                  seed = 123)

modclost <- stan_glmer(formula = Clostridi ~   stagione +(1|codaz)
                        + RVA + RVB + RVC + RVH,
                        data=Clostr,
                        family = binomial(link = logit), 
                        cores=8,
                        seed = 1966)

plot(p_direction(modclost))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()


modlaws <- stan_glmer(formula = Lawsonia ~   stagione +(1|codaz)
                       + RVA + RVB + RVC + RVH,
                       data=Lawsonia,
                       family = binomial(link = logit), 
                       cores=8,
                       seed = 1966)

plot(p_direction(modlaws))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()


#OLD STUFF----

# dtYM <- expand_grid(codaz = levels(factor(dt$codaz)),
#   year = c("2016", "2017",  "2018",  "2019"), 
#                     month = c( "01", "02",  "03",  "04",  "05",  "06",  "07", "08", "09", "10",  "11",  "12" ), 
# ageclass = levels(factor(dt$ageclass)), 
#  
# )

# DT <- dt %>% 
#   group_by(codaz, year, month, ageclass,  RV) %>% count() %>% 
#   pivot_wider(names_from = "RV", values_from ="n", values_fill = 0 ) %>% 
#   mutate(Conferiti = P+N) %>% 
#      right_join(dtYM, 
#               by = c("codaz", "year", "month", "ageclass"))    
#    
#   
# DT[is.na(DT)] <- 0
# 
# DT <- DT %>% 
#   mutate(quarter = recode(month, 
#                          "01" = "Q1", 
#                          "02" = "Q1",
#                          "03" = "Q1",
#                          "04" = "Q2",
#                          "05" = "Q2",
#                          "06" = "Q2",
#                          "07" = "Q3",
#                          "08" = "Q3",
#                          "09" = "Q3", 
#                          "10" = "Q4",
#                          "11" = "Q4",
#                          "12" = "Q4"), 
#          prov = substr(codaz, 4,5))



# mymodfun <- function(df, y){  
#   
#   mod <-stan_glmer(formula = paste(y, "~",  "stagione", "+", "ageclass", "+","(1|codaz)"),
#                    data=df,
#                    family = binomial(),
#                    cores=8,
#                    seed = 123)
#   plot(p_direction(mod))+scale_fill_brewer(palette="Blues")+
#     theme_ipsum_rc()
# }

# RV <- mymodfun(df=Rota, y = "RV")
# RVA <- mymodfun(df=Rota, y = "RVA")
# RVB <- mymodfun(df=Rota, y = "RVB")
# RVC <- mymodfun(df=Rota, y = "RVC")
# RVH <- mymodfun(df=Rota, y = "RVH")

# mymodfun <- function(df, y){
#   
#   mod <- brm(formula = paste(y, "~",  "stagione", "+", "ageclass", "+","(1|codaz)"),
#              data=df,
#              family = bernoulli(link = "logit"),
#              warmup = 1000,
#              iter = 4000,
#              chains = 4,
#              inits= "0",
#              cores=8,
#              seed = 123)
#   plot(p_direction(mod))+scale_fill_brewer(palette="Blues")+
#     theme_ipsum_rc()
# }
# 




# 
# mymodfun2 <- function(df, y){  
#   mod <- brm(formula = paste(y, "~",  "stagione", "+", "ageclass", "+","(1|codaz)",
#                              "+","RVA","+","RVB","+","RVC", "+","RVH"),
#              data=df,
#              family = bernoulli(link = "logit"),
#              warmup = 1000,
#              iter = 4000,
#              chains = 4,
#              inits= "0",
#              cores=8,
#              seed = 123)
#   plot(p_direction(mod))+scale_fill_brewer(palette="Blues")+
#     theme_ipsum_rc()
# }
# 
# 
# RV <- mymodfun(df=Rota, y = "RV")
# RVA <- mymodfun(df=Rota, y = "RVA")
# RVB <- mymodfun(df=Rota, y = "RVB")
# RVC <- mymodfun(df=Rota, y = "RVC")
# RVH <- mymodfun(df=Rota, y = "RVH")


### Profili di coeinfezione----

dt[, c(28:31, 33:39)] <- dt[, c(28:31, 33:39)]!=0

nomi_abb<-toupper(names(dt)[c(28:31, 33:39)])
X<-  apply(dt[, c(28:31, 33:39)], 1, function(x) nomi_abb[x])
XX<-lapply(X, paste, collapse="/")
dt$profilo<-unlist(XX)


dt <- dt %>% 
  mutate(profilo = str_remove_all(profilo, "/NA"),
         profilo = str_remove_all(profilo, "NA/"), 
         profilo= ifelse(profilo == "NA", "NEG", profilo))


dt %>% drop_na( profilo) %>% 
  group_by(profilo) %>% 
  summarise(n = n()) %>% 
  mutate(frq = n/sum(n)) %>% 
  mutate(profilo = fct_reorder(profilo,frq)) %>% 
  top_n(30) %>% 
  ggplot(aes(x = profilo ,  y = n, label = round(100*frq, 2))) +
  coord_flip()+
  geom_point( aes(x=profilo, y=n), size=12, color="steelblue" )+
  geom_text(aes(x = profilo, y = n), color="white", size=4)+
  geom_segment( aes(x=profilo, xend=profilo, y=0, yend=n), color="grey")+
  theme_ipsum_rc()+
  labs(y="n.coinf",x="")


#Analisi delle corrispondenze multiple----

dt <- dt[, c(9:13, 15, 17, 41 )]
#plot_bar(dt)

dt <- dt %>% 
  na.omit()

cats <- apply(dt, 2, function(x) nlevels(as.factor(x)))


res.mca <- MCA(dt, quali.sup = c(8), graph = FALSE)

fviz_mca_ind(res.mca)


fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
fviz_mca_biplot(res.mca, 
               # repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal(), 
                # alpha.ind = 0.3, 
                geom.ind = "point")

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            # repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

fviz_contrib(res.mca, choice = "ind", axes = 1, top = 20)

fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

fviz_ellipses(res.mca, c("PEDV", "RVA"),
              geom = "point")

mca1_vars_df <-  data.frame(res.mca$var$coord, Variable = rep(names(cats), 
                                                              cats))
mca1_obs_df <-  data.frame(res.mca$ind$coord)



ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + 
  geom_hline(yintercept = 0,colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_point(colour = "gray50", alpha = 0.7) + 
  geom_density2d(colour = "gray80") + 
  geom_text(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable)) + 
  ggtitle("MCA") + 
  scale_colour_discrete(name = "Variable")+
  theme_ipsum()




# nomi_abb<-toupper(names(dt)[c(32:39)])
# X<-  apply(dt[, c(32:39)], 1, function(x) nomi_abb[x])
# XX<-lapply(X, paste, collapse="/")
# dt$profilo2<-unlist(XX)
# 
# dt <- dt %>% 
#   mutate(profilo2 = str_remove_all(profilo2, "/NA"),
#          profilo2 = str_remove_all(profilo2, "NA/"), 
#          profilo2= ifelse(profilo2 == "NA", "NEG", profilo2))
# 
# dt %>% drop_na( profilo2) %>% 
#   group_by(profilo2) %>% 
# summarise(n = n()) %>% 
#  mutate(frq = n/sum(n)) %>% 
#   mutate(profilo2 = fct_reorder(profilo2,frq)) %>% 
#   top_n(30) %>% 
#   ggplot(aes(x = profilo2 ,  y = frq, label = frq)) +
#   geom_segment( aes(x=profilo2, xend=profilo2, y=0, yend=n), color="grey")+
#   geom_point( aes(x=profilo2, y=n), size=4.5, color="steelblue" )+
#   #geom_text(color="black", size=2)+
#   coord_flip()+
#   theme_ipsum_rc()+
#   labs(y="n.coinf",x="")
# 
#     
# 
# 



  
# 
# dt %>% 
#   count(M = floor_date(dtprelievo, "week")) %>% 
#   mutate(m = rollmean(n, k = 4, fill = NA) )%>%   
#   ggplot(aes(M, n))+
#   geom_line()+
#   geom_line(aes(x=M, y=m), col = "blue", size = 1.5)+
#   theme_ipsum_rc()+
#   labs(y="n.coinf",x="", y = "n.casi")
# 
# 
# 
# 
#   
#   pivot_wider(names_from = RV, values_from = n, values_fill = 0) %>% 
#   mutate(
#     Casi = P+N, 
#     Prev = P/(P+N)) %>% View()
#   ggplot(aes(x = Month, y= Casi))+
#   geom_line()
# 
# 
# dt %>% 
#   group_by(dtprelievo,  RV) %>% 
#   summarise(n= n()) %>% View()
#   pivot_wider(names_from = RV, values_from = n, values_fill = 0) %>% View()
#   mutate(Prev = P/(P+N)) %>% 
#   ggplot(aes(x = dtprelievo, y= n))+
#   geom_line()
# 
# 
# dt %>% filter(RVA == "P" & RVH == "P") %>% 
#   count(M = floor_date(dtprelievo, "month")) %>% 
#   mutate(m = rollmean(n, k = 3, fill = NA) )%>%   
#   ggplot(aes(M, n))+
#   geom_line()+
#   geom_line(aes(x=M, y=m), col = "blue", size = 1.5)
# 


# #ANALISI DELLE CORRISPONDENZE
# 
# library("gplots")
# 
# library(fastDummies)
# 
# dummydt <- dt %>% 
#   select(11:13, 15, 17, 18:22) %>% 
#   dummy_cols(remove_selected_columns = TRUE)  
# 
# 
# 
# df<-data.frame(dt[, 9],dummydt)
# 
# 
# 
# 
# 
# 
# tabella <-  df %>% 
#   group_by(ageclass) %>% 
#   summarise_all(sum, na.rm = T)  %>%  
#   select(ageclass, ends_with("P")) %>% View()
#   column_to_rownames(var="ageclass") %>% 
#   as.data.frame() %>% 
#   select(1:5)   
# 
# #Math CA
# 
# n <- sum(tabella)
# P = tabella / n
# 
# dati %>% 
#  group_by( ageclass) %>% 
#   summarise(n = n())
# 
# 
# # dt <- as.table(as.matrix(tabella))
# # balloonplot(t(dt), main =" ", xlab ="", ylab="",
# #             label = FALSE, show.margins = FALSE)
# # 
# # 
# # # Create row profile
# # data.ca.row <-  tabella/rowSums(tabella)
# # View(data.ca.row)
# # mass.row <- colMeans(data.ca.row)
# # 
# # # Create column profile
# # data.ca.col = t(tabella)/colSums(tabella)
# # View(t(data.ca.col))
# # mass.col = rowMeans(t(data.ca.col))
# 
# 
# 
# #CA
# res.ca<-CA(tabella, graph = FALSE)
# 
# library(Factoshiny)
# Factoshiny(res.ca)
# 
# 
# 
# summary(res.ca)
# fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 100))
# fviz_screeplot(res.ca) +
#   geom_hline(yintercept=11.1, linetype=2, color="red")
# fviz_ca_biplot(res.ca, repel = TRUE)
# 
# 
# fviz_ca_biplot(res.ca, 
#                map ="colprincipal", arrow = c(TRUE, TRUE),
#                repel = TRUE)
# 
# fviz_ca_row(res.ca, repel = TRUE)
# fviz_ca_row(res.ca, col.row = "cos2",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#             repel = TRUE)
# 
# row <- get_ca_row(res.ca)
# library("corrplot")
# corrplot(row$cos2, is.corr=FALSE)
# fviz_cos2(res.ca, choice = "row", axes = 1:2)
# fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)
# fviz_contrib(res.ca, choice = "row", axes = 1:2, top = 10)
# 
# 
# fviz_ca_row(res.ca, col.row = "contrib",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#             repel = TRUE)
# 
# col <- get_ca_col(res.ca)
# fviz_ca_col(res.ca, col.col = "cos2", 
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE)
# 
# fviz_cos2(res.ca, choice = "col", axes = 1:2)
# 
# fviz_ca_biplot(res.ca, 
#                map ="colprincipal", arrow = c(TRUE, TRUE),
#                repel = TRUE)
# fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(TRUE, TRUE),
#                repel = TRUE)
# 




##Hierarchical clustering-----
res.hcpc <- HCPC (res.mca, graph = FALSE, max = 5)

fviz_dend(res.hcpc)
fviz_cluster(res.hcpc, geom = "point", main = "Factor map")

# eig.val <- get_eigenvalue(res.mca)
# fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
# 
# fviz_mca_biplot(res.mca, 
#                 repel = TRUE, # Avoid text overlapping (slow if many point)
#                 ggtheme = theme_minimal())
# 
# fviz_mca_var(res.mca, choice = "mca.cor", 
#              repel = TRUE, # Avoid text overlapping (slow)
#              ggtheme = theme_minimal())
# fviz_mca_var(res.mca, 
#              repel = TRUE, # Avoid text overlapping (slow)
#              ggtheme = theme_minimal())
# fviz_ellipses(res.mca, 1:4, geom = "point")
