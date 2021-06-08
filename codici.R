library("tidyverse")
library("readxl")
library("here")
library("lubridate")
library("zoo")
library("hrbrthemes")

dati <- read_excel("data/Dati epidemiogici RV_2016-2019-070621.xlsx")

dt <- dati %>% 
  mutate(codaz = str_to_upper(codaz), 
         codaz = gsub("[[:punct:][:blank:]]","", codaz), 
         ageclass = str_remove(ageclass, "Suino"), 
         prelievo = str_c(day, "-", month, "-", year), 
         dtprelievo = dmy(prelievo), 
         Year = year(dtprelievo), 
         Month = month(dtprelievo), 
         Week = week(dtprelievo), 
         rva = ifelse(RVA == "P", "RVA", 0),
         rvb = ifelse(RVB == "P", "RVB", 0), 
         rvc = ifelse(RVC == "P", "RVC", 0), 
         rvh = ifelse(RVH == "P", "RVH", 0), 
         pedv = ifelse(PEDV == "P", "PEDV", 0),
         coli = ifelse(ecoli == "P", "EColi", 0), 
         lawsonia = ifelse(Lawsonia == "P", "Lawsonia", 0), 
         brachyod = ifelse(Brachyspira_hyod == "P", "Brachyod", 0),
         brachypil = ifelse(Brachyspira_pilos == "P", "Brachypil", 0), 
         clperfr = ifelse(Clperfr == "P", "Clperf", 0),
         cldiff = ifelse(Cldiff == "P", "Cldiff", 0))



dt[, 28:38] <- dt[, 28:38]!=0

nomi_abb<-toupper(names(dt)[28:38])
X<-  apply(dt[, 28:38], 1, function(x) nomi_abb[x])
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
  ggplot(aes(x = profilo ,  y = frq, label = frq)) +
  geom_segment( aes(x=profilo, xend=profilo, y=0, yend=n), color="grey")+
  geom_point( aes(x=profilo, y=n), size=4.5, color="steelblue" )+
  #geom_text(color="black", size=2)+
  coord_flip()+
  theme_ipsum_rc()+
  labs(y="n.coinf",x="")

    
dt %>% 
  group_by(Year,Month, RV) %>% 
  summarise(n= n()) %>% 
  pivot_wider(names_from = RV, values_from = n, values_fill = 0) %>% 
  mutate(Prev = P/(P+N)) %>%
  ggplot(aes(x = Month, y= Prev))+
  geom_line()


dt %>% 
  group_by(dtprelievo,  RV) %>% 
  summarise(n= n()) %>% 
  pivot_wider(names_from = RV, values_from = n, values_fill = 0) %>% 
  mutate(Prev = P/(P+N)) %>% 
  ggplot(aes(x = dtprelievo, y= Prev))+
  geom_line()


dt %>% filter(RVA == "P" & RVH == "P") %>% 
  count(M = floor_date(dtprelievo, "month")) %>% 
  mutate(m = rollmean(n, k = 3, fill = NA) )%>%   
  ggplot(aes(M, n))+
  geom_line()+
  geom_line(aes(x=M, y=m), col = "blue", size = 1.5)





 