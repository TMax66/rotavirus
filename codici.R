library("tidyverse")
library("readxl")
library("here")
library("lubridate")
library("zoo")

dati <- read_excel("data/Dati epidemiogici RV_2016-2019-070621.xlsx")

dt <- dati %>% 
  mutate(codaz = str_to_upper(codaz), 
         codaz = gsub("[[:punct:][:blank:]]","", codaz), 
         ageclass = str_remove(ageclass, "Suino"), 
         prelievo = str_c(day, "-", month, "-", year), 
         dtprelievo = dmy(prelievo), 
         Year = year(dtprelievo), 
         Month = month(dtprelievo), 
         Week = week(dtprelievo)) 

        
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
  
