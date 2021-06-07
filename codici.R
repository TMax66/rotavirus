library("tidyverse")
library("readxl")
library("here")
library("lubridate")

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

         