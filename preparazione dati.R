dati <- read_excel("data/Dati epidemiogici RV_2016-2019-070621.xlsx")

## data handling----

dt <- dati %>% 
  
  
  mutate( nconf = str_remove_all(nconf, " "),
          #nconf = str_c(year, nconf), 
          codaz = str_to_upper(codaz), 
          codaz = gsub("[[:punct:][:blank:]]","", codaz), 
          ageclass = str_remove(ageclass, "Suino"),
          ageclass = str_remove(ageclass, "suino"),
          ageclass = str_remove(ageclass, " "),
          ageclass = recode(ageclass, 
                            "magronaggio" = "ingrasso"), 
          prelievo = str_c(day, "-", month, "-", year), 
          dtprelievo = dmy(prelievo), 
          Year = year(dtprelievo), 
          Month = month(dtprelievo), 
          Week = week(dtprelievo), 
          rva = ifelse(RVA == "P", "RVA", 0),
          rvb = ifelse(RVB == "P", "RVB", 0), 
          rvc = ifelse(RVC == "P", "RVC", 0), 
          rvh = ifelse(RVH == "P", "RVH", 0),
          rv = ifelse(RV == "P", "RV", 0), 
          pedv = ifelse(PEDV == "P", "PEDV", 0),
          coli = ifelse(ecoli == "P", "EColi", 0), 
          lawsonia = ifelse(Lawsonia == "P", "Lawsonia", 0), 
          brachyod = ifelse(Brachyspira_hyod == "P", "Brachyod", 0),
          brachypil = ifelse(Brachyspira_pilos == "P", "Brachypil", 0), 
          clperfr = ifelse(Clperfr == "P", "Clperf", 0),
          cldiff = ifelse(Cldiff == "P", "Cldiff", 0), 
          quart = quarter(dtprelievo,fiscal_start = 11), 
          stagione = ifelse(quart==1, "Winter", 
                            ifelse(quart==2, "Spring", 
                                   ifelse(quart==3, "Summer", "Autumn"))))
# 
# dt %>% 
#   group_by(codaz, day, month, year) %>% 
#   count() %>% View()

Brachybinom <- dt %>% 
  filter(!is.na(brachyod) | !is.na(brachypil)) %>% 
  mutate(Brachyspira = ifelse(brachyod=="Brachyod", "Pos", 
                              ifelse(brachypil== "Brachypil", "Pos", "Neg"))) %>%  
  select(Brachyspira, RVA,RVB, RVC, RVH, RV, ageclass, stagione, codaz)

Brachybinom$Brachyspira <- as.numeric(as.factor(Brachybinom$Brachyspira))-1


Clostr <- dt %>% 
  filter(!is.na(Clperfr) | !is.na(Cldiff)) %>% 
  mutate(Clostridi = ifelse(Clperfr == "P", "Pos", 
                            ifelse(Cldiff == "P", "Pos", "N"))) %>% 
  select(Clostridi, RVA,RVB, RVC, RVH, RV, ageclass, stagione, codaz)
 Clostr$Clostridi <- as.numeric(as.factor(Clostr$Clostridi))-1


Rota <- dt %>% 
  select(RV,RVA,RVB, RVC, RVH, ageclass, stagione, codaz)

Rota$RV <- as.numeric(as.factor(Rota$RV))-1
Rota$RVA <- as.numeric(as.factor(Rota$RVA))-1
Rota$RVB <- as.numeric(as.factor(Rota$RVB))-1
Rota$RVC <- as.numeric(as.factor(Rota$RVC))-1
Rota$RVH <- as.numeric(as.factor(Rota$RVH))-1

Lawsonia <- dt %>% 
  filter(!is.na(Lawsonia) & ageclass != "sottoscrofa") %>% 
  mutate(Lawsonia = ifelse(Lawsonia == "P", "POS", "NEG")) %>% 
  select(Lawsonia, RVA,RVB, RVC, RVH, RV, ageclass, stagione, codaz)
Lawsonia$Lawsonia <- as.numeric(as.factor(Lawsonia$Lawsonia))-1
