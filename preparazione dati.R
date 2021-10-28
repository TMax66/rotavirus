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
                                   ifelse(quart==3, "Summer", "Autumn")))) %>% 
  filter(ageclass != "riproduttore")
