# library("tidyverse")
# library("readxl")
# library("here")
# library("lubridate")
# library("zoo")
# library("hrbrthemes")

source("librerie.R")

dati <- read_excel("data/Dati epidemiogici RV_2016-2019-070621.xlsx")

## data handling----
dt <- dati %>% 
  mutate( nconf = str_remove_all(nconf, " "),
          nconf = str_c(year, nconf), 
    codaz = str_to_upper(codaz), 
         codaz = gsub("[[:punct:][:blank:]]","", codaz), 
         ageclass = str_remove(ageclass, "Suino"),
         ageclass = str_remove(ageclass, "suino"),
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
         cldiff = ifelse(Cldiff == "P", "Cldiff", 0))

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


#ANALISI DELLE CORRISPONDENZE-----



dummydt <- dt %>% 
  select(11:13, 15, 17, 18:22) %>% 
  dummy_cols(remove_selected_columns = TRUE)  

df<-data.frame(dt[, 9],dummydt)

tabella <-  df %>% 
  group_by(ageclass) %>% 
  summarise_all(sum, na.rm = T)  %>% 
  select(ageclass, ends_with("P")) %>% 
  column_to_rownames(var="ageclass") %>% 
  as.data.frame()
 


res.ca<-CA(tabella, graph = FALSE)
summary(res.ca)
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
fviz_screeplot(res.ca) +
  geom_hline(yintercept=11.1, linetype=2, color="red")
fviz_ca_biplot(res.ca, repel = TRUE)


fviz_ca_biplot(res.ca, 
               map ="colprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)
