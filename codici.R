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
          #nconf = str_c(year, nconf), 
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
         cldiff = ifelse(Cldiff == "P", "Cldiff", 0), 
         quart = quarter(dtprelievo,fiscal_start = 11), 
    stagione = ifelse(quart==1, "Winter", 
                      ifelse(quart==2, "Spring", 
                             ifelse(quart==3, "Summer", "Autumn"))))

dt %>% 
  group_by(codaz, day, month, year) %>% 
  count() %>% View()
 
Brachy <- dt %>% 
  filter(!is.na(brachyod) | !is.na(brachypil)) %>% 
  mutate(Brachyspira = ifelse(brachyod=="Brachyod", "Pos", 
                              ifelse(brachypil== "Brachypil", "Pos", "Neg")))
Clostr <- dt %>% 
  filter(!is.na(Clperfr) | !is.na(Cldiff)) %>% View()
  mutate(Clostridi = ifelse(Clperfr == "P", "Pos", 
                            ifelse(Cldiff == "P", "Pos", "N")))

##Modello di regressione rotavirus predittore di infezioni batteriche?-----

library(brms)
library(lme4)
  # 
  mod <- brm(formula = Brachyspira ~ RV+ (1|codaz),
                            data=Brachy,
                            family = bernoulli(link = "logit"),
                            warmup = 500,
                            iter = 2000,
                            chains = 2,
                            inits= "0",
                            cores=2,
                            seed = 123)

Brachy <- Brachy %>% 
  mutate(Brachyspira = factor(Brachyspira))
  
mod <- glm(Brachyspira ~ RVA+ RVB+ RVC+RVH+PEDV+  ageclass, data = Brachy, family = binomial)

mod <- glmer(as.factor(RVA)  ~  Brachyspira + RVB+ RVC+RVH+PEDV+ (1|codaz), data = Brachy, family = binomial)


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
