source("librerie.R")
source("preparazione dati.R")


##Modelli di regressione rotavirus-----

RV <- stan_glmer(RV~0+as.factor(stagione)+as.factor(ageclass)+(1|codaz), 
           data = Rota, 
           family = binomial(link = logit), 
           # prior = student_t(df=7, location = 0, scale = 2.5), 
           # prior_intercept = student_t(df=7, location = 0, scale = 2.5), 
           cores = 8, seed = 1966)
RVloo <- loo(RV)

RVint <- stan_glmer(RV~stagione*ageclass+(1|codaz), 
                 data = Rota, 
                 family = binomial(link = logit), 
                 # prior = student_t(df=7, location = 0, scale = 2.5), 
                 # prior_intercept = student_t(df=7, location = 0, scale = 2.5), 
                 cores = 8, seed = 1966)

RVintloo <- loo(RVint)

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


# modbrachy <- brm(formula = Brachyspira ~   stagione +(1|codaz)
#                            + RVA + RVB + RVC + RVH,
#            data=Brachy,
#            family = bernoulli(link = "logit"),
#            warmup = 1000,
#            iter = 4000,
#            chains = 4,
#            inits= "0",
#            cores=8,
#            seed = 123)



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
