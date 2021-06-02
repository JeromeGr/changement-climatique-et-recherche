# codes pour les ACM 

# j recode vite fait cette variable parce que je l'ai pas trouvée
# mais il me semble bien qu'on l'a déjà recodée quelque part
climat_recherche$vols_nb_h <- as.character(climat_recherche$volsh)
climat_recherche$vols_nb_h[base_acm$volsnb=="0"] <- "0"
table(climat_recherche$vols_nb_h, climat_recherche$volsh, useNA = "always")

# création de la base avec seulement les variables utiles
base_acm <- climat_recherche[,c(
  "preoccupe",
  "vols_nb_h", "chgtpratique","renoncedep.env",
  "dixannees.marche", "dixannees.vote","avionperso",
  "avionpersochgt","opinionecolo.decroissance",
  "carriere",
  "sitpro2",
  "discipline_agr3","enfantsage_det",
  "ageAgr" ,
  "sexe"
)]


library(FactoMineR)
library(explor)
library(factoextra)

names(base_acm)
freq(base_acm$dixannees.vote)
res.acm <- MCA(base_acm[complete.cases(base_acm[,1:11])& # ULTRA BOURRIN, à ne pas forcément faire, ça vire la ligne dès qu'il y a ne serait ce qu'un NA
                          base_acm$dixannees.vote != "Je ne souhaite pas répondre"&
                          base_acm$dixannees.marche  != "Je ne souhaite pas répondre",], # parce que ça tirait trop l'axe 2, mais en fait c'est tiré par autre chose et c'est pas inintéressant, peut être, donc sans doute à laisser
               quali.sup=c(10:15))
explor(res.acm)

# ça ça sera ta base pour sortir des graphs exportables
# mais en l'état le plan est pas assez lisible pour que ce soit utile
# donc pour plus tard, pour t'amuser
fviz_mca_var(res.acm, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


fviz_mca_var(res.acm, col.var = "contrib", choice = "var.cat",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
