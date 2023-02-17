# Programme de Damien pour faire les slides de la présentation au CMH du 17 fev 23


# préambules
library(dplyr)
library(tidyverse)
library(ggplot2)
# tinytex::install_tinytex() 
library(tinytex)# pour le rmarkdown en PDF (il faut latec)
library(broom)
library(broom.helpers)
library(gtsummary)
library(GGally)
library(questionr)


# lancement de la base 
load("climat.RData")
source("recodages.R", encoding = "UTF-8")

#climatRegr_r$sitproD`Doctorant·e` <- climatRegr_r == c("ATER", "Doctorant·e contractuel·le", "Doctorant·e CIFRE")
  
climat$sitpro <- factor(climat$sitpro,
                        levels = c(
                          "Directeur·rice de recherche", "Professeur·e des universités",
                          "Chargé·e de recherche", "Maître·sse de conférences",
                          "Ingénieur·e de recherche", "Post-doctorant·e",
                          "ATER", "Doctorant·e contractuel·le", "Doctorant·e CIFRE",
                          "Ingénieur·e d'études", "Chargé·e d'études/de mission",
                          "Assistant ingénieur·e", "Technicien·ne",
                          "Adjoint·e technique", "Autre"
                        ))

climatRegr_r$sitproD <- as.character(climatRegr_r$sitpro)

 climatRegr_r$sitproD[climatRegr_r$sitproD %in% c("ATER", "Doctorant·e contractuel·le", "Doctorant·e CIFRE")] <- "Doctorant·e"
#climatRegr_r$sitproD == "ATER" | climatRegr_r$sitproD == "Doctorant·e contractuel·le" | climatRegr_r$sitproD == "Doctorant·e CIFRE" <- "Doctorant·e"

 climatRegr_r$sitproD <- factor(climatRegr_r$sitproD,
                         levels = c(
                           "Directeur·rice de recherche", "Professeur·e des universités",
                           "Chargé·e de recherche", "Maître·sse de conférences",
                           "Ingénieur·e de recherche", "Post-doctorant·e",
                           "Doctorant·e"
                         ))
 

# table(climatRegr_r$sitproD)

# Croisement sitpro * vols(O/N)--------- 
 climatRegr_r %>%
   tbl_cross(row = "sitproD", col = "vols_dicho", percent = "row", missing = "no",
             label = list(sitproD ~ "Statut Professionnel", vols_dicho ~"Avoir volé en 2019"))
# test pour Julien 
climatRegr_r %>%
   tbl_cross(row = "sitproD", col = "vols_dicho", percent = "row", missing = "no", statistic = "{p}%", margin = "column")
 
 
climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "vols_dicho", percent = "row", missing = "no", statistic = "{p}%")


# Croisement sitpro * Nb heures ----------
climatRegr_r$volshcat <- with(climatRegr_r,
                              case_when(volsnb == 0 ~ "0h",
                                        volsh == "De 1h à 10h" ~ "De 1h à 10h",
                                        volsh == "De 11h à 20h" ~ "De 11h à 20h",
                                        volsh == "De 20h à 50h" ~ "De 20h à 50h",
                                        volsh == "Plus de 50h" ~ "Plus de 50h"))
climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "volshcat", percent = "row", missing = "no")

climatRegr_r$volshcatr <- with(climatRegr_r,
                              case_when(volsnb == 0 ~ "Pas volé",
                                        volsh == "De 1h à 10h" ~ "De 1h à 20h",
                                        volsh == "De 11h à 20h" ~ "De 1h à 20h",
                                        volsh == "De 20h à 50h" ~ "Plus de 20h",
                                        volsh == "Plus de 50h" ~ "Plus de 20h"))

climatRegr_r$volshcatr <- factor(climatRegr_r$volshcatr,
                               levels = c("Pas volé", "De 1h à 20h","Plus de 20h"))


climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "volshcatr", percent = "row", missing = "no", 
            label = list(sitproD ~ "Statut Professionnel", volshcatr ~"Nombre d'heures de vol"))

# vols sur 3 ans -------
table(climatRegr_r$vols_dicho3ans)

climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "vols_dicho3ans", percent = "row", missing = "no", 
            label = list(sitproD ~ "Statut Professionnel", vols_dicho3ans ~"Avoir volé au cours des 3 derniers années"))

# nombre de vols -----
climatRegr_r$volsnbtranch3 <- cut(climatRegr_r$volsnb,
                            include.lowest = TRUE,
                            right = TRUE,
                            breaks = c(0, 0.4, 2, 4, 65)
)
table(climatRegr_r$volsnbtranch3)

climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "volsnbtranch3", percent = "row", missing = "no", 
            label = list(sitproD ~ "Statut Professionnel", volsnbtranch3 ~"Nombre d'Aller simples en avion"))

climatRegr_r$volsnbtranch3 <- as.character(climatRegr_r$volsnbtranch3)
climatRegr_r$volsnbtranch3 <- with(climatRegr_r,
                               case_when(volsnbtranch3 == "[0,0.4]" ~ "Pas volé",
                                         volsnbtranch3 == "(0.4,2]" ~ "1 à 2 vols",
                                         volsnbtranch3 == "(2,4]" ~ "3 à 4 vols ",
                                         volsnbtranch3 == "(4,65]" ~ "Plus de 4 vols"
                                         ))
climatRegr_r$volsnbtranch3 <- factor(climatRegr_r$volsnbtranch3,
                                 levels = c("Pas volé", "1 à 2 vols","3 à 4 vols ", "Plus de 4 vols")) #pour trier les réponses

climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "volsnbtranch3", percent = "row", missing = "no", 
            label = list(sitproD ~ "Statut Professionnel", volsnbtranch3 ~"Nombre d'Aller simples en avion"))

#### variables à regarder 







####Création de variables à partir des données du module sur l'avion LIGNE 846



# TO DO : le même tableau mais avec un nbvol en catégoriel + le même tableau mais avec heures de vols en catégoriel 



#### tests  
