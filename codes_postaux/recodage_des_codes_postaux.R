library(questionr)
library(tidyverse)
# travail sur les codes postaux recodés ----


# chargement de la base communes insee (pour avoir les infos sur les villes)
communes <- read.csv2("table_communes_2020.csv")
#là on cole les 0 devant les codes postaux à 4 chiffres 
#je le fais en deux temps parce que ça générait des erreurs autrement
#(je coupe la base en deux et je réempile. Reste une énigment, la base d'arrivée est plus grosse que la base de départ...)
# il y a des warnings dont j'ignore le sens (NAs introduced by coercion)
# mais j'ai l'impression que ça ne pose pas de problèmes sur l'appariement
communes_cp_4 <- communes[as.numeric(communes$CODGEO)<10000&
                            !is.na(communes$CODGEO),]
communes_cp_5 <- communes[as.numeric(communes$CODGEO)>=10000,]
communes_cp_4$code_insee <- paste("0",communes_cp_4$CODGEO, sep="")
communes_cp_5$code_insee <- communes_cp_5$CODGEO
com <- rbind(communes_cp_4, communes_cp_5)
#enigme: communes fait 34968 lignes et com 35328

# chargement de la base codes postaux (pour convertir les codes insee en codes postaux)
cp <- read.csv2("Fichiers insee/correspondance-code-insee-code-postal.csv", encoding = "UTF8")
#on ne garde que les deux colonnes qui nous intéressent
cp <- cp[,1:2]
# ça c'est Jérôme qui l'avait fait, donc je reprends
#"Comme on a des lignes avec plusieurs codes postaux qui s'enchainent (ex : 59000/59800/59670 pour Lille), on crée une ligne pour chaque" :
cp<-cp %>%   separate_rows(Code.Postal) 

# fusion de ces deux bases insee
communes_cp <- merge(cp, com, by.x="Code.INSEE", by.y="CODGEO", all.x= T, all.y=T)
# on demande à ce que lyon, paris et marseille apparaissent sans les arrondissements
communes_cp$Code.Postal[communes_cp$LIBGEO =="Lyon"] <- "69000"
communes_cp$Code.Postal[communes_cp$LIBGEO =="Paris"] <- "75000"
communes_cp$Code.Postal[communes_cp$LIBGEO =="Marseille"] <- "13000"

# résidence ----

# chargement de la base des villes déclarées par les enquêtés
villes_enquete <- read.csv2("villes_recodage.csv", encoding = "UTF8")
# on travaille que sur les codes postaux ok - ceux en attente de recodage sont exclus
villes_enquete_ok <- villes_enquete[villes_enquete$resid_inconnu =="ok",]
villes_enquete_ok$cp_resid[villes_enquete_ok$cp_resid<10000] <- 
  paste("0",villes_enquete_ok$cp_resid[
    villes_enquete_ok$cp_resid<10000], sep="")

# on vérifie si tous les codes postaux des répondants sont dans la base cp_communes
a <- as.list(villes_enquete_ok$cp_resid)
b <- as.list(communes_cp$Code.Postal)
c <- intersect(a,b)
inconnus <- setdiff(a,b)

#trois codes postaux inconnus: 20144, 76420, 98800

# travail ----

# on travaille que sur les codes postaux ok - ceux en attente de recodage sont exclus
villes_enquete_ok <- villes_enquete[
  villes_enquete$travail_inconnu =="ok"&
    !is.na(villes_enquete$cp_travail),]
villes_enquete_ok$cp_travail[villes_enquete_ok$cp_travail<10000] <- 
  paste("0",villes_enquete_ok$cp_travail[
    villes_enquete_ok$cp_travail<10000], sep="")

# on vérifie si tous les codes postaux des répondants sont dans la base cp_communes
a <- as.list(villes_enquete_ok$cp_travail)
b <- as.list(communes_cp$Code.Postal)
c <- intersect(a,b)
inconnus <- setdiff(a,b)

#deux codes postaux inconnus: 76821, 98800


