library(tidyverse)

source("codes_postaux/recodage_des_codes_postaux_residence.txt")
source("codes_postaux/recodage_des_codes_postaux_travail.txt")

climat$res.dep <- substr(climat$res.cp, 1, 2)
climat$trav.dep <- substr(climat$trav.cp, 1, 2)

# travail sur les codes postaux recodés ----


# chargement de la base communes insee (pour avoir les infos sur les villes)
communes <- read.csv2("codes_postaux/table_communes_2020.csv", fileEncoding="ISO-8859-1")
# là on cole les 0 devant les codes postaux à 4 chiffres 
communes$code_insee <- if_else(nchar(communes$CODGEO) == 4 & !is.na(communes$CODGEO),
                               paste("0", communes$CODGEO, sep=""),
                               communes$CODGEO)

communes$TAAV2017 <- fct_recode(as.character(communes$TAAV2017),
                                "Commune hors attraction des villes"="0",
                                "Aire de moins de 50 000 habitants"="1",
                                "Aire de 50 000 à moins de 200 000 habitants"="2",
                                "Aire de 200 000 à moins de 700 000 habitants"="3",
                                "Aire de 700 000 habitants ou plus (hors Paris)"="4",
                                "Aire de Paris"="5")

communes$TUU2017 <- fct_recode(as.character(communes$TUU2017),
                               "Commune rurale"="0",
                               "Commune appartenant à une unité urbaine de 2 000 à 4 999 habitants"="1",
                               "Commune appartenant à une unité urbaine de 5 000 à 9 999 habitants"="2",
                               "Commune appartenant à une unité urbaine de 10 000 à 19 999 habitants"="3",
                               "Commune appartenant à une unité urbaine de 20 000 à 49 999 habitants"="4",
                               "Commune appartenant à une unité urbaine de 50 000 à 99 999 habitants"="5",
                               "Commune appartenant à une unité urbaine de 100 000 à 199 999 habitants"="6",
                               "Commune appartenant à une unité urbaine de 200 000 à 1 999 999 habitants"="7",
                               "Commune appartenant à l'unité urbaine de Paris"="8")

communes$CATEAAV2020 <- fct_recode(as.character(communes$CATEAAV2020),
                                   "Commune-centre"="11",
                                   "Autre commune du pôle principal"="12",
                                   "Commune d'un pôle secondaire"="13",
                                   "Commune de la couronne"="20",
                                   "Commune hors attraction des villes"="30")

# chargement de la base codes postaux (pour convertir les codes insee en codes postaux)
cp <- read.csv2("codes_postaux/Fichiers insee/correspondance-code-insee-code-postal.csv",
                fileEncoding = "UTF8")
#on ne garde que les deux colonnes qui nous intéressent
cp <- cp[,1:2]
# Comme on a des lignes avec plusieurs codes postaux qui s'enchainent (ex : 59000/59800/59670 pour Lille),
# on crée une ligne pour chaque
cp <- cp %>% separate_rows(Code.Postal) 

# fusion de ces deux bases insee
communes_cp <- merge(cp, communes, by.x="Code.INSEE", by.y="CODGEO", all=T)
# on demande à ce que lyon, paris et marseille apparaissent sans les arrondissements
communes_cp$Code.Postal[communes_cp$LIBGEO == "Lyon"] <- "69000"
communes_cp$Code.Postal[communes_cp$LIBGEO == "Paris"] <- "75000"
communes_cp$Code.Postal[communes_cp$LIBGEO == "Marseille"] <- "13000"

# Éliminer les codes postaux en doublon, sinon left_join répète les individus correspondants
communes_cp <- filter(communes_cp, !duplicated(Code.Postal) & !is.na(Code.Postal))

vars <- c("Code.Postal", "TAAV2017", "TUU2017", "CATEAAV2020")

climat <- left_join(climat, rename_with(select(communes_cp, all_of(vars)), ~ paste0("res.", .x)),
                    by=c("res.cp"="res.Code.Postal"), na_matches="never")

climat <- left_join(climat, rename_with(select(communes_cp, all_of(vars)), ~ paste0("trav.", .x)),
                    by=c("trav.cp"="trav.Code.Postal"), na_matches="never")

# 
# ## Vérifications
# 
# # chargement de la base des villes déclarées par les enquêtés
# villes_enquete <- read.csv2("villes_recodage.csv", encoding = "UTF8")
# # on travaille que sur les codes postaux ok - ceux en attente de recodage sont exclus
# villes_enquete_ok <- villes_enquete[villes_enquete$resid_inconnu =="ok",]
# villes_enquete_ok$res.cpid[villes_enquete_ok$res.cpid<10000] <- 
#   paste("0",villes_enquete_ok$res.cpid[
#     villes_enquete_ok$res.cpid<10000], sep="")
# 
# # on vérifie si tous les codes postaux des répondants sont dans la base cp_communes
# a <- as.list(villes_enquete_ok$res.cpid)
# b <- as.list(communes_cp$Code.Postal)
# c <- intersect(a,b)
# inconnus <- setdiff(a,b)
# 
# #trois codes postaux inconnus: 20144, 76420, 98800
# 
# # travail ----
# 
# # on travaille que sur les codes postaux ok - ceux en attente de recodage sont exclus
# villes_enquete_ok <- villes_enquete[
#   villes_enquete$travail_inconnu =="ok"&
#     !is.na(villes_enquete$trav.cpail),]
# villes_enquete_ok$trav.cpail[villes_enquete_ok$trav.cpail<10000] <- 
#   paste("0",villes_enquete_ok$trav.cpail[
#     villes_enquete_ok$trav.cpail<10000], sep="")
# 
# # on vérifie si tous les codes postaux des répondants sont dans la base cp_communes
# a <- as.list(villes_enquete_ok$trav.cpail)
# b <- as.list(communes_cp$Code.Postal)
# c <- intersect(a,b)
# inconnus <- setdiff(a,b)
# 
# #deux codes postaux inconnus: 76821, 98800
