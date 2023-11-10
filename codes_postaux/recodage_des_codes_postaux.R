# Codage des variables géographiques sur le lieu de travail et de résidence

# Le questionnaire proposait une liste de communes en autocomplétion,
# qui affichait aussi leur code postal. Pour les choix dans la liste,
# une première étape vise à récupérer le code commune correspondant à partir
# de la table de l'Insee. Pour les saisies hors liste, une deuxième étape
# récupère les codes postaux nettoyés manuellement et impute des codes communes
# en retenant la plus grand commune qui a le code postal en question.

library(tidyverse)

# chargement de la base communes insee (pour avoir les infos sur les villes)
communes <- read.csv2("codes_postaux/table_communes_2020.csv", fileEncoding="CP1252")
# là on cole les 0 devant les codes postaux à 4 chiffres 
communes$code_insee <- if_else(nchar(communes$CODGEO) == 4 & !is.na(communes$CODGEO),
                               paste("0", communes$CODGEO, sep=""),
                               communes$CODGEO)

communes$TAAV2017 <- fct_recode(as.character(communes$TAAV2017),
                                "Hors attraction des villes"="0",
                                "Moins de 50 000 habitants"="1",
                                "50 000 à 199 000 habitants"="2",
                                "200 000 à 699 000 habitants"="3",
                                "700 000 habitants ou plus (hors Paris)"="4",
                                "Aire de Paris"="5")

communes$TDAAV2017 <- fct_recode(as.character(communes$TDAAV2017),
                                 "Hors attraction des villes"="0",
                                 "Moins de 10 000 habitants"="11",
                                 "10 000 à 19 999 habitants"="12",
                                 "20 000 à 29 999 habitants"="13",
                                 "30 000 à 49 999 habitants"="14",
                                 "50 000 à 74 999 habitants"="21",
                                 "75 000 à 99 999 habitants"="22",
                                 "100 000 à 124 999 habitants"="23",
                                 "125 000 à 149 999 habitants"="24",
                                 "150 000 à 199 999 habitants"="25",
                                 "200 000 à 299 999 habitants"="31",
                                 "300 000 à 399 999 habitants"="32",
                                 "400 000 à 499 999 habitants"="33",
                                 "500 000 à 699 999 habitants"="34",
                                 "700 000 à 999 999 habitants"="41",
                                 "1 000 000 habitants ou plus (hors Paris)"="42",
                                 "Aire de Paris"="50")

communes$TUU2017 <- fct_recode(as.character(communes$TUU2017),
                               "Commune rurale"="0",
                               "2 000 à 4 999 habitants"="1",
                               "5 000 à 9 999 habitants"="2",
                               "10 000 à 19 999 habitants"="3",
                               "20 000 à 49 999 habitants"="4",
                               "50 000 à 99 999 habitants"="5",
                               "100 000 à 199 999 habitants"="6",
                               "200 000 à 1 999 999 habitants"="7",
                               "Unité urbaine de Paris"="8")


communes$TDUU2017 <- fct_recode(as.character(communes$TDUU2017),
                                "Commune hors unité urbaine"="7",
                                "Moins de 2 500 habitants"="11",
                                "2 500 à 2 999 habitants"="12",
                                "3 000 à 3 999 habitants"="13",
                                "4 000 à 4 999 habitants"="14",
                                "5 000 à 6 999 habitants"="21",
                                "7 000 à 9 999 habitants"="22",
                                "10 000 à 14 999 habitants"="31",
                                "15 000 à 19 999 habitants"="32",
                                "20 000 à 24 999 habitants"="41",
                                "25 000 à 29 999 habitants"="42",
                                "30 000 à 39 999 habitants"="43",
                                "40 000 à 49 999 habitants"="44",
                                "50 000 à 69 999 habitants"="51",
                                "70 000 à 99 999 habitants"="52",
                                "100 000 à 149 999 habitants"="61",
                                # Aucun cas
                                # 150 000 à 199 999 habitants"="62",
                                "200 000 à 299 999 habitants"="71",
                                "300 000 à 499 999 habitants"="72",
                                "500 000 à 1 999 999 habitants"="73",
                                "Unité urbaine de Paris"="80")
communes$TDUU2017 <- relevel(communes$TDUU2017, "Commune hors unité urbaine")

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
communes_cp <- merge(cp, communes, by.x="Code.INSEE", by.y="code_insee", all=T)
# on demande à ce que lyon, paris et marseille apparaissent sans les arrondissements
communes_cp$Code.Postal[communes_cp$LIBGEO == "Lyon"] <- "69000"
communes_cp$Code.Postal[communes_cp$LIBGEO == "Paris"] <- "75000"
communes_cp$Code.Postal[communes_cp$LIBGEO == "Marseille"] <- "13000"

communes_cp$codenom <- sprintf("[%s] %s", communes_cp$Code.Postal,
                               gsub("œ", "oe", communes_cp$LIBGEO))

# Première reconnaissance : le code postal et le nom doivent correspondre
# On recode les communes qui ont fusionné et celles qui ont plusieurs codes postaux
for(var in c("communes.SaisieVille", "communes.SaisieVilleTravail"))
    climat[[var]] <- recode(climat[[var]],
                            # Villes qui ont fusionné
                            "[14170] Saint-Pierre-sur-Dives"="[14170] Saint-Pierre-en-Auge",
                            "[14240] Caumont-l'Éventé"="[14240] Caumont-sur-Aure",
                            "[14260] Aunay-sur-Odon"="[14260] Les Monts d'Aunay",
                            "[14480] Cully"="[14740] Moulins en Bessin",
                            "[14610] Anguerny"="[14610] Colomby-Anguerny",
                            "[22650] Ploubalay"="[22650] Beaussais-sur-Mer",
                            "[24330] Eyliac"="[24330] Bassillac et Auberoche",
                            "[25870] Auxon-Dessous"="[25870] Les Auxons",
                            "[27510] Tourny"="[27630] Vexin-sur-Epte",
                            "[35150] Piré-sur-Seiche"="[35150] Piré-Chancé",
                            "[38300] Ruy"="[38300] Ruy-Montceau",
                            "[41290] Oucques"="[41290] Oucques La Nouvelle",
                            "[45330] Malesherbes"="[45330] Le Malesherbois",
                            "[49150] Cuon"="[49150] Baugé-en-Anjou",
                            "[50120] Équeurdreville-Hainneville"="[50100] Cherbourg-en-Cotentin",
                            "[50130] Cherbourg-Octeville"="[50100] Cherbourg-en-Cotentin",
                            "[54840] Velaine-en-Haye"="[54840] Bois-de-Haye",
                            "[67370] Pfettisheim"="[67370] Truchtersheim",
                            "[68350] Didenheim"="[68350] Brunstatt-Didenheim",
                            "[69490] Pontcharra-sur-Turdine"="[69490] Vindry-sur-Turdine",
                            "[71520] Clermain"="[71520] Navour-sur-Grosne",
                            "[74960] Cran-Gevrier"="[74000] Annecy",
                            "[74370] Pringy"="[74000] Annecy",
                            "[74940] Annecy-le-Vieux"="[74000] Annecy",
                            "[76330] Notre-Dame-de-Gravenchon"="[76330] Port-Jérôme-sur-Seine",
                            "[78150] Le Chesnay"="[78150] Le Chesnay-Rocquencourt",
                            "[86130] Jaunay-Clan"="[86130] Jaunay-Marigny",
                            "[86380] Cheneché"="[86380] Saint-Martin-la-Pallu",
                            "[86470] La Chapelle-Montreuil"="[86470] Boivre-la-Vallée",
                            "[38660] Saint-Hilaire"="[38660] Plateau-des-Petites-Roches",
                            "[38730] Le Pin"="[38850] Villages du Lac de Paladru",
                            "[49350] Gennes"="[49350] Gennes-Val-de-Loire",
                            "[71270] La Villeneuve"="[71270] Clux-Villeneuve",
                            "[79210] Usseau"="[79210] Val-du-Mignon",
                            "[91000] Évry"="[91000] Évry-Courcouronnes",
                            "[93400] Saint-Ouen"="[93400] Saint-Ouen-sur-Seine",
                            "[24620] Les Eyzies-de-Tayac-Sireuil"="[24620] Les Eyzies",
                            # Villes avec des codes postaux différents de ceux prévus
                            "[20144] Zonza"="[20124] Zonza",
                            "[76420] Bois-Guillaume"="[76230] Bois-Guillaume",
                            "[80080] Amiens"="[80000] Amiens",
                            "[80090] Amiens"="[80000] Amiens",
                            "[84140] Avignon"="[84000] Avignon",
                            "[31077] Toulouse"="[31000] Toulouse",
                            "[28310] Allaines-Mervilliers"="[28310] Janville-en-Beauce",
                            "[54100] Nancy"="[54000] Nancy")

climat$res.codeinsee <- communes_cp$Code.INSEE[match(climat$communes.SaisieVille,
                                                     communes_cp$codenom)]
climat$trav.codeinsee <- communes_cp$Code.INSEE[match(climat$communes.SaisieVilleTravail,
                                                      communes_cp$codenom)]

# Arrondissements de Paris, Lyon, Marseille
climat$res.codeinsee[grepl("Paris [0-9][0-9]?er? Arrondissement$",
                           climat$communes.SaisieVille)] <- "75056"
climat$res.codeinsee[grepl("Lyon [0-9][0-9]?er? Arrondissement$",
                           climat$communes.SaisieVille)] <- "69123"
climat$res.codeinsee[grepl("Marseille [0-9][0-9]?(e|er|°) Arrondissement ?$",
                           climat$communes.SaisieVille)] <- "13055"
climat$res.codeinsee[climat$communes.SaisieVille == "[75001] Paris"]  <- "75056"
climat$res.codeinsee[climat$communes.SaisieVille == "[13001] Marseille"]  <- "13055"

climat$trav.codeinsee[grepl("Paris [0-9][0-9]?er? Arrondissement$",
                            climat$communes.SaisieVilleTravail)] <- "75056"
climat$trav.codeinsee[grepl("Lyon [0-9][0-9]?er? Arrondissement$",
                            climat$communes.SaisieVilleTravail)] <- "69123"
climat$trav.codeinsee[grepl("Marseille [0-9][0-9]?(e|er|°) Arrondissement ?$",
                            climat$communes.SaisieVilleTravail)] <- "13055"
climat$trav.codeinsee[climat$communes.SaisieVilleTravail == "[75001] Paris"]  <- "75056"
climat$trav.codeinsee[climat$communes.SaisieVilleTravail == "[13001] Marseille"]  <- "13055"


## Deuxième traitement : code postaux nettoyés par Julien

source("codes_postaux/recodage_des_codes_postaux_residence.txt")
source("codes_postaux/recodage_des_codes_postaux_travail.txt")

# On trie pour retenir les plus grandes aires/unités et les communes-centre
communes_cp <- arrange(communes_cp, desc(TDAAV2017), desc(TDUU2017), CATEAAV2020)

climat$res.codeinsee[is.na(climat$res.codeinsee)] <- communes_cp$Code.INSEE[match(climat$res.cp[is.na(climat$res.codeinsee)],
                                                                                  communes_cp$Code.Postal, incomparables=NA)]
climat$trav.codeinsee[is.na(climat$trav.codeinsee)] <- communes_cp$Code.INSEE[match(climat$trav.cp[is.na(climat$trav.codeinsee)],
                                                                                    communes_cp$Code.Postal, incomparables=NA)]


## Récupération des variables

# Éliminer les communes qui ont plusieurs codes postaux,
# sinon left_join répète les individus correspondants
communes_cp <- filter(communes_cp, !duplicated(Code.INSEE) & !is.na(Code.INSEE))

climat$res.dep <- if_else(!is.na(climat$res.codeinsee), substr(climat$res.codeinsee, 1, 2),
                          substr(climat$res.cp, 1, 2))
climat$trav.dep <- if_else(!is.na(climat$trav.codeinsee), substr(climat$trav.codeinsee, 1, 2),
                           substr(climat$trav.cp, 1, 2))

vars <- c("Code.INSEE", "AAV2020", "TAAV2017", "TDAAV2017",
          "TUU2017", "TDUU2017", "CATEAAV2020")

climat <- left_join(climat, rename_with(select(communes_cp, all_of(vars)), ~ paste0("res.", .x)),
                    by=c("res.codeinsee"="res.Code.INSEE"), na_matches="never")

climat <- left_join(climat, rename_with(select(communes_cp, all_of(vars)), ~ paste0("trav.", .x)),
                    by=c("trav.codeinsee"="trav.Code.INSEE"), na_matches="never")

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
