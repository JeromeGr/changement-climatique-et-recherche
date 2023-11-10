library(questionr)
library(tidyverse)

#chargement de la base

load("climat.RData")
 
# Élimination des hors-champ
# is.na(rechpub) peut indiquer à la fois un statut d'office classé en Oui,
# ou une non réponse : on exclut les non réponses (surtout des personnes qui n'ont repondu à rien)
climat <- filter(climat,
                 rechpub == "Oui" |
                   docto == "Oui" |
                   sitpro %in% c("Chargé·e de recherche", "Directeur·rice de recherche", 
                                 "Doctorant·e contractuel·le", "Ingénieur·e d'études",
                                 "Maître·sse de conférences",  "Ingénieur·e de recherche",
                                 "Professeur·e des universités",  "Post-doctorant·e",
                                 "Assistant ingénieur·e", "Doctorant·e CIFRE",
                                 "ATER", "Adjoint·e technique") |
                   !is.na(bap) | 
                   employeur %in% c("CNRS", "Une grande école ou un grand établissement",
                                    "Une université", "Inserm", "CEA", "IRD",
                                    "Cirad", "Inrae", "Inria", "CNES", "Ifremer", 
                                    "ONERA", "Ined") |
                   tutelles.CNRS == "Oui" | tutelles.Univ == "Oui" |
                   tutelles.Ecole == "Oui" | tutelles.Inserm == "Oui" |
                   tutelles.Inrae == "Oui" | tutelles.Inria == "Oui" |
                   tutelles.IRD == "Oui" | tutelles.Ined == "Oui" |
                   tutelles.CEA == "Oui" | tutelles.CNES == "Oui"|
                   tutelles.ONERA == "Oui" | tutelles.Cirad == "Oui" |
                   tutelles.Ifremer == "Oui" |
                   sitpro.other == "Indépendante, chercheuse associée dans un labo")

# À ce stade on ne doit avoir que des personnes qui ont répondu au moins à quelques questions
stopifnot(all(climat$interviewtime > 0))

# Et on vire des trolls (il faut rajouter de garder les NAs sinon on les perd)
climat<-climat %>% filter((nbpublisang!=666 | is.na(nbpublisang)))
climat<-climat %>% filter((tpsdomtrav.tgv_h!=85 | is.na(tpsdomtrav.tgv_h)))
climat<-climat %>% filter (datestamp!="2020-07-01 14:22:52")

#Je vire ceux dont la ligne est totalement vide
#Peut être rajouter des conditions
climat<-climat %>% filter(!(sexe=="" & age=="" & statut=="" & employeur=="" & changclim=="" & preoccupe==""))

# La modalité Moins de 18 ans est vide : la retirer
climat$age <- droplevels(climat$age)

climat$sexe <- fct_recode(climat$sexe, NULL="Autre")

climat$agenum <- climat$age %>%
  fct_recode(
    "22" = "18-24 ans",
    "27" = "25-29 ans",
    "32" = "30-34 ans",
    "37" = "35-39 ans",
    "42" = "40-44 ans",
    "47" = "45-49 ans",
    "52" = "50-54 ans",
    "57" = "55-59 ans",
    "62" = "60-64 ans",
    "67" = "65-69 ans",
    "72" = "70 ans ou plus"
  ) %>%
    as.character() %>%
    as.numeric()

#Regroupement catégories d'âge (pour avoir des catégories plus homogènes en termes de nombre de personnes)
climat$ageAgr<-as.character(climat$age)
climat$ageAgr[climat$age %in% c("70 ans ou plus", "65-69 ans")]<-"65 ans et plus"
climat$ageAgr[climat$age %in% c("18-24 ans", "25-29 ans")]<-"Moins de 29 ans"
climat$ageAgr[climat$age %in% c("55-59 ans", "60-64 ans")]<-"55-64 ans"

climat$ageAgr <- factor(climat$ageAgr,
                        levels = c("Moins de 29 ans", "30-34 ans", "35-39 ans", "40-44 ans",
                                   "45-49 ans", "50-54 ans", "55-64 ans", "65 ans et plus" ))
climat$ageAgr2 <- fct_collapse(climat$age,
                               "Moins de 30 ans"=c("18-24 ans", "25-29 ans"),
                               "30-39 ans"=c("30-34 ans", "35-39 ans"),
                               "40-49 ans"=c("40-44 ans", "45-49 ans"),
                               "50-59 ans"=c("50-54 ans", "55-59 ans"),
                               "60 ans et plus"=c("60-64 ans", "65-69 ans", "70 ans ou plus"))


#Age accadémique
climat$ageaccad<-2020-climat$theseannee

climat$ageaccad_tranch<-quant.cut(climat$ageaccad, 6)

climat$ageaccad_tranch2 <- ifelse(climat$these=="Non", "Pas de thèse",
                                  as.character(cut(climat$ageaccad,
                                                   include.lowest = TRUE,
                                                   right = TRUE,
                                                   breaks = c(0, 2, 5, 8, 13, 18, 23, 29, 114))))
climat$ageaccad_tranch2 <- factor(climat$ageaccad_tranch2,
                                  levels = c(
                                    "Pas de thèse", "[0,2]", "(2,5]",
                                    "(5,8]", "(8,13]", "(13,18]", "(18,23]", "(29,114]"))


# variable combinant existence et causes du changement climatique
climat$acthum2 <- factor(climat$acthum,
                         levels=c("Oui, elles en sont l'unique cause", 
                                  "Oui, elles jouent un grand rôle",
                                  "Oui, elles jouent un petit rôle",
                                  "Non, elles ne jouent aucun rôle",
                                  "Il n'y a pas de changement climatique",
                                  "Sans opinion"))
climat$acthum2[substr(climat$changclim, 1, 3) == "Non"] <- "Il n'y a pas de changement climatique"
climat$acthum2[climat$changclim == "Sans opinion"] <- "Sans opinion"

climat$acthum3 <- fct_collapse(climat$acthum2,
                               "Oui, elles jouent un grand rôle ou en sont l'unique cause"=
                                   c("Oui, elles en sont l'unique cause", 
                                     "Oui, elles jouent un grand rôle"))

climat$preoccupe2 <- climat$preoccupe
climat$preoccupe2[substr(climat$changclim, 1, 3) == "Non"] <- "Pas du tout préoccupé·e"
climat$preoccupe2[climat$changclim == "Sans opinion"] <- "Sans opinion"

# Si au moins une case du tableau a été remplie, on remplit les cases vides avec des 0
# Si la case total de chaque ligne est vide ou inférieure à la case 5 ans ou indispensable,
# on prend la valeur la plus élevée de la ligne comme total
vars <- c("ordis.fixeperso", "ordis.fixepro", "ordis.partage",
          "ordis.portablepro", "ordis.portableperso",
          "ordis.tablettepro", "ordis.tabletteperso")
suffixes <- c("_nb", "_5ans", "_indisp")
climat$ordisremplis <- rowSums(!is.na(select(climat, as.vector(outer(vars, suffixes, paste0))))) > 0
for(var in vars) {
    for(suffixe in suffixes)
        climat[climat$ordisremplis, paste0(var, suffixe)] <-
            coalesce(climat[climat$ordisremplis, paste0(var, suffixe)], 0)

    climat[[paste0(var, "_nb")]] <- pmax(climat[[paste0(var, "_nb")]],
                                         climat[[paste0(var, "_5ans")]],
                                         climat[[paste0(var, "_indisp")]])
}
rm(vars, suffixes)

#Nombre total d'ordinateurs et tablettes
climat$ordis.nbtotal <- rowSums(select(climat, ordis.fixeperso_nb, ordis.fixepro_nb, ordis.partage_nb,
                                       ordis.portablepro_nb, ordis.portableperso_nb,
                                       ordis.tablettepro_nb, ordis.tabletteperso_nb))

climat$ordis.nbtotaltranch<-quant.cut(climat$ordis.nbtotal, 6)

climat$ordis.nbtotalpro <- rowSums(select(climat, ordis.fixepro_nb, ordis.portablepro_nb, ordis.tablettepro_nb))

#Nombre total d'ordinateurs et tablettes de moins de 5 ans
climat$ordis.5anstotal <- rowSums(select(climat, ordis.fixeperso_5ans, ordis.fixepro_5ans, ordis.partage_5ans,
                                         ordis.portablepro_5ans, ordis.portableperso_5ans,
                                         ordis.tablettepro_5ans, ordis.tabletteperso_5ans))

climat$ordis.5anstotalpro <- rowSums(select(climat, ordis.fixepro_5ans, ordis.portablepro_5ans, ordis.tablettepro_5ans))

#Nombre total d'ordinateurs et tablettes indispensables
climat$ordis.indisptotal <- rowSums(select(climat, ordis.fixeperso_indisp, ordis.fixepro_indisp, ordis.partage_indisp,
                                           ordis.portablepro_indisp, ordis.portableperso_indisp,
                                           ordis.tablettepro_indisp, ordis.tabletteperso_indisp))

climat$ordis.indisptotalpro <- rowSums(select(climat, ordis.fixepro_indisp, ordis.portablepro_indisp, ordis.tablettepro_indisp))

# vols_dicho : a volé ou n'a pas volé ----
climat$vols_dicho <- ifelse(climat$volsnb=="0", "pas_vol", "vol")
climat$vols_dicho[is.na(climat$volsnb)] <- NA

# heures de vol approximatives en prenant le centre de chaque intervalle
climat$volshnum <- with(climat,
                        case_when(volsnb == 0 ~ 0,
                                  volsh == "De 1h à 10h" ~ 5,
                                  volsh == "De 11h à 20h" ~ 15,
                                  volsh == "De 20h à 50h" ~ 35,
                                  volsh == "Plus de 50h" ~ 60))

#Durée moyenne des vols effectués (hors module ?)
climat$volsduree_moy<-climat$volshnum/climat$volsnb
climat$volsduree_moy[climat$volsnb==0]<-0

#Voler/pas voler depuis 3 ans
climat$vols_dicho3ans <- ifelse(climat$volsnb=="0", ifelse(climat$vols2ans=="Non", "N'a pas volé en 3 ans", "A volé depuis 3 ans"),  "A volé depuis 3 ans")
climat$vols_dicho3ans[is.na(climat$volsnb)] <- NA

#Ne pas voler en 2019
climat$vols_dicho <- ifelse(climat$volsnb=="0", "N'a pas volé en 2019", "A volé en 2019")
climat$vols_dicho[is.na(climat$volsnb)] <- NA

#Prendre moins l'avion depuis 5 ans pour des confs, réunions, congrès
climat$Moinsavionconf[climat$solevolges.conf %in% c("Fortement diminué", "Un peu augmenté")]<-"Oui"
climat$Moinsavionconf[climat$solevolges.conf %in% c("Été à peu près stables", "Un peu augmenté", "Fortement augmenté")]<-"Non"
climat$Moinsavionconf<-as.factor(climat$Moinsavionconf)

#Prendre moins l'avion/voiture/bateau depuis 5 ans pour le recueil de données
climat$Moinsaviondonnees[climat$solevolges.donnees %in% c("Fortement diminué", "Fortement diminué")]<-"Oui"
climat$Moinsaviondonnees[climat$solevolges.donnees %in% c("Été à peu près stables", "Un peu augmenté", "Fortement augmenté")]<-"Non"
climat$Moinsaviondonnees<-as.factor(climat$Moinsaviondonnees)

#Contruction variable alternative (sans les "stables") : Prendre moins l'avion/voiture/bateau depuis 5 ans pour le recueil de données
#On ne tient pas compte des "stables"
climat$Moinsaviondonnees2[climat$solevolges.donnees %in% c("Fortement diminué", "Fortement diminué")]<-"Oui"
climat$Moinsaviondonnees2[climat$solevolges.donnees %in% c("Un peu augmenté", "Fortement augmenté")]<-"Non"
climat$Moinsaviondonnees2<-as.factor(climat$Moinsaviondonnees2)

#Prendre moins l'avion depuis 5 ans dans la vie privée
climat$Moinsavionperso[climat$avionpersochgt %in% c("Oui, je le prends beaucoup moins", "Oui, je le prends un peu moins")]<-"Oui"
climat$Moinsavionperso[climat$avionpersochgt %in% c("Oui, je le prends beaucoup plus", "Oui, je le prends un peu plus", "Non")]<-"Non"
climat$Moinsavionperso<-as.factor(climat$Moinsavionperso)

#Aucun vol perso
climat$aucunvolperso[climat$avionperso!="Aucun aller-retour" & !is.na(climat$avionperso)]<-"Non"
climat$aucunvolperso[climat$avionperso=="Aucun aller-retour"]<-"Oui"

#volsnb en tranche
climat$volsnbtranch<-quant.cut(climat$volsnb, 30)
climat$volsnbtranch2 <- cut(climat$volsnb,
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      breaks = c(0, 0.4, 1, 2, 3, 4, 5, 8, 65)
)

#On met le temps de vol à zéro pour ceux qui n'ont indiqué aucun vol
climat$volsh<-factor(ifelse(!is.na(climat$volsnb) & climat$volsnb==0, "0h",
                            as.character(climat$volsh)),
                     levels=c("0h", "De 1h à 10h", "De 11h à 20h", "De 20h à 50h", "Plus de 50h"))

#Dichotomisation extremement preoccupé, réduire les GES et décroissance
climat$extrpreoccupe <- ifelse(climat$preoccupe2=="Extrêmement préoccupé·e", "Oui", "Non")
climat$reducrechexemp <- ifelse(climat$solreducrech=="La recherche doit montrer l'exemple", "Oui", "Non")

# Recodage nécessaire uniquement avec l'ancienne version de la base,
# qui contient les modalités longues et tronquées
climat$solreducrech<-as.character(climat$solreducrech)
climat$solreducrech[climat$solreducrech == "La recherche publique doit montrer l'exemple en matière de diminution des émissions de gaz à effet de serre en les rédui"] <- "La recherche doit montrer l'exemple"
climat$solreducrech[climat$solreducrech == "La recherche publique doit réduire ses émissions de gaz à effet de serre d'un tiers environ"] <- "La recherche doit réduire ses émissions d'un tiers"
climat$solreducrech[climat$solreducrech == "En raison de son rôle, la recherche publique peut bénéficier d'un statut dérogatoire, c'est-à-dire fournir des efforts m"] <- "La recherche peut bénéficier d'un statut dérogatoire"
climat$solreducrech<-as.factor(climat$solreducrech)

climat$solreducrech2 <- fct_recode(climat$solreducrech,
                                   "Recherche doit montrer l'exemple, réduire de plus d'1/3"="La recherche doit montrer l'exemple",
                                   "Recherche doit réduire émissions de GES de 1/3 environ"="La recherche doit réduire ses émissions d'un tiers",
                                   "Recherche peut bénéficier d'un statut dérogatoire"="La recherche peut bénéficier d'un statut dérogatoire")
climat$solreducrech3 <- fct_recode(climat$solreducrech,
                                   "Réduire de plus d'un tiers"="La recherche doit montrer l'exemple",
                                   "Réduire d'un tiers"="La recherche doit réduire ses émissions d'un tiers",
                                   "Réduire de moins d'un tiers"="La recherche peut bénéficier d'un statut dérogatoire")

climat$tresdecroissance <- ifelse(climat$opinionecolo.decroissance=="Tout à fait d'accord", "Oui", "Non")

#Variables en tranche pour hindex, nbpublis
climat$hindextranch<-quant.cut(climat$hindex, 6)
climat$nbpublistranch<-quant.cut(climat$nbpublis, 6)

climat$hindextranch2 <- cut(climat$hindex,
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      breaks = c(0, 8, 13, 18, 23, 30, 50, 80, 176)
)

climat$nbpublistranch2 <- cut(climat$nbpublis,
                                        include.lowest = TRUE,
                                        right = TRUE,
                                        breaks = c(0, 0.5, 2, 4, 7, 12, 20, 40, 180)
)

#"connaitre son h-index", dichotomique
climat$hindexconnDicho[climat$hindexconn=="Oui"]<-"Oui"
climat$hindexconnDicho[climat$hindexconn %in% c("Non", "Je ne suis pas certain de ce qu'est le h-index")]<-"Non"
freq(climat$hindexconn, total = T)

#Nombre de conf à l'étranger les cinq dernières années
climat$conffois5ans<-ifelse(climat$conf!="Oui, dans les 5 dernières années", "Zéro fois", as.character(climat$conffois))

#Bilan carbone dichotomique
climat$bilandicho <- ifelse(climat$dixannees.bilan=="Oui", "Oui", "Non")
climat$bilandicho[climat$dixannees.bilan=="Je ne souhaite pas répondre"]<-"NA"
climat$bilandicho <- fct_relevel(climat$bilandicho, "Non")


#Privilégier la visio pour des limiter les émissions de GES

climat$visioprivilegiee.emissionsoui[climat$visioprivilegiee.emissions %in% c("Oui, beaucoup","Oui, un peu")]<-"Oui"
climat$visioprivilegiee.emissionsoui[climat$visioprivilegiee.emissions %in% c("Non, pas vraiment","Non, pas du tout")]<-"Non"
climat$visioprivilegiee.emissionsoui <- as.factor(climat$visioprivilegiee.emissionsoui)
climat$visioprivilegiee.emissionsoui <- fct_relevel(climat$visioprivilegiee.emissionsoui, "Non")


#La visio génère des pb techniques

climat$visiopb.techniqueoui[climat$visiopb.technique %in% c("Oui, beaucoup","Oui, un peu")]<-"Oui"
climat$visiopb.techniqueoui[climat$visiopb.technique %in% c("Non, pas vraiment","Non, pas du tout")]<-"Non"
climat$visiopb.techniqueoui <- as.factor(climat$visiopb.techniqueoui)
climat$visiopb.techniqueoui <- fct_relevel(climat$visiopb.techniqueoui, "Non")

#La visio limite les aspects relationnels

climat$visiopb.relationoui[climat$visiopb.relation %in% c("Oui, beaucoup","Oui, un peu")]<-"Oui"
climat$visiopb.relationoui[climat$visiopb.relation %in% c("Non, pas vraiment","Non, pas du tout")]<-"Non"
climat$visiopb.relationoui <- as.factor(climat$visiopb.relationoui)
climat$visiopb.relationoui <- fct_relevel(climat$visiopb.relationoui, "Non")

climat$visiopb.relationbcpoui[climat$visiopb.relation %in% c("Oui, beaucoup")]<-"Oui, beaucoup"
climat$visiopb.relationbcpoui[climat$visiopb.relation %in% c("Oui, un peu", "Non, pas vraiment","Non, pas du tout")]<-"Pas oui beaucoup"
climat$visiopb.relationbcpoui <- as.factor(climat$visiopb.relationbcpoui)
climat$visiopb.relationbcpoui <- fct_relevel(climat$visiopb.relationbcpoui, "Pas oui beaucoup")

#Visio : difficile écrire schémas, équation

climat$visiopb.ecrireoui[climat$visiopb.ecrire %in% c("Oui, beaucoup","Oui, un peu")]<-"Oui"
climat$visiopb.ecrireoui[climat$visiopb.ecrire %in% c("Non, pas vraiment","Non, pas du tout")]<-"Non"
climat$visiopb.ecrireoui <- as.factor(climat$visiopb.ecrireoui)
climat$visiopb.ecrireoui <- fct_relevel(climat$visiopb.ecrireoui, "Non")

#La visio plus fatiguante

climat$visiopb.fatigueoui[climat$visiopb.fatigue %in% c("Oui, beaucoup","Oui, un peu")]<-"Oui"
climat$visiopb.fatigueoui[climat$visiopb.fatigue %in% c("Non, pas vraiment","Non, pas du tout")]<-"Non"
climat$visiopb.fatigueoui <- as.factor(climat$visiopb.fatigueoui)
climat$visiopb.fatigueoui <- fct_relevel(climat$visiopb.fatigueoui, "Non")

## Recodage de climat$enfantsnb en climat$enfantsnb_rec
climat$enfantsnb_rec <- as.character(climat$enfantsnb)
climat$enfantsnb_rec <- fct_recode(climat$enfantsnb_rec,
                                             "2 ou plus" = "2",
                                             "2 ou plus" = "3",
                                             "2 ou plus" = "4",
                                             "2 ou plus" = "5",
                                             "2 ou plus" = "13",
                                             "2 ou plus" = "6",
                                             "2 ou plus" = "9",
                                             "2 ou plus" = "7",
                                             "2 ou plus" = "10"
)


#recodage du nombre d'enfants avec l'âge
climat$enfantsage_rec <- NULL
climat$enfantsage_rec[climat$enfantsage <= 5] <- "moins de 5 ans"
climat$enfantsage_rec[climat$enfantsage <= 15 &
                                  climat$enfantsage > 5 ] <- "Entre 5 et 15 ans"
# climat$enfantsage_rec[climat$enfantsage <= 15 &
#                                   climat$enfantsage > 10 ] <- "Entre 10 et 15 ans"
climat$enfantsage_rec[climat$enfantsage > 15 ] <- "Plus de 15 ans"
climat$enfantsage_rec[climat$enfantsnb_rec == "0" ] <- "Sans enfant"
climat$enfantsage_rec <- as.factor(climat$enfantsage_rec)


# Fusionner les deux variables de discipline (chercheurs vs. ITA)
# On ne garde que la discipline des ITA des BAP A à D, qui participent directement à la recherche
climat$disciplineita_rech <- if_else(substr(climat$bap, 1, 5) %in% c("BAP A", "BAP B", "BAP C", "BAP D"),
                                     climat$disciplineita, factor(NA_character_))
climat$discipline <- coalesce(climat$discipline, climat$disciplineita_rech)

# Discipline agrégée
# Noms inspirés de https://data.esr.gouv.fr/FR/T895/P311/tableau_des_enseignants_de_l_enseignement_superieur_public_niveau_etablissement_-_ressources_humaines#TDB

climat$discipline_agr <- fct_recode(
  climat$discipline,
  "Droit, économie, gestion"="01 : Droit privé et sciences criminelles",
  "Droit, économie, gestion"="02 : Droit public",
  "Droit, économie, gestion"="03 : Histoire du droit et des institutions",
  "Droit, économie, gestion"="04 : Science politique",
  "Droit, économie, gestion"="05 : Sciences économiques",
  "Droit, économie, gestion"="06 : Sciences de gestion",
  "Langues et littératures"="07 : Sciences du langage : linguistique et phonétique générales",
  "Langues et littératures"="08 : Langues et littératures anciennes",
  "Langues et littératures"="09 : Langue et littérature françaises",
  "Langues et littératures"="10 : Littératures comparées",
  "Langues et littératures"="11 : Langues et littératures anglaises et anglo-saxonnes",
  "Langues et littératures"="12 : Langues et littératures germaniques et scandinaves",
  "Langues et littératures"="13 : Langues et littératures slaves",
  "Langues et littératures"="14 : Langues et littératures romanes : espagnol, italien, portugais, autres langues romanes",
  "Langues et littératures"="15 : Langues et littératures arabes, chinoises, japonaises, hébraïques, d'autres domaines linguistiques",
  "Sciences humaines"="16 : Psychologie, psychologie clinique, psychologie sociale",
  "Sciences humaines"="17 : Philosophie",
  "Sciences humaines"="18 : Architecture et Arts : plastiques, du spectacle, musique, musicologie, esthétique, sciences de l'art",
  "Sciences humaines"="19 : Sociologie, démographie",
  "Sciences humaines"="20 : Anthropologie biologique, ethnologie, préhistoire",
  "Sciences humaines"="21 : Histoire et civilisations : histoire et archéologie des mondes anciens et des mondes médiévaux",
  "Sciences humaines"="22 : Histoire et civilisations : histoire des mondes modernes, histoire du monde contemporain",
  "Sciences humaines"="23 : Géographie physique, humaine, économique et régionale",
  "Sciences humaines"="24 : Aménagement de l'espace, urbanisme",
  "Mathématiques et informatique"="25 : Mathématiques",
  "Mathématiques et informatique"="26 : Mathématiques appliquées et applications des mathématiques",
  "Mathématiques et informatique"="27 : Informatique",
  "Physique"="28 : Milieux denses et matériaux",
  "Physique"="29 : Constituants élémentaires",
  "Physique"="30 : Milieux dilués et optique",
  "Chimie"="31 : Chimie théorique, physique, analytique",
  "Chimie"="32 : Chimie organique, inorganique, industrielle",
  "Chimie"="33 : Chimie des matériaux",
  "Sciences de la Terre et de l'Univers"="34 : Astronomie, astrophysique",
  "Sciences de la Terre et de l'Univers"="35 : Structure et évolution de la Terre et des autres planètes",
  "Sciences de la Terre et de l'Univers"="36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère",
  "Sciences de la Terre et de l'Univers"="37 : Météorologie, océanographie physique et physique de l'environnement",
  "Santé"="42 : Morphologie et morphogenèse",
  "Santé"="43 : Biophysique et imagerie médicale",
  "Santé"="44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition",
  "Santé"="45 : Microbiologie, maladies transmissibles et hygiène",
  "Santé"="46 : Santé publique, environnement et société",
  "Santé"="47 : Cancérologie, génétique, hématologie, immunologie",
  "Santé"="48 : Anesthésiologie, réanimation, médecine d'urgence, pharmacologie et thérapeutique",
  "Santé"="49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation",
  "Santé"="50 : Pathologie ostéo-articulaire, dermatologie et chirurgie plastique",
  "Santé"="51 : Pathologie cardiorespiratoire et vasculaire",
  "Santé"="52 : Maladies des appareils digestif et urinaire",
  "Santé"="53 : Médecine interne, gériatrie, chirurgie générale et médecine générale",
  "Santé"="54 : Développement et pathologie de l'enfant, gynécologie-obstétrique, endocrinologie et reproduction",
  "Santé"="55 : Pathologie de la tête et du cou",
  "Santé"="56 : Développement, croissance et prévention",
  "Santé"="57 : Sciences biologiques, médecine et chirurgie buccales",
  "Santé"="58 : Sciences physiques et physiologiques endodontiques et prothétiques",
  "Génies : méca, info, élec, énergie"="60 : Mécanique, génie mécanique, génie civil",
  "Génies : méca, info, élec, énergie"="61 : Génie informatique, automatique et traitement du signal",
  "Génies : méca, info, élec, énergie"="62 : Énergétique, génie des procédés",
  "Génies : méca, info, élec, énergie"="63 : Génie Électrique, Électronique, optronique et systèmes",
  "Biologie"="64 : Biochimie et biologie moléculaire",
  "Biologie"="65 : Biologie cellulaire",
  "Biologie"="66 : Physiologie",
  "Biologie"="67 : Biologie des populations et écologie",
  "Biologie"="68 : Biologie des organismes",
  "Biologie"="69 : Neurosciences",
  "Sciences humaines"="70 : Sciences de l'éducation",
  "Sciences humaines"="71 : Sciences de l'information et de la communication",
  "Sciences humaines"="72 : Épistémologie, histoire des sciences et des techniques",
  "Sciences humaines"="73 : Cultures et langues régionales",
  "Sciences humaines"="74 : Sciences et techniques des activités physiques et sportives",
  "Sciences humaines"="76 : Théologie catholique",
  "Sciences humaines"="77 : Théologie protestante",
  "Santé"="80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)",
  "Santé"="81/86 : Sciences du médicament et des autres produits de santé (ex-40)",
  "Santé"="82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)",
  "Santé"="90 : Maïeutique",
  "Santé"="91 : Sciences de la rééducation et de la réadaptation",
  "Santé"="92 : Sciences infirmières"
)



#Recodage discipline pour avoir une subdivision plus fine avec des effectifs plus homogènes

climat$discipline_agr2 <- fct_recode(
  climat$discipline,
  "Droit, économie, gestion"="01 : Droit privé et sciences criminelles",
  "Droit, économie, gestion"="02 : Droit public",
  "Droit, économie, gestion"="03 : Histoire du droit et des institutions",
  "Droit, économie, gestion"="04 : Science politique",
  "Droit, économie, gestion"="05 : Sciences économiques",
  "Droit, économie, gestion"="06 : Sciences de gestion",
  "Autres lettres et sciences humaines"="07 : Sciences du langage : linguistique et phonétique générales",
  "Autres lettres et sciences humaines"="08 : Langues et littératures anciennes",
  "Autres lettres et sciences humaines"="09 : Langue et littérature françaises",
  "Autres lettres et sciences humaines"="10 : Littératures comparées",
  "Autres lettres et sciences humaines"="11 : Langues et littératures anglaises et anglo-saxonnes",
  "Autres lettres et sciences humaines"="12 : Langues et littératures germaniques et scandinaves",
  "Autres lettres et sciences humaines"="13 : Langues et littératures slaves",
  "Autres lettres et sciences humaines"="14 : Langues et littératures romanes : espagnol, italien, portugais, autres langues romanes",
  "Autres lettres et sciences humaines"="15 : Langues et littératures arabes, chinoises, japonaises, hébraïques, d'autres domaines linguistiques",
  "Autres lettres et sciences humaines"="16 : Psychologie, psychologie clinique, psychologie sociale",
  "Autres lettres et sciences humaines"="17 : Philosophie",
  "Autres lettres et sciences humaines"="18 : Architecture et Arts : plastiques, du spectacle, musique, musicologie, esthétique, sciences de l'art",
  "Socio, démo, anthropo"="19 : Sociologie, démographie",
  "Socio, démo, anthropo"="20 : Anthropologie biologique, ethnologie, préhistoire",
  "Histoire, géo, urba"="21 : Histoire et civilisations : histoire et archéologie des mondes anciens et des mondes médiévaux",
  "Histoire, géo, urba"="22 : Histoire et civilisations : histoire des mondes modernes, histoire du monde contemporain",
  "Histoire, géo, urba"="23 : Géographie physique, humaine, économique et régionale",
  "Histoire, géo, urba"="24 : Aménagement de l'espace, urbanisme",
  "Mathématiques"="25 : Mathématiques",
  "Mathématiques"="26 : Mathématiques appliquées et applications des mathématiques",
  "Informatique"="27 : Informatique",
  "Physique"="28 : Milieux denses et matériaux",
  "Physique"="29 : Constituants élémentaires",
  "Physique"="30 : Milieux dilués et optique",
  "Chimie"="31 : Chimie théorique, physique, analytique",
  "Chimie"="32 : Chimie organique, inorganique, industrielle",
  "Chimie"="33 : Chimie des matériaux",
  "Sciences de la Terre et de l'Univers"="34 : Astronomie, astrophysique",
  "Sciences de la Terre et de l'Univers"="35 : Structure et évolution de la Terre et des autres planètes",
  "Sciences de la Terre et de l'Univers"="36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère",
  "Sciences de la Terre et de l'Univers"="37 : Météorologie, océanographie physique et physique de l'environnement",
  "Santé"="42 : Morphologie et morphogenèse",
  "Santé"="43 : Biophysique et imagerie médicale",
  "Santé"="44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition",
  "Santé"="45 : Microbiologie, maladies transmissibles et hygiène",
  "Santé"="46 : Santé publique, environnement et société",
  "Santé"="47 : Cancérologie, génétique, hématologie, immunologie",
  "Santé"="48 : Anesthésiologie, réanimation, médecine d'urgence, pharmacologie et thérapeutique",
  "Santé"="49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation",
  "Santé"="50 : Pathologie ostéo-articulaire, dermatologie et chirurgie plastique",
  "Santé"="51 : Pathologie cardiorespiratoire et vasculaire",
  "Santé"="52 : Maladies des appareils digestif et urinaire",
  "Santé"="53 : Médecine interne, gériatrie, chirurgie générale et médecine générale",
  "Santé"="54 : Développement et pathologie de l'enfant, gynécologie-obstétrique, endocrinologie et reproduction",
  "Santé"="55 : Pathologie de la tête et du cou",
  "Santé"="56 : Développement, croissance et prévention",
  "Santé"="57 : Sciences biologiques, médecine et chirurgie buccales",
  "Santé"="58 : Sciences physiques et physiologiques endodontiques et prothétiques",
  "Génies : méca, info, élec, énergie"="60 : Mécanique, génie mécanique, génie civil",
  "Génies : méca, info, élec, énergie"="61 : Génie informatique, automatique et traitement du signal",
  "Génies : méca, info, élec, énergie"="62 : Énergétique, génie des procédés",
  "Génies : méca, info, élec, énergie"="63 : Génie Électrique, Électronique, optronique et systèmes",
  "Biologie"="64 : Biochimie et biologie moléculaire",
  "Biologie"="65 : Biologie cellulaire",
  "Biologie"="66 : Physiologie",
  "Biologie"="67 : Biologie des populations et écologie",
  "Biologie"="68 : Biologie des organismes",
  "Biologie"="69 : Neurosciences",
  "Autres lettres et sciences humaines"="70 : Sciences de l'éducation",
  "Autres lettres et sciences humaines"="71 : Sciences de l'information et de la communication",
  "Autres lettres et sciences humaines"="72 : Épistémologie, histoire des sciences et des techniques",
  "Autres lettres et sciences humaines"="73 : Cultures et langues régionales",
  "Autres lettres et sciences humaines"="74 : Sciences et techniques des activités physiques et sportives",
  "Autres lettres et sciences humaines"="76 : Théologie catholique",
  "Autres lettres et sciences humaines"="77 : Théologie protestante",
  "Santé"="80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)",
  "Santé"="81/86 : Sciences du médicament et des autres produits de santé (ex-40)",
  "Santé"="82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)",
  "Santé"="90 : Maïeutique",
  "Santé"="91 : Sciences de la rééducation et de la réadaptation",
  "Santé"="92 : Sciences infirmières"
)

#Pour tenir compte de la spécificité de sous disciplines en matière de temps de vol 
#si ces disciplines comportent suffisamment d'effectif et se distinguent des autres avec lesquelles elles sont regroupées, je les isole
# Ex : Biologie des populations et écologie et anthropo distingué de socio ; et climato distingué aussi
climat$discipline_agr3 <- fct_recode(climat$discipline,
"Droit, économie, gestion"="01 : Droit privé et sciences criminelles",
"Droit, économie, gestion"="02 : Droit public",
"Droit, économie, gestion"="03 : Histoire du droit et des institutions",
"Droit, économie, gestion"="04 : Science politique",
"Droit, économie, gestion"="05 : Sciences économiques",
"Droit, économie, gestion"="06 : Sciences de gestion",
"Autres lettres et sciences humaines"="07 : Sciences du langage : linguistique et phonétique générales",
"Autres lettres et sciences humaines"="08 : Langues et littératures anciennes",
"Autres lettres et sciences humaines"="09 : Langue et littérature françaises",
"Autres lettres et sciences humaines"="10 : Littératures comparées",
"Autres lettres et sciences humaines"="11 : Langues et littératures anglaises et anglo-saxonnes",
"Autres lettres et sciences humaines"="12 : Langues et littératures germaniques et scandinaves",
"Autres lettres et sciences humaines"="13 : Langues et littératures slaves",
"Autres lettres et sciences humaines"="14 : Langues et littératures romanes : espagnol, italien, portugais, autres langues romanes",
"Autres lettres et sciences humaines"="15 : Langues et littératures arabes, chinoises, japonaises, hébraïques, d'autres domaines linguistiques",
"Autres lettres et sciences humaines"="16 : Psychologie, psychologie clinique, psychologie sociale",
"Autres lettres et sciences humaines"="17 : Philosophie",
"Archi/arts, anthropo ethno"="18 : Architecture et Arts : plastiques, du spectacle, musique, musicologie, esthétique, sciences de l'art",
"Socio, démo"="19 : Sociologie, démographie",
"Archi/arts, anthropo ethno"="20 : Anthropologie biologique, ethnologie, préhistoire",
"Histoire, géo, urba"="21 : Histoire et civilisations : histoire et archéologie des mondes anciens et des mondes médiévaux",
"Histoire, géo, urba"="22 : Histoire et civilisations : histoire des mondes modernes, histoire du monde contemporain",
"Histoire, géo, urba"="23 : Géographie physique, humaine, économique et régionale",
"Histoire, géo, urba"="24 : Aménagement de l'espace, urbanisme",
"Mathématiques"="25 : Mathématiques",
"Mathématiques"="26 : Mathématiques appliquées et applications des mathématiques",
"Informatique"="27 : Informatique",
"Physique"="28 : Milieux denses et matériaux",
"Physique"="29 : Constituants élémentaires",
"Physique"="30 : Milieux dilués et optique",
"Chimie"="31 : Chimie théorique, physique, analytique",
"Chimie"="32 : Chimie organique, inorganique, industrielle",
"Chimie"="33 : Chimie des matériaux",
"Astro, géologie"="34 : Astronomie, astrophysique",
"Astro, géologie"="35 : Structure et évolution de la Terre et des autres planètes",
"Astro, géologie"="36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère",
"Météo, océano, physiqu environt"="37 : Météorologie, océanographie physique et physique de l'environnement",
"Santé"="42 : Morphologie et morphogenèse",
"Santé"="43 : Biophysique et imagerie médicale",
"Santé"="44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition",
"Santé"="45 : Microbiologie, maladies transmissibles et hygiène",
"Santé"="46 : Santé publique, environnement et société",
"Santé"="47 : Cancérologie, génétique, hématologie, immunologie",
"Santé"="48 : Anesthésiologie, réanimation, médecine d'urgence, pharmacologie et thérapeutique",
"Santé"="49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation",
"Santé"="50 : Pathologie ostéo-articulaire, dermatologie et chirurgie plastique",
"Santé"="51 : Pathologie cardiorespiratoire et vasculaire",
"Santé"="52 : Maladies des appareils digestif et urinaire",
"Santé"="53 : Médecine interne, gériatrie, chirurgie générale et médecine générale",
"Santé"="54 : Développement et pathologie de l'enfant, gynécologie-obstétrique, endocrinologie et reproduction",
"Santé"="55 : Pathologie de la tête et du cou",
"Santé"="56 : Développement, croissance et prévention",
"Santé"="57 : Sciences biologiques, médecine et chirurgie buccales",
"Santé"="58 : Sciences physiques et physiologiques endodontiques et prothétiques",
"Génies : méca, info, élec, énergie"="60 : Mécanique, génie mécanique, génie civil",
"Génies : méca, info, élec, énergie"="61 : Génie informatique, automatique et traitement du signal",
"Génies : méca, info, élec, énergie"="62 : Énergétique, génie des procédés",
"Génies : méca, info, élec, énergie"="63 : Génie Électrique, Électronique, optronique et systèmes",
"Biologie"="64 : Biochimie et biologie moléculaire",
"Biologie"="65 : Biologie cellulaire",
"Biologie"="66 : Physiologie",
"Biologie des populations et écologie"="67 : Biologie des populations et écologie",
"Biologie"="68 : Biologie des organismes",
"Biologie"="69 : Neurosciences",
"Autres lettres et sciences humaines"="70 : Sciences de l'éducation",
"Autres lettres et sciences humaines"="71 : Sciences de l'information et de la communication",
"Autres lettres et sciences humaines"="72 : Épistémologie, histoire des sciences et des techniques",
"Autres lettres et sciences humaines"="73 : Cultures et langues régionales",
"Autres lettres et sciences humaines"="74 : Sciences et techniques des activités physiques et sportives",
"Autres lettres et sciences humaines"="76 : Théologie catholique",
"Autres lettres et sciences humaines"="77 : Théologie protestante",
"Santé"="80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)",
"Santé"="81/86 : Sciences du médicament et des autres produits de santé (ex-40)",
"Santé"="82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)",
"Santé"="90 : Maïeutique",
"Santé"="91 : Sciences de la rééducation et de la réadaptation",
"Santé"="92 : Sciences infirmières"
)


climat$discipline_agr4 <- fct_recode(climat$discipline,
"Droit, économie, gestion"="01 : Droit privé et sciences criminelles",
"Droit, économie, gestion"="02 : Droit public",
"Droit, économie, gestion"="03 : Histoire du droit et des institutions",
"Droit, économie, gestion"="04 : Science politique",
"Droit, économie, gestion"="05 : Sciences économiques",
"Droit, économie, gestion"="06 : Sciences de gestion",
"Autres lettres et sciences humaines"="07 : Sciences du langage : linguistique et phonétique générales",
"Autres lettres et sciences humaines"="08 : Langues et littératures anciennes",
"Autres lettres et sciences humaines"="09 : Langue et littérature françaises",
"Autres lettres et sciences humaines"="10 : Littératures comparées",
"Autres lettres et sciences humaines"="11 : Langues et littératures anglaises et anglo-saxonnes",
"Autres lettres et sciences humaines"="12 : Langues et littératures germaniques et scandinaves",
"Autres lettres et sciences humaines"="13 : Langues et littératures slaves",
"Autres lettres et sciences humaines"="14 : Langues et littératures romanes : espagnol, italien, portugais, autres langues romanes",
"Autres lettres et sciences humaines"="15 : Langues et littératures arabes, chinoises, japonaises, hébraïques, d'autres domaines linguistiques",
"Autres lettres et sciences humaines"="16 : Psychologie, psychologie clinique, psychologie sociale",
"Autres lettres et sciences humaines"="17 : Philosophie",
"Autres lettres et sciences humaines"="18 : Architecture et Arts : plastiques, du spectacle, musique, musicologie, esthétique, sciences de l'art",
"Autres lettres et sciences humaines"="19 : Sociologie, démographie",
"Histoire, géo, urba, anthropo"="20 : Anthropologie biologique, ethnologie, préhistoire",
"Histoire, géo, urba, anthropo"="21 : Histoire et civilisations : histoire et archéologie des mondes anciens et des mondes médiévaux",
"Histoire, géo, urba, anthropo"="22 : Histoire et civilisations : histoire des mondes modernes, histoire du monde contemporain",
"Histoire, géo, urba, anthropo"="23 : Géographie physique, humaine, économique et régionale",
"Histoire, géo, urba, anthropo"="24 : Aménagement de l'espace, urbanisme",
"Mathématiques"="25 : Mathématiques",
"Mathématiques"="26 : Mathématiques appliquées et applications des mathématiques",
"Informatique"="27 : Informatique",
"Physique"="28 : Milieux denses et matériaux",
"Physique"="29 : Constituants élémentaires",
"Physique"="30 : Milieux dilués et optique",
"Chimie"="31 : Chimie théorique, physique, analytique",
"Chimie"="32 : Chimie organique, inorganique, industrielle",
"Chimie"="33 : Chimie des matériaux",
"Astronomie"="34 : Astronomie, astrophysique",
"Géologie"="35 : Structure et évolution de la Terre et des autres planètes",
"Géologie"="36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère",
"Météo, océano, physique environt"="37 : Météorologie, océanographie physique et physique de l'environnement",
"Santé et recherche médicale"="42 : Morphologie et morphogenèse",
"Santé et recherche médicale"="43 : Biophysique et imagerie médicale",
"Santé et recherche médicale"="44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition",
"Santé et recherche médicale"="45 : Microbiologie, maladies transmissibles et hygiène",
"Santé et recherche médicale"="46 : Santé publique, environnement et société",
"Santé et recherche médicale"="47 : Cancérologie, génétique, hématologie, immunologie",
"Santé et recherche médicale"="48 : Anesthésiologie, réanimation, médecine d'urgence, pharmacologie et thérapeutique",
"Santé et recherche médicale"="49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation",
"Santé et recherche médicale"="50 : Pathologie ostéo-articulaire, dermatologie et chirurgie plastique",
"Santé et recherche médicale"="51 : Pathologie cardiorespiratoire et vasculaire",
"Santé et recherche médicale"="52 : Maladies des appareils digestif et urinaire",
"Santé et recherche médicale"="53 : Médecine interne, gériatrie, chirurgie générale et médecine générale",
"Santé et recherche médicale"="54 : Développement et pathologie de l'enfant, gynécologie-obstétrique, endocrinologie et reproduction",
"Santé et recherche médicale"="55 : Pathologie de la tête et du cou",
"Santé et recherche médicale"="56 : Développement, croissance et prévention",
"Santé et recherche médicale"="57 : Sciences biologiques, médecine et chirurgie buccales",
"Santé et recherche médicale"="58 : Sciences physiques et physiologiques endodontiques et prothétiques",
"Ingénierie"="60 : Mécanique, génie mécanique, génie civil",
"Ingénierie"="61 : Génie informatique, automatique et traitement du signal",
"Ingénierie"="62 : Énergétique, génie des procédés",
"Ingénierie"="63 : Génie Électrique, Électronique, optronique et systèmes",
"Biologie"="64 : Biochimie et biologie moléculaire",
"Biologie"="65 : Biologie cellulaire",
"Biologie"="66 : Physiologie",
"Biologie des populations et écologie"="67 : Biologie des populations et écologie",
"Biologie"="68 : Biologie des organismes",
"Biologie"="69 : Neurosciences",
"Autres lettres et sciences humaines"="70 : Sciences de l'éducation",
"Autres lettres et sciences humaines"="71 : Sciences de l'information et de la communication",
"Autres lettres et sciences humaines"="72 : Épistémologie, histoire des sciences et des techniques",
"Autres lettres et sciences humaines"="73 : Cultures et langues régionales",
"Autres lettres et sciences humaines"="74 : Sciences et techniques des activités physiques et sportives",
"Autres lettres et sciences humaines"="76 : Théologie catholique",
"Autres lettres et sciences humaines"="77 : Théologie protestante",
"Santé et recherche médicale"="80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)",
"Santé et recherche médicale"="81/86 : Sciences du médicament et des autres produits de santé (ex-40)",
"Santé et recherche médicale"="82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)",
"Santé et recherche médicale"="90 : Maïeutique",
"Santé et recherche médicale"="91 : Sciences de la rééducation et de la réadaptation",
"Santé et recherche médicale"="92 : Sciences infirmières"
)


# nouveau découpage des disciplines afin de tenir compte des variations dans le score écolo
# et plus généralement essayer d'avoir des comportements homogènes
# certaines sont de "petits groupes" (droit gestion notamment) mais se distinguent vraiment des autres. 
climat$discipline_agr5 <- fct_recode(
    climat$discipline,
    "Droit, gestion"="01 : Droit privé et sciences criminelles",
    "Droit, gestion"="02 : Droit public",
    "Droit, gestion"="03 : Histoire du droit et des institutions",
    "Autres sciences sociales"="04 : Science politique",
    "Économie"="05 : Sciences économiques",
    "Droit, gestion"="06 : Sciences de gestion",
    "Lettres"="07 : Sciences du langage : linguistique et phonétique générales",
    "Lettres"="08 : Langues et littératures anciennes",
    "Lettres"="09 : Langue et littérature françaises",
    "Lettres"="10 : Littératures comparées",
    "Lettres"="11 : Langues et littératures anglaises et anglo-saxonnes",
    "Lettres"="12 : Langues et littératures germaniques et scandinaves",
    "Lettres"="13 : Langues et littératures slaves",
    "Lettres"="14 : Langues et littératures romanes : espagnol, italien, portugais, autres langues romanes",
    "Lettres"="15 : Langues et littératures arabes, chinoises, japonaises, hébraïques, d'autres domaines linguistiques",
    "Santé et recherche médicale"="16 : Psychologie, psychologie clinique, psychologie sociale",
    "Lettres"="17 : Philosophie",
    "Lettres"="18 : Architecture et Arts : plastiques, du spectacle, musique, musicologie, esthétique, sciences de l'art",
    "Autres sciences sociales"="19 : Sociologie, démographie",
    "Histoire, anthropologie"="20 : Anthropologie biologique, ethnologie, préhistoire",
    "Histoire, anthropologie"="21 : Histoire et civilisations : histoire et archéologie des mondes anciens et des mondes médiévaux",
    "Histoire, anthropologie"="22 : Histoire et civilisations : histoire des mondes modernes, histoire du monde contemporain",
    "Autres sciences sociales"="23 : Géographie physique, humaine, économique et régionale",
    "Autres sciences sociales"="24 : Aménagement de l'espace, urbanisme",
    "Mathématiques"="25 : Mathématiques",
    "Mathématiques"="26 : Mathématiques appliquées et applications des mathématiques",
    "Informatique"="27 : Informatique",
    "Physique"="28 : Milieux denses et matériaux",
    "Physique"="29 : Constituants élémentaires",
    "Physique"="30 : Milieux dilués et optique",
    "Chimie"="31 : Chimie théorique, physique, analytique",
    "Chimie"="32 : Chimie organique, inorganique, industrielle",
    "Chimie"="33 : Chimie des matériaux",
    "Astronomie"="34 : Astronomie, astrophysique",
    "Géologie"="35 : Structure et évolution de la Terre et des autres planètes",
    "Géologie"="36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère",
    "Météo, océano, physique environt"="37 : Météorologie, océanographie physique et physique de l'environnement",
    "Santé et recherche médicale"="42 : Morphologie et morphogenèse",
    "Santé et recherche médicale"="43 : Biophysique et imagerie médicale",
    "Santé et recherche médicale"="44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition",
    "Santé et recherche médicale"="45 : Microbiologie, maladies transmissibles et hygiène",
    "Santé et recherche médicale"="46 : Santé publique, environnement et société",
    "Santé et recherche médicale"="47 : Cancérologie, génétique, hématologie, immunologie",
    "Santé et recherche médicale"="48 : Anesthésiologie, réanimation, médecine d'urgence, pharmacologie et thérapeutique",
    "Santé et recherche médicale"="49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation",
    "Santé et recherche médicale"="50 : Pathologie ostéo-articulaire, dermatologie et chirurgie plastique",
    "Santé et recherche médicale"="51 : Pathologie cardiorespiratoire et vasculaire",
    "Santé et recherche médicale"="52 : Maladies des appareils digestif et urinaire",
    "Santé et recherche médicale"="53 : Médecine interne, gériatrie, chirurgie générale et médecine générale",
    "Santé et recherche médicale"="54 : Développement et pathologie de l'enfant, gynécologie-obstétrique, endocrinologie et reproduction",
    "Santé et recherche médicale"="55 : Pathologie de la tête et du cou",
    "Santé et recherche médicale"="56 : Développement, croissance et prévention",
    "Santé et recherche médicale"="57 : Sciences biologiques, médecine et chirurgie buccales",
    "Santé et recherche médicale"="58 : Sciences physiques et physiologiques endodontiques et prothétiques",
    "Ingénierie"="60 : Mécanique, génie mécanique, génie civil",
    "Ingénierie"="61 : Génie informatique, automatique et traitement du signal",
    "Ingénierie"="62 : Énergétique, génie des procédés",
    "Ingénierie"="63 : Génie Électrique, Électronique, optronique et systèmes",
    "Biologie"="64 : Biochimie et biologie moléculaire",
    "Biologie"="65 : Biologie cellulaire",
    "Biologie"="66 : Physiologie",
    "Écologie, organismes et populations"="67 : Biologie des populations et écologie",
    "Écologie, organismes et populations"="68 : Biologie des organismes",
    "Biologie"="69 : Neurosciences",
    "Autres sciences sociales"="70 : Sciences de l'éducation",
    "Autres sciences sociales"="71 : Sciences de l'information et de la communication",
    "Autres sciences sociales"="72 : Épistémologie, histoire des sciences et des techniques",
    "Lettres"="73 : Cultures et langues régionales",
    "Autres sciences sociales"="74 : Sciences et techniques des activités physiques et sportives",
    "Lettres"="76 : Théologie catholique",
    "Lettres"="77 : Théologie protestante",
    "Santé et recherche médicale"="80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)",
    "Santé et recherche médicale"="81/86 : Sciences du médicament et des autres produits de santé (ex-40)",
    "Santé et recherche médicale"="82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)",
    "Santé et recherche médicale"="90 : Maïeutique",
    "Santé et recherche médicale"="91 : Sciences de la rééducation et de la réadaptation",
    "Santé et recherche médicale"="92 : Sciences infirmières"
)




## création d'une variable binaire sur le thème de recherche écolo (pour l'instant oui, non, oui dans le passé)
climat$rechecoB<-0
climat$rechecoB[climat$recheco %in% c("Oui", "Non, mais je l'ai fait par le passé")]<-"Oui"
climat$rechecoB[climat$recheco=="Non" & !is.na(climat$recheco)]<-"Non"
climat$rechecoB[is.na(climat$recheco)]<-NA

climat$champmateriel <- with(climat,
                             ifelse(is.na(discipline) & is.na(bap), NA,
                                    substr(discipline, 1, 2) %in% c(25:37, 42:58, 60:82, 90:92) |
                                        substr(bap, 5, 5) %in% c("A", "B", "C", "E")))

for(var in c("tgir", "info", "extensif", "trescouteux", "couteux", "petit", "aucun"))
    climat[[paste0("materiel.", var)]] <- fct_expand(climat[[paste0("materiel.", var)]], "Non concerné·e")

climat$materiel.tgir[!climat$champmateriel] <- "Non concerné·e"
climat$materiel.info[!climat$champmateriel] <- "Non concerné·e"
climat$materiel.extensif[!climat$champmateriel] <- "Non concerné·e"
climat$materiel.trescouteux[!climat$champmateriel] <- "Non concerné·e"
climat$materiel.couteux[!climat$champmateriel] <- "Non concerné·e"
climat$materiel.petit[!climat$champmateriel] <- "Non concerné·e"
climat$materiel.aucun[!climat$champmateriel] <- "Non concerné·e"

climat$materiel <- with(climat, materiel.tgir == "Oui" | materiel.info == "Oui" |
                            materiel.extensif == "Oui" | materiel.trescouteux == "Oui" |
                            materiel.couteux == "Oui" | materiel.petit == "Oui")

#Type de matériel en une seule variable
climat$maxmateriel[climat$materiel==FALSE]<-"Aucun materiel"
climat$maxmateriel[climat$materiel.petit=="Oui"]<-"Petit materiel"
climat$maxmateriel[climat$materiel.couteux=="Oui"]<-"Materiel couteux"
climat$maxmateriel[climat$materiel.trescouteux=="Oui"]<-"Materiel très couteux"
climat$maxmateriel[climat$materiel.couteux=="Oui"& climat$materiel.trescouteux=="Oui"]<-"Materiel couteux et très couteux"
climat$maxmateriel[climat$materiel.extensif=="Oui"]<-"Dispositifs extensifs"
climat$maxmateriel[climat$materiel.tgir=="Oui"]<-"Très grande infrastructure"


# Repérer les personnes qui ont rempli le tableau sur les vols,
# soit parce qu'ils ont déclaré au moins un vol, soit parce qu'ils n'ont pas volé
climat$volsremplis <- with(climat,
                           if_else(tiragemodule == "1",
                                   (!is.na(volsnb) & volsnb == 0) | !is.na(volsdist1),
                                   NA))

for(i in 1:5) {
  volsnbi <- paste0("volsnb", i)
  volsdisti <- paste0("volsdist", i)
  volshi <- paste0("volsh", i)
  volsgesi <- paste0("volsges", i)
  
  # Si le trajet a été renseigné mais pas son nombre, on suppose un aller-retour unique
  climat[is.na(climat[[volsnbi]]) & !is.na(climat[[volsdisti]]), volsnbi] <- 1

  # Si le tableau sur les vols a été rempli, mettre les NA sur les autres vols à 0
  climat[is.na(climat[[volsnbi]]) &
           !is.na(climat$volsremplis) & climat$volsremplis, volsnbi] <- 0
  climat[is.na(climat[[volsdisti]]) &
           !is.na(climat$volsremplis) & climat$volsremplis, volsdisti] <- 0
  climat[is.na(climat[[volshi]]) &
           !is.na(climat$volsremplis) & climat$volsremplis, volshi] <- 0
  climat[is.na(climat[[volsgesi]]) &
           !is.na(climat$volsremplis) & climat$volsremplis, volsgesi] <- 0
  
  
  # Calcul de la distance/tps/GES totaux par ligne du tableau en fonction du nombre d'aller-retour déclarés
  volsdist_toti <- paste0("volsdist_tot", i)
  climat[[volsdist_toti]] <- climat[[volsdisti]] * climat[[volsnbi]]
  
  volsh_toti <- paste0("volsh_tot", i)
  climat[[volsh_toti]] <- climat[[volshi]] * climat[[volsnbi]]
  
  volsges_toti <- paste0("volsges_tot", i)
  climat[[volsges_toti]] <- climat[[volshi]] * climat[[volsnbi]]
  
  }


# Somme des vols déclarés dans le tableau
climat$volsnb_tot <- rowSums(select(climat, paste0("volsnb", 1:5)))

climat$volsdist_tot <- rowSums(select(climat, paste0("volsdist", 1:5)) * select(climat, paste0("volsnb", 1:5)))

climat$volsh_tot <- rowSums(select(climat, paste0("volsh", 1:5)) * select(climat, paste0("volsnb", 1:5)))
climat$volsges_tot <- rowSums(select(climat, paste0("volsges", 1:5))* select(climat, paste0("volsnb", 1:5)))

# Pour ceux qui ont atteint le nombre maximum de lignes,
# prendre le nombre d'heures estimé à partir de la tranche déclarée initialement
# s'il est supérieur à la somme des durées des vols renseignés dans le tableau
climat$volsh_tot2 <- if_else(is.na(climat$volsdist5) | climat$volsdist5 == 0 |
                               climat$volsh_tot > climat$volshnum,
                             climat$volsh_tot,
                             climat$volshnum)


####Création de variables à partir des données du module sur l'avion


#Durée moyenne des vols effectués (hors module ?)
climat$volsduree_moy<-climat$volshnum/climat$volsnb
climat$volsduree_moy[climat$volsnb==0]<-0

#Distance moyenne des vols effectués et déclaré dans le tableau (Attention j'avais oublié _tot après volsnb : refaire les régressions :((( ?))))
climat$volsdist_moy<-climat$volsdist_tot/climat$volsnb_tot
climat$volsdist_moy[climat$volsnb==0 & climat$tiragemodule=="1"]<-0

#Calcul distance totale par motif de vol, par personne

climat$volsdist_totconf <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Conférence, présentation", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Conférence, présentation", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Conférence, présentation", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Conférence, présentation", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Conférence, présentation", climat$volsdist_tot5, 0), NA )

climat$volsdist_totsejrech <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Séjour de recherche", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Séjour de recherche", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Séjour de recherche", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Séjour de recherche", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Séjour de recherche", climat$volsdist_tot5, 0), NA )

climat$volsdist_totworkshop <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Réunion, workshop", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Réunion, workshop", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Réunion, workshop", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Réunion, workshop", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Réunion, workshop", climat$volsdist_tot5, 0), NA )

climat$volsdist_totcours <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Enseignement, formation, école d'été", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Enseignement, formation, école d'été", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Enseignement, formation, école d'été", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Enseignement, formation, école d'été", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Enseignement, formation, école d'été", climat$volsdist_tot5, 0), NA )

climat$volsdist_totterrain <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Terrain, production et recueil de données", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Terrain, production et recueil de données", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Terrain, production et recueil de données", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Terrain, production et recueil de données", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Terrain, production et recueil de données", climat$volsdist_tot5, 0), NA )

climat$volsdist_totfinanc <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Obtention de financements", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Obtention de financements", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Obtention de financements", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Obtention de financements", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Obtention de financements", climat$volsdist_tot5, 0), NA )

climat$volsdist_toteval <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Évaluation de la recherche", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Évaluation de la recherche", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Évaluation de la recherche", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Évaluation de la recherche", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Évaluation de la recherche", climat$volsdist_tot5, 0), NA )

climat$volsdist_totjury <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Jury", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Jury", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Jury", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Jury", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Jury", climat$volsdist_tot5, 0), NA )

climat$volsdist_totautre <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Autre", climat$volsdist_tot1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Autre", climat$volsdist_tot2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Autre", climat$volsdist_tot3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Autre", climat$volsdist_tot4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Autre", climat$volsdist_tot5, 0), NA )

climat$volsdist_totconfreu <- climat$volsdist_totconf + climat$volsdist_totworkshop +
    climat$volsdist_totjury + climat$volsdist_totfinanc + climat$volsdist_toteval

#Calcul distance totale vols intérieurs vs vols internationaux
climat$volsdist_interieur <- ifelse(!is.na(climat$volsdist_tot),
                                    ifelse(climat$volsdepart1pays %in% "FR" & climat$volsarrivee1pays %in% "FR", climat$volsdist_tot1, 0) +
                                        ifelse(climat$volsdepart2pays %in% "FR" & climat$volsarrivee2pays %in% "FR", climat$volsdist_tot2, 0) +
                                        ifelse(climat$volsdepart3pays %in% "FR" & climat$volsarrivee3pays %in% "FR", climat$volsdist_tot3, 0) +
                                        ifelse(climat$volsdepart4pays %in% "FR" & climat$volsarrivee4pays %in% "FR", climat$volsdist_tot4, 0) +
                                        ifelse(climat$volsdepart5pays %in% "FR" & climat$volsarrivee5pays %in% "FR", climat$volsdist_tot5, 0),
                                    NA)

climat$volsdist_internat <- ifelse(!is.na(climat$volsdist_tot),
                                   ifelse(climat$volsdepart1pays %in% "FR" & climat$volsarrivee1pays %in% "FR", 0, climat$volsdist_tot1) +
                                       ifelse(climat$volsdepart2pays %in% "FR" & climat$volsarrivee2pays %in% "FR", 0, climat$volsdist_tot2) +
                                       ifelse(climat$volsdepart3pays %in% "FR" & climat$volsarrivee3pays %in% "FR", 0, climat$volsdist_tot3) +
                                       ifelse(climat$volsdepart4pays %in% "FR" & climat$volsarrivee4pays %in% "FR", 0, climat$volsdist_tot4) +
                                       ifelse(climat$volsdepart5pays %in% "FR" & climat$volsarrivee5pays %in% "FR", 0, climat$volsdist_tot5),
                                   NA)

#Nombre de vols par motif

climat$volsnbconf <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Conférence, présentation", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Conférence, présentation", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Conférence, présentation", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Conférence, présentation", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Conférence, présentation", climat$volsnb5, 0), NA )

climat$volsnbsejrech <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Séjour de recherche", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Séjour de recherche", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Séjour de recherche", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Séjour de recherche", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Séjour de recherche", climat$volsnb5, 0), NA )

climat$volsnbworkshop <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Réunion, workshop", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Réunion, workshop", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Réunion, workshop", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Réunion, workshop", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Réunion, workshop", climat$volsnb5, 0), NA )

climat$volsnbcours <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Enseignement, formation, école d'été", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Enseignement, formation, école d'été", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Enseignement, formation, école d'été", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Enseignement, formation, école d'été", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Enseignement, formation, école d'été", climat$volsnb5, 0), NA )

climat$volsnbterrain <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Terrain, production et recueil de données", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Terrain, production et recueil de données", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Terrain, production et recueil de données", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Terrain, production et recueil de données", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Terrain, production et recueil de données", climat$volsnb5, 0), NA )

climat$volsnbfinanc <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Obtention de financements", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Obtention de financements", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Obtention de financements", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Obtention de financements", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Obtention de financements", climat$volsnb5, 0), NA )

climat$volsnbeval <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Évaluation de la recherche", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Évaluation de la recherche", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Évaluation de la recherche", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Évaluation de la recherche", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Évaluation de la recherche", climat$volsnb5, 0), NA )

climat$volsnbjury <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Jury", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Jury", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Jury", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Jury", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Jury", climat$volsnb5, 0), NA )

climat$volsnbautre <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsmotif1) & climat$volsmotif1=="Autre", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsmotif2) & climat$volsmotif2=="Autre", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsmotif3) & climat$volsmotif3=="Autre", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsmotif4) & climat$volsmotif4=="Autre", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsmotif5) & climat$volsmotif5=="Autre", climat$volsnb5, 0), NA )

#Nb de vols, par pays
climat$volsnbFrance<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="FR", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="FR", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="FR", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="FR", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="FR", 0, climat$volsnb5), NA )

climat$volsnbUSA<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="US", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="US", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="US", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="US", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="US", 0, climat$volsnb5), NA )

climat$volsnbItalie<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="IT", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="IT", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="IT", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="IT", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="IT", 0, climat$volsnb5), NA )

climat$volsnbAllemagne<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="DE", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="DE", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="DE", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="DE", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="DE", 0, climat$volsnb5), NA )

climat$volsnbEspagne<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="ES", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="ES", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="ES", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="ES", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="ES", 0, climat$volsnb5), NA )

climat$volsnbCanada<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="CA", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="CA", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="CA", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="CA", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="CA", 0, climat$volsnb5), NA )

climat$volsnbGB<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="GB", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="GB", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="GB", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="GB", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="GB", 0, climat$volsnb5), NA )

climat$volsnbChine<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="CN", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="CN", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="CN", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="CN", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="CN", 0, climat$volsnb5), NA )

climat$volsnbJapon<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="JP", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="JP", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="JP", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="JP", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="JP", 0, climat$volsnb5), NA )

climat$volsnbAutriche<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="AT", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="AT", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="AT", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="AT", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="AT", 0, climat$volsnb5), NA )

climat$volsnbPortugal<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="PT", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="PT", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="PT", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="PT", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="PT", 0, climat$volsnb5), NA )

climat$volsnbPologne<-ifelse(!(is.na(climat$volsnb_tot)), ifelse(is.na(climat$volsarrivee1pays) | climat$volsarrivee1pays!="PL", 0, climat$volsnb1)+
  ifelse(is.na(climat$volsarrivee2pays) | climat$volsarrivee2pays!="PL", 0, climat$volsnb2)+
  ifelse(is.na(climat$volsarrivee3pays) | climat$volsarrivee3pays!="PL", 0, climat$volsnb3)+
  ifelse(is.na(climat$volsarrivee4pays) | climat$volsarrivee4pays!="PL", 0, climat$volsnb4)+
  ifelse(is.na(climat$volsarrivee5pays) | climat$volsarrivee5pays!="PL", 0, climat$volsnb5), NA )

###Calcul en fonction des durées (il faudrait pondérer par le nombre d'aller-retour)

climat$volsnb_moins2j <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsjours1) & climat$volsjours1=="Moins de deux jours", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsjours2) & climat$volsjours2=="Moins de deux jours", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsjours3) & climat$volsjours3=="Moins de deux jours", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsjours4) & climat$volsjours4=="Moins de deux jours", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsjours5) & climat$volsjours5=="Moins de deux jours", climat$volsnb5, 0), NA )

climat$volsnb_2j_1sem <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsjours1) & climat$volsjours1=="De deux jours à une semaine", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsjours2) & climat$volsjours2=="De deux jours à une semaine", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsjours3) & climat$volsjours3=="De deux jours à une semaine", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsjours4) & climat$volsjours4=="De deux jours à une semaine", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsjours5) & climat$volsjours5=="De deux jours à une semaine", climat$volsnb5, 0), NA )

climat$volsnb_1sem_1mois <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsjours1) & climat$volsjours1=="De plus d'une semaine à un mois", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsjours2) & climat$volsjours2=="De plus d'une semaine à un mois", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsjours3) & climat$volsjours3=="De plus d'une semaine à un mois", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsjours4) & climat$volsjours4=="De plus d'une semaine à un mois", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsjours5) & climat$volsjours5=="De plus d'une semaine à un mois", climat$volsnb5, 0), NA )

climat$volsnb_sup1mois <- ifelse(!(is.na(climat$volsnb_tot)), ifelse(!is.na(climat$volsjours1) & climat$volsjours1=="Plus d'un mois", climat$volsnb1, 0) + 
  ifelse(!is.na(climat$volsjours2) & climat$volsjours2=="Plus d'un mois", climat$volsnb2, 0) +
  ifelse(!is.na(climat$volsjours3) & climat$volsjours3=="Plus d'un mois", climat$volsnb3, 0) +
  ifelse(!is.na(climat$volsjours4) & climat$volsjours4=="Plus d'un mois", climat$volsnb4, 0) +
  ifelse(!is.na(climat$volsjours5) & climat$volsjours5=="Plus d'un mois", climat$volsnb5, 0), NA )

#Calcul distance totale par durée sur place

climat$volsdist_moins2j <- with(climat, ifelse(!(is.na(volsnb_tot)),
  ifelse(!is.na(volsjours1) & volsjours1 == "Moins de deux jours", volsdist_tot1, 0) + 
  ifelse(!is.na(volsjours2) & volsjours2 == "Moins de deux jours", volsdist_tot2, 0) +
  ifelse(!is.na(volsjours3) & volsjours3 == "Moins de deux jours", volsdist_tot3, 0) +
  ifelse(!is.na(volsjours4) & volsjours4 == "Moins de deux jours", volsdist_tot4, 0) +
  ifelse(!is.na(volsjours5) & volsjours5 == "Moins de deux jours", volsdist_tot5, 0), NA))

climat$volsdist_2j_1sem <- with(climat, ifelse(!(is.na(volsnb_tot)),
  ifelse(!is.na(volsjours1) & volsjours1 == "De deux jours à une semaine", volsdist_tot1, 0) + 
  ifelse(!is.na(volsjours2) & volsjours2 == "De deux jours à une semaine", volsdist_tot2, 0) +
  ifelse(!is.na(volsjours3) & volsjours3 == "De deux jours à une semaine", volsdist_tot3, 0) +
  ifelse(!is.na(volsjours4) & volsjours4 == "De deux jours à une semaine", volsdist_tot4, 0) +
  ifelse(!is.na(volsjours5) & volsjours5 == "De deux jours à une semaine", volsdist_tot5, 0), NA))

climat$volsdist_1sem_1mois <- with(climat, ifelse(!(is.na(volsnb_tot)),
  ifelse(!is.na(volsjours1) & volsjours1 == "De plus d'une semaine à un mois", volsdist_tot1, 0) + 
  ifelse(!is.na(volsjours2) & volsjours2 == "De plus d'une semaine à un mois", volsdist_tot2, 0) +
  ifelse(!is.na(volsjours3) & volsjours3 == "De plus d'une semaine à un mois", volsdist_tot3, 0) +
  ifelse(!is.na(volsjours4) & volsjours4 == "De plus d'une semaine à un mois", volsdist_tot4, 0) +
  ifelse(!is.na(volsjours5) & volsjours5 == "De plus d'une semaine à un mois", volsdist_tot5, 0), NA))

climat$volsdist_sup1mois <- with(climat, ifelse(!(is.na(volsnb_tot)),
  ifelse(!is.na(volsjours1) & volsjours1 == "Plus d'un mois", volsdist_tot1, 0) + 
  ifelse(!is.na(volsjours2) & volsjours2 == "Plus d'un mois", volsdist_tot2, 0) +
  ifelse(!is.na(volsjours3) & volsjours3 == "Plus d'un mois", volsdist_tot3, 0) +
  ifelse(!is.na(volsjours4) & volsjours4 == "Plus d'un mois", volsdist_tot4, 0) +
  ifelse(!is.na(volsjours5) & volsjours5 == "Plus d'un mois", volsdist_tot5, 0), NA))

#Recodage temps de transport domicile travail
varstpsdomtrav <- paste0("tpsdomtrav.", c("urbain_h", "urbain_m", "tgv_h", "tgv_m", "train_h", "train_m",
                                          "voit_h", "voit_m", "covoit_h", "covoit_m", "moto_h", "moto_m",
                                          "velo_h", "velo_m", "marche_h", "marche_m"))
climat$tpsdomtravrempli <- rowSums(!is.na(climat[varstpsdomtrav])) > 0

# Mettre à 0 les cases non remplies si au moins une case l'a été dans le tableau
for(var in varstpsdomtrav)
  climat[climat$tpsdomtravrempli & is.na(climat[[var]]), var] <- 0

#Attention, il faudra nettoyer les temps (Il y a des couillons qui ont converti leurs heures de transport en minutes. Ex : 6h, 360 minutes..)
climat$tpsdomtrav.urbain_h<- as.numeric(climat$tpsdomtrav.urbain_h)
climat$tpsdomtrav.urbainMin<-climat$tpsdomtrav.urbain_h*60+climat$tpsdomtrav.urbain_m
climat$tpsdomtrav.tgvMin<-climat$tpsdomtrav.tgv_h*60+climat$tpsdomtrav.tgv_m
climat$tpsdomtrav.trainMin<-climat$tpsdomtrav.train_h*60+climat$tpsdomtrav.train_m
#un gars qui a réussi à rentrer "5 h" dans son temps de voiture (et ça fait tout bugger puisque c'est plus numérique)
climat$tpsdomtrav.voit_h[climat$tpsdomtrav.voit_h=="5 h"]<-5
climat$tpsdomtrav.voit_h<-as.numeric(climat$tpsdomtrav.voit_h)
climat$tpsdomtrav.voitMin<-climat$tpsdomtrav.voit_h*60+climat$tpsdomtrav.voit_m
climat$tpsdomtrav.covoitMin<-climat$tpsdomtrav.covoit_h*60+climat$tpsdomtrav.covoit_m
climat$tpsdomtrav.motoMin<-climat$tpsdomtrav.moto_h*60+climat$tpsdomtrav.moto_m
climat$tpsdomtrav.veloMin<-climat$tpsdomtrav.velo_h*60+climat$tpsdomtrav.velo_m
climat$tpsdomtrav.marcheMin<-climat$tpsdomtrav.marche_h*60+climat$tpsdomtrav.marche_m

#Construction d'un "score écolo"
# On suppose que si au moins une case a été cochée, les autres sont "Non"
climat$ScoreEcolo<-0
climat$ScoreEcolo[climat$dixannees.bilan=="Oui" & !is.na(climat$dixannees.bilan)]<-climat$ScoreEcolo[climat$dixannees.bilan=="Oui" & !is.na(climat$dixannees.bilan)]+1
climat$ScoreEcolo[climat$dixannees.giec=="Oui" & !is.na(climat$dixannees.giec)]<-climat$ScoreEcolo[climat$dixannees.giec=="Oui" & !is.na(climat$dixannees.giec)]+1
climat$ScoreEcolo[climat$dixannees.asso=="Oui"& !is.na(climat$dixannees.asso)]<-climat$ScoreEcolo[climat$dixannees.asso=="Oui" & !is.na(climat$dixannees.asso)]+1
climat$ScoreEcolo[climat$dixannees.marche=="Oui" & !is.na(climat$dixannees.marche)]<-climat$ScoreEcolo[climat$dixannees.marche=="Oui" & !is.na(climat$dixannees.marche)]+1
climat$ScoreEcolo[climat$dixannees.vote=="Oui" & !is.na(climat$dixannees.vote)]<-climat$ScoreEcolo[climat$dixannees.vote=="Oui" & !is.na(climat$dixannees.vote)]+1
climat$ScoreEcolo[is.na(climat$dixannees.bilan) & is.na(climat$dixannees.giec) &is.na(climat$dixannees.asso) & is.na(climat$dixannees.marche) & is.na(climat$dixannees.vote)]<-NA

#Autre façon de calculer un score écolo
climat$ScoreEcoloPond<-0
climat$ScoreEcoloPond[climat$dixannees.bilan=="Oui" & !is.na(climat$dixannees.bilan)]<-climat$ScoreEcoloPond[climat$dixannees.bilan=="Oui" & !is.na(climat$dixannees.bilan)]+2
climat$ScoreEcoloPond[climat$dixannees.giec=="Oui" & !is.na(climat$dixannees.giec)]<-climat$ScoreEcoloPond[climat$dixannees.giec=="Oui" & !is.na(climat$dixannees.giec)]+1
climat$ScoreEcoloPond[climat$dixannees.asso=="Oui"& !is.na(climat$dixannees.asso)]<-climat$ScoreEcoloPond[climat$dixannees.asso=="Oui" & !is.na(climat$dixannees.asso)]+2
climat$ScoreEcoloPond[climat$dixannees.marche=="Oui" & !is.na(climat$dixannees.marche)]<-climat$ScoreEcoloPond[climat$dixannees.marche=="Oui" & !is.na(climat$dixannees.marche)]+2
climat$ScoreEcoloPond[climat$dixannees.vote=="Oui" & !is.na(climat$dixannees.vote)]<-climat$ScoreEcoloPond[climat$dixannees.vote=="Oui" & !is.na(climat$dixannees.vote)]+1
climat$ScoreEcoloPond[is.na(climat$dixannees.bilan) & is.na(climat$dixannees.giec) &is.na(climat$dixannees.asso) & is.na(climat$dixannees.marche) & is.na(climat$dixannees.vote)]<-NA




climat$opinionecolo.effondrement2 <- climat$opinionecolo.effondrement
climat$opinionecolo.effondrement2[climat$opinionecolo.cata %in%
                                    c("Plutôt pas d'accord",
                                      "Pas du tout d'accord")] <- "Pas du tout d'accord"
climat$opinionecolo.effondrement2[climat$opinionecolo.cata == "Sans opinion"] <- "Sans opinion"



# participation à une ANR, ERC, etc.
tableaurempli <-
    rowSums(!is.na(as.matrix(select(climat,
                                    paste0("projets.", c("anr", "europe", "france", "inter", "prive"), "_n"),
                                    paste0("projets.", c("anr", "europe", "france", "inter", "prive"), "_m"),
                                    paste0("projets.", c("anr", "europe", "france", "inter", "prive"), "_r"))))) > 0
for(inst in c("anr", "europe", "france", "inter", "prive")) {
  varnon <- paste0("projets.", inst, "_n")
  varr <- paste0("projets.", inst, "_r")
  varm <- paste0("projets.", inst, "_m")
  for(sit in c("r", "m")) {
    var <- paste0("projets.", inst, "_", sit)
    var2 <- paste0("projets.", inst, "_", sit, "2")
    inst2 <- c(anr="projet ANR",
               europe="projet européen",
               france="projet France",
               inter="projet international",
               prive="projet privé")[inst]
    sit2 <- c(r="Responsable", m="Membre")[sit]
    # NA signifie non coché, ce qui mélange Non réponse et Non :
    # on les sépare selon qu'une (autre) case a été cochée dans le tableau ou non
    # 1 signifie coché
    # 0 signifie que le répondant n'est pas allé jusqu'à cette question/page
    # Si Oui et Non ont été cochés tous les deux, on retient Oui
    # Vérification avec :
    # table(paste(climat$projets.anr_r, climat$projets.anr_m, climat$projets.anr_n),
    #             climat$projets.anr_m2, useNA="a")
    climat[[var2]] <- if_else(is.na(climat[[var]]) & tableaurempli,
                              paste(sit2, inst2, "non"),
                              if_else(climat[[var]] == 1,
                                      paste(sit2, inst2, "oui"),
                                      NA_character_))
  }
}

rm(tableaurempli)

#On met dans une même variable membres et responsables
#Certains ont coché membre ET responsable. Par défaut, on les considère comme responsable seulement
climat$projets.anr[climat$projets.anr_m2=="Membre projet ANR non"]<-"Ni membre ni responsable projet ANR"
climat$projets.anr[climat$projets.anr_m2=="Membre projet ANR oui"]<-"Membre projet ANR"
climat$projets.anr[climat$projets.anr_r2=="Responsable projet ANR oui"]<-"Responsable projet ANR"

climat$projets.france[climat$projets.france_m2=="Membre projet France non"]<-"Ni membre ni responsable projet France"
climat$projets.france[climat$projets.france_m2=="Membre projet France oui"]<-"Membre projet France"
climat$projets.france[climat$projets.france_r2=="Responsable projet France oui"]<-"Responsable projet France"

climat$projets.europe[climat$projets.europe_m2=="Membre projet européen non"]<-"Ni membre ni responsable projet européen"
climat$projets.europe[climat$projets.europe_m2=="Membre projet européen oui"]<-"Membre projet européen"
climat$projets.europe[climat$projets.europe_r2=="Responsable projet européen oui"]<-"Responsable projet européen"

climat$projets.inter[climat$projets.inter_m2=="Membre projet international non"]<-"Ni membre ni responsable projet international"
climat$projets.inter[climat$projets.inter_m2=="Membre projet international oui"]<-"Membre projet international"
climat$projets.inter[climat$projets.inter_r2=="Responsable projet international oui"]<-"Responsable projet international"

climat$projets.prive[climat$projets.prive_m2=="Membre projet privé non"]<-"Ni membre ni responsable projet privé"
climat$projets.prive[climat$projets.prive_m2=="Membre projet privé oui"]<-"Membre projet privé"
climat$projets.prive[climat$projets.prive_r2=="Responsable projet privé oui"]<-"Responsable projet privé"


#Financements : regroupement membres et responsables
climat$particip_ANR <- climat$projets.anr_r2 == "Responsable projet ANR oui" | climat$projets.anr_m2 == "Membre projet ANR oui"
climat$particip_Fr <- climat$projets.france_r2 == "Responsable projet France oui" | climat$projets.france_m2 == "Membre projet France oui"
climat$particip_Europ <- climat$projets.europe_r2 == "Responsable projet européen oui" | climat$projets.europe_m2 == "Membre projet européen oui"
climat$particip_Intern <- climat$projets.inter_r2 == "Responsable projet international oui" | climat$projets.inter_m2 == "Membre projet international oui"
climat$particip_prive <- climat$projets.prive_r2 == "Responsable projet privé oui" | climat$projets.prive_m2 == "Membre projet privé oui"


#Membre ou responsable d'un projet financé
climat$Profin_Mb_Resp <- NA
climat$Profin_Mb_Resp[!is.na(climat$projets.anr_m2) & !is.na(climat$projets.france_m2) & 
                          !is.na(climat$projets.europe_m2) & !is.na(climat$projets.inter_m2) &
                          !is.na(climat$projets.prive_m2)] <- "Ni membre ni resp d'un 1 projet financé"
climat$Profin_Mb_Resp[climat$projets.anr_m2 == "Membre projet ANR oui" | climat$projets.france_m2 == "Membre projet France oui" |
                          climat$projets.europe_m2 == "Membre projet européen oui" | climat$projets.inter_m2 == "Membre projet international oui" |
                          climat$projets.prive_r2 == "Responsable projet privé oui"] <- "Membre d'au moins 1 projet financé"
climat$Profin_Mb_Resp[climat$projets.anr_r2 == "Responsable projet ANR oui" | climat$projets.france_r2 == "Responsable projet France oui" | 
                          climat$projets.europe_r2 == "Responsable projet européen oui" | climat$projets.inter_r2 == "Responsable projet international oui" |
                          climat$projets.prive_r2 == "Responsable projet privé oui"] <- "Responsable d'au moins 1 projet financé"


# Construction d'un "score international"
# On suppose que si au moins une case a été cochée dans le tableau, les autres sont "Non"
climat$ScoreInternational <- with(climat,
                                  if_else(is.na(international.poste) & is.na(international.naiss) &
                                              is.na(international.natio) & is.na(international.scol) &
                                              is.na(international.etudes) & is.na(international.postdoc) &
                                              is.na(international.travail) & is.na(international.prog) & 
                                              is.na(international.asso),
                                          NA_real_,
                                          coalesce(international.poste == "Oui", 0) +
                                              coalesce(international.naiss == "Oui", 0) +
                                              coalesce(international.natio == "Oui", 0) +
                                              coalesce(international.scol == "Oui", 0) +
                                              coalesce(international.etudes == "Oui", 0) +
                                              coalesce(international.postdoc == "Oui", 0) +
                                              coalesce(international.travail == "Oui", 0) +
                                              coalesce(international.prog == "Oui", 0) +
                                              coalesce(international.asso == "Oui", 0) +
                                              particip_Europ + particip_Intern))




# Construction d'un score international perso et pro



climat$ScoreInternational_perso <- with(climat,
                                            if_else(is.na(international.poste) & is.na(international.naiss) &
                                                      is.na(international.natio) & is.na(international.scol) &
                                                      is.na(international.etudes) & is.na(international.postdoc) &
                                                      is.na(international.travail) & is.na(international.prog) & 
                                                      is.na(international.asso),
                                                    NA_real_,
                                                    coalesce(international.naiss == "Oui", 0) +
                                                      coalesce(international.natio == "Oui", 0) +
                                                      coalesce(international.scol == "Oui", 0) +
                                                      coalesce(international.etudes == "Oui", 0)))

climat$ScoreInternational_pro <- with(climat,
                                          if_else(is.na(international.poste) & is.na(international.naiss) &
                                                    is.na(international.natio) & is.na(international.scol) &
                                                    is.na(international.etudes) & is.na(international.postdoc) &
                                                    is.na(international.travail) & is.na(international.prog) & 
                                                    is.na(international.asso),
                                                  NA_real_,
                                                  coalesce(international.poste == "Oui", 0) +
                                                    coalesce(international.postdoc == "Oui", 0) +
                                                    coalesce(international.travail == "Oui", 0) +
                                                    coalesce(international.prog == "Oui", 0) +
                                                    coalesce(international.asso == "Oui", 0) +
                                                    particip_Europ + particip_Intern))






#REvenu : on agrège les catégories avec peu de monde
climat$revenuAgr<-ifelse(climat$revenu %in% c("De 10 000 à 15 000 euros par mois", "Plus de 15 000 par mois", "De 8 000 à 9 999 euros par mois"), "Au moins 8000 euros par mois", as.character(climat$revenu))


#Calcul du revenu par tête dans le foyer
#Une demie part par enfant
climat$couple1[climat$couple=="Oui"]<-1
climat$couple1[climat$couple=="Non"]<-0
#Calcul du nombre de personnes dans le foyer, (calcul "fiscal" avec les enfants =1/2)
climat$tailleFiscFoyer<-1+climat$couple1+climat$enfantsnb/2

# deuxième calcul, efface le précédent (test julien)

# une part par adulte, une demie part par enfant
# on corrige pour l'instant juste le truc en ne comptant pas les enfants de plus de 20 ans
climat$couple1[climat$couple=="Oui"]<-1
climat$couple1[climat$couple=="Non"]<-0
#si le plus jeune enfant a plus de 20 ans, 0
climat$enfants_foyer <- climat$enfantsnb
climat$enfants_foyer[climat$enfantsage>=20] <- 0
# on considère comme sans enfants les personnes n'ayant pas répondu à la question
# mais ayant répondu à la précédente et la suivante
climat$enfants_foyer[is.na(climat$enfants_foyer) &
                       !is.na(climat$couple) &
                       !is.na(climat$dippar.m)] <- 0

climat$enfants_foyer_agr <- cut(climat$enfants_foyer, c(0, 1, 2, 3, Inf), right=FALSE,
                                labels=c("Aucun", "1 enfant", "2 enfants", "3 enfants et plus"))

climat$tailleFiscFoyer<-1+climat$couple1*0.5+climat$enfants_foyer*0.4

climat$revenuTete <- NA
climat$revenuTete[climat$revenu=="Moins de 1 500 euros par mois" & !is.na(climat$revenu) ]<-1200/climat$tailleFiscFoyer[climat$revenu=="Moins de 1 500 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat$revenu)]<-2000/climat$tailleFiscFoyer[climat$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat$revenu)]<-3000/climat$tailleFiscFoyer[climat$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat$revenu)]<-4000/climat$tailleFiscFoyer[climat$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 4 500 à 5 999 euros par mois" & !is.na(climat$revenu)]<-5250/climat$tailleFiscFoyer[climat$revenu=="De 4 500 à 5 999 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat$revenu)]<-7000/climat$tailleFiscFoyer[climat$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat$revenu)]<-9000/climat$tailleFiscFoyer[climat$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat$revenu)]<-12500/climat$tailleFiscFoyer[climat$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="Plus de 15 000 par mois" & !is.na(climat$revenu)]<-20000/climat$tailleFiscFoyer[climat$revenu=="Plus de 15 000 par mois" & !is.na(climat$revenu)]

climat$nbAdultesFoyer<-1+climat$couple1

climat$revenuAdulte <- NA
climat$revenuAdulte[climat$revenu=="Moins de 1 500 euros par mois" & !is.na(climat$revenu) ]<-1200/climat$nbAdultesFoyer[climat$revenu=="Moins de 1 500 euros par mois" & !is.na(climat$revenu)]
climat$revenuAdulte[climat$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat$revenu)]<-2000/climat$nbAdultesFoyer[climat$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuAdulte[climat$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat$revenu)]<-3000/climat$nbAdultesFoyer[climat$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuAdulte[climat$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat$revenu)]<-4000/climat$nbAdultesFoyer[climat$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuAdulte[climat$revenu=="De 4 500 à 5 999 euros par mois" & !is.na(climat$revenu)]<-5250/climat$nbAdultesFoyer[climat$revenu=="De 4 500 à 5 999 euros par mois" & !is.na(climat$revenu)]
climat$revenuAdulte[climat$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat$revenu)]<-7000/climat$nbAdultesFoyer[climat$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat$revenu)]
climat$revenuAdulte[climat$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat$revenu)]<-9000/climat$nbAdultesFoyer[climat$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat$revenu)]
climat$revenuAdulte[climat$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat$revenu)]<-12500/climat$nbAdultesFoyer[climat$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat$revenu)]
climat$revenuAdulte[climat$revenu=="Plus de 15 000 par mois" & !is.na(climat$revenu)]<-20000/climat$nbAdultesFoyer[climat$revenu=="Plus de 15 000 par mois" & !is.na(climat$revenu)]

climat$revenuMenage <- NA
climat$revenuMenage[climat$revenu=="Moins de 1 500 euros par mois" & !is.na(climat$revenu) ]<-1200
climat$revenuMenage[climat$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat$revenu)]<-2000
climat$revenuMenage[climat$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat$revenu)]<-3000
climat$revenuMenage[climat$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat$revenu)]<-4000
climat$revenuMenage[climat$revenu=="De 4 500 à 5 999 euros par mois" & !is.na(climat$revenu)]<-5250
climat$revenuMenage[climat$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat$revenu)]<-7000
climat$revenuMenage[climat$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat$revenu)]<-9000
climat$revenuMenage[climat$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat$revenu)]<-12500
climat$revenuMenage[climat$revenu=="Plus de 15 000 par mois" & !is.na(climat$revenu)]<-20000


# origine sociale ----


## Recodage de climat$statutpar.p en climat$statutpar.p_rec
climat$statutpar.p_rec <- climat$statutpar.p %>%
  fct_recode(
    "Public" = "Fonctionnaire ou salarié·e du public",
    "Privé" = "Salarié·e du privé",
    "Privé" = "À son compte ou libéral",
    "Sans emploi" = "Au chômage",
    "Sans emploi" = "Inactif/Inactive ou retraité·e",
    NULL = "Décédé·e",
    NULL = "Ne sait pas"
  )

## Recodage de climat$statutpar.m en climat$statutpar.m_rec
climat$statutpar.m_rec <- climat$statutpar.m %>%
  fct_recode(
    "Public" = "Fonctionnaire ou salarié·e du public",
    "Privé" = "Salarié·e du privé",
    "Privé" = "À son compte ou libéral",
    "Sans emploi" = "Au chômage",
    "Sans emploi" = "Inactif/Inactive ou retraité·e",
    NULL = "Décédé·e",
    NULL = "Ne sait pas"
  )

climat$statut_parents <- paste(climat$statutpar.m_rec, climat$statutpar.p_rec)

## Recodage de climat$statut_parents en climat$statut_parents_rec
climat$statut_parents_rec <- climat$statut_parents %>%
  fct_recode(
    NULL = "NA NA",
    "Privé" = "NA Privé",
    "Public" = "NA Public",
    NULL = "NA Sans emploi",
    "Privé" = "Privé NA",
    "Privé" = "Privé Privé",
    "Privé" = "Privé Sans emploi",
    "Public" = "Public NA",
    "Privé Public" = "Public Privé",
    "Public" = "Public Public",
    "Public" = "Public Sans emploi",
    NULL = "Sans emploi NA",
    "Privé" = "Sans emploi Privé",
    "Privé" = "Sans emploi Public",
    NULL = "Sans emploi Sans emploi"
  )


## Recodage de climat$statut_parents_rec en climat$un_parent_public
climat$un_parent_public <- climat$statut_parents_rec %>%
  fct_recode(
    "Non" = "Privé",
    "Oui" = "Public",
    "Oui" = "Privé Public"
  )

# Fait de chercher à être promu pour les titulaires
# On exclut les chargé.es de mission, dont les statuts/contrats sont variables
climat$carriere_tit <- if_else(climat$sitpro %in% c("Maître·sse de conférences", "Directeur·rice de recherche", "Professeur·e des universités",
                                                    "Chargé·e de recherche", "Ingénieur·e de recherche", "Ingénieur·e d'études",
                                                    "Assistant ingénieur·e", "Technicien·ne", "Adjoint·e technique"),
                               climat$carriere, factor("Non"))

# Fait de chercher à être titularisé pour les précaires
# On exclut les chargé.es de mission, dont les statuts/contrats sont variables
climat$carriere_prec <- if_else(!climat$sitpro %in% c("Maître·sse de conférences", "Directeur·rice de recherche", "Professeur·e des universités",
                                                    "Chargé·e de recherche", "Ingénieur·e de recherche", "Ingénieur·e d'études",
                                                    "Assistant ingénieur·e", "Technicien·ne", "Adjoint·e technique"),
                               climat$carriere, factor("Non"))

#S'estimer bien ou mal payé
climat$malpaye[climat$paie %in% c("Mal payé·e" , "Très mal payé·e")]<-"Oui"
climat$malpaye[climat$paie %in% c("Bien payé·e", "Correctement payé·e", "Très bien payé·e")]<-"Non"
climat$malpaye<-fct_relevel(climat$malpaye, "Non")

climat$res.idf <- if_else(climat$res.dep %in% c("75", "77", "78", "91", "92", "93", "94", "95"),
                          "Île de France", "Province")
climat$trav.idf <- if_else(climat$trav.dep %in% c("75", "77", "78", "91", "92", "93", "94", "95"),
                           "Île de France", "Province")

# Nom en clair des aires d'attraction des villes de 500 000 habitants et plus
climat$res.metropole <- recode_factor(climat$res.AAV2020,
                                      "1"="Paris",
                                      "2"="Lyon",
                                      "3"="Aix-Marseille",
                                      "4"="Lille",
                                      "5"="Toulouse",
                                      "6"="Bordeaux",
                                      "8"="Nantes",
                                      "10"="Strasbourg",
                                      "12"="Montpellier",
                                      "13"="Rennes",
                                      "14"="Grenoble",
                                      "15"="Rouen",
                                      "17"="Nice",
                                      "18"="Toulon",
                                      "19"="Tours",
                                      "20"="Nancy",
                                      .default="Autre")
climat$trav.metropole <- recode_factor(climat$trav.AAV2020,
                                       "1"="Paris",
                                       "2"="Lyon",
                                       "3"="Aix-Marseille",
                                       "4"="Lille",
                                       "5"="Toulouse",
                                       "6"="Bordeaux",
                                       "8"="Nantes",
                                       "10"="Strasbourg",
                                       "12"="Montpellier",
                                       "13"="Rennes",
                                       "14"="Grenoble",
                                       "15"="Rouen",
                                       "17"="Nice",
                                       "18"="Toulon",
                                       "19"="Tours",
                                       "20"="Nancy",
                                       .default="Autre")




## Recodage de climat$trav.dep en climat$trav_zone
climat$trav_zone <- climat$trav.dep %>%
  fct_recode(
    "loin" = "04",
    "loin" = "06",
    "loin" = "09",
    "loin" = "15",
    "loin" = "23",
    "loin" = "29",
    "loin" = "2B",
    "loin" = "31",
    "loin" = "64",
    "loin" = "65",
    "loin" = "66",
    "IDF" = "75",
    "IDF" = "77",
    "loin" = "81",
    "loin" = "83",
    "IDF" = "91",
    "IDF" = "92",
    "IDF" = "93",
    "IDF" = "94",
    "IDF" = "95",
    "loin" = "97",
    "loin" = "98"
  )%>% as.character()


climat$trav_zone[!is.na(climat$trav_zone) &
                   ! climat$trav_zone %in% c("loin", "IDF")] <- "peu_loin"



## Recodage de climat$trav.dep en climat$trav_zone_dom
climat$trav_zone_dom <- climat$trav.dep %>%
  fct_recode(
    "loin" = "04",
    "loin" = "06",
    "loin" = "09",
    "loin" = "15",
    "loin" = "23",
    "loin" = "29",
    "loin" = "2B",
    "loin" = "31",
    "loin" = "64",
    "loin" = "65",
    "loin" = "66",
    "IDF" = "75",
    "IDF" = "77",
    "loin" = "81",
    "loin" = "83",
    "IDF" = "91",
    "IDF" = "92",
    "IDF" = "93",
    "IDF" = "94",
    "IDF" = "95",
    "dom" = "97",
    "dom" = "98"
  )%>% as.character()


climat$trav_zone_dom[!is.na(climat$trav_zone_dom) &
                   ! climat$trav_zone_dom %in% c("loin", "IDF", "dom")] <- "peu_loin"


## Réordonnancement de climat$trav_zone_dom
climat$trav_zone_dom <- climat$trav_zone_dom %>%
  fct_relevel(
    "IDF", "loin", "peu_loin", "dom"
  )


#Le quiz

recode_quiz <- function(x, rep) {
  reponses <- c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", 
                "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg")
  i <- which(reponses == rep)
  fct_collapse(x,
               "Sous-estimé"=reponses[1:length(reponses) < i-1],
               "Correct"=reponses[(i-1):(i+1)],
               "Surestimé"=reponses[1:length(reponses) > i+1])
}

recode_quiz2 <- function(x, rep) {
  reponses <- c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", 
                "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg")
  i <- which(reponses == rep)
  fct_collapse(x,
               "Très sous-estimé"=reponses[1:length(reponses) < i-1],
               "Un peu sous-estimé"=reponses[1:length(reponses) == i-1],
               "Correct"=reponses[i],
               "Un peu surestimé"=reponses[1:length(reponses) == i+1],
               "Très surestimé"=reponses[1:length(reponses) > i+1])
}

climat$quizfacteurs.voiture2 <- recode_quiz(climat$quizfacteurs.voiture, "3 000 kg")
climat$quizfacteurs.avion2 <- recode_quiz(climat$quizfacteurs.avion, "1 000 kg")
climat$quizfacteurs.TGV2 <- recode_quiz(climat$quizfacteurs.TGV, "5 kg")
climat$quizfacteurs.ordi2 <- recode_quiz(climat$quizfacteurs.ordi, "250 kg")
climat$quizfacteurs.visio2 <- recode_quiz(climat$quizfacteurs.visio, "100 g")
climat$quizfacteurs.these2 <- recode_quiz(climat$quizfacteurs.these, "5 kg")
climat$quizfacteurs.steak2 <- recode_quiz(climat$quizfacteurs.steak, "5 kg")

climat$quizfacteurs.voiture3 <- recode_quiz2(climat$quizfacteurs.voiture, "3 000 kg")
climat$quizfacteurs.avion3 <- recode_quiz2(climat$quizfacteurs.avion, "1 000 kg")
climat$quizfacteurs.TGV3 <- recode_quiz2(climat$quizfacteurs.TGV, "5 kg")
climat$quizfacteurs.ordi3 <- recode_quiz2(climat$quizfacteurs.ordi, "250 kg")
climat$quizfacteurs.visio3 <- recode_quiz2(climat$quizfacteurs.visio, "100 g")
climat$quizfacteurs.these3 <- recode_quiz2(climat$quizfacteurs.these, "5 kg")
climat$quizfacteurs.steak3 <- recode_quiz2(climat$quizfacteurs.steak, "5 kg")

rm(recode_quiz, recode_quiz2)

#Score agrégé au quiz
recode_quiz3 <- function(x, rep) {
  reponses <- c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", 
                "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg")
  i <- which(reponses == rep)
  fct_collapse(x,
               "0"=reponses[1:length(reponses) < i-1],
               "1"=reponses[1:length(reponses) == i-1],
               "2"=reponses[i],
               "1"=reponses[1:length(reponses) == i+1],
               "0"=reponses[1:length(reponses) > i+1])
}

climat$quizfacteurs.voiture4 <- as.numeric(recode_quiz3(climat$quizfacteurs.voiture, "3 000 kg"))
climat$quizfacteurs.avion4 <- as.numeric(recode_quiz3(climat$quizfacteurs.avion, "1 000 kg"))
climat$quizfacteurs.TGV4 <- as.numeric(recode_quiz3(climat$quizfacteurs.TGV, "5 kg"))
climat$quizfacteurs.ordi4 <- as.numeric(recode_quiz3(climat$quizfacteurs.ordi, "250 kg"))
climat$quizfacteurs.visio4 <- as.numeric(recode_quiz3(climat$quizfacteurs.visio, "100 g"))
climat$quizfacteurs.these4 <- as.numeric(recode_quiz3(climat$quizfacteurs.these, "5 kg"))
climat$quizfacteurs.steak4 <- as.numeric(recode_quiz3(climat$quizfacteurs.steak, "5 kg"))

rm(recode_quiz3)

#Remarque : au passage en numérique, ça ajoute 1, mais on s'en fout

climat$scorequiz <- rowSums(select(climat, quizfacteurs.voiture4, quizfacteurs.avion4, quizfacteurs.TGV4, quizfacteurs.ordi4,
                                   quizfacteurs.visio4, quizfacteurs.these4, quizfacteurs.steak4))

# Corrélation entre les réponses d'un individu et les réponses correctes
# (permet de tenir compte des estimations des émissions relatives en ignorant
# la tendance d'un individu à sur- ou sous-estimer les émissions en général)
cor_quiz <- function(x, method="pearson") {
  # Si toutes les réponses sont les mêmes, la corrélation donne une erreur
  if(length(unique(x)) == 1)
    return(NaN)
  
  reponses <- c("10 g"=10, "100 g"=100, "1 kg"=1000, "5 kg"=5000,
                "25 kg"=25000, "50 kg"=50000, "100 kg"=100000, 
                "250 kg"=250000, "500 kg"=500000, "1 000 kg"=1000000,
                "2 000 kg"=2000000, "3 000 kg"=3000000, "5 000 kg"=5000000)
  correct <- c("3 000 kg", "1 000 kg", "5 kg", "250 kg", "100 g", "5 kg", "5 kg")
  cor(reponses[correct], reponses[x], method=method)
}

# Corrélation en attribuant des valeurs de 1 à 13 aux réponses
# (ne tient pas compte du niveau d'émissions qu'elles représentent)
cor_quiz2 <- function(x) {
  # Si toutes les réponses sont les mêmes, la corrélation donne une erreur
  if(length(unique(x)) == 1)
    return(NaN)
  
  reponses <- c("10 g"=1, "100 g"=2, "1 kg"=3, "5 kg"=4,
                "25 kg"=5, "50 kg"=6, "100 kg"=7, 
                "250 kg"=8, "500 kg"=9, "1 000 kg"=10,
                "2 000 kg"=11, "3 000 kg"=12, "5 000 kg"=13)
  correct <- c("3 000 kg", "1 000 kg", "5 kg", "250 kg", "100 g", "5 kg", "5 kg")
  cor(reponses[correct], reponses[x])
}

# Score d'écart par rapport aux bonnes réponses en soustrayant la moyenne
# (approche similaire à la précédente)
ecartabs_quiz <- function(x) {
  # Si toutes les réponses sont les mêmes, la corrélation donne une erreur
  if(length(unique(x)) == 1)
    return(NaN)

  reponses <- c("10 g"=1, "100 g"=2, "1 kg"=3, "5 kg"=4,
                "25 kg"=5, "50 kg"=6, "100 kg"=7, 
                "250 kg"=8, "500 kg"=9, "1 000 kg"=10,
                "2 000 kg"=11, "3 000 kg"=12, "5 000 kg"=13)
  correct <- c("3 000 kg", "1 000 kg", "5 kg", "250 kg", "100 g", "5 kg", "5 kg")
  correctcentre <- reponses[correct] - mean(reponses[correct])
  xcentre <- reponses[x] - mean(reponses[x])
  sum(abs(correctcentre - xcentre))
}

vars <- paste0("quizfacteurs.",
               c("voiture", "avion", "TGV",
                 "ordi", "visio", "these", "steak"))

# Désactivé car prend du temps
# climat$quizfacteurs.corpearson <- transmute(rowwise(climat), cor_quiz(c_across(all_of(vars)),
#                                                                       method="pearson"))[[1]]
# climat$quizfacteurs.corspearman <- transmute(rowwise(climat), cor_quiz(c_across(all_of(vars)),
#                                                                        method="spearman"))[[1]]
# climat$quizfacteurs.corkendall <- transmute(rowwise(climat), cor_quiz(c_across(all_of(vars)),
#                                                                       method="kendall"))[[1]]
# climat$quizfacteurs.corpearson2 <- transmute(rowwise(climat), cor_quiz2(c_across(all_of(vars))))[[1]]
# climat$quizfacteurs.ecartabs <- transmute(rowwise(climat), ecartabs_quiz(c_across(all_of(vars))))[[1]]


rm(cor_quiz, vars)

#Autre façon de calculer un score au quizz

passagenum<- function(x) {
  ifelse(x=="10 g", 0.01, ifelse(x=="100 g", 0.1, ifelse(x=="1 kg", 1, ifelse(x=="5 kg", 5, ifelse(x=="25 kg", 25, ifelse(x=="50 kg", 50, ifelse(x=="100 kg", 100, ifelse(x=="250 kg", 250, ifelse(x=="500 kg", 500, ifelse(x=="1 000 kg", 1000, ifelse(x=="2 000 kg", 2000, ifelse(x=="3 000 kg", 3000, 5000))))))))))))
}

climat$quizfacteurs.voiturenum <- passagenum(climat$quizfacteurs.voiture)
climat$quizfacteurs.avionnum <- passagenum(climat$quizfacteurs.avion)
climat$quizfacteurs.TGVnum <- passagenum(climat$quizfacteurs.TGV)
climat$quizfacteurs.ordinum <- passagenum(climat$quizfacteurs.ordi)
climat$quizfacteurs.visionum <- passagenum(climat$quizfacteurs.visio)
climat$quizfacteurs.thesenum <- passagenum(climat$quizfacteurs.these)
climat$quizfacteurs.steaknum <- passagenum(climat$quizfacteurs.steak)

recode_quiz3 <- function(x, rep) {
  ifelse(x>=rep, x/rep, rep/x)
}

climat$quizfacteurs.voiture_err <- recode_quiz3(climat$quizfacteurs.voiturenum, 3000)
climat$quizfacteurs.avion_err <- recode_quiz3(climat$quizfacteurs.avionnum, 1000)
climat$quizfacteurs.TGV_err <- recode_quiz3(climat$quizfacteurs.TGVnum, 5)
climat$quizfacteurs.ordi_err <- recode_quiz3(climat$quizfacteurs.ordinum, 250)
climat$quizfacteurs.visio_err <- recode_quiz3(climat$quizfacteurs.visionum, 0.1)
climat$quizfacteurs.these_err <- recode_quiz3(climat$quizfacteurs.thesenum, 5)
climat$quizfacteurs.steak_err <- recode_quiz3(climat$quizfacteurs.steaknum, 5)

climat$scorequiz_err <- rowSums(select(climat, quizfacteurs.voiture_err, quizfacteurs.avion_err, quizfacteurs.TGV_err, quizfacteurs.ordi_err,
                                   quizfacteurs.visio_err, quizfacteurs.these_err, quizfacteurs.steak_err))

#Rapport entre poids carbone estimé de l'avion et de la visio

climat$quiz.rapportav.visio<-climat$quizfacteurs.avionnum/climat$quizfacteurs.visionum


#Sous estimer le poids de l'avion (à faire)
#On considère pour cela qu'il faut à la fois le sous estimer, mais plus que les autres items (car si on sous estime tout, c'est juste qu'on se rend pas compte de l'importance générale du poids carbone des choses)



#Avoir déposé un commentaire
climat$commenter<- ifelse(!is.na(climat$commentaires), "Oui", "Non")

#Temps partiel en numérique : porportion d'un temps complet (1 pour les temps complets)
climat<-tidyr::extract(climat, tpsquotite, "tpsquotiteVal", "(\\d+)", remove=FALSE)
climat$tpsquotiteVal<-as.numeric(climat$tpsquotiteVal)
climat$tpsquotiteVal[climat$tpsplein=="Oui"]<-100
climat$tpsquotiteNum<-climat$tpsquotiteVal/100

#Variables passées en numérique
climat$nbpublisang<-as.numeric(climat$nbpublisang)


###Temps de remplissage/vagues
#Variable avec uniquement la date
climat$dateDebut<-strftime(strptime(climat$startdate, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
climat$dateFin<-strftime(strptime(climat$datestamp, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")

#Réponse avant ou après 3e relance
climat$apres3erelance[climat$dateDebut<"2020-10-12"]<-"Avant 3e relance"
climat$apres3erelance[climat$dateDebut>="2020-10-12"]<-"Après 3e relance"

#Avoir rempli le jour même
#On devrait affiner (certains ont rempli un jour d'envoi pour une autre vague)
climat$remplijourenvoi[climat$dateDebut %in% c("2020-06-26", "2020-06-29", "2020-06-30","2020-07-06" ,  "2020-07-07", "2020-09-07", "2020-10-12", "2020-10-15", "2020-11-16", "2020-11-24")]<-"Oui"
climat$remplijourenvoi[!(climat$dateDebut %in% c("2020-06-26", "2020-06-29", "2020-06-30","2020-07-06" ,  "2020-07-07", "2020-09-07", "2020-10-12", "2020-10-15", "2020-11-16", "2020-11-24"))]<-"Non"

#Durée de remplissage
climat$datestamp1 <- as.POSIXct(climat$datestamp, format ="%Y-%m-%d %H:%M:%S")
climat$startdate1 <- as.POSIXct(climat$startdate, format ="%Y-%m-%d %H:%M:%S")
climat$TpsRempliMin<-as.numeric(difftime(climat$datestamp1, climat$startdate1,  units="mins"))


#Durée de remplissage sur le même jour
climat$TpsRemp_MmJour[climat$dateDebut==climat$dateFin]<-as.numeric(difftime(climat$datestamp1[climat$dateDebut==climat$dateFin], climat$startdate1[climat$dateDebut==climat$dateFin],  units="mins"))


#Vague de réponse
climat$NumVague[climat$dateDebut<"2020-07-07"]<-"Après premier message"
climat$NumVague["2020-07-07"<=climat$dateDebut & climat$dateDebut<"2020-09-07"]<-"Après première relance"
climat$NumVague["2020-09-07"<=climat$dateDebut & climat$dateDebut<"2020-10-12"]<-"Après deuxième relance"
climat$NumVague["2020-10-12"<=climat$dateDebut & climat$dateDebut<"2020-11-16"]<-"Après troisième relance"
climat$NumVague["2020-11-16"<=climat$dateDebut]<-"Après quatrième relance"

#Vague de réponse numérique
climat$vaguenum[climat$dateDebut<"2020-07-07"]<-1
climat$vaguenum["2020-07-07"<=climat$dateDebut & climat$dateDebut<"2020-09-07"]<-2
climat$vaguenum["2020-09-07"<=climat$dateDebut & climat$dateDebut<"2020-10-12"]<-3
climat$vaguenum["2020-10-12"<=climat$dateDebut & climat$dateDebut<"2020-11-16"]<-4
climat$vaguenum["2020-11-16"<=climat$dateDebut]<-5



#####Ordonner des variables
# situation professionnelle sitpro réordonnée par logique hiérarchique ----
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

climat$sitpro_reduite <- fct_recode(climat$sitpro,
                                    "Doctorant·e contractuel·le" = "Doctorant·e CIFRE",
                                    "Autre"="Chargé·e d'études/de mission",
                                    "Autre"="Adjoint·e technique")

climat$NumVague <- factor(climat$NumVague,
                          levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))


climat$conffois5ans <- factor(climat$conffois5ans,
                              levels = c(
                                "Zéro fois", "Moins d'une fois par an", "Une fois par an", 
                                "Deux fois par an", "Trois fois par an", "Plus de trois fois par an"
                              )
)

climat$enfantsage_rec <- factor(climat$enfantsage_rec,
                                levels = c("Sans enfant", "moins de 5 ans", "Entre 5 et 15 ans", "Plus de 15 ans"))




#######################-
#Modalités de référence dans les régressions----


climatRegr <- climat

climatRegr$apres3erelance <- as.factor(climatRegr$apres3erelance)
climatRegr$apres3erelance <- relevel(climatRegr$apres3erelance, ref = "Avant 3e relance")

climatRegr$sexe <- relevel(climatRegr$sexe, ref = "Homme")

climatRegr$ageAgr <- relevel(climatRegr$ageAgr, ref = "50-54 ans")

climatRegr$revenuAgr <- as.factor(climatRegr$revenuAgr)
climatRegr$revenuAgr <- relevel(climatRegr$revenuAgr, ref = "De 4 500 à 5 999 euros par mois")

climatRegr$sitpro2 <- relevel(climatRegr$sitpro, ref = "Maître·sse de conférences")

climatRegr$discipline_agr <- relevel(climatRegr$discipline_agr, ref = "Physique")
climatRegr$discipline_agr2 <- relevel(climatRegr$discipline_agr, ref = "Physique")
climatRegr$discipline_agr3 <- relevel(climatRegr$discipline_agr3, ref = "Physique")
climatRegr$discipline_agr4 <- relevel(climatRegr$discipline_agr4, ref = "Physique")

climatRegr$discipline <- relevel(climatRegr$discipline , ref = "25 : Mathématiques")

climatRegr$carriere <- relevel(climatRegr$carriere, ref = "Non")

climatRegr$Profin_Mb_Resp <- as.factor(climatRegr$Profin_Mb_Resp)
climatRegr$Profin_Mb_Resp <- relevel(climatRegr$Profin_Mb_Resp, ref = "Ni membre ni resp d'un 1 projet financé")

climatRegr$visiopdtconf <- relevel(climatRegr$visiopdtconf, ref = "1 à 3 fois par mois")
climatRegr$visioavtconf <- relevel(climatRegr$visioavtconf, ref = "1 à 3 fois par mois")

climatRegr$solinstit.limitevols <- relevel(climatRegr$solinstit.limitevols, ref = "C'est prioritaire")

climatRegr$solinstit.vols6h <- relevel(climatRegr$solinstit.vols6h, ref = "C'est prioritaire")

climatRegr$solinstit.train <- relevel(climatRegr$solinstit.train, ref = "C'est prioritaire")

climatRegr$solrisqreducavion.qual <- relevel(climatRegr$solrisqreducavion.qual, ref = "C'est peu probable")
climatRegr$solrisqreducavion.fin <- relevel(climatRegr$solrisqreducavion.fin, ref = "C'est peu probable")
climatRegr$solrisqreducavion.diffusion <- relevel(climatRegr$solrisqreducavion.diffusion, ref = "C'est peu probable")
climatRegr$solrisqreducavion.donnees <- relevel(climatRegr$solrisqreducavion.donnees, ref = "C'est peu probable")
climatRegr$solrisqreducavion.avantages <- relevel(climatRegr$solrisqreducavion.avantages, ref = "C'est peu probable")
climatRegr$solrisqreducavion.insertion <- relevel(climatRegr$solrisqreducavion.insertion, ref = "C'est peu probable")
climatRegr$solrisqreducavion.isoler <- relevel(climatRegr$solrisqreducavion.isoler, ref = "C'est peu probable")
climatRegr$solrisqreducavion.bureaucratie <- relevel(climatRegr$solrisqreducavion.bureaucratie, ref = "C'est peu probable")

climatRegr$solevolges.conf <- relevel(climatRegr$solevolges.conf, ref = "Été à peu près stables")


climatRegr$paie2 <- as.factor(climatRegr$paie)
climatRegr$paie2 <- relevel(climatRegr$paie2, ref = "Mal payé·e")

climatRegr$employeur <- relevel(climatRegr$employeur , ref = "Une université")

climatRegr$preoccupe2 <- relevel(climatRegr$preoccupe, ref = "Très préoccupé·e")

#climatRegr$NumVague <- relevel(climatRegr$NumVague, ref = "Après premier message")

climatRegr$commenter<-fct_relevel(climatRegr$commenter, "Non")

climatRegr$discipline<-fct_relevel(climatRegr$discipline, "19 : Sociologie, démographie")

climatRegr$vols_dicho3ans <- fct_relevel(climatRegr$vols_dicho3ans, "N'a pas volé en 3 ans")

climatRegr$vols_dicho <- fct_relevel(climatRegr$vols_dicho, "N'a pas volé en 2019")

climatRegr$projets.anr <- fct_relevel(climatRegr$projets.anr, "Ni membre ni responsable projet ANR")
climatRegr$projets.anr <- fct_relevel(climatRegr$projets.france, "Ni membre ni responsable projet France")
climatRegr$projets.europe <- fct_relevel(climatRegr$projets.europe, "Ni membre ni responsable projet européen")
climatRegr$projets.inter <- fct_relevel(climatRegr$projets.inter, "Ni membre ni responsable projet international")
climatRegr$projets.prive <- fct_relevel(climatRegr$projets.prive, "Ni membre ni responsable projet privé")


climatRegr$hindexconnDicho<-fct_relevel(climatRegr$hindexconnDicho, "Non")

climatRegr$trav.TUU2017<-fct_relevel(climatRegr$trav.TUU2017, "Unité urbaine de Paris")
climatRegr$trav.TAAV2017<-fct_relevel(climatRegr$trav.TAAV2017, "Aire de Paris")

climatRegr$dippar.p<-fct_relevel(climatRegr$dippar.p, "Bac +4 ou 5")
climatRegr$dippar.m<-fct_relevel(climatRegr$dippar.m, "Bac +4 ou 5")

climatRegr$statutpar.p<-fct_relevel(climatRegr$statutpar.p, "Fonctionnaire ou salarié·e du public")
climatRegr$statutpar.m<-fct_relevel(climatRegr$statutpar.m, "Fonctionnaire ou salarié·e du public")

climatRegr$conffois5ans<-fct_relevel(climatRegr$conffois5ans, "Zéro fois")

climatRegr$volsh2<-fct_relevel(climatRegr$volsh, "De 1h à 10h")

climatRegr$ageaccad_tranch2<-fct_relevel(climatRegr$ageaccad_tranch2, "[0,2]")

climatRegr$quiz<-fct_relevel(climatRegr$quiz, "Je décline le quiz")

climatRegr$datedebut<-as.numeric(as.Date(climatRegr$dateDebut))-18438

climatRegr$extrpreoccupe<-as.factor(climatRegr$extrpreoccupe)
climatRegr$extrpreoccupe<-fct_relevel(climatRegr$extrpreoccupe, "Non")

climatRegr$reducrechexemp<-as.factor(climatRegr$reducrechexemp)
climatRegr$reducrechexemp<-fct_relevel(climatRegr$reducrechexemp, "Non")

climatRegr$tresdecroissance<-as.factor(climatRegr$tresdecroissance)
climatRegr$tresdecroissance<-fct_relevel(climatRegr$tresdecroissance, "Non")

climatRegr$labopratiques.tri <- relevel(climatRegr$labopratiques.tri, "Non")
climatRegr$labopratiques.train <- relevel(climatRegr$labopratiques.train, "Non")
climatRegr$labopratiques.charte <- relevel(climatRegr$labopratiques.charte, "Non")

################-
#Recodage pour les ACM----

climatACM<-climat

#on renomme pour plus de visibilité
climatACM <- rename.variable(climatACM, "opinionecolo.decroissance", "decroitre_necessair")
climatACM <- rename.variable(climatACM, "opinionecolo.efforts", "efforts_seul_inutile")
climatACM <- rename.variable(climatACM, "opinionecolo.cata", "cata_ecolo_proche")
climatACM <- rename.variable(climatACM, "opinionecolo.proteger", "environnt_pas_croissance")
climatACM <- rename.variable(climatACM, "opinionecolo.contraintes", "contrain_reglo_pas_confort")
climatACM <- rename.variable(climatACM, "opinionecolo.techno", "meilleur_techno_solution")


climatACM <- rename.variable(climatACM, "solreducperso.conf", "avion_conf")
climatACM <- rename.variable(climatACM, "solreducperso.donnees", "deplact_donnees")
climatACM <- rename.variable(climatACM, "solreducperso.info", "chgt_inform")
climatACM <- rename.variable(climatACM, "solreducperso.exp", "exp")
climatACM <- rename.variable(climatACM, "solreducperso.domicile", "domic_trav")
climatACM <- rename.variable(climatACM, "solreducrech", "secteur")

climatACM$avion_conf<-fct_recode(climatACM$avion_conf, 
                                         ">1/3"="Oui, d'au moins un tiers",
                                 "<1/3"="Oui, mais de moins d'un tiers",
                                 "Déjà bas"="Non, car elles sont déjà très basses")

climatACM$chgt_inform<-fct_recode(climatACM$chgt_inform, 
                                 ">1/3"="Oui, d'au moins un tiers",
                                 "<1/3"="Oui, mais de moins d'un tiers",
                                 "Déjà bas"="Non, car elles sont déjà très basses")

climatACM$deplact_donnees<-fct_recode(climatACM$deplact_donnees, 
                                 ">1/3"="Oui, d'au moins un tiers",
                                 "<1/3"="Oui, mais de moins d'un tiers",
                                 "Déjà bas"="Non, car elles sont déjà très basses")


climatACM$exp<-fct_recode(climatACM$exp, 
                                      ">1/3"="Oui, d'au moins un tiers",
                                      "<1/3"="Oui, mais de moins d'un tiers",
                                      "Déjà bas"="Non, car elles sont déjà très basses")

climatACM$secteur<-as.character(climatACM$secteur)
climatACM$secteur[climatACM$secteur=="La recherche doit montrer l'exemple"]<-"Supp_1/3"
climatACM$secteur[climatACM$secteur=="La recherche doit réduire ses émissions d'un tiers"]<-"Egal_1/3"
climatACM$secteur[climatACM$secteur=="La recherche peut bénéficier d'un statut dérogatoire"]<-"Statut dérog"




# Creation de la base Recherche intitulée climatRegr_r ----

climatRegr_r <- subset(climatRegr, !sitpro2 %in% c(
  "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
  "Chargé·e d'études/de mission","Adjoint·e technique",
  "Autre"))

####################################@
#Rebus
#On met le temps de vol à zéro pour ceux qui ont indiqué aucun vol et qui sont dans le module 1
#climat$volsdist_tot<-ifelse(climat$volsnb==0 & climat$tiragemodule=="1", 0, climat$volsdist_tot)
#On récupère une quarantaine de personnes dans la case 0
#Il faudrait voir si la même manoeuvre ne peut pas être faite pour les sous catégories de volsdist_tot

#Idem pour le nombre de vol
#climat$volsnb_tot<-ifelse(climat$volsnb==0 & climat$tiragemodule=="1", 0, climat$volsnb_tot)
#climat$volsnb_tot<-ifelse(climat$volsnb==0 & climat$tiragemodule=="1" & !(climat$volsnb_tot>0), 0, climat$volsnb_tot)


