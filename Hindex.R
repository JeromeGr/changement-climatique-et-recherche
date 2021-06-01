library(texreg)
library(dplyr)
library(ggplot2)
library(GGally)

# sous base seulement recherche
climat_recherche <- climat[!climat$sitpro2 %in% c(
  "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
  "Chargé·e d'études/de mission","Adjoint·e technique",
  "Autre"
),]

#Passer en numérique le hindex
climat_recherche$hindex<-as.numeric(climat_recherche$hindex)

#Elimination des quelques individus aux valeurs aberrantes
climat_recherche<-climat_recherche %>% filter(hindex<300| is.na(hindex))
climat_recherche<-climat_recherche %>% filter(nbpublis<200| is.na(nbpublis))

##########################################################################################################################################
##########################################################################################################################################
#Recodage/création de variables/préparation des régressions (modalités de références)

#Création de variables en tranche pour hindex, nbpublis et nombre de vols
climat_recherche$hindextranch<-quant.cut(climat_recherche$hindex, 6)
climat_recherche$nbpublistranch<-quant.cut(climat_recherche$nbpublis, 6)

#icut(climat_recherche, hindex)
climat_recherche$hindextranch2 <- cut(climat_recherche$hindex,
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      breaks = c(0, 8, 13, 18, 23, 30, 50, 80, 176)
)

climat_recherche$nbpublistranch2 <- cut(climat_recherche$nbpublis,
                                        include.lowest = TRUE,
                                        right = TRUE,
                                        breaks = c(0, 0.5, 2, 4, 7, 12, 20, 40, 180)
)

#volsnb en tranche
climat_recherche$volsnbtranch<-quant.cut(climat_recherche$volsnb, 30)
climat_recherche$volsnbtranch2 <- cut(climat_recherche$volsnb,
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      breaks = c(0, 0.4, 1, 2, 3, 4, 5, 8, 65)
)



#Construction de la variable dichotomique "connaitre son h-index"
climat_recherche$hindexconnDicho[climat_recherche$hindexconn=="Oui"]<-"Oui"
climat_recherche$hindexconnDicho[climat_recherche$hindexconn %in% c("Non", "Je ne suis pas certain de ce qu'est le h-index")]<-"Non"
freq(climat_recherche$hindexconn, total = T)

#Reconstitution de la variable correspondant au nombre de conf à l'étranger les cinq dernières années
climat_recherche$conffois5ans<-ifelse(climat_recherche$conf!="Oui, dans les 5 dernières années", "Zéro fois", as.character(climat_recherche$conffois))

## Réordonnancement de climat_recherche$conffois5ans
climat_recherche$conffois5ans <- factor(climat_recherche$conffois5ans,
                                        levels = c(
                                          "Zéro fois", "Moins d'une fois par an", "Une fois par an", 
                                          "Deux fois par an", "Trois fois par an", "Plus de trois fois par an"
                                        )
)

## Réordonnancement de ageAgr
climat_recherche$ageAgr <- factor(climat_recherche$ageAgr,
                  levels = c( "Moins de 29 ans", "30-34 ans", "35-39 ans", "40-44 ans", "45-49 ans","50-54 ans", "55-64 ans", "65 ans et plus" ))
climat_recherche$ageAgr <- relevel(climat_recherche$ageAgr, ref = "50-54 ans")
freq(climat_recherche$ageAgr)
freq(climat$ageAgr)
#On met le temps de vol à zéro pour ceux qui n'ont indiqué aucun vol
climat_recherche$volsh<-ifelse(!is.na(climat_recherche$volsnb) & climat_recherche$volsnb==0, "0h", as.character(climat_recherche$volsh))

## Recodage de climat$enfantsnb en climat$enfantsnb_rec
climat_recherche$enfantsnb_rec <- as.character(climat_recherche$enfantsnb)
climat_recherche$enfantsnb_rec <- fct_recode(climat_recherche$enfantsnb_rec,
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
climat_recherche$enfantsage_rec <- NULL
climat_recherche$enfantsage_rec[climat_recherche$enfantsage <= 5] <- "moins de 5 ans"
climat_recherche$enfantsage_rec[climat_recherche$enfantsage <= 15 &
                        climat_recherche$enfantsage > 5 ] <- "Entre 5 et 15 ans"
# climat_recherche$enfantsage_rec[climat_recherche$enfantsage <= 15 &
#                                   climat_recherche$enfantsage > 10 ] <- "Entre 10 et 15 ans"
climat_recherche$enfantsage_rec[climat_recherche$enfantsage > 15 ] <- "Plus de 15 ans"
climat_recherche$enfantsage_rec[climat_recherche$enfantsnb_rec == "0" ] <- "Sans enfant"
climat_recherche$enfantsage_rec <- as.factor(climat_recherche$enfantsage_rec)
## Réordonnancement de climat_recherche$enfantsage_rec 
climat_recherche$enfantsage_rec <- factor(climat_recherche$enfantsage_rec,
                                levels = c("Sans enfant", "moins de 5 ans", "Entre 5 et 15 ans", "Plus de 15 ans")
)

#Age accadémique
climat_recherche$ageaccad<-2020-climat_recherche$theseannee
freq(climat_recherche$ageaccad_tranch2)

climat_recherche$ageaccad_tranch<-quant.cut(climat_recherche$ageaccad, 6)

climat_recherche$ageaccad_tranch2 <- ifelse(climat_recherche$these=="Non", "Pas de thèse", as.character(cut(climat_recherche$ageaccad,
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      breaks = c(0, 2, 5, 8, 13, 18, 23, 29, 114))))
climat_recherche$ageaccad_tranch2 <- factor(climat_recherche$ageaccad_tranch2,
                                        levels = c(
                                          "Pas de thèse", "[0,2]", "(2,5]",
                                          "(5,8]", "(8,13]", "(13,18]", "(18,23]", "(29,114]"
                                        )
)


#Nombre de conférences en numérique (approximation)

climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Zéro fois"]<-0
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Moins d'une fois par an"]<-0.5
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Une fois par an"]<-1
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Deux fois par an"]<-2
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Trois fois par an"]<-3
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Plus de trois fois par an"]<-4.5


#########################################
####Création de variables à partir des données du module sur l'avion

#On met le temps de vol à zéro pour ceux qui ont indiqué aucun vol et qui sont dans le module 1
climat_recherche$volsdist_tot<-ifelse(climat_recherche$volsnb==0 & climat_recherche$tiragemodule=="1", 0, climat_recherche$volsdist_tot)
#On récupère une quarantaine de personnes dans la case 0
#Il faudrait voir si la même manoeuvre ne peut pas être faite pour les sous catégories de volsdist_tot

#Idem pour le nombre de vol
climat_recherche$volsnb_tot<-ifelse(climat_recherche$volsnb==0 & climat_recherche$tiragemodule=="1", 0, climat_recherche$volsnb_tot)

#Durée moyenne des vols effectués (hors module ?)
climat_recherche$volsduree_moy<-climat_recherche$volshnum/climat_recherche$volsnb
climat_recherche$volsduree_moy[climat_recherche$volsnb==0]<-0

#Distance moyenne des vols effectués et déclaré dans le tableau (Attention j'avais oublié _tot après volsnb : refaire les régressions :((( ?))))
climat_recherche$volsdist_moy<-climat_recherche$volsdist_tot/climat_recherche$volsnb_tot
climat_recherche$volsdist_moy[climat_recherche$volsnb==0 & climat_recherche$tiragemodule=="1"]<-0

#Calcul distance totale par motif de vol, par personne

climat_recherche$volsdist_totconf <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Conférence, présentation", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Conférence, présentation", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Conférence, présentation", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Conférence, présentation", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif5=="Conférence, présentation", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totsejrech <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Séjour de recherche", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Séjour de recherche", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Séjour de recherche", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Séjour de recherche", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Séjour de recherche", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totworkshop <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Réunion, workshop", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Réunion, workshop", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Réunion, workshop", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Réunion, workshop", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Réunion, workshop", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totcours <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totterrain <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Terrain, production et recueil de données", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Terrain, production et recueil de données", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Terrain, production et recueil de données", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Terrain, production et recueil de données", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Terrain, production et recueil de données", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totfinanc <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Obtention de financements", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Obtention de financements", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Obtention de financements", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Obtention de financements", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Obtention de financements", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_toteval <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Évaluation de la recherche", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Évaluation de la recherche", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Évaluation de la recherche", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Évaluation de la recherche", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Évaluation de la recherche", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totjury <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Jury", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Jury", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Jury", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Jury", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Jury", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totautre <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Autre", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Autre", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Autre", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Autre", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Autre", climat_recherche$volsdist_tot5, 0)

#Nombre de vols par motif

climat_recherche$volsnbconf <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Conférence, présentation", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Conférence, présentation", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Conférence, présentation", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Conférence, présentation", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif5=="Conférence, présentation", climat_recherche$volsnb5, 0)

climat_recherche$volsnbsejrech <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Séjour de recherche", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Séjour de recherche", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Séjour de recherche", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Séjour de recherche", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Séjour de recherche", climat_recherche$volsnb5, 0)

climat_recherche$volsnbworkshop <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Réunion, workshop", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Réunion, workshop", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Réunion, workshop", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Réunion, workshop", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Réunion, workshop", climat_recherche$volsnb5, 0)

climat_recherche$volsnbcours <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Enseignement, formation, école d'été", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Enseignement, formation, école d'été", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Enseignement, formation, école d'été", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Enseignement, formation, école d'été", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Enseignement, formation, école d'été", climat_recherche$volsnb5, 0)

climat_recherche$volsnbterrain <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Terrain, production et recueil de données", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Terrain, production et recueil de données", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Terrain, production et recueil de données", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Terrain, production et recueil de données", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Terrain, production et recueil de données", climat_recherche$volsnb5, 0)

climat_recherche$volsnbfinanc <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Obtention de financements", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Obtention de financements", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Obtention de financements", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Obtention de financements", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Obtention de financements", climat_recherche$volsnb5, 0)

climat_recherche$volsnbeval <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Évaluation de la recherche", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Évaluation de la recherche", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Évaluation de la recherche", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Évaluation de la recherche", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Évaluation de la recherche", climat_recherche$volsnb5, 0)

climat_recherche$volsnbjury <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Jury", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Jury", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Jury", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Jury", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Jury", climat_recherche$volsnb5, 0)

climat_recherche$volsnbautre <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Autre", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Autre", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Autre", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Autre", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Autre", climat_recherche$volsnb5, 0)

#Nb de vols, par pays
climat_recherche$volsnbFrance<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="FR", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="FR", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="FR", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="FR", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="FR", 0, climat_recherche$volsnb5)

climat_recherche$volsnbUSA<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="US", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="US", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="US", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="US", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="US", 0, climat_recherche$volsnb5)

climat_recherche$volsnbItalie<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="IT", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="IT", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="IT", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="IT", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="IT", 0, climat_recherche$volsnb5)

climat_recherche$volsnbAllemagne<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="DE", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="DE", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="DE", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="DE", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="DE", 0, climat_recherche$volsnb5)

climat_recherche$volsnbEspagne<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="ES", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="ES", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="ES", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="ES", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="ES", 0, climat_recherche$volsnb5)

climat_recherche$volsnbCanada<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="CA", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="CA", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="CA", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="CA", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="CA", 0, climat_recherche$volsnb5)

climat_recherche$volsnbGB<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="GB", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="GB", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="GB", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="GB", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="GB", 0, climat_recherche$volsnb5)

climat_recherche$volsnbChine<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="CN", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="CN", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="CN", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="CN", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="CN", 0, climat_recherche$volsnb5)

climat_recherche$volsnbJapon<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="JP", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="JP", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="JP", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="JP", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="JP", 0, climat_recherche$volsnb5)

climat_recherche$volsnbAutriche<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="AT", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="AT", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="AT", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="AT", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="AT", 0, climat_recherche$volsnb5)

climat_recherche$volsnbPortugal<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="PT", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="PT", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="PT", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="PT", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="PT", 0, climat_recherche$volsnb5)

climat_recherche$volsnbPologne<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="PL", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="PL", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="PL", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="PL", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="PL", 0, climat_recherche$volsnb5)


############################

#Définition des modalités de référence dans les régressions
climat_recherche$hindexconnDicho<-as.factor(climat_recherche$hindexconnDicho)
climat_recherche$hindexconnDicho<-fct_relevel(climat_recherche$hindexconnDicho, "Non")

climat_recherche$dippar.p<-fct_relevel(climat_recherche$dippar.p, "Bac +4 ou 5")
climat_recherche$dippar.m<-fct_relevel(climat_recherche$dippar.m, "Bac +4 ou 5")

climat_recherche$statutpar.p<-fct_relevel(climat_recherche$statutpar.p, "Fonctionnaire ou salarié·e du public")
climat_recherche$statutpar.m<-fct_relevel(climat_recherche$statutpar.m, "Fonctionnaire ou salarié·e du public")

climat_recherche$conffois5ans<-fct_relevel(climat_recherche$conffois5ans, "Zéro fois")

climat_recherche$volsh<-fct_relevel(climat_recherche$volsh, "De 1h à 10h")

climat_recherche$ageaccad_tranch2<-fct_relevel(climat_recherche$ageaccad_tranch2, "[0,2]")
##########################################################################################################################################
##########################################################################################################################################
##Exploration des distributions des variables d'intérêts et des liens entre elles, hors régressions
#hindex, nombre de publis et nombre de vols


#Publis
distrinbpublis<-climat_recherche %>% group_by(nbpublis) %>% summarize(nbpersonnes=n())

ggplot(distrinbpublis, aes(x=nbpublis, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+
  labs(y="nombre d'enquêtés", x="Nombre de publications depuis 2017,")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution nbpublis.pdf",
       width=9, height=5)

distrinbpublis<-climat_recherche %>% group_by(nbpublistranch2) %>% summarize(nbpersonnes=n())

ggplot(distrinbpublis, aes(x=nbpublistranch2, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+ 
  labs(y="nombre d'enquêtés", x="Nombre de publications depuis 2017, en tranche")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution nbpublis en tranche.pdf",
       width=9, height=5)

#Hindex

distrihindex<-climat_recherche %>% group_by(hindex) %>% summarize(nbpersonnes=n())

ggplot(distrihindex, aes(x=hindex, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+
  labs(y="nombre d'enquêtés", x="h-index")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution Hindex.pdf",
       width=9, height=5)

distrihindex<-climat_recherche %>% group_by(hindextranch2) %>% summarize(nbpersonnes=n())

ggplot(distrihindex, aes(x=hindextranch2, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+ 
  labs(y="nombre d'enquêtés", x="h-index en tranche")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution Hindex en tranche.pdf",
       width=9, height=5)


#Distrib du log du h-index
climat_recherche$hindexln<-log(climat_recherche$hindex)
freq(climat_recherche$hindexln)
distrihindexln<-climat_recherche %>% filter(hindexln>=0)%>% group_by(hindexln) %>% summarize(nbpersonnes=n())

ggplot(distrihindexln, aes(x=hindexln, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()


#Nb vols

distrivolsnb<-climat_recherche %>% group_by(volsnb) %>% summarize(nbpersonnes=n())

ggplot(distrivolsnb, aes(x=volsnb, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+
  labs(y="nombre d'enquêtés", x="Nombre de vols en 2019,")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution volsnb.pdf",
       width=9, height=5)

distrivolsnb<-climat_recherche %>% group_by(volsnbtranch2) %>% summarize(nbpersonnes=n())

ggplot(distrivolsnb, aes(x=volsnbtranch2, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+ 
  labs(y="nombre d'enquêtés", x="Nombre de vols en 2019, en tranche")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution volsnb en tranche.pdf",
       width=9, height=5)

#Nbconférences lors des 5 dernières années

districonffois5ans<-climat_recherche %>% group_by(conffois5ans) %>% summarize(nbpersonnes=n())

ggplot(districonffois5ans, aes(x=conffois5ans, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+ 
  labs(y="nombre d'enquêtés", x="Nombre de participations à des conférences à l'étranger, les 5 dernières années")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution du nombre de participation conf étranger.pdf",
       width=9, height=5)

freq(climat_recherche$conffois5ans)

#Distribution par discipline et sexe, boxplot
ggplot(climat_recherche)+
  geom_boxplot(aes(y=nbpublis, x=discipline_agr3))+coord_flip()


ggplot(climat_recherche)+
  geom_boxplot(aes(y=nbpublis, x=discipline_agr3, color=sexe))+coord_flip()

ggplot(climat_recherche)+
  geom_boxplot(aes(y=hindex, x=discipline_agr3))+coord_flip()

ggplot(climat_recherche)+
  geom_boxplot(aes(y=hindex, x=discipline_agr3, color=sexe))+coord_flip()

####Distribution des vols par tranche de nombre de publis et de h-index

#Boxplot
ggplot(climat_recherche)+
  geom_boxplot(aes(y=volsnb, x=nbpublistranch2))+
labs(y="Nombre moyens de vols en avion en 2019", x= "Nombre de publications 2017-mi2020")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Boxplot, nombre de vols en fonction du nombre de publis en tranches.pdf",
       width=9, height=5)


ggplot(climat_recherche)+
  geom_boxplot(aes(y=nbpublistranch2, x=volsnb))+coord_flip()
ggplot(climat_recherche)+
  geom_boxplot(aes(y=hindextranch2, x=volsnb))+coord_flip()

ggplot(climat_recherche, aes(x = , y = volsnb, fill = hindextranch2)) +
  geom_boxplot(colour = "grey30") + 
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0, NA),
                     expand = c(0, 0)) +
  guides(fill = FALSE)

#Moyennes
graphMoyenne<-climat_recherche %>% group_by(nbpublistranch2) %>%summarise(MoyenneVols = mean(volsnb, na.rm=T),
                                                                           Ecart = sd(volsnb, na.rm=T))

ggplot(graphMoyenne, aes(x = nbpublistranch2, y = MoyenneVols, fill = nbpublistranch2)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  guides(fill = FALSE) +
  labs(y="Nombre moyens de vols en avion en 2019", x= "Nombre de publications 2017-mi2020")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre moyen de vols en fonction du nombre de publis en tranches, moyennes.pdf",
       width=9, height=5)

graphMoyenne<-climat_recherche %>% group_by(hindextranch2) %>%summarise(MoyenneVols = mean(volsnb, na.rm=T),
                                                                          Ecart = sd(volsnb, na.rm=T))

ggplot(graphMoyenne, aes(x = hindextranch2, y = MoyenneVols, fill = hindextranch2)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") + +
  geom_errorbar(
    aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  guides(fill = FALSE) +
  labs(y="Nombre moyens de vols en avion en 2019", x= "H-index")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre moyen de vols en fonction du hindex en tranches, moyennes.pdf",
       width=9, height=5)



graphMoyenne<-climat_recherche %>% group_by(volsnbtranch2) %>%summarise(MoyennePublis = mean(nbpublis, na.rm=T),
                                                                          Ecart = sd(nbpublis, na.rm=T))

ggplot(graphMoyenne, aes(x = volsnbtranch2, y = MoyennePublis, fill = volsnbtranch2)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyennePublis - Ecart, ymax = MoyennePublis+ Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  guides(fill = FALSE) +
  labs(y="Nombre moyens de publications 2017-mi2020", x= "Nombre de voyages en avion en 2019")


####Distribution du nombre de conférences par nombre de publis et h-index
#Nombre de conf en numérique (conversion approx)


graphMoyenne<-climat_recherche %>% group_by(nbpublistranch2) %>%summarise(Moyenneconf = mean(conffois5ansnum, na.rm=T),
                                                                          Ecart = sd(conffois5ansnum, na.rm=T))

ggplot(graphMoyenne, aes(x = nbpublistranch2, y = Moyenneconf, fill = nbpublistranch2)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = Moyenneconf - Ecart, ymax = Moyenneconf + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  guides(fill = FALSE) +
  labs(y="Nombre annuel moyen de conférences à l'étranger ces 5 dernières années", x= "Nombre de publications 2017-mi2020")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre moyen de conférences à l'étranger en fonction du nombre de publis en tranches, moyennes.pdf",
       width=9, height=5)


#Distinction entre les écolos (angle changement climatique) et les autres dans les distributions

#Ecolo angle changement climatique : inquiets/chgt pratiques avion/marche climat
climat_recherche$ecolochgtclimat<-ifelse(climat_recherche$dixannees.marche=="Oui" & 
                                           climat_recherche$avionpersochgt %in% c("Oui, je le prends beaucoup moins", "Oui, je le prends un peu moins") & 
                                           climat_recherche$preoccupe=="Extrêmement préoccupé·e", "Ecolo préoccupé et agissant", "Autre")

#Trop peu de monde dans la tranche sup des publis quand on prend que les écolos (il faut agrandir les tranches)
climat_recherche$nbpublistranch3 <- cut(climat_recherche$nbpublis,
                                        include.lowest = TRUE,
                                        right = TRUE,
                                        breaks = c(0, 0.5, 2, 4, 7, 12, 20, 40)
)

graphMoyenneEcolo<-climat_recherche %>% filter(!is.na(nbpublistranch3) & !is.na(ecolochgtclimat)) %>%
  group_by(nbpublistranch3,ecolochgtclimat) %>% summarise(MoyenneVols = mean(volsnb, na.rm=T),
                                                                          Ecart = sd(volsnb, na.rm=T), Effectifs = n())

ggplot(graphMoyenneEcolo, aes(x = nbpublistranch3, y = MoyenneVols, fill = ecolochgtclimat)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  labs(y="Nombre moyens de vols en avion en 2019", x= "Nombre de publications 2017-mi2020")


graphMoyenneEcolo<-climat_recherche %>% group_by(volsnbtranch2,ecolochgtclimat ) %>%summarise(MoyennePublis = mean(nbpublis, na.rm=T),
                                                                        Ecart = sd(nbpublis, na.rm=T), Effectifs=n())

ggplot(graphMoyenneEcolo, aes(x = volsnbtranch2, y = MoyennePublis, fill = ecolochgtclimat)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyennePublis - Ecart, ymax = MoyennePublis+ Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  labs(y="Nombre moyens de publications 2017-mi2020", x= "Nombre de voyages en avion en 2019")


#Pour avoir des crochets autour des effectifs
Tabgraph$Effectif<-paste("[", Tabgraph$Effectif, "]", sep="")

Tabgraph %>% filter(Av_SansTop8 %in% c("Moy_ParMembre", "Moy_ParMembre_8mb")) %>%
  ggplot(aes(x=AnneeEntree, y=Moy_av, fill=Av_SansTop8, order=Av_SansTop8))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Effectif), position=position_dodge(width=0.9), vjust=-0.4, size=3) +
  theme_classic()+
  scale_fill_manual(name = "",
                    breaks= c("Moy_ParMembre", "Moy_ParMembre_8mb"),
                    values=c("black", "grey"),
                    labels= c("Moy_ParMembre"="All members [headcount]" ,
                              "Moy_ParMembre_8mb"="Minus top 9 beneficiaries\n of all TCs since 2000 [headcount]"))+
  xlab("Date of TC entry") +
  ylab("Average amount received\n between 2013 and mid-2019 (euros)")+
  theme(legend.position = c(0.70, 0.85))

ggsave("/Users/jeromegreffion/Documents/Recherche/ANR Medici/Commission transparence/Chapitre livre Hélène/Illustrations/Average amount by date of TC entrance, non normalisé, en barre, années regroup.pdf",
       width=9, height=5)


#Ecolo en général (pas forcemment angle changt climatique) : fort "score écolo"
climat_recherche$scorecoloaggr<-ifelse(climat_recherche$ScoreEcoloPond>5, "Elevé", "Non élevé")

graphMoyenneEcolo<-climat_recherche %>% group_by(nbpublistranch2,scorecoloaggr) %>%summarise(MoyenneVols = mean(volsnb, na.rm=T),
                                                                                               Ecart = sd(volsnb, na.rm=T))

ggplot(graphMoyenneEcolo, aes(x = nbpublistranch2, y = MoyenneVols, fill = scorecoloaggr)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  labs(y="Nombre moyens de vols en avion en 2019", x= "Nombre de publications 2017-mi2020")


climat$score

#Graphique montant totaux par date d'entrée

Tot_AnneeEntr<-MontantAvZero2000 %>% 
  filter(DatesortieCT > "2001-01-01" & MoisAnneeAv >="2013-01-01" &
           !(nom %in% c("SIMONIN", "THIERRY", "VIENS", "FOUCAUD", "BERTHEZENE", "PETIT", "CASTAIGNE", "DUPUIS",
                        "TOULOUSE", "CHANU","PARROT", "BLAESI", "ALLEMAND", "RACT", "RICATTE", "PEPIN", "BLUM BOISGARD",  
                        "CROCHET", "BAYET", "AMEDEE MANESME", "LEGRAND SIBENALER", "LASSALE"))) %>%
  group_by(AnneeEntree, Nom.Prénom) %>% 
  summarize(montant=sum(Montant.Ttc), 
            montantNorm=sum(MontantNorm)) %>%
  ungroup %>% group_by(AnneeEntree) %>% 
  summarize(Moy_ParMembre=mean(montant), ET_entreMembres=sd(montant),  
            Coef_de_variation=ET_entreMembres/Moy_ParMembre, Effectif=n(),
            Moy_ParMembreNorm=mean(montantNorm))

Tot_AnneeEntr_8mb<-MontantAvZero2000 %>% 
  filter(DatesortieCT > "2001-01-01" & MoisAnneeAv >="2013-01-01" &
           !(Nom.Prénom %in% c("FALISSARD BRUNO", "DANCHIN NICOLAS",	"BONNET FABRICE",	"VESPIGNANI HERVE",	"TOURAINE PHILIPPE", "GUERET PASCAL",	"POUCHAIN DENIS",	"STAHL JEAN PAUL",	"VARIN REMI"))&
           !(nom %in% c("SIMONIN", "THIERRY", "VIENS", "FOUCAUD", "BERTHEZENE", "PETIT", "CASTAIGNE", "DUPUIS",
                        "TOULOUSE", "CHANU","PARROT", "BLAESI", "ALLEMAND", "RACT", "RICATTE", "PEPIN", "BLUM BOISGARD", 
                        "CROCHET", "BAYET", "AMEDEE MANESME", "LEGRAND SIBENALER", "LASSALE"))) %>%
  group_by(AnneeEntree, Nom.Prénom) %>% 
  summarize(montant=sum(Montant.Ttc), 
            montantNorm=sum(MontantNorm)) %>%
  ungroup %>% group_by(AnneeEntree) %>% 
  summarize(Moy_ParMembre=mean(montant), ET_entreMembres=sd(montant),  
            Coef_de_variation=ET_entreMembres/Moy_ParMembre, Effectif=n(),
            Moy_ParMembreNorm=mean(montantNorm))



Tabgraph1<- Tot_AnneeEntr_8mb %>% 
  select("AnneeEntree", "Moy_ParMembre", "Effectif", "Moy_ParMembreNorm") %>%
  rename(Moy_ParMembre_8mb = Moy_ParMembre, Moy_ParMembreNorm_8mb = Moy_ParMembreNorm) %>%
  pivot_longer(cols = c("Moy_ParMembre_8mb", "Moy_ParMembreNorm_8mb"), 
               names_to = "Av_SansTop8", 
               values_to = "Moy_av")
Tabgraph2<- Tot_AnneeEntr %>% 
  select("AnneeEntree", "Moy_ParMembre", "Effectif", "Moy_ParMembreNorm") %>%
  pivot_longer(cols = c("Moy_ParMembre", "Moy_ParMembreNorm"),
               names_to = "Av_SansTop8", 
               values_to = "Moy_av")


Tabgraph<-rbind(Tabgraph1, Tabgraph2)
#Plutôt diagramme en barre qui donne moins l'impression d'une évolution (temps en abscisse)
Tabgraph$AnneeEntree<-as.factor(Tabgraph$AnneeEntree)
#Tabgraph$Annee<-paste("TC", Tabgraph$Annee, sep=" ")
#Je réordonne le factor pour avoir les barres dans le bon ordre
#Tabgraph$Av_SansTop8 <- ordered(Tabgraph$Av_SansTop8, levels = c("Moy_ParMembre", "Moy_ParMembre_2mb", "Mont_moyMoins66ans", "Mont_moyPlus66ans"))

#Si années regroupées
#Tabgraph$AnneeEntree <- ordered(Tabgraph$AnneeEntree, levels = c("Before 2003", "2003-2005", "2008", "2011-2013", "2014-2015", "2017-2020"))
Tabgraph$AnneeEntree <- ordered(Tabgraph$AnneeEntree, levels = c("Before 2003", "2003", "2005", "2008", "2011-2013", "2014-2015", "2017-2020"))

#Pour avoir des crochets autour des effectifs
Tabgraph$Effectif<-paste("[", Tabgraph$Effectif, "]", sep="")

Tabgraph %>% filter(Av_SansTop8 %in% c("Moy_ParMembre", "Moy_ParMembre_8mb")) %>%
  ggplot(aes(x=AnneeEntree, y=Moy_av, fill=Av_SansTop8, order=Av_SansTop8))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Effectif), position=position_dodge(width=0.9), vjust=-0.4, size=3) +
  theme_classic()+
  scale_fill_manual(name = "",
                    breaks= c("Moy_ParMembre", "Moy_ParMembre_8mb"),
                    values=c("black", "grey"),
                    labels= c("Moy_ParMembre"="All members [headcount]" ,
                              "Moy_ParMembre_8mb"="Minus top 9 beneficiaries\n of all TCs since 2000 [headcount]"))+
  xlab("Date of TC entry") +
  ylab("Average amount received\n between 2013 and mid-2019 (euros)")+
  theme(legend.position = c(0.70, 0.85))

ggsave("/Users/jeromegreffion/Documents/Recherche/ANR Medici/Commission transparence/Chapitre livre Hélène/Illustrations/Average amount by date of TC entrance, non normalisé, en barre, années regroup.pdf",
       width=9, height=5)



######################################################################################################################################################
######################################################################################################################################################
##########Hindex
#1710 personnes ont donné leur Hindex
freq(climat_recherche$hindex, total = T)
mean(climat_recherche$hindex, na.rm=T)

##############################################@@
#Qui connait son hindex ?
#Quelle situation pro pour les non concernés ou incertains ?

climat_recherche %>% filter (hindexconn=="Non concerné·e") %>% group_by (sitpro2)  %>% count(sitpro2)
climat_recherche %>% filter (hindexconn=="Je ne suis pas certain de ce qu'est le h-index") %>% group_by (sitpro2)  %>% count(sitpro2)

#Régressions

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + revenuTete , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + revenuTete + enfantsnb + couple , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete , data=climat_recherche, family=binomial(logit))


reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  nbpublis , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  carriere , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  statutpar.p , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + ScoreEcolo + revenuTete , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + ScoreEcolo + revenuTete + paie , data=climat_recherche, family=binomial(logit))


reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(hindexconnDicho ~ nbpublis, data=climat_recherche, family=binomial(logit))

summary(reglog2)


freq (climat_recherche$paie)



################################################################
#Hindex, regressions avec l'avion  : première approche, avec les données avions totales ()
freq(climat_recherche$volsnb)
mean(climat_recherche$volsnb, na.rm=T)



#Hors avions
res.reg1<- lm(hindex ~ sexe + ageAgr, data=climat_recherche)
res.reg1<- lm(hindex ~ sexe + ageAgr  + sitpro2, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)
res.reg1<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete  , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + revenuTete , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + revenuTete + enfantsnb + couple , data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete , data=climat_recherche)


res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  carriere , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  dippar.p , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + ScoreEcolo, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3  + malpaye , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + dixannees.vote, data=climat_recherche)


summary(res.reg1)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +nbpublis, data=climat_recherche)



res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.reseaux, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.collegues, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.cv, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme+
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.reseaux+
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.collegues+
                 international.postdoc + international.prog + international.asso, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme + nbpublis, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux +nbpublis, data=climat_recherche)

summary(res.reg1)

#intégrant les vols en avion
res.reg1 <- lm(hindex ~ sexe + ageAgr + volsnb, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb + nbpublis, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +volsnb, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +nbpublis +volsnb, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnbtranch, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnbtranch2, data=climat_recherche)


res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsh, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsh + volsnb, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsduree_moy + volsnb, data=climat_recherche)


res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + vols2ans, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb + vols2ans, data=climat_recherche)



res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 +aviontrain, data=climat_recherche)


res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + revenuTete , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + avionperso +avionpersochgt, data=climat_recherche)



summary(res.reg1)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + quizfacteurs.voiture3 , data=climat_recherche)

freq(climat_recherche$quizfacteurs.voiture4)

####Hindex en tranche

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + hindextranch, data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + hindextranch2, data=climat_recherche)

summary(res.reg1)




freq(climat_recherche$volsnbUSA)

#Peut être regarder le lien avec le pays de destination
freq(climat_recherche$volsarrivee1pays, sort="dec")
freq(climat_recherche$volsarrivee2pays, sort="dec")
?freq

###################
#Hindex : en détaillant à partir du détail des vols déclarés dans le module




res.reg2<- lm(hindex ~ sexe + ageAgr, data=climat_recherche)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 , data=climat_recherche)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + volsdist_totconf, data=climat_recherche)

###########################Calcul en fonction des durées (il faudrait pondérer par le nombre d'aller-retour)

climat_recherche$volsnbmoins2j <- ifelse(!is.na(climat_recherche$volsjours1) & climat_recherche$volsjours1=="Moins de deux jours", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsjours2) & climat_recherche$volsjours2=="Moins de deux jours", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsjours3) & climat_recherche$volsjours3=="Moins de deux jours", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours4=="Moins de deux jours", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours5=="Moins de deux jours", climat_recherche$volsnb5, 0)

climat_recherche$volsnb_2j_1sem <- ifelse(!is.na(climat_recherche$volsjours1) & climat_recherche$volsjours1=="De deux jours à une semaine", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsjours2) & climat_recherche$volsjours2=="De deux jours à une semaine", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsjours3) & climat_recherche$volsjours3=="De deux jours à une semaine", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours4=="De deux jours à une semaine", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsjours5) & climat_recherche$volsjours5=="De deux jours à une semaine", climat_recherche$volsnb5, 0)

climat_recherche$volsnb_1sem_1mois <- ifelse(!is.na(climat_recherche$volsjours1) & climat_recherche$volsjours1=="De plus d'une semaine à un mois", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsjours2) & climat_recherche$volsjours2=="De plus d'une semaine à un mois", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsjours3) & climat_recherche$volsjours3=="De plus d'une semaine à un mois", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours4=="De plus d'une semaine à un mois", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsjours5) & climat_recherche$volsjours5=="De plus d'une semaine à un mois", climat_recherche$volsnb5, 0)

climat_recherche$volsnb_sup1mois <- ifelse(!is.na(climat_recherche$volsjours1) & climat_recherche$volsjours1=="Plus d'un mois", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsjours2) & climat_recherche$volsjours2=="Plus d'un mois", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsjours3) & climat_recherche$volsjours3=="Plus d'un mois", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours4=="Plus d'un mois", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsjours5) & climat_recherche$volsjours5=="Plus d'un mois", climat_recherche$volsnb5, 0)



#Je retiens juste ceux du module 1 (pour éviter d'attribuer des distances=0 à ceux qui n'ont juste pas répondu au module)
climat_recherche_mod1<- climat_recherche %>% filter(tiragemodule == "1")

freq(climat_recherche_mod1$volsdist_totconf)
mean(climat_recherche$volsnb, na.rm=T)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_tot, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot + volsdist_moy, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbconf + volsnbsejrech + volsnbworkshop + volsnbcours + volsnbterrain + volsnbfinanc +
                volsnbeval + volsnbjury + volsnbautre, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbconf, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbsejrech, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbworkshop, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbcours, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbterrain, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbfinanc , data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbeval, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbjury, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbautre, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_2j_1sem , data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_1sem_1mois , data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_sup1mois, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)

summary(res.reg2)


freq(climat_recherche_mod1$volsdist_tot)
mean(climat_recherche$volsdistTot1, na.rm=T)


######################################################################################################################################################
#################################################################################################################################
##########Publications depuis 2017
mean(climat_recherche$nbpublis, na.rm=T)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2  + discipline_agr3 + volsnb, data=climat_recherche)



#Hors avions
res.reg1<- lm(nbpublis ~ sexe + ageAgr, data=climat_recherche)
res.reg1<- lm(nbpublis ~ sexe + ageAgr + ageaccad_tranch2, data=climat_recherche)

res.reg1<- lm(nbpublis ~ sexe + ageAgr  + sitpro2, data=climat_recherche)
res.reg1<- lm(nbpublis ~ sexe + ageAgr + ageaccad_tranch2  + sitpro2, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)
res.reg1<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete  , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + hindex, data=climat_recherche)



climat_recherche$ageAgr<-fct_relevel(climat_recherche$ageAgr, "35-39 ans")

res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + enfantsnb + couple , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + enfantsnb , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe*enfantsnb_rec + ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe*enfantsnb_rec*ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + enfantsnb_rec*ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe*enfantsage_rec + ageAgr  + sitpro2 + discipline_agr3 + couple , data=climat_recherche)

summary(res.reg1)



res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  carriere , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  dippar.p , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + discipline_agr3 +  dippar.m , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  statutpar.m , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + ScoreEcolo, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3  + malpaye , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + dixannees.vote, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans, data=climat_recherche)


res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.reseaux, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.collegues, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.cv, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme+
               international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.reseaux+
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.collegues+
                 international.postdoc + international.prog + international.asso, data=climat_recherche)


res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme+
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans + apportconf.travaux + apportconf.reseaux , data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme, data=climat_recherche)


res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme, data=climat_recherche)
summary(res.reg1)

res.reg2 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme + conffois5ans, data=climat_recherche)

res.reg2 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme + conffois5ans + apportconf.tourisme:conffois5ans, data=climat_recherche)

res.reg2 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme:conffois5ans, data=climat_recherche)
res.reg2 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme*conffois5ans, data=climat_recherche)

summary(res.reg2)
ggcoef_model(res.reg2)

summary(res.reg1)

freq(climat_recherche$apportconf.collegues)


#intégrant les vols en avion
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + volsnb, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb + hindex, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +volsnb, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +hindex +volsnb, data=climat_recherche)


res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnbtranch, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnbtranch2, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsh, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsh + volsnb, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsduree_moy + volsnb, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + vols2ans, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb + vols2ans, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 +aviontrain, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + revenuTete , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + avionperso +revenuTete , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + avionpersochgt, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + 
                 international.postdoc + international.prog + international.asso +volsnb, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + volsnb + ecolochgtclimat, data=climat_recherche)



summary(res.reg1)
freq(climat$volsnb)

freq(climat_recherche$avionpersochgt)



####Nb publis en tranche

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch, data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2, data=climat_recherche)

summary(res.reg1)


freq(climat_recherche$nbpublistranch2)


#Je retiens juste ceux du module 1 (pour éviter d'attribuer des distances=0 à ceux qui n'ont juste pas répondu au module)
climat_recherche_mod1<- climat_recherche %>% filter(tiragemodule == "1")

freq(climat_recherche_mod1$volsdist_tot)
mean(climat_recherche$volsnb, na.rm=T)
mean(climat_recherche_mod1$volsdist_moy, na.rm=T)

summary(res.reg2)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_tot, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_moy, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot + volsdist_moy, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbconf + volsnbsejrech + volsnbworkshop + volsnbcours + volsnbterrain + volsnbfinanc +
                volsnbeval + volsnbjury + volsnbautre, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbconf, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbsejrech, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbworkshop, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbcours, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbterrain, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbfinanc , data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbeval, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbjury, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbautre, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_2j_1sem , data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_1sem_1mois , data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_sup1mois, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + volsnbconf + volsnbsejrech + volsnbworkshop + volsnbcours + volsnbterrain + volsnbfinanc +
                volsnbeval + volsnbjury + volsnbautre + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + 
                volsnbEspagne+ volsnbGB + volsnbChine + volsnbJapon+ volsnbAutriche + volsnbPortugal + volsnbPologne, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance , data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + 
                volsnb_sup1mois + volsnbFrance, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot + volsdist_moy +volsnbFrance, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ nbpublis, data=climat_recherche)


summary(res.reg2)

summary(res.reg2)





######################################################################################################################################################
#################################################################################################################################
################Vols en avions

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublis + hindex, data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 + hindextranch2, data=climat_recherche)

#Lien avec être plus ou moins écolo
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublis + ecolochgtclimat, data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + ecolochgtclimat, data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch3 + ecolochgtclimat, data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch3 + dixannees.marche +preoccupe + avionpersochgt  , data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  dixannees.marche +preoccupe + avionpersochgt  , data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  dixannees.marche +preoccupe + avionpersochgt +chgtpratique  , data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  dixannees.marche +preoccupe + avionpersochgt +effortsconso  , data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche)

summary(res.reg1)
climat$chgt

######################################################################################################################################################
#################################################################################################################################
################Origine sociale
climat_recherche$PR_DR[climat_recherche$sitpro2 %in% c("Directeur·rice de recherche", "Professeur·e des universités")]<-"PR ou DR"
climat_recherche$PR_DR[climat_recherche$sitpro2 %in% c("Chargé·e de recherche", "Maître·sse de conférences")]<-"CR ou MCF"

#Les références dans la régression
climat_recherche$PR_DR<-as.factor(climat_recherche$PR_DR)
climat_recherche$PR_DR<-fct_relevel(climat_recherche$PR_DR, "CR ou MCF")



reglog2 <- glm(PR_DR ~ sexe + ageAgr + statutpar.p , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr  + statutpar.m , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr  + dippar.p , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr  + dippar.m , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr  + dippar.m , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr + couple + enfantsnb , data=climat_recherche, family=binomial(logit))



summary(reglog2)

freq(climat_recherche$PR_DR)



########################
#Ressources. Sur les produits de variables et analyses des interactions entre deux variables :
#http://larmarange.github.io/analyse-R/effets-d-interaction.html
#https://commonweb.unifr.ch/artsdean/pub/gestens/f/as/files/4665/9547_131825.pdf
