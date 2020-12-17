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

# Et on vire les trolls (il faut rajouter de garder les NAs sinon on les perd)
climat<-climat %>% filter((nbpublisang!=666 | is.na(nbpublisang)))

climat<-climat %>% filter((tpsdomtrav.tgv_h!=85 | is.na(tpsdomtrav.tgv_h)))

# La modalité Moins de 18 ans est vide : la retirer
climat$age <- droplevels(climat$age)

#Regroupement catégories d'âge (pour avoir des catégories plus homogènes en termes de nombre de personnes)
climat$ageAgr<-as.character(climat$age)
climat$ageAgr[climat$age %in% c("70 ans ou plus", "65-69 ans")]<-"65 ans et plus"
climat$ageAgr[climat$age %in% c("18-24 ans", "25-29 ans")]<-"Moins de 29 ans"
climat$ageAgr[climat$age %in% c("55-59 ans", "60-64 ans")]<-"55-64 ans"

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

climat$preoccupe2 <- climat$preoccupe
climat$preoccupe2[substr(climat$changclim, 1, 3) == "Non"] <- "Pas du tout préoccupé·e"
climat$preoccupe2[climat$changclim == "Sans opinion"] <- "Sans opinion"

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

# situation professionnelle sitpro réordonnée par logique hiérarchique ----
climat$sitpro <- factor(climat$sitpro,
                        levels = c(
                          "Directeur·rice de recherche", "Professeur·e des universités",
                          "Chargé·e de recherche", "Maître·sse de conférences", "Post-doctorant·e",
                          "ATER", "Doctorant·e contractuel·le", "Doctorant·e CIFRE", "Ingénieur·e de recherche",
                          "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
                          "Chargé·e d'études/de mission", "Adjoint·e technique", "Autre"
                        ))

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

#Recodage temps de transport domicile travail
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
for(inst in c("anr", "europe", "france", "inter", "prive")) {
  varnon <- paste0("projets.", inst, "_n")
  varr <- paste0("projets.", inst, "_r")
  varm <- paste0("projets.", inst, "_m")
  # Indique si au moins une case a été cochée sur la ligne (Responsable, membre, non)
  unecochee <- (!is.na(climat[[varnon]]) & climat[[varnon]] == 1) |
               (!is.na(climat[[varr]]) & climat[[varr]] == 1) |
               (!is.na(climat[[varm]]) & climat[[varm]] == 1)
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
    # on les sépare selon qu'une (autre) case a été cochée sur la ligne ou non
    # 1 signifie coché
    # 0 signifie que le répondant n'est pas allé jusqu'à cette question/page
    # Si Oui et Non ont été cochés tous les deux, on retient Oui
    # Vérification avec :
    # table(paste(climat$projets.anr_r, climat$projets.anr_m, climat$projets.anr_n),
    #             climat$projets.anr_m2, useNA="a")
    climat[[var2]] <- if_else(is.na(climat[[var]]) & unecochee,
                              paste(sit2, inst2, "non"),
                              if_else(climat[[var]] == 1,
                                      paste(sit2, inst2, "oui"),
                                      NA_character_))
  }
}

#Financements : regroupement membres et responsables
climat$particip_ANR<-0
climat$particip_ANR[climat$projets.anr_r2 == "Responsable projet ANR oui" | climat$projets.anr_m2 == "Membre projet ANR oui"]<-1
climat$particip_Fr<-0
climat$particip_Fr[climat$projets.france_r2 == "Responsable projet France oui" | climat$projets.france_m2 =="Membre projet France oui"]<-1
climat$particip_Europ<-0
climat$particip_Europ[climat$projets.europe_r2 =="Responsable projet européen oui" | climat$projets.europe_m2 == "Membre projet européen oui"]<-1
climat$particip_Intern<-0
climat$particip_Intern[climat$projets.inter_r2 == "Responsable projet international oui" | climat$projets.inter_m2 == "Membre projet international oui"]<-1
climat$particip_prive<-0
climat$particip_prive[climat$projets.prive_r2 == "Responsable projet privé oui" | climat$projets.prive_m2 == "Membre projet privé oui"]<-1


#Membre ou responsable d'un projet financé
climat$Profin_Mb_Resp[climat$projets.anr_r2 =="Responsable projet ANR oui" | climat$projets.france_r2 == "Responsable projet France oui" | 
                        climat$projets.europe_r2 =="Responsable projet européen oui" | climat$projets.inter_r2 == "Responsable projet international oui" | 
                        climat$projets.prive_r2  =="Responsable projet privé oui" ]<-"Responsable d'au moins 1 projet financé"
climat$Profin_Mb_Resp[is.na(climat$Profin_Mb_Resp) & (climat$projets.anr_m2 == "Membre projet ANR oui" | climat$projets.france_m2 == "Membre projet France oui" |
                                                        climat$projets.europe_m2 == "Membre projet européen oui" | climat$projets.inter_m2 == "Membre projet international oui" | 
                                                        climat$projets.prive_r2  == "Responsable projet privé oui") ]<-"Membre d'au moins 1 projet financé"
climat$Profin_Mb_Resp[is.na(climat$Profin_Mb_Resp)]<-"Ni membre ni resp d'un 1 projet financé"



#Calcul du revenu par tête dans le foyer
#Une demie part par enfant
climat$couple1[climat$couple=="Oui"]<-1
climat$couple1[climat$couple=="Non"]<-0
#Calcul du nombre de personnes dans le foyer, (calcul "fiscal" avec les enfants =1/2)
climat$tailleFiscFoyer<-1+climat$couple1+climat$enfantsnb/2

climat$revenuTete[climat$revenu=="Moins de 1 500 euros par mois" & !is.na(climat$revenu) ]<-750/climat$tailleFiscFoyer[climat$revenu=="Moins de 1 500 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat$revenu)]<-2000/climat$tailleFiscFoyer[climat$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat$revenu)]<-3000/climat$tailleFiscFoyer[climat$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat$revenu)]<-4000/climat$tailleFiscFoyer[climat$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat$revenu)]<-7000/climat$tailleFiscFoyer[climat$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat$revenu)]<-9000/climat$tailleFiscFoyer[climat$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat$revenu)]<-12500/climat$tailleFiscFoyer[climat$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat$revenu)]
climat$revenuTete[climat$revenu=="Plus de 15 000 par mois" & !is.na(climat$revenu)]<-20000/climat$tailleFiscFoyer[climat$revenu=="Plus de 15 000 par mois" & !is.na(climat$revenu)]

#######################
#Modalités de référence dans les régressions
climat$sexe <- as.factor(climat$sexe)
climat$sexe <- relevel(climat$sexe, ref = "Homme")

climat$ageAgr <- as.factor(climat$ageAgr)
climat$ageAgr <- relevel(climat$ageAgr, ref = "50-54 ans")

climat$couple <- as.factor(climat$couple)
climat$couple <- relevel(climat$couple, ref = "Oui")

#climat$revenuAgr <- as.factor(climat$revenuAgr)
#climat$revenuAgr <- relevel(climat$revenuAgr, ref = "De 4 500 à 5 999 euros par mois")

climat$sitpro2 <- relevel(climat$sitpro, ref = "Maître·sse de conférences")

climat$discipline_agr <- relevel(climat$discipline_agr, ref = "Physique")
climat$discipline_agr2 <- relevel(climat$discipline_agr, ref = "Physique")
climat$discipline_agr3 <- relevel(climat$discipline_agr3, ref = "Physique")

climat$discipline <- as.factor(climat$discipline)
climat$discipline <- relevel(climat$discipline , ref = "25 : Mathématiques")

climat$carriere <- as.factor(climat$carriere)
climat$carriere <- relevel(climat$carriere , ref = "Non")

climat$Profin_Mb_Resp <- as.factor(climat$Profin_Mb_Resp)
climat$Profin_Mb_Resp <- relevel(climat$Profin_Mb_Resp , ref = "Ni membre ni resp d'un 1 projet financé")

climat$solinstit.limitevols <- as.factor(climat$solinstit.limitevols)
climat$solinstit.limitevols <- relevel(climat$solinstit.limitevols, ref = "C'est prioritaire")

climat$solinstit.vols6h <- as.factor(climat$solinstit.vols6h)
climat$solinstit.vols6h <- relevel(climat$solinstit.vols6h, ref = "C'est prioritaire")

climat$solinstit.train <- as.factor(climat$solinstit.train)
climat$solinstit.train <- relevel(climat$solinstit.train, ref = "C'est prioritaire")

climat$solrisqreducavion.qual <- as.factor(climat$solrisqreducavion.qual)
climat$solrisqreducavion.qual <- relevel(climat$solrisqreducavion.qual, ref = "C'est peu probable")
climat$solrisqreducavion.fin <- as.factor(climat$solrisqreducavion.fin)
climat$solrisqreducavion.fin <- relevel(climat$solrisqreducavion.fin, ref = "C'est peu probable")
climat$solrisqreducavion.diffusion <- as.factor(climat$solrisqreducavion.diffusion)
climat$solrisqreducavion.diffusion <- relevel(climat$solrisqreducavion.diffusion, ref = "C'est peu probable")
climat$solrisqreducavion.donnees <- as.factor(climat$solrisqreducavion.donnees)
climat$solrisqreducavion.donnees <- relevel(climat$solrisqreducavion.donnees, ref = "C'est peu probable")
climat$solrisqreducavion.avantages <- as.factor(climat$solrisqreducavion.avantages)
climat$solrisqreducavion.avantages <- relevel(climat$solrisqreducavion.avantages, ref = "C'est peu probable")
climat$solrisqreducavion.insertion <- as.factor(climat$solrisqreducavion.insertion)
climat$solrisqreducavion.insertion <- relevel(climat$solrisqreducavion.insertion, ref = "C'est peu probable")
climat$solrisqreducavion.isoler <- as.factor(climat$solrisqreducavion.isoler)
climat$solrisqreducavion.isoler <- relevel(climat$solrisqreducavion.isoler, ref = "C'est peu probable")
climat$solrisqreducavion.bureaucratie <- as.factor(climat$solrisqreducavion.bureaucratie)
climat$solrisqreducavion.bureaucratie <- relevel(climat$solrisqreducavion.bureaucratie, ref = "C'est peu probable")

climat$paie <- as.factor(climat$paie)
climat$paie <- relevel(climat$paie , ref = "Mal payé·e")

climat$employeur <- as.factor(climat$employeur)
climat$employeur <- relevel(climat$employeur , ref = "Une université")

#climat$NumVague <- as.factor(climat$NumVague)
#climat$NumVague <- relevel(climat$NumVague, ref = "Après premier message")
