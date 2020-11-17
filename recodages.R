library(questionr)
library(tidyverse)

#Chemin du dossier personnalisé
#Jérôme
setwd("/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche")


#chargement de la base

climat <- read.csv("climat1410.csv", fileEncoding ="UTF-8")



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
                          "Chargé·e d’études/de mission", "Adjoint·e technique", "Autre",
                          ""
                        ))

# discipline_agregee ----
freq(climat$discipline)
freq(climat$discipline_agregee)
?freq
climat$discipline_agregee <- fct_recode(
  climat$discipline,
  "Droit, économie, gestion"="01 : Droit privé et sciences criminelles",
  "Droit, économie, gestion"="02 : Droit public",
  "Droit, économie, gestion"="03 : Histoire du droit et des institutions",
  "Droit, économie, gestion"="04 : Science politique",
  "Droit, économie, gestion"="05 : Sciences économiques",
  "Droit, économie, gestion"="06 : Sciences de gestion",
  "Lettres et sciences humaines (1)"="07 : Sciences du langage : linguistique et phonétique générales",
  "Lettres et sciences humaines (1)"="08 : Langues et littératures anciennes",
  "Lettres et sciences humaines (1)"="09 : Langue et littérature françaises",
  "Lettres et sciences humaines (1)"="11 : Langues et littératures anglaises et anglo-saxonnes",
  "Lettres et sciences humaines (1)"="12 : Langues et littératures germaniques et scandinaves",
  "Lettres et sciences humaines (1)"="13 : Langues et littératures slaves",
  "Lettres et sciences humaines (1)"="14 : Langues et littératures romanes : espagnol, italien, portugais, autres langues romanes",
  "Lettres et sciences humaines (1)"="15 : Langues et littératures arabes, chinoises, japonaises, hébraïques, d'autres domaines linguistiques",
  "Lettres et sciences humaines (1)"="16 : Psychologie, psychologie clinique, psychologie sociale",
  "Lettres et sciences humaines (1)"="17 : Philosophie",
  "Lettres et sciences humaines (1)"="18 : Architecture et Arts : plastiques, du spectacle, musique, musicologie, esthétique, sciences de l'art",
  "Lettres et sciences humaines (1)"="19 : Sociologie, démographie",
  "Lettres et sciences humaines (1)"="20 : Anthropologie biologique, ethnologie, préhistoire",
  "Lettres et sciences humaines (1)"="21 : Histoire et civilisations : histoire et archéologie des mondes anciens et des mondes médiévaux",
  "Lettres et sciences humaines (1)"="22 : Histoire et civilisations : histoire des mondes modernes, histoire du monde contemporain",
  "Lettres et sciences humaines (1)"="23 : Géographie physique, humaine, économique et régionale",
  "Lettres et sciences humaines (1)"="24 : Aménagement de l'espace, urbanisme",
  "Sciences (1)"="25 : Mathématiques",
  "Sciences (1)"="26 : Mathématiques appliquées et applications des mathématiques",
  "Sciences (1)"="27 : Informatique",
  "Sciences (1)"="28 : Milieux denses et matériaux",
  "Sciences (1)"="29 : Constituants élémentaires",
  "Sciences (1)"="30 : Milieux dilués et optique",
  "Sciences (1)"="31 : Chimie théorique, physique, analytique",
  "Sciences (1)"="32 : Chimie organique, inorganique, industrielle",
  "Sciences (1)"="33 : Chimie des matériaux",
  "Sciences (1)"="34 : Astronomie, astrophysique",
  "Sciences (1)"="35 : Structure et évolution de la Terre et des autres planètes",
  "Sciences (1)"="36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère",
  "Sciences (1)"="37 : Météorologie, océanographie physique et physique de l'environnement",
  "Médecine, odontologie"="42 : Morphologie et morphogenèse",
  "Médecine, odontologie"="43 : Biophysique et imagerie médicale",
  "Médecine, odontologie"="44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition",
  "Médecine, odontologie"="45 : Microbiologie, maladies transmissibles et hygiène",
  "Médecine, odontologie"="46 : Santé publique, environnement et société",
  "Médecine, odontologie"="47 : Cancérologie, génétique, hématologie, immunologie",
  "Médecine, odontologie"="48 : Anesthésiologie, réanimation, médecine d'urgence, pharmacologie et thérapeutique",
  "Médecine, odontologie"="49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation",
  "Médecine, odontologie"="51 : Pathologie cardiorespiratoire et vasculaire",
  "Médecine, odontologie"="54 : Développement et pathologie de l'enfant, gynécologie-obstétrique, endocrinologie et reproduction",
  "Médecine, odontologie"="57 : Sciences biologiques, médecine et chirurgie buccales",
  "Médecine, odontologie"="58 : Sciences physiques et physiologiques endodontiques et prothétiques",
  "Sciences (2)"="60 : Mécanique, génie mécanique, génie civil",
  "Sciences (2)"="61 : Génie informatique, automatique et traitement du signal",
  "Sciences (2)"="62 : Énergétique, génie des procédés",
  "Sciences (2)"="63 : Génie Électrique, Électronique, optronique et systèmes",
  "Sciences (2)"="64 : Biochimie et biologie moléculaire",
  "Sciences (2)"="65 : Biologie cellulaire",
  "Sciences (2)"="66 : Physiologie",
  "Sciences (2)"="67 : Biologie des populations et écologie",
  "Sciences (2)"="68 : Biologie des organismes",
  "Sciences (2)"="69 : Neurosciences",
  "Lettres et sciences humaines (2)"="70 : Sciences de l'éducation",
  "Lettres et sciences humaines (2)"="71 : Sciences de l'information et de la communication",
  "Lettres et sciences humaines (2)"="72 : Épistémologie, histoire des sciences et des techniques",
  "Lettres et sciences humaines (2)"="73 : Cultures et langues régionales",
  "Lettres et sciences humaines (2)"="74 : Sciences et techniques des activités physiques et sportives",
  "Pharmacie"="80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)",
  "Pharmacie"="81/86 : Sciences du médicament et des autres produits de santé (ex-40)",
  "Pharmacie"="82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)",
  "Autres santé"="90 : Maïeutique",
  "Autres santé"="91 : Sciences de la rééducation et de la réadaptation"
)

#Recodage discipline pour avoir  une subdivision plus fine avec des effectifs plus homogènes (et des intitulés plus explicites, plus que sciences 1 et 2 par ex)

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
  "Astro, terre, atmo, océans"="34 : Astronomie, astrophysique",
  "Astro, terre, atmo, océans"="35 : Structure et évolution de la Terre et des autres planètes",
  "Astro, terre, atmo, océans"="36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère",
  "Astro, terre, atmo, océans"="37 : Météorologie, océanographie physique et physique de l'environnement",
  "Médecine, pharma, santé"="42 : Morphologie et morphogenèse",
  "Médecine, pharma, santé"="43 : Biophysique et imagerie médicale",
  "Médecine, pharma, santé"="44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition",
  "Médecine, pharma, santé"="45 : Microbiologie, maladies transmissibles et hygiène",
  "Médecine, pharma, santé"="46 : Santé publique, environnement et société",
  "Médecine, pharma, santé"="47 : Cancérologie, génétique, hématologie, immunologie",
  "Médecine, pharma, santé"="48 : Anesthésiologie, réanimation, médecine d'urgence, pharmacologie et thérapeutique",
  "Médecine, pharma, santé"="49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation",
  "Médecine, pharma, santé"="51 : Pathologie cardiorespiratoire et vasculaire",
  "Médecine, pharma, santé"="54 : Développement et pathologie de l'enfant, gynécologie-obstétrique, endocrinologie et reproduction",
  "Médecine, pharma, santé"="57 : Sciences biologiques, médecine et chirurgie buccales",
  "Médecine, pharma, santé"="58 : Sciences physiques et physiologiques endodontiques et prothétiques",
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
  "Médecine, pharma, santé"="80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)",
  "Médecine, pharma, santé"="81/86 : Sciences du médicament et des autres produits de santé (ex-40)",
  "Médecine, pharma, santé"="82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)",
  "Médecine, pharma, santé"="90 : Maïeutique",
  "Médecine, pharma, santé"="91 : Sciences de la rééducation et de la réadaptation"
)

freq (climat$discipline_agr2)

# participation à une ANR, ERC, etc.
for(inst in c("anr", "europe", "france", "inter", "prive")) {
  for(sit in c("r", "m")) {
    var <- paste0("projets.", inst, "_", sit, ".")
    var2 <- paste0("projets.", inst, "_", sit)
    inst2 <- c(anr="projet ANR",
               europe="projet européen",
               france="projet France",
               inter="projet international",
               prive="projet privé")[inst]
    sit2 <- c(r="Reponsable", m="Membre")[sit]
    climat[[var2]] <- if_else(is.na(climat[[var]]) | climat[[var]] == 0,
                              paste(sit2, inst2, "non"),
                              paste(sit2, inst2, "oui"))
  }
}

