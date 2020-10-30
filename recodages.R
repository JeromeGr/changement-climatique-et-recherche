library(questionr)
library(tidyverse)


#chargement de la base

climat <- read.csv("climat1410.csv", encoding ="UTF-8")



# vols_dicho : a volé ou n'a pas volé ----
climat$vols_dicho <- ifelse(climat$volsnb=="0", "pas_vol", "vol")
climat$vols_dicho[is.na(climat$volsnb)] <- NA

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

