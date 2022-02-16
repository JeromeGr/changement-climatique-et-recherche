data <- read.csv("~/Private/results-survey_113464_R_data_file_151220.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


# LimeSurvey Field type: F
data[, 1] <- as.numeric(data[, 1])
# attributes(data)$variable.labels[1] <- "id"
# names(data)[1] <- "id"
# LimeSurvey Field type: DATETIME23.2
data[, 2] <- as.character(data[, 2])
# attributes(data)$variable.labels[2] <- "submitdate"
# names(data)[2] <- "submitdate"
# LimeSurvey Field type: F
data[, 3] <- as.numeric(data[, 3])
# attributes(data)$variable.labels[3] <- "lastpage"
# names(data)[3] <- "lastpage"
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
# attributes(data)$variable.labels[4] <- "startlanguage"
# names(data)[4] <- "startlanguage"
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
# attributes(data)$variable.labels[5] <- "Tête de série"
# names(data)[5] <- "q_"
# LimeSurvey Field type: A
data[, 6] <- as.character(data[, 6])
# attributes(data)$variable.labels[6] <- "token"
# names(data)[6] <- "token"
# LimeSurvey Field type: DATETIME23.2
data[, 7] <- as.character(data[, 7])
# attributes(data)$variable.labels[7] <- "startdate"
# names(data)[7] <- "startdate"
# LimeSurvey Field type: DATETIME23.2
data[, 8] <- as.character(data[, 8])
# attributes(data)$variable.labels[8] <- "datestamp"
# names(data)[8] <- "datestamp"
# LimeSurvey Field type: A
data[, 9] <- as.character(data[, 9])
# # attributes(data)$variable.labels[9] <- "{if(is_empty(tiragemodule.NAOK), rand(1, 2), tiragemodule.NAOK)}"
# names(data)[9] <- "tiragemodule"
# LimeSurvey Field type: A
data[, 10] <- as.character(data[, 10])
# attributes(data)$variable.labels[10] <- "Êtes-vous..."
data[, 10] <- factor(data[, 10], levels=c("F","H","A"),labels=c("Femme", "Homme", "Autre"))
# names(data)[10] <- "sexe"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
# attributes(data)$variable.labels[11] <- "Quel âge avez-vous ?"
data[, 11] <- factor(data[, 11], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12"),labels=c("Moins de 18 ans", "18-24 ans", "25-29 ans", "30-34 ans", "35-39 ans", "40-44 ans", "45-49 ans", "50-54 ans", "55-59 ans", "60-64 ans", "65-69 ans", "70 ans ou plus"))
# names(data)[11] <- "age"
# LimeSurvey Field type: A
data[, 12] <- as.character(data[, 12])
# attributes(data)$variable.labels[12] <- "En juin 2020, étiez-vous doctorant·e ?"
data[, 12] <- factor(data[, 12], levels=c("Oui","Non"),labels=c("Oui", "Non"))
# names(data)[12] <- "docto"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
# attributes(data)$variable.labels[13] <- "En juin 2020, quel était votre statut d'emploi principal ?"
data[, 13] <- factor(data[, 13], levels=c("Fonct","CDI","CDD","Indep","Chom","Retr","Autre"),labels=c("Fonctionnaire", "CDI", "CDD", "À votre compte", "Chômeur/chômeuse/sans emploi", "Retraité·e", "Autre"))
# names(data)[13] <- "statut"
# LimeSurvey Field type: A
data[, 14] <- as.character(data[, 14])
# attributes(data)$variable.labels[14] <- "Quelle était votre situation professionnelle principale ?"
data[, 14] <- factor(data[, 14], levels=c("PR","MCF","ATER","DocC","CIFRE","DR","CR","CE","PD","IR","IE","AI","T","AT","-oth-"),labels=c("Professeur·e des universités", "Maître·sse de conférences", "ATER", "Doctorant·e contractuel·le", "Doctorant·e CIFRE", "Directeur·rice de recherche", "Chargé·e de recherche", "Chargé·e d'études/de mission", "Post-doctorant·e", "Ingénieur·e de recherche", "Ingénieur·e d\'études", "Assistant ingénieur·e", "Technicien·ne", "Adjoint·e technique", "Autre"))
# names(data)[14] <- "sitpro"
# LimeSurvey Field type: A
data[, 15] <- as.character(data[, 15])
# attributes(data)$variable.labels[15] <- "[Autre] Quelle était votre situation professionnelle principale ?"
# names(data)[15] <- "sitpro_other"
# LimeSurvey Field type: A
data[, 16] <- as.character(data[, 16])
# attributes(data)$variable.labels[16] <- "En juin 2020, étiez-vous affilié·e ou rattaché·e (à titre principal ou secondaire) à une institution de recherche publique ?"
data[, 16] <- factor(data[, 16], levels=c("Oui","Non"),labels=c("Oui", "Non"))
# names(data)[16] <- "rechpub"
# LimeSurvey Field type: F
data[, 17] <- as.numeric(data[, 17])
# attributes(data)$variable.labels[17] <- "Quelle était votre discipline de recherche principale ? Merci de choisir parmi les sections CNU suivantes. Vous pouvez taper les premiers caractères ou le numéro pour lancer une recherche."
data[, 17] <- factor(data[, 17], levels=c(01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,76,77,80,81,82,90,91,92),labels=c("01 : Droit privé et sciences criminelles", "02 : Droit public", "03 : Histoire du droit et des institutions", "04 : Science politique", "05 : Sciences économiques", "06 : Sciences de gestion", "07 : Sciences du langage : linguistique et phonétique générales", "08 : Langues et littératures anciennes", "09 : Langue et littérature françaises", "10 : Littératures comparées", "11 : Langues et littératures anglaises et anglo-saxonnes", "12 : Langues et littératures germaniques et scandinaves", "13 : Langues et littératures slaves", "14 : Langues et littératures romanes : espagnol, italien, portugais, autres langues romanes", "15 : Langues et littératures arabes, chinoises, japonaises, hébraïques, d\'autres domaines linguistiques", "16 : Psychologie, psychologie clinique, psychologie sociale", "17 : Philosophie", "18 : Architecture et Arts : plastiques, du spectacle, musique, musicologie, esthétique, sciences de l\'art", "19 : Sociologie, démographie", "20 : Anthropologie biologique, ethnologie, préhistoire", "21 : Histoire et civilisations : histoire et archéologie des mondes anciens et des mondes médiévaux", "22 : Histoire et civilisations : histoire des mondes modernes, histoire du monde contemporain", "23 : Géographie physique, humaine, économique et régionale", "24 : Aménagement de l\'espace, urbanisme", "25 : Mathématiques", "26 : Mathématiques appliquées et applications des mathématiques", "27 : Informatique", "28 : Milieux denses et matériaux", "29 : Constituants élémentaires", "30 : Milieux dilués et optique", "31 : Chimie théorique, physique, analytique", "32 : Chimie organique, inorganique, industrielle", "33 : Chimie des matériaux", "34 : Astronomie, astrophysique", "35 : Structure et évolution de la Terre et des autres planètes", "36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère", "37 : Météorologie, océanographie physique et physique de l\'environnement", "42 : Morphologie et morphogenèse", "43 : Biophysique et imagerie médicale", "44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition", "45 : Microbiologie, maladies transmissibles et hygiène", "46 : Santé publique, environnement et société", "47 : Cancérologie, génétique, hématologie, immunologie", "48 : Anesthésiologie, réanimation, médecine d\'urgence, pharmacologie et thérapeutique", "49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation", "50 : Pathologie ostéo-articulaire, dermatologie et chirurgie plastique", "51 : Pathologie cardiorespiratoire et vasculaire", "52 : Maladies des appareils digestif et urinaire", "53 : Médecine interne, gériatrie, chirurgie générale et médecine générale", "54 : Développement et pathologie de l\'enfant, gynécologie-obstétrique, endocrinologie et reproduction", "55 : Pathologie de la tête et du cou", "56 : Développement, croissance et prévention", "57 : Sciences biologiques, médecine et chirurgie buccales", "58 : Sciences physiques et physiologiques endodontiques et prothétiques", "60 : Mécanique, génie mécanique, génie civil", "61 : Génie informatique, automatique et traitement du signal", "62 : Énergétique, génie des procédés", "63 : Génie Électrique, Électronique, optronique et systèmes", "64 : Biochimie et biologie moléculaire", "65 : Biologie cellulaire", "66 : Physiologie", "67 : Biologie des populations et écologie", "68 : Biologie des organismes", "69 : Neurosciences", "70 : Sciences de l\'éducation", "71 : Sciences de l\'information et de la communication", "72 : Épistémologie, histoire des sciences et des techniques", "73 : Cultures et langues régionales", "74 : Sciences et techniques des activités physiques et sportives", "76 : Théologie catholique", "77 : Théologie protestante", "80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)", "81/86 : Sciences du médicament et des autres produits de santé (ex-40)", "82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)", "90 : Maïeutique", "91 : Sciences de la rééducation et de la réadaptation", "92 : Sciences infirmières"))
# names(data)[17] <- "discipline"
# LimeSurvey Field type: A
data[, 18] <- as.character(data[, 18])
# attributes(data)$variable.labels[18] <- "De quelle branche d'activité professionnelle dépendiez-vous ?"
data[, 18] <- factor(data[, 18], levels=c("A","B","C","D","E","F","G","J","Autre","NSP"),labels=c("BAP A : Sciences du vivant", "BAP B : Sciences chimiques et sciences des matériaux", "BAP C : Sciences de l\'ingénieur et instrumentation scientifique", "BAP D : Sciences humaines et sociales", "BAP E : Informatique, statistique et calcul scientifique", "BAP F : Culture, communication, production et diffusion des savoirs", "BAP G : Patrimoine, logistique, restauration et prévention", "BAP J : Gestion et pilotage", "Autre", "Je ne sais pas"))
# names(data)[18] <- "bap"
# LimeSurvey Field type: F
data[, 19] <- as.numeric(data[, 19])
# attributes(data)$variable.labels[19] <- "Êtes-vous proche d'une des disciplines suivantes ? Merci de choisir parmi les sections CNU. Vous pouvez taper les premiers caractères ou le numéro pour lancer une recherche."
data[, 19] <- factor(data[, 19], levels=c(01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,76,77,80,81,82,90,91,92),labels=c("01 : Droit privé et sciences criminelles", "02 : Droit public", "03 : Histoire du droit et des institutions", "04 : Science politique", "05 : Sciences économiques", "06 : Sciences de gestion", "07 : Sciences du langage : linguistique et phonétique générales", "08 : Langues et littératures anciennes", "09 : Langue et littérature françaises", "10 : Littératures comparées", "11 : Langues et littératures anglaises et anglo-saxonnes", "12 : Langues et littératures germaniques et scandinaves", "13 : Langues et littératures slaves", "14 : Langues et littératures romanes : espagnol, italien, portugais, autres langues romanes", "15 : Langues et littératures arabes, chinoises, japonaises, hébraïques, d\'autres domaines linguistiques", "16 : Psychologie, psychologie clinique, psychologie sociale", "17 : Philosophie", "18 : Architecture et Arts : plastiques, du spectacle, musique, musicologie, esthétique, sciences de l\'art", "19 : Sociologie, démographie", "20 : Anthropologie biologique, ethnologie, préhistoire", "21 : Histoire et civilisations : histoire et archéologie des mondes anciens et des mondes médiévaux", "22 : Histoire et civilisations : histoire des mondes modernes, histoire du monde contemporain", "23 : Géographie physique, humaine, économique et régionale", "24 : Aménagement de l\'espace, urbanisme", "25 : Mathématiques", "26 : Mathématiques appliquées et applications des mathématiques", "27 : Informatique", "28 : Milieux denses et matériaux", "29 : Constituants élémentaires", "30 : Milieux dilués et optique", "31 : Chimie théorique, physique, analytique", "32 : Chimie organique, inorganique, industrielle", "33 : Chimie des matériaux", "34 : Astronomie, astrophysique", "35 : Structure et évolution de la Terre et des autres planètes", "36 : Terre solide : géodynamique des enveloppes supérieures, paléobiosphère", "37 : Météorologie, océanographie physique et physique de l\'environnement", "42 : Morphologie et morphogenèse", "43 : Biophysique et imagerie médicale", "44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition", "45 : Microbiologie, maladies transmissibles et hygiène", "46 : Santé publique, environnement et société", "47 : Cancérologie, génétique, hématologie, immunologie", "48 : Anesthésiologie, réanimation, médecine d\'urgence, pharmacologie et thérapeutique", "49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation", "50 : Pathologie ostéo-articulaire, dermatologie et chirurgie plastique", "51 : Pathologie cardiorespiratoire et vasculaire", "52 : Maladies des appareils digestif et urinaire", "53 : Médecine interne, gériatrie, chirurgie générale et médecine générale", "54 : Développement et pathologie de l\'enfant, gynécologie-obstétrique, endocrinologie et reproduction", "55 : Pathologie de la tête et du cou", "56 : Développement, croissance et prévention", "57 : Sciences biologiques, médecine et chirurgie buccales", "58 : Sciences physiques et physiologiques endodontiques et prothétiques", "60 : Mécanique, génie mécanique, génie civil", "61 : Génie informatique, automatique et traitement du signal", "62 : Énergétique, génie des procédés", "63 : Génie Électrique, Électronique, optronique et systèmes", "64 : Biochimie et biologie moléculaire", "65 : Biologie cellulaire", "66 : Physiologie", "67 : Biologie des populations et écologie", "68 : Biologie des organismes", "69 : Neurosciences", "70 : Sciences de l\'éducation", "71 : Sciences de l\'information et de la communication", "72 : Épistémologie, histoire des sciences et des techniques", "73 : Cultures et langues régionales", "74 : Sciences et techniques des activités physiques et sportives", "76 : Théologie catholique", "77 : Théologie protestante", "80/85 : Sciences physico-chimiques et ingénierie appliquée à la santé (ex-39)", "81/86 : Sciences du médicament et des autres produits de santé (ex-40)", "82/87 : Sciences biologiques, fondamentales et cliniques (ex-41)", "90 : Maïeutique", "91 : Sciences de la rééducation et de la réadaptation", "92 : Sciences infirmières"))
# names(data)[19] <- "disciplineita"
# LimeSurvey Field type: A
data[, 20] <- as.character(data[, 20])
# attributes(data)$variable.labels[20] <- "En juin 2020, quel était votre employeur principal ?"
data[, 20] <- factor(data[, 20], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","-oth-"),labels=c("CNRS", "Une université", "Une grande école ou un grand établissement", "Inserm", "Inrae", "Inria", "IRD", "Ined", "CEA", "CNES", "ONERA", "Cirad", "Ifremer", "Une autre institution publique", "Une entreprise", "Autre"))
# names(data)[20] <- "employeur"
# LimeSurvey Field type: A
data[, 21] <- as.character(data[, 21])
# attributes(data)$variable.labels[21] <- "[Autre] En juin 2020, quel était votre employeur principal ?"
# names(data)[21] <- "employeur_other"
# LimeSurvey Field type: F
data[, 22] <- as.numeric(data[, 22])
# attributes(data)$variable.labels[22] <- "[CNRS] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 22] <- factor(data[, 22], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[22] <- "tutelles_CNRS"
# LimeSurvey Field type: F
data[, 23] <- as.numeric(data[, 23])
# attributes(data)$variable.labels[23] <- "[Une université] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 23] <- factor(data[, 23], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[23] <- "tutelles_Univ"
# LimeSurvey Field type: F
data[, 24] <- as.numeric(data[, 24])
# attributes(data)$variable.labels[24] <- "[Une grande école ou un grand établissement] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 24] <- factor(data[, 24], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[24] <- "tutelles_Ecole"
# LimeSurvey Field type: F
data[, 25] <- as.numeric(data[, 25])
# attributes(data)$variable.labels[25] <- "[Inserm] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 25] <- factor(data[, 25], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[25] <- "tutelles_Inserm"
# LimeSurvey Field type: F
data[, 26] <- as.numeric(data[, 26])
# attributes(data)$variable.labels[26] <- "[Inrae] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 26] <- factor(data[, 26], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[26] <- "tutelles_Inrae"
# LimeSurvey Field type: F
data[, 27] <- as.numeric(data[, 27])
# attributes(data)$variable.labels[27] <- "[Inria] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 27] <- factor(data[, 27], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[27] <- "tutelles_Inria"
# LimeSurvey Field type: F
data[, 28] <- as.numeric(data[, 28])
# attributes(data)$variable.labels[28] <- "[IRD] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 28] <- factor(data[, 28], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[28] <- "tutelles_IRD"
# LimeSurvey Field type: F
data[, 29] <- as.numeric(data[, 29])
# attributes(data)$variable.labels[29] <- "[Ined] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 29] <- factor(data[, 29], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[29] <- "tutelles_Ined"
# LimeSurvey Field type: F
data[, 30] <- as.numeric(data[, 30])
# attributes(data)$variable.labels[30] <- "[CEA] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 30] <- factor(data[, 30], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[30] <- "tutelles_CEA"
# LimeSurvey Field type: F
data[, 31] <- as.numeric(data[, 31])
# attributes(data)$variable.labels[31] <- "[CNES] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 31] <- factor(data[, 31], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[31] <- "tutelles_CNES"
# LimeSurvey Field type: F
data[, 32] <- as.numeric(data[, 32])
# attributes(data)$variable.labels[32] <- "[ONERA] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 32] <- factor(data[, 32], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[32] <- "tutelles_ONERA"
# LimeSurvey Field type: F
data[, 33] <- as.numeric(data[, 33])
# attributes(data)$variable.labels[33] <- "[Cirad] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 33] <- factor(data[, 33], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[33] <- "tutelles_Cirad"
# LimeSurvey Field type: F
data[, 34] <- as.numeric(data[, 34])
# attributes(data)$variable.labels[34] <- "[Ifremer] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 34] <- factor(data[, 34], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[34] <- "tutelles_Ifremer"
# LimeSurvey Field type: F
data[, 35] <- as.numeric(data[, 35])
# attributes(data)$variable.labels[35] <- "[Une autre institution publique] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 35] <- factor(data[, 35], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[35] <- "tutelles_Pub"
# LimeSurvey Field type: F
data[, 36] <- as.numeric(data[, 36])
# attributes(data)$variable.labels[36] <- "[Une entreprise] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
data[, 36] <- factor(data[, 36], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[36] <- "tutelles_Entr"
# LimeSurvey Field type: A
data[, 37] <- as.character(data[, 37])
# attributes(data)$variable.labels[37] <- "[Autre] En juin 2020, quelles étaient les tutelles de votre laboratoire principal de rattachement (ou de l'unité ou l'équipe dans laquelle vous travailliez) ?"
# names(data)[37] <- "tutelles_other"
# LimeSurvey Field type: A
data[, 38] <- as.character(data[, 38])
# attributes(data)$variable.labels[38] <- "Pensez-vous que le climat de la planète est en train de changer (hausse des températures depuis une centaine d'années) ?"
data[, 38] <- factor(data[, 38], levels=c("OuiC","OuiP","NonP","NonC","SO"),labels=c("Oui, certainement", "Oui, probablement", "Non, probablement pas", "Non, certainement pas", "Sans opinion"))
# names(data)[38] <- "changclim"
# LimeSurvey Field type: A
data[, 39] <- as.character(data[, 39])
# attributes(data)$variable.labels[39] <- "D'après vous, les activités humaines sont-elles la cause de ce changement climatique ?"
data[, 39] <- factor(data[, 39], levels=c("A2","A3","A4","A5","A6"),labels=c("Non, elles ne jouent aucun rôle", "Oui, elles jouent un petit rôle", "Oui, elles jouent un grand rôle", "Oui, elles en sont l'unique cause", "Sans opinion"))
# names(data)[39] <- "acthum"
# LimeSurvey Field type: A
data[, 40] <- as.character(data[, 40])
# attributes(data)$variable.labels[40] <- "Dans quelle mesure êtes-vous préoccupé·e par le changement climatique ?"
data[, 40] <- factor(data[, 40], levels=c("A2","A3","A4","A5","A6","A7"),labels=c("Pas du tout préoccupé·e", "Un peu préoccupé·e", "Assez préoccupé·e", "Très préoccupé·e", "Extrêmement préoccupé·e", "Sans opinion"))
# names(data)[40] <- "preoccupe"
# LimeSurvey Field type: A
data[, 41] <- as.character(data[, 41])
# attributes(data)$variable.labels[41] <- "Etes-vous plus ou moins préoccupé·e qu'il y a 5 ans ?"
data[, 41] <- factor(data[, 41], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Beaucoup plus", "Un peu plus", "Ni plus ni moins", "Un peu moins", "Beaucoup moins", "Sans opinion"))
# names(data)[41] <- "pluspreoccupe"
# LimeSurvey Field type: A
data[, 42] <- as.character(data[, 42])
# attributes(data)$variable.labels[42] <- "Quel que soit votre domaine d'activité, participez-vous à des recherches ayant un lien avec l'écologie, l'environnement ou le climat ?"
data[, 42] <- factor(data[, 42], levels=c("A1","A2","A3"),labels=c("Oui", "Non, mais je l'ai fait par le passé", "Non"))
# names(data)[42] <- "recheco"
# LimeSurvey Field type: A
data[, 43] <- as.character(data[, 43])
# attributes(data)$variable.labels[43] <- "[Réorienté vos recherches vers des thématiques plus en lien avec l'écologie, l'environnement ou le climat] Avez-vous déjà :"
data[, 43] <- factor(data[, 43], levels=c("A2","A3","A4"),labels=c("Oui, je l'ai déjà fait", "J'y ai pensé", "Non"))
# names(data)[43] <- "themrech_reoriente"
# LimeSurvey Field type: A
data[, 44] <- as.character(data[, 44])
# attributes(data)$variable.labels[44] <- "[Renoncé à des programmes/thématiques de recherche en raison de leur impact négatif sur l'environnement] Avez-vous déjà :"
data[, 44] <- factor(data[, 44], levels=c("A2","A3","A4"),labels=c("Oui, je l'ai déjà fait", "J'y ai pensé", "Non"))
# names(data)[44] <- "themrech_renonce"
# LimeSurvey Field type: A
data[, 45] <- as.character(data[, 45])
# attributes(data)$variable.labels[45] <- "Pensez-vous que l\'urgence climatique exige des changements profonds dans la pratique de nos métiers ?"
data[, 45] <- factor(data[, 45], levels=c("A2","A3","A4","A5","A6"),labels=c("Oui, tout à fait d'accord", "Oui, plutôt d'accord", "Non, plutôt pas d'accord", "Non, pas du tout d'accord", "Sans opinion"))
# names(data)[45] <- "chgtpratique"
# LimeSurvey Field type: A
data[, 46] <- as.character(data[, 46])
# attributes(data)$variable.labels[46] <- "[Vous pouvez trier vos déchets papier] Dans votre laboratoire (ou dans l'unité ou l'équipe dans laquelle vous travaillez) :"
data[, 46] <- factor(data[, 46], levels=c("A2","A3","A4","A5","A6"),labels=c("Je ne sais pas", "Oui", "Non", "Non, mais la question a été abordée", "Non concerné·e"))
# names(data)[46] <- "labopratiques_tri"
# LimeSurvey Field type: A
data[, 47] <- as.character(data[, 47])
# attributes(data)$variable.labels[47] <- "[Vous êtes encouragé·es à prendre le train plutôt que l'avion même si c'est plus cher ou plus long ] Dans votre laboratoire (ou dans l'unité ou l'équipe dans laquelle vous travaillez) :"
data[, 47] <- factor(data[, 47], levels=c("A2","A3","A4","A5","A6"),labels=c("Je ne sais pas", "Oui", "Non", "Non, mais la question a été abordée", "Non concerné·e"))
# names(data)[47] <- "labopratiques_train"
# LimeSurvey Field type: A
data[, 48] <- as.character(data[, 48])
# attributes(data)$variable.labels[48] <- "[Il existe une charte, un groupe de travail ou un·e responsable désigné·e pour réduire l'empreinte écologique des activités de recherche] Dans votre laboratoire (ou dans l'unité ou l'équipe dans laquelle vous travaillez) :"
data[, 48] <- factor(data[, 48], levels=c("A2","A3","A4","A5","A6"),labels=c("Je ne sais pas", "Oui", "Non", "Non, mais la question a été abordée", "Non concerné·e"))
# names(data)[48] <- "labopratiques_charte"
# LimeSurvey Field type: A
data[, 49] <- as.character(data[, 49])
# attributes(data)$variable.labels[49] <- "[Limiter l'impact environnemental des pots et buffets (réduire les déchets plastiques, proposer des menus bio, locaux ou végétariens…)] Dans votre laboratoire (ou dans l'unité ou l'équipe dans laquelle vous travaillez), des mesures collectives ont été mises en œuvre pour :"
data[, 49] <- factor(data[, 49], levels=c("A2","A3","A4","A5","A6"),labels=c("Je ne sais pas", "Oui", "Non", "Non, mais la question a été abordée", "Non concerné·e"))
# names(data)[49] <- "labomesures_pots"
# LimeSurvey Field type: A
data[, 50] <- as.character(data[, 50])
# attributes(data)$variable.labels[50] <- "[Limiter la production de déchets des expériences/expérimentations] Dans votre laboratoire (ou dans l'unité ou l'équipe dans laquelle vous travaillez), des mesures collectives ont été mises en œuvre pour :"
data[, 50] <- factor(data[, 50], levels=c("A2","A3","A4","A5","A6"),labels=c("Je ne sais pas", "Oui", "Non", "Non, mais la question a été abordée", "Non concerné·e"))
# names(data)[50] <- "labomesures_dechets"
# LimeSurvey Field type: A
data[, 51] <- as.character(data[, 51])
# attributes(data)$variable.labels[51] <- "[Réduire les émissions de gaz à effet de serre liées aux missions d'observation ou de collecte de données] Dans votre laboratoire (ou dans l'unité ou l'équipe dans laquelle vous travaillez), des mesures collectives ont été mises en œuvre pour :"
data[, 51] <- factor(data[, 51], levels=c("A2","A3","A4","A5","A6"),labels=c("Je ne sais pas", "Oui", "Non", "Non, mais la question a été abordée", "Non concerné·e"))
# names(data)[51] <- "labomesures_donnees"
# LimeSurvey Field type: A
data[, 52] <- as.character(data[, 52])
# attributes(data)$variable.labels[52] <- "[Réduire la consommation électrique des serveurs (stockage, calcul…)] Dans votre laboratoire (ou dans l'unité ou l'équipe dans laquelle vous travaillez), des mesures collectives ont été mises en œuvre pour :"
data[, 52] <- factor(data[, 52], levels=c("A2","A3","A4","A5","A6"),labels=c("Je ne sais pas", "Oui", "Non", "Non, mais la question a été abordée", "Non concerné·e"))
# names(data)[52] <- "labomesures_serveurs"
# LimeSurvey Field type: A
data[, 53] <- as.character(data[, 53])
# attributes(data)$variable.labels[53] <- "[Éteignez-vous ou mettez-vous en veille l\'ordinateur que vous utilisez au travail quand vous quittez le bureau le soir ?] À titre individuel, quand c'est techniquement possible :"
data[, 53] <- factor(data[, 53], levels=c("A2","A3","A4","A5"),labels=c("Toujours ou souvent", "Parfois", "Rarement ou jamais", "Je n'ai pas le contrôle/non concerné·e"))
# names(data)[53] <- "individupratiques_ordi"
# LimeSurvey Field type: A
data[, 54] <- as.character(data[, 54])
# attributes(data)$variable.labels[54] <- "[Durant l'hiver, baissez-vous le chauffage de votre bureau hors de vos horaires de travail ?] À titre individuel, quand c'est techniquement possible :"
data[, 54] <- factor(data[, 54], levels=c("A2","A3","A4","A5"),labels=c("Toujours ou souvent", "Parfois", "Rarement ou jamais", "Je n'ai pas le contrôle/non concerné·e"))
# names(data)[54] <- "individupratiques_chauffage"
# LimeSurvey Field type: A
data[, 55] <- as.character(data[, 55])
# attributes(data)$variable.labels[55] <- "[Limitez-vous l'envoi de pièces jointes volumineuses dans vos courriels ?] À titre individuel, quand c'est techniquement possible :"
data[, 55] <- factor(data[, 55], levels=c("A2","A3","A4","A5"),labels=c("Toujours ou souvent", "Parfois", "Rarement ou jamais", "Je n'ai pas le contrôle/non concerné·e"))
# names(data)[55] <- "individupratiques_piecesjointes"
# LimeSurvey Field type: A
data[, 56] <- as.character(data[, 56])
# attributes(data)$variable.labels[56] <- "[Imprimez-vous en recto-verso ?] À titre individuel, quand c'est techniquement possible :"
data[, 56] <- factor(data[, 56], levels=c("A2","A3","A4","A5"),labels=c("Toujours ou souvent", "Parfois", "Rarement ou jamais", "Je n'ai pas le contrôle/non concerné·e"))
# names(data)[56] <- "individupratiques_rectoverso"
# LimeSurvey Field type: F
data[, 57] <- as.numeric(data[, 57])
# attributes(data)$variable.labels[57] <- "[Ordinateurs partagés au laboratoire] [Nombre] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[57] <- "ordis_partage_nb"
# LimeSurvey Field type: F
data[, 58] <- as.numeric(data[, 58])
# attributes(data)$variable.labels[58] <- "[Ordinateurs partagés au laboratoire] [Combien ont moins de 5 ans ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[58] <- "ordis_partage_5ans"
# LimeSurvey Field type: F
data[, 59] <- as.numeric(data[, 59])
# attributes(data)$variable.labels[59] <- "[Ordinateurs partagés au laboratoire] [Combien vous semblent indispensables ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[59] <- "ordis_partage_indisp"
# LimeSurvey Field type: F
data[, 60] <- as.numeric(data[, 60])
# attributes(data)$variable.labels[60] <- "[Ordinateurs fixes individuels achetés avec des financements professionnels  ] [Nombre] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[60] <- "ordis_fixepro_nb"
# LimeSurvey Field type: F
data[, 61] <- as.numeric(data[, 61])
# attributes(data)$variable.labels[61] <- "[Ordinateurs fixes individuels achetés avec des financements professionnels  ] [Combien ont moins de 5 ans ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[61] <- "ordis_fixepro_5ans"
# LimeSurvey Field type: F
data[, 62] <- as.numeric(data[, 62])
# attributes(data)$variable.labels[62] <- "[Ordinateurs fixes individuels achetés avec des financements professionnels  ] [Combien vous semblent indispensables ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[62] <- "ordis_fixepro_indisp"
# LimeSurvey Field type: F
data[, 63] <- as.numeric(data[, 63])
# attributes(data)$variable.labels[63] <- "[Ordinateurs fixes individuels achetés par vous] [Nombre] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[63] <- "ordis_fixeperso_nb"
# LimeSurvey Field type: F
data[, 64] <- as.numeric(data[, 64])
# attributes(data)$variable.labels[64] <- "[Ordinateurs fixes individuels achetés par vous] [Combien ont moins de 5 ans ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[64] <- "ordis_fixeperso_5ans"
# LimeSurvey Field type: F
data[, 65] <- as.numeric(data[, 65])
# attributes(data)$variable.labels[65] <- "[Ordinateurs fixes individuels achetés par vous] [Combien vous semblent indispensables ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[65] <- "ordis_fixeperso_indisp"
# LimeSurvey Field type: F
data[, 66] <- as.numeric(data[, 66])
# attributes(data)$variable.labels[66] <- "[Ordinateurs portables individuels achetés avec des financements professionnels  ] [Nombre] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[66] <- "ordis_portablepro_nb"
# LimeSurvey Field type: F
data[, 67] <- as.numeric(data[, 67])
# attributes(data)$variable.labels[67] <- "[Ordinateurs portables individuels achetés avec des financements professionnels  ] [Combien ont moins de 5 ans ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[67] <- "ordis_portablepro_5ans"
# LimeSurvey Field type: F
data[, 68] <- as.numeric(data[, 68])
# attributes(data)$variable.labels[68] <- "[Ordinateurs portables individuels achetés avec des financements professionnels  ] [Combien vous semblent indispensables ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[68] <- "ordis_portablepro_indisp"
# LimeSurvey Field type: F
data[, 69] <- as.numeric(data[, 69])
# attributes(data)$variable.labels[69] <- "[Ordinateurs portables individuels achetés par vous] [Nombre] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[69] <- "ordis_portableperso_nb"
# LimeSurvey Field type: F
data[, 70] <- as.numeric(data[, 70])
# attributes(data)$variable.labels[70] <- "[Ordinateurs portables individuels achetés par vous] [Combien ont moins de 5 ans ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[70] <- "ordis_portableperso_5ans"
# LimeSurvey Field type: F
data[, 71] <- as.numeric(data[, 71])
# attributes(data)$variable.labels[71] <- "[Ordinateurs portables individuels achetés par vous] [Combien vous semblent indispensables ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[71] <- "ordis_portableperso_indisp"
# LimeSurvey Field type: F
data[, 72] <- as.numeric(data[, 72])
# attributes(data)$variable.labels[72] <- "[Tablettes achetées avec des financements professionnels] [Nombre] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[72] <- "ordis_tablettepro_nb"
# LimeSurvey Field type: F
data[, 73] <- as.numeric(data[, 73])
# attributes(data)$variable.labels[73] <- "[Tablettes achetées avec des financements professionnels] [Combien ont moins de 5 ans ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[73] <- "ordis_tablettepro_5ans"
# LimeSurvey Field type: F
data[, 74] <- as.numeric(data[, 74])
# attributes(data)$variable.labels[74] <- "[Tablettes achetées avec des financements professionnels] [Combien vous semblent indispensables ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[74] <- "ordis_tablettepro_indisp"
# LimeSurvey Field type: F
data[, 75] <- as.numeric(data[, 75])
# attributes(data)$variable.labels[75] <- "[Tablettes achetées par vous] [Nombre] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[75] <- "ordis_tabletteperso_nb"
# LimeSurvey Field type: F
data[, 76] <- as.numeric(data[, 76])
# attributes(data)$variable.labels[76] <- "[Tablettes achetées par vous] [Combien ont moins de 5 ans ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[76] <- "ordis_tabletteperso_5ans"
# LimeSurvey Field type: F
data[, 77] <- as.numeric(data[, 77])
# attributes(data)$variable.labels[77] <- "[Tablettes achetées par vous] [Combien vous semblent indispensables ?] Pour votre travail, vous utilisez (laissez vides les cases qui ne vous concernent pas ou qui seraient à 0) :"
# names(data)[77] <- "ordis_tabletteperso_indisp"
# LimeSurvey Field type: F
data[, 78] <- as.numeric(data[, 78])
# attributes(data)$variable.labels[78] <- "[Je n'ai jamais changé mon ordinateur à usage professionnel] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 78] <- factor(data[, 78], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[78] <- "chgtordi_jamais"
# LimeSurvey Field type: F
data[, 79] <- as.numeric(data[, 79])
# attributes(data)$variable.labels[79] <- "[J'ai eu un nouveau contrat de travail] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 79] <- factor(data[, 79], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[79] <- "chgtordi_contrat"
# LimeSurvey Field type: F
data[, 80] <- as.numeric(data[, 80])
# attributes(data)$variable.labels[80] <- "[On me l'a volé, je l'ai perdu] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 80] <- factor(data[, 80], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[80] <- "chgtordi_vole"
# LimeSurvey Field type: F
data[, 81] <- as.numeric(data[, 81])
# attributes(data)$variable.labels[81] <- "[Il ne fonctionnait plus du tout] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 81] <- factor(data[, 81], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[81] <- "chgtordi_fctplus"
# LimeSurvey Field type: F
data[, 82] <- as.numeric(data[, 82])
# attributes(data)$variable.labels[82] <- "[Il ne fonctionnait plus très bien] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 82] <- factor(data[, 82], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[82] <- "chgtordi_fctplusbien"
# LimeSurvey Field type: F
data[, 83] <- as.numeric(data[, 83])
# attributes(data)$variable.labels[83] <- "[Il n'était pas assez puissant] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 83] <- factor(data[, 83], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[83] <- "chgtordi_puissant"
# LimeSurvey Field type: F
data[, 84] <- as.numeric(data[, 84])
# attributes(data)$variable.labels[84] <- "[Il ne pouvait plus être mis à jour] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 84] <- factor(data[, 84], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[84] <- "chgtordi_misajour"
# LimeSurvey Field type: F
data[, 85] <- as.numeric(data[, 85])
# attributes(data)$variable.labels[85] <- "[J'en voulais un neuf] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 85] <- factor(data[, 85], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[85] <- "chgtordi_neuf"
# LimeSurvey Field type: F
data[, 86] <- as.numeric(data[, 86])
# attributes(data)$variable.labels[86] <- "[J'en voulais un mieux (meilleure autonomie, écran, etc.)] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 86] <- factor(data[, 86], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[86] <- "chgtordi_mieux"
# LimeSurvey Field type: F
data[, 87] <- as.numeric(data[, 87])
# attributes(data)$variable.labels[87] <- "[On me l'a proposé/j'en avais l'opportunité] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 87] <- factor(data[, 87], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[87] <- "chgtordi_propose"
# LimeSurvey Field type: F
data[, 88] <- as.numeric(data[, 88])
# attributes(data)$variable.labels[88] <- "[On m'y a obligé] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 88] <- factor(data[, 88], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[88] <- "chgtordi_oblige"
# LimeSurvey Field type: F
data[, 89] <- as.numeric(data[, 89])
# attributes(data)$variable.labels[89] <- "[Autre] La dernière fois que vous avez changé d'ordinateur à usage professionnel, pour quelles raisons l'avez-vous fait ?"
data[, 89] <- factor(data[, 89], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[89] <- "chgtordi_autre"
# LimeSurvey Field type: A
data[, 90] <- as.character(data[, 90])
# attributes(data)$variable.labels[90] <- "[Financement de déplacements en avion] Au cours des 5 dernières années, vous est-il arrivé, pour terminer un reliquat de budget, de réaliser les dépenses suivantes alors qu'elles n'étaient pas vraiment indispensables (pour vous ou d'autres personnes) ?"
data[, 90] <- factor(data[, 90], levels=c("A2","A3","A4"),labels=c("Oui", "Non", "Je n'ai pas eu à utiliser de reliquat de budget"))
# names(data)[90] <- "reliquat_avion"
# LimeSurvey Field type: A
data[, 91] <- as.character(data[, 91])
# attributes(data)$variable.labels[91] <- "[Achat d'ordinateurs, écrans ou tablettes] Au cours des 5 dernières années, vous est-il arrivé, pour terminer un reliquat de budget, de réaliser les dépenses suivantes alors qu'elles n'étaient pas vraiment indispensables (pour vous ou d'autres personnes) ?"
data[, 91] <- factor(data[, 91], levels=c("A2","A3","A4"),labels=c("Oui", "Non", "Je n'ai pas eu à utiliser de reliquat de budget"))
# names(data)[91] <- "reliquat_ordi"
# LimeSurvey Field type: F
data[, 92] <- as.numeric(data[, 92])
# attributes(data)$variable.labels[92] <- "[De très grandes infrastructures de recherche mutualisées, nécessitant beaucoup d'énergie (synchrotron, radiotélescope, navire océanographique, aéronef, etc.)] Utilisez-vous régulièrement les équipements suivants pour réaliser vos expériences et observations scientifiques ?"
data[, 92] <- factor(data[, 92], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[92] <- "materiel_tgir"
# LimeSurvey Field type: F
data[, 93] <- as.numeric(data[, 93])
# attributes(data)$variable.labels[93] <- "[De grands dispositifs informatiques (supercalculateur ou data center)] Utilisez-vous régulièrement les équipements suivants pour réaliser vos expériences et observations scientifiques ?"
data[, 93] <- factor(data[, 93], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[93] <- "materiel_info"
# LimeSurvey Field type: F
data[, 94] <- as.numeric(data[, 94])
# attributes(data)$variable.labels[94] <- "[Des dispositifs expérimentaux extensifs hors laboratoire (champs, serres, fermes, etc.)] Utilisez-vous régulièrement les équipements suivants pour réaliser vos expériences et observations scientifiques ?"
data[, 94] <- factor(data[, 94], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[94] <- "materiel_extensif"
# LimeSurvey Field type: F
data[, 95] <- as.numeric(data[, 95])
# attributes(data)$variable.labels[95] <- "[De grands équipements très coûteux (IRM, microscope à balayage, peigne de fréquences optiques, animalerie, etc.)] Utilisez-vous régulièrement les équipements suivants pour réaliser vos expériences et observations scientifiques ?"
data[, 95] <- factor(data[, 95], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[95] <- "materiel_trescouteux"
# LimeSurvey Field type: F
data[, 96] <- as.numeric(data[, 96])
# attributes(data)$variable.labels[96] <- "[Des équipements coûteux (oscilloscope, centrifugeuse, hotte, congélateur, etc.)] Utilisez-vous régulièrement les équipements suivants pour réaliser vos expériences et observations scientifiques ?"
data[, 96] <- factor(data[, 96], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[96] <- "materiel_couteux"
# LimeSurvey Field type: F
data[, 97] <- as.numeric(data[, 97])
# attributes(data)$variable.labels[97] <- "[Du petit matériel de laboratoire (verrerie, petit électronique, etc.)] Utilisez-vous régulièrement les équipements suivants pour réaliser vos expériences et observations scientifiques ?"
data[, 97] <- factor(data[, 97], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[97] <- "materiel_petit"
# LimeSurvey Field type: F
data[, 98] <- as.numeric(data[, 98])
# attributes(data)$variable.labels[98] <- "[Aucun de ces équipements] Utilisez-vous régulièrement les équipements suivants pour réaliser vos expériences et observations scientifiques ?"
data[, 98] <- factor(data[, 98], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[98] <- "materiel_aucun"
# LimeSurvey Field type: A
data[, 99] <- as.character(data[, 99])
# attributes(data)$variable.labels[99] <- "[Les vols en avion pour les conférences, réunions, congrès] Au cours des 5 dernières années, sans tenir compte de la période depuis le premier confinement, diriez-vous que, dans les domaines suivants, vos émissions de gaz à effet de serre ont..."
data[, 99] <- factor(data[, 99], levels=c("A1","A2","A3","A4","A5","A6","A7"),labels=c("Fortement augmenté", "Un peu augmenté", "Été à peu près stables", "Un peu diminué", "Fortement diminué", "Non concerné·e", "Je ne sais pas"))
# names(data)[99] <- "solevolges_conf"
# LimeSurvey Field type: A
data[, 100] <- as.character(data[, 100])
# attributes(data)$variable.labels[100] <- "[Les déplacements pour missions de terrain, d'observation ou de collecte de données (en avion, voiture ou bateau)] Au cours des 5 dernières années, sans tenir compte de la période depuis le premier confinement, diriez-vous que, dans les domaines suivants, vos émissions de gaz à effet de serre ont..."
data[, 100] <- factor(data[, 100], levels=c("A1","A2","A3","A4","A5","A6","A7"),labels=c("Fortement augmenté", "Un peu augmenté", "Été à peu près stables", "Un peu diminué", "Fortement diminué", "Non concerné·e", "Je ne sais pas"))
# names(data)[100] <- "solevolges_donnees"
# LimeSurvey Field type: A
data[, 101] <- as.character(data[, 101])
# attributes(data)$variable.labels[101] <- "[Les expériences et observations scientifiques (hors déplacements)] Au cours des 5 dernières années, sans tenir compte de la période depuis le premier confinement, diriez-vous que, dans les domaines suivants, vos émissions de gaz à effet de serre ont..."
data[, 101] <- factor(data[, 101], levels=c("A1","A2","A3","A4","A5","A6","A7"),labels=c("Fortement augmenté", "Un peu augmenté", "Été à peu près stables", "Un peu diminué", "Fortement diminué", "Non concerné·e", "Je ne sais pas"))
# names(data)[101] <- "solevolges_expe"
# LimeSurvey Field type: A
data[, 102] <- as.character(data[, 102])
# attributes(data)$variable.labels[102] <- "[L'équipement en postes informatiques et sa fréquence de renouvellement] Au cours des 5 dernières années, sans tenir compte de la période depuis le premier confinement, diriez-vous que, dans les domaines suivants, vos émissions de gaz à effet de serre ont..."
data[, 102] <- factor(data[, 102], levels=c("A1","A2","A3","A4","A5","A6","A7"),labels=c("Fortement augmenté", "Un peu augmenté", "Été à peu près stables", "Un peu diminué", "Fortement diminué", "Non concerné·e", "Je ne sais pas"))
# names(data)[102] <- "solevolges_info"
# LimeSurvey Field type: A
data[, 103] <- as.character(data[, 103])
# attributes(data)$variable.labels[103] <- "[Les déplacements domicile-travail en voiture, moto, scooter ou avion] Au cours des 5 dernières années, sans tenir compte de la période depuis le premier confinement, diriez-vous que, dans les domaines suivants, vos émissions de gaz à effet de serre ont..."
data[, 103] <- factor(data[, 103], levels=c("A1","A2","A3","A4","A5","A6","A7"),labels=c("Fortement augmenté", "Un peu augmenté", "Été à peu près stables", "Un peu diminué", "Fortement diminué", "Non concerné·e", "Je ne sais pas"))
# names(data)[103] <- "solevolges_domicile"
# LimeSurvey Field type: F
data[, 104] <- as.numeric(data[, 104])
# attributes(data)$variable.labels[104] <- "Pour l'année 2019 (du 1er janvier au 31 décembre), combien de vols aller-retour en avion avez-vous réalisés environ dans le cadre professionnel hors trajets domicile-travail ? Pour un aller simple, indiquez 0,5 allers-retours."
# names(data)[104] <- "volsnb"
# LimeSurvey Field type: A
data[, 105] <- as.character(data[, 105])
# attributes(data)$variable.labels[105] <- "Pour environ combien d'heures de vol au total ?"
data[, 105] <- factor(data[, 105], levels=c("A3","A4","A5","A6"),labels=c("De 1h à 10h", "De 11h à 20h", "De 20h à 50h", "Plus de 50h"))
# names(data)[105] <- "volsh"
# LimeSurvey Field type: A
data[, 106] <- as.character(data[, 106])
# attributes(data)$variable.labels[106] <- "Au cours des deux années précédentes (2017 et 2018), avec-vous pris au moins une fois l'avion dans le cadre professionnel hors trajets domicile-travail ?"
data[, 106] <- factor(data[, 106], levels=c("A2","A3","A4"),labels=c("Oui, pour plus de 10h au total", "Oui, pour moins de 10h au total", "Non"))
# names(data)[106] <- "vols2ans"
# LimeSurvey Field type: A
data[, 107] <- as.character(data[, 107])
# attributes(data)$variable.labels[107] <- "Si vous avez pris l\'avion en 2019 (sinon, passez à la question suivante), merci de décrire dans le tableau suivant vos 5 premiers vols aller-retour en avion de l'année 2019, en regroupant ceux faits plusieurs fois pour des motifs proches. Indiquez les noms français des villes (ex : Londres plutôt que London), en utilisant quand c'est possible les propositions qui s'affichent (ces noms nous serviront à calculer la distance du vol). Pour un aller simple, indiquez 0,5 allers-retours."
# names(data)[107] <- "volstexte"
# LimeSurvey Field type: A
data[, 108] <- as.character(data[, 108])
# attributes(data)$variable.labels[108] <- ""
# names(data)[108] <- "volstextevide"
# LimeSurvey Field type: A
data[, 109] <- as.character(data[, 109])
# attributes(data)$variable.labels[109] <- "Ville de départ(décollage)"
# names(data)[109] <- "volstextedepart"
# LimeSurvey Field type: A
data[, 110] <- as.character(data[, 110])
# attributes(data)$variable.labels[110] <- "Ville d\'arrivée(atterrissage final)"
# names(data)[110] <- "volstextearrivee"
# LimeSurvey Field type: A
data[, 111] <- as.character(data[, 111])
# attributes(data)$variable.labels[111] <- "Nombre de jourstravaillés sur place"
# names(data)[111] <- "volstextejours"
# LimeSurvey Field type: A
data[, 112] <- as.character(data[, 112])
# attributes(data)$variable.labels[112] <- "Motif principaldu déplacement"
# names(data)[112] <- "volstextemotif"
# LimeSurvey Field type: A
data[, 113] <- as.character(data[, 113])
# attributes(data)$variable.labels[113] <- "Motif secondairedu déplacement(éventuellement)"
# names(data)[113] <- "volstextemotifsec"
# LimeSurvey Field type: A
data[, 114] <- as.character(data[, 114])
# attributes(data)$variable.labels[114] <- "Nombre d\'allers-retourseffectués"
# names(data)[114] <- "volstextenb"
# LimeSurvey Field type: A
data[, 115] <- as.character(data[, 115])
# attributes(data)$variable.labels[115] <- "Trajet 1"
# names(data)[115] <- "volstitre1"
# LimeSurvey Field type: A
data[, 116] <- as.character(data[, 116])
# attributes(data)$variable.labels[116] <- ""
# names(data)[116] <- "volsdepart1"
# LimeSurvey Field type: A
data[, 117] <- as.character(data[, 117])
# attributes(data)$variable.labels[117] <- ""
# names(data)[117] <- "volsarrivee1"
# LimeSurvey Field type: A
data[, 118] <- as.character(data[, 118])
# attributes(data)$variable.labels[118] <- ""
data[, 118] <- factor(data[, 118], levels=c("A2","A3","A4","A5"),labels=c("Moins de deux jours", "De deux jours à une semaine", "De plus d'une semaine à un mois", "Plus d'un mois"))
# names(data)[118] <- "volsjours1"
# LimeSurvey Field type: A
data[, 119] <- as.character(data[, 119])
# attributes(data)$variable.labels[119] <- ""
data[, 119] <- factor(data[, 119], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9"),labels=c("Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[119] <- "volsmotif1"
# LimeSurvey Field type: A
data[, 120] <- as.character(data[, 120])
# attributes(data)$variable.labels[120] <- ""
data[, 120] <- factor(data[, 120], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("Aucun", "Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[120] <- "volsmotifsec1"
# LimeSurvey Field type: F
data[, 121] <- as.numeric(data[, 121])
# attributes(data)$variable.labels[121] <- ""
# names(data)[121] <- "volsnb1"
# LimeSurvey Field type: A
data[, 122] <- as.character(data[, 122])
# attributes(data)$variable.labels[122] <- "Trajet 2"
# names(data)[122] <- "volstexte2"
# LimeSurvey Field type: A
data[, 123] <- as.character(data[, 123])
# attributes(data)$variable.labels[123] <- ""
# names(data)[123] <- "volsdepart2"
# LimeSurvey Field type: A
data[, 124] <- as.character(data[, 124])
# attributes(data)$variable.labels[124] <- ""
# names(data)[124] <- "volsarrivee2"
# LimeSurvey Field type: A
data[, 125] <- as.character(data[, 125])
# attributes(data)$variable.labels[125] <- ""
data[, 125] <- factor(data[, 125], levels=c("A2","A3","A4","A5"),labels=c("Moins de deux jours", "De deux jours à une semaine", "De plus d'une semaine à un mois", "Plus d'un mois"))
# names(data)[125] <- "volsjours2"
# LimeSurvey Field type: A
data[, 126] <- as.character(data[, 126])
# attributes(data)$variable.labels[126] <- ""
data[, 126] <- factor(data[, 126], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9"),labels=c("Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[126] <- "volsmotif2"
# LimeSurvey Field type: A
data[, 127] <- as.character(data[, 127])
# attributes(data)$variable.labels[127] <- ""
data[, 127] <- factor(data[, 127], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("Aucun", "Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[127] <- "volsmotifsec2"
# LimeSurvey Field type: F
data[, 128] <- as.numeric(data[, 128])
# attributes(data)$variable.labels[128] <- ""
# names(data)[128] <- "volsnb2"
# LimeSurvey Field type: A
data[, 129] <- as.character(data[, 129])
# attributes(data)$variable.labels[129] <- "Trajet 3"
# names(data)[129] <- "volstexte3"
# LimeSurvey Field type: A
data[, 130] <- as.character(data[, 130])
# attributes(data)$variable.labels[130] <- ""
# names(data)[130] <- "volsdepart3"
# LimeSurvey Field type: A
data[, 131] <- as.character(data[, 131])
# attributes(data)$variable.labels[131] <- ""
# names(data)[131] <- "volsarrivee3"
# LimeSurvey Field type: A
data[, 132] <- as.character(data[, 132])
# attributes(data)$variable.labels[132] <- ""
data[, 132] <- factor(data[, 132], levels=c("A2","A3","A4","A5"),labels=c("Moins de deux jours", "De deux jours à une semaine", "De plus d'une semaine à un mois", "Plus d'un mois"))
# names(data)[132] <- "volsjours3"
# LimeSurvey Field type: A
data[, 133] <- as.character(data[, 133])
# attributes(data)$variable.labels[133] <- ""
data[, 133] <- factor(data[, 133], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9"),labels=c("Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[133] <- "volsmotif3"
# LimeSurvey Field type: A
data[, 134] <- as.character(data[, 134])
# attributes(data)$variable.labels[134] <- ""
data[, 134] <- factor(data[, 134], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("Aucun", "Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[134] <- "volsmotifsec3"
# LimeSurvey Field type: F
data[, 135] <- as.numeric(data[, 135])
# attributes(data)$variable.labels[135] <- ""
# names(data)[135] <- "volsnb3"
# LimeSurvey Field type: A
data[, 136] <- as.character(data[, 136])
# attributes(data)$variable.labels[136] <- "Trajet 4"
# names(data)[136] <- "volstexte4"
# LimeSurvey Field type: A
data[, 137] <- as.character(data[, 137])
# attributes(data)$variable.labels[137] <- ""
# names(data)[137] <- "volsdepart4"
# LimeSurvey Field type: A
data[, 138] <- as.character(data[, 138])
# attributes(data)$variable.labels[138] <- ""
# names(data)[138] <- "volsarrivee4"
# LimeSurvey Field type: A
data[, 139] <- as.character(data[, 139])
# attributes(data)$variable.labels[139] <- ""
data[, 139] <- factor(data[, 139], levels=c("A2","A3","A4","A5"),labels=c("Moins de deux jours", "De deux jours à une semaine", "De plus d'une semaine à un mois", "Plus d'un mois"))
# names(data)[139] <- "volsjours4"
# LimeSurvey Field type: A
data[, 140] <- as.character(data[, 140])
# attributes(data)$variable.labels[140] <- ""
data[, 140] <- factor(data[, 140], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9"),labels=c("Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[140] <- "volsmotif4"
# LimeSurvey Field type: A
data[, 141] <- as.character(data[, 141])
# attributes(data)$variable.labels[141] <- ""
data[, 141] <- factor(data[, 141], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("Aucun", "Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[141] <- "volsmotifsec4"
# LimeSurvey Field type: F
data[, 142] <- as.numeric(data[, 142])
# attributes(data)$variable.labels[142] <- ""
# names(data)[142] <- "volsnb4"
# LimeSurvey Field type: A
data[, 143] <- as.character(data[, 143])
# attributes(data)$variable.labels[143] <- "Trajet 5"
# names(data)[143] <- "volstexte5"
# LimeSurvey Field type: A
data[, 144] <- as.character(data[, 144])
# attributes(data)$variable.labels[144] <- ""
# names(data)[144] <- "volsdepart5"
# LimeSurvey Field type: A
data[, 145] <- as.character(data[, 145])
# attributes(data)$variable.labels[145] <- ""
# names(data)[145] <- "volsarrivee5"
# LimeSurvey Field type: A
data[, 146] <- as.character(data[, 146])
# attributes(data)$variable.labels[146] <- ""
data[, 146] <- factor(data[, 146], levels=c("A2","A3","A4","A5"),labels=c("Moins de deux jours", "De deux jours à une semaine", "De plus d'une semaine à un mois", "Plus d'un mois"))
# names(data)[146] <- "volsjours5"
# LimeSurvey Field type: A
data[, 147] <- as.character(data[, 147])
# attributes(data)$variable.labels[147] <- ""
data[, 147] <- factor(data[, 147], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9"),labels=c("Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[147] <- "volsmotif5"
# LimeSurvey Field type: A
data[, 148] <- as.character(data[, 148])
# attributes(data)$variable.labels[148] <- ""
data[, 148] <- factor(data[, 148], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("Aucun", "Conférence, présentation", "Séjour de recherche", "Réunion, workshop", "Enseignement, formation, école d'été", "Terrain, production et recueil de données", "Obtention de financements", "Évaluation de la recherche", "Jury", "Autre"))
# names(data)[148] <- "volsmotifsec5"
# LimeSurvey Field type: F
data[, 149] <- as.numeric(data[, 149])
# attributes(data)$variable.labels[149] <- ""
# names(data)[149] <- "volsnb5"
# LimeSurvey Field type: A
data[, 150] <- as.character(data[, 150])
# attributes(data)$variable.labels[150] <- "Parmi ces vols, quelle proportion avez-vous réalisée en classe premium, affaires ou business ?"
data[, 150] <- factor(data[, 150], levels=c("A2","A3","A4","A5"),labels=c("Zéro", "Moins d'un quart", "Entre un quart et la moitié", "Plus de la moitié"))
# names(data)[150] <- "volspremium"
# LimeSurvey Field type: A
data[, 151] <- as.character(data[, 151])
# attributes(data)$variable.labels[151] <- "En 2019, avez-vous pris l'avion pour un trajet professionnel qui prend moins de 6 heures en train (hors déplacements domicile-travail) ?"
data[, 151] <- factor(data[, 151], levels=c("Oui","Non","NSP"),labels=c("Oui", "Non", "Je ne sais pas"))
# names(data)[151] <- "aviontrain"
# LimeSurvey Field type: F
data[, 152] <- as.numeric(data[, 152])
# attributes(data)$variable.labels[152] <- "[L'avion est plus rapide] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
data[, 152] <- factor(data[, 152], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[152] <- "aviontrainraisons_rapide"
# LimeSurvey Field type: F
data[, 153] <- as.numeric(data[, 153])
# attributes(data)$variable.labels[153] <- "[L'avion est moins cher] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
data[, 153] <- factor(data[, 153], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[153] <- "aviontrainraisons_cher"
# LimeSurvey Field type: F
data[, 154] <- as.numeric(data[, 154])
# attributes(data)$variable.labels[154] <- "[L'avion est plus pratique] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
data[, 154] <- factor(data[, 154], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[154] <- "aviontrainraisons_pratique"
# LimeSurvey Field type: F
data[, 155] <- as.numeric(data[, 155])
# attributes(data)$variable.labels[155] <- "[J'aime prendre l'avion] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
data[, 155] <- factor(data[, 155], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[155] <- "aviontrainraisons_aime"
# LimeSurvey Field type: F
data[, 156] <- as.numeric(data[, 156])
# attributes(data)$variable.labels[156] <- "[L'avion permet d'éviter une nuit sur place] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
data[, 156] <- factor(data[, 156], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[156] <- "aviontrainraisons_nuit"
# LimeSurvey Field type: F
data[, 157] <- as.numeric(data[, 157])
# attributes(data)$variable.labels[157] <- "[Des règles administratives m'incitent à prendre l'avion] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
data[, 157] <- factor(data[, 157], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[157] <- "aviontrainraisons_admin"
# LimeSurvey Field type: F
data[, 158] <- as.numeric(data[, 158])
# attributes(data)$variable.labels[158] <- "[Cela me permet de cumuler des miles] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
data[, 158] <- factor(data[, 158], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[158] <- "aviontrainraisons_miles"
# LimeSurvey Field type: F
data[, 159] <- as.numeric(data[, 159])
# attributes(data)$variable.labels[159] <- "[Je ne me suis pas posé la question] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
data[, 159] <- factor(data[, 159], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[159] <- "aviontrainraisons_paspose"
# LimeSurvey Field type: A
data[, 160] <- as.character(data[, 160])
# attributes(data)$variable.labels[160] <- "[Autre] Pour quelles raisons principales prenez-vous l'avion dans ces cas-là ?"
# names(data)[160] <- "aviontrainraisons_other"
# LimeSurvey Field type: A
data[, 161] <- as.character(data[, 161])
# attributes(data)$variable.labels[161] <- "Au cours des 5 dernières années, avez-vous, pour des raisons environnementales, pris le train plutôt que l'avion dans le cadre professionnel alors que le trajet était plus long ?"
data[, 161] <- factor(data[, 161], levels=c("A2","A3","A4","A5"),labels=c("Oui, plusieurs fois", "Oui, une fois", "Non, mais j'y ai pensé", "Non"))
# names(data)[161] <- "trainavion"
# LimeSurvey Field type: A
data[, 162] <- as.character(data[, 162])
# attributes(data)$variable.labels[162] <- "Avez-vous déjà participé à un colloque/une conférence à l'étranger ?"
data[, 162] <- factor(data[, 162], levels=c("OuiM5","OuiP5","Non"),labels=c("Oui, dans les 5 dernières années", "Oui, mais il y a plus de 5 ans", "Non"))
# names(data)[162] <- "conf"
# LimeSurvey Field type: A
data[, 163] <- as.character(data[, 163])
# attributes(data)$variable.labels[163] <- "Combien de fois environ dans les 5 dernières années ?"
data[, 163] <- factor(data[, 163], levels=c("A2","A3","A4","A5","A6"),labels=c("Moins d'une fois par an", "Une fois par an", "Deux fois par an", "Trois fois par an", "Plus de trois fois par an"))
# names(data)[163] <- "conffois"
# LimeSurvey Field type: A
data[, 164] <- as.character(data[, 164])
# attributes(data)$variable.labels[164] <- "[D'avancer dans votre travail (commentaires/échanges)] Le dernier événement de ce type auquel vous avez participé vous a-t-il permis :"
data[, 164] <- factor(data[, 164], levels=c("A2","A3","A4","A5","A6"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[164] <- "apportconf_travaux"
# LimeSurvey Field type: A
data[, 165] <- as.character(data[, 165])
# attributes(data)$variable.labels[165] <- "[De développer/entretenir vos réseaux internationaux] Le dernier événement de ce type auquel vous avez participé vous a-t-il permis :"
data[, 165] <- factor(data[, 165], levels=c("A2","A3","A4","A5","A6"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[165] <- "apportconf_reseaux"
# LimeSurvey Field type: A
data[, 166] <- as.character(data[, 166])
# attributes(data)$variable.labels[166] <- "[D'entretenir/renforcer des liens professionnels ou amicaux avec des collègues qui travaillent en France] Le dernier événement de ce type auquel vous avez participé vous a-t-il permis :"
data[, 166] <- factor(data[, 166], levels=c("A2","A3","A4","A5","A6"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[166] <- "apportconf_collegues"
# LimeSurvey Field type: A
data[, 167] <- as.character(data[, 167])
# attributes(data)$variable.labels[167] <- "[D'améliorer votre CV] Le dernier événement de ce type auquel vous avez participé vous a-t-il permis :"
data[, 167] <- factor(data[, 167], levels=c("A2","A3","A4","A5","A6"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[167] <- "apportconf_cv"
# LimeSurvey Field type: A
data[, 168] <- as.character(data[, 168])
# attributes(data)$variable.labels[168] <- "[De visiter, de faire du tourisme] Le dernier événement de ce type auquel vous avez participé vous a-t-il permis :"
data[, 168] <- factor(data[, 168], levels=c("A2","A3","A4","A5","A6"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[168] <- "apportconf_tourisme"
# LimeSurvey Field type: A
data[, 169] <- as.character(data[, 169])
# attributes(data)$variable.labels[169] <- "[Un conflit avec des engagements privés (famille, rendez-vous...)] En 2019, avez-vous renoncé à un ou plusieurs déplacements professionnels à l'étranger (y compris décidé de ne pas candidater à une conférence) pour les raisons suivantes ?"
data[, 169] <- factor(data[, 169], levels=c("A2","A3","A4"),labels=c("Oui, c'était une raison déterminante", "Oui, mais c'était une raison secondaire", "Non"))
# names(data)[169] <- "renoncedep_prive"
# LimeSurvey Field type: A
data[, 170] <- as.character(data[, 170])
# attributes(data)$variable.labels[170] <- "[Un conflit avec d\'autres engagements professionnels] En 2019, avez-vous renoncé à un ou plusieurs déplacements professionnels à l'étranger (y compris décidé de ne pas candidater à une conférence) pour les raisons suivantes ?"
data[, 170] <- factor(data[, 170], levels=c("A2","A3","A4"),labels=c("Oui, c'était une raison déterminante", "Oui, mais c'était une raison secondaire", "Non"))
# names(data)[170] <- "renoncedep_pro"
# LimeSurvey Field type: A
data[, 171] <- as.character(data[, 171])
# attributes(data)$variable.labels[171] <- "[Des raisons de confort et de santé (durée du trajet, décalage horaire, fatigue...)] En 2019, avez-vous renoncé à un ou plusieurs déplacements professionnels à l'étranger (y compris décidé de ne pas candidater à une conférence) pour les raisons suivantes ?"
data[, 171] <- factor(data[, 171], levels=c("A2","A3","A4"),labels=c("Oui, c'était une raison déterminante", "Oui, mais c'était une raison secondaire", "Non"))
# names(data)[171] <- "renoncedep_confort"
# LimeSurvey Field type: A
data[, 172] <- as.character(data[, 172])
# attributes(data)$variable.labels[172] <- "[Des considérations environnementales] En 2019, avez-vous renoncé à un ou plusieurs déplacements professionnels à l'étranger (y compris décidé de ne pas candidater à une conférence) pour les raisons suivantes ?"
data[, 172] <- factor(data[, 172], levels=c("A2","A3","A4"),labels=c("Oui, c'était une raison déterminante", "Oui, mais c'était une raison secondaire", "Non"))
# names(data)[172] <- "renoncedep_env"
# LimeSurvey Field type: A
data[, 173] <- as.character(data[, 173])
# attributes(data)$variable.labels[173] <- "[Des difficultés à financer la mission] En 2019, avez-vous renoncé à un ou plusieurs déplacements professionnels à l'étranger (y compris décidé de ne pas candidater à une conférence) pour les raisons suivantes ?"
data[, 173] <- factor(data[, 173], levels=c("A2","A3","A4"),labels=c("Oui, c'était une raison déterminante", "Oui, mais c'était une raison secondaire", "Non"))
# names(data)[173] <- "renoncedep_mission"
# LimeSurvey Field type: A
data[, 174] <- as.character(data[, 174])
# attributes(data)$variable.labels[174] <- "[La possibilité de remplacer le voyage par la visioconférence] En 2019, avez-vous renoncé à un ou plusieurs déplacements professionnels à l'étranger (y compris décidé de ne pas candidater à une conférence) pour les raisons suivantes ?"
data[, 174] <- factor(data[, 174], levels=c("A2","A3","A4"),labels=c("Oui, c'était une raison déterminante", "Oui, mais c'était une raison secondaire", "Non"))
# names(data)[174] <- "renoncedep_visio"
# LimeSurvey Field type: A
data[, 175] <- as.character(data[, 175])
# attributes(data)$variable.labels[175] <- "En temps normal (avant le confinement), combien de jours en moyenne travaillez-vous uniquement de chez vous ? Ne tenez pas compte des éventuels jours travaillés le week-end."
data[, 175] <- factor(data[, 175], levels=c("A2","A3","A4","A5","A6","A7","A8"),labels=c("Jamais", "Moins d'un jour par semaine", "1 jour par semaine", "2 jours par semaine", "3 jours par semaine", "4 jours par semaine", "5 jours par semaine"))
# names(data)[175] <- "joursdomicile"
# LimeSurvey Field type: A
data[, 176] <- as.character(data[, 176])
# attributes(data)$variable.labels[176] <- "Parlons de vos trajets domicile-travail, en temps normal (avant le confinement)."
# names(data)[176] <- "textedomtrav"
# LimeSurvey Field type: A
data[, 177] <- as.character(data[, 177])
# attributes(data)$variable.labels[177] <- "[Bus, métro, tramway, RER] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[177] <- "tpsdomtrav_urbain_h"
# LimeSurvey Field type: F
data[, 178] <- as.numeric(data[, 178])
# attributes(data)$variable.labels[178] <- "[Bus, métro, tramway, RER] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[178] <- "tpsdomtrav_urbain_m"
# LimeSurvey Field type: F
data[, 179] <- as.numeric(data[, 179])
# attributes(data)$variable.labels[179] <- "[TGV] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[179] <- "tpsdomtrav_tgv_h"
# LimeSurvey Field type: F
data[, 180] <- as.numeric(data[, 180])
# attributes(data)$variable.labels[180] <- "[TGV] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[180] <- "tpsdomtrav_tgv_m"
# LimeSurvey Field type: F
data[, 181] <- as.numeric(data[, 181])
# attributes(data)$variable.labels[181] <- "[Autres trains] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[181] <- "tpsdomtrav_train_h"
# LimeSurvey Field type: F
data[, 182] <- as.numeric(data[, 182])
# attributes(data)$variable.labels[182] <- "[Autres trains] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[182] <- "tpsdomtrav_train_m"
# LimeSurvey Field type: A
data[, 183] <- as.character(data[, 183])
# attributes(data)$variable.labels[183] <- "[Seul·e dans une voiture] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[183] <- "tpsdomtrav_voit_h"
# LimeSurvey Field type: F
data[, 184] <- as.numeric(data[, 184])
# attributes(data)$variable.labels[184] <- "[Seul·e dans une voiture] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[184] <- "tpsdomtrav_voit_m"
# LimeSurvey Field type: F
data[, 185] <- as.numeric(data[, 185])
# attributes(data)$variable.labels[185] <- "[À plusieurs dans une voiture] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[185] <- "tpsdomtrav_covoit_h"
# LimeSurvey Field type: F
data[, 186] <- as.numeric(data[, 186])
# attributes(data)$variable.labels[186] <- "[À plusieurs dans une voiture] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[186] <- "tpsdomtrav_covoit_m"
# LimeSurvey Field type: F
data[, 187] <- as.numeric(data[, 187])
# attributes(data)$variable.labels[187] <- "[Moto, scooter] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[187] <- "tpsdomtrav_moto_h"
# LimeSurvey Field type: F
data[, 188] <- as.numeric(data[, 188])
# attributes(data)$variable.labels[188] <- "[Moto, scooter] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[188] <- "tpsdomtrav_moto_m"
# LimeSurvey Field type: F
data[, 189] <- as.numeric(data[, 189])
# attributes(data)$variable.labels[189] <- "[Vélo ou trottinette] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[189] <- "tpsdomtrav_velo_h"
# LimeSurvey Field type: F
data[, 190] <- as.numeric(data[, 190])
# attributes(data)$variable.labels[190] <- "[Vélo ou trottinette] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[190] <- "tpsdomtrav_velo_m"
# LimeSurvey Field type: F
data[, 191] <- as.numeric(data[, 191])
# attributes(data)$variable.labels[191] <- "[Marche] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[191] <- "tpsdomtrav_marche_h"
# LimeSurvey Field type: F
data[, 192] <- as.numeric(data[, 192])
# attributes(data)$variable.labels[192] <- "[Marche] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[192] <- "tpsdomtrav_marche_m"
# LimeSurvey Field type: F
data[, 193] <- as.numeric(data[, 193])
# attributes(data)$variable.labels[193] <- "[Avion] [heures par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[193] <- "tpsdomtrav_avion_h"
# LimeSurvey Field type: F
data[, 194] <- as.numeric(data[, 194])
# attributes(data)$variable.labels[194] <- "[Avion] [minutes par semaine] Avant le confinement, combien de temps passiez-vous en moyenne par semaine dans les  transports suivants pour vos déplacements domicile-travail ? Indiquez la somme de tous les trajets pour aller et revenir du travail, y compris les trajets longue distance. Laissez vides les cases qui ne vous concernent pas ou qui seraient à 0."
# names(data)[194] <- "tpsdomtrav_avion_m"
# LimeSurvey Field type: F
data[, 195] <- as.numeric(data[, 195])
# attributes(data)$variable.labels[195] <- "[Je tiens à la flexibilité de mes horaires] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 195] <- factor(data[, 195], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[195] <- "covoitpqpas_flex"
# LimeSurvey Field type: F
data[, 196] <- as.numeric(data[, 196])
# attributes(data)$variable.labels[196] <- "[J'ai des horaires trop atypiques ou trop irréguliers] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 196] <- factor(data[, 196], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[196] <- "covoitpqpas_atypique"
# LimeSurvey Field type: F
data[, 197] <- as.numeric(data[, 197])
# attributes(data)$variable.labels[197] <- "[Cela demande trop d'organisation] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 197] <- factor(data[, 197], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[197] <- "covoitpqpas_orga"
# LimeSurvey Field type: F
data[, 198] <- as.numeric(data[, 198])
# attributes(data)$variable.labels[198] <- "[Je dois déposer les enfants à l'école] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 198] <- factor(data[, 198], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[198] <- "covoitpqpas_ecole"
# LimeSurvey Field type: F
data[, 199] <- as.numeric(data[, 199])
# attributes(data)$variable.labels[199] <- "[Je n'ai trouvé personne avec un trajet et des horaires proches] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 199] <- factor(data[, 199], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[199] <- "covoitpqpas_personne"
# LimeSurvey Field type: F
data[, 200] <- as.numeric(data[, 200])
# attributes(data)$variable.labels[200] <- "[J'ai peur du manque de fiabilité des autres] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 200] <- factor(data[, 200], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[200] <- "covoitpqpas_fiabilite"
# LimeSurvey Field type: F
data[, 201] <- as.numeric(data[, 201])
# attributes(data)$variable.labels[201] <- "[Je crains de covoiturer avec quelqu'un avec qui je ne m'entends pas] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 201] <- factor(data[, 201], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[201] <- "covoitpqpas_entente"
# LimeSurvey Field type: F
data[, 202] <- as.numeric(data[, 202])
# attributes(data)$variable.labels[202] <- "[Le trajet est trop court] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 202] <- factor(data[, 202], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[202] <- "covoitpqpas_court"
# LimeSurvey Field type: F
data[, 203] <- as.numeric(data[, 203])
# attributes(data)$variable.labels[203] <- "[Je ne sais pas comment trouver un·e covoitureur·se] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
data[, 203] <- factor(data[, 203], levels=c(1,0),labels=c("Oui", "Non"))
# names(data)[203] <- "covoitpqpas_trouver"
# LimeSurvey Field type: A
data[, 204] <- as.character(data[, 204])
# attributes(data)$variable.labels[204] <- "[Autre] Pour les trajets que vous réalisez seul·e dans votre voiture, pourquoi ne faites-vous pas du covoiturage ?"
# names(data)[204] <- "covoitpqpas_other"
# LimeSurvey Field type: A
data[, 205] <- as.character(data[, 205])
# attributes(data)$variable.labels[205] <- "Avec qui réalisez-vous généralement les trajets en voiture à plusieurs ?"
data[, 205] <- factor(data[, 205], levels=c("A2","A3","A4","A5"),labels=c("Avec mon/ma conjoint·e", "Avec des collègues", "Avec des amis", "Autre"))
# names(data)[205] <- "covoitqui"
# LimeSurvey Field type: A
data[, 206] <- as.character(data[, 206])
# attributes(data)$variable.labels[206] <- "Avant le confinement quel était votre usage habituel de la visioconférence ou de l'audioconférence dans le cadre professionnel ?"
data[, 206] <- factor(data[, 206], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Jamais", "Moins d\'une fois par mois", "1 à 3 fois par mois", "1 à 4 fois par semaine", "1 à 2 fois par jour", "Plus de 2 fois par jour"))
# names(data)[206] <- "visioavtconf"
# LimeSurvey Field type: A
data[, 207] <- as.character(data[, 207])
# attributes(data)$variable.labels[207] <- "Pendant le confinement quel a été votre usage de la visioconférence ou de l'audioconférence dans le cadre professionnel ?"
data[, 207] <- factor(data[, 207], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Jamais", "Moins d\'une fois par mois", "1 à 3 fois par mois", "1 à 4 fois par semaine", "1 à 2 fois par jour", "Plus de 2 fois par jour"))
# names(data)[207] <- "visiopdtconf"
# LimeSurvey Field type: A
data[, 208] <- as.character(data[, 208])
# attributes(data)$variable.labels[208] <- "[Réunion de travail de 3 à 5 personnes] Actuellement, d'après votre expérience, pour quels usages la visioconférence ou l'audioconférence vous semblent-elles adaptées ?"
data[, 208] <- factor(data[, 208], levels=c("A7","A1","A2","A3","A4","A5"),labels=c("Je n'ai jamais testé", "Très adaptée", "Plutôt adaptée", "Plutôt pas adaptée", "Pas adaptée du tout", "Sans opinion"))
# names(data)[208] <- "visiousages_reunion5"
# LimeSurvey Field type: A
data[, 209] <- as.character(data[, 209])
# attributes(data)$variable.labels[209] <- "[Réunion de travail de 15 personnes] Actuellement, d'après votre expérience, pour quels usages la visioconférence ou l'audioconférence vous semblent-elles adaptées ?"
data[, 209] <- factor(data[, 209], levels=c("A7","A1","A2","A3","A4","A5"),labels=c("Je n'ai jamais testé", "Très adaptée", "Plutôt adaptée", "Plutôt pas adaptée", "Pas adaptée du tout", "Sans opinion"))
# names(data)[209] <- "visiousages_reunion15"
# LimeSurvey Field type: A
data[, 210] <- as.character(data[, 210])
# attributes(data)$variable.labels[210] <- "[Oral de jury (recrutement, thèse, HDR…)] Actuellement, d'après votre expérience, pour quels usages la visioconférence ou l'audioconférence vous semblent-elles adaptées ?"
data[, 210] <- factor(data[, 210], levels=c("A7","A1","A2","A3","A4","A5"),labels=c("Je n'ai jamais testé", "Très adaptée", "Plutôt adaptée", "Plutôt pas adaptée", "Pas adaptée du tout", "Sans opinion"))
# names(data)[210] <- "visiousages_jury"
# LimeSurvey Field type: A
data[, 211] <- as.character(data[, 211])
# attributes(data)$variable.labels[211] <- "[Présentation en séminaire] Actuellement, d'après votre expérience, pour quels usages la visioconférence ou l'audioconférence vous semblent-elles adaptées ?"
data[, 211] <- factor(data[, 211], levels=c("A7","A1","A2","A3","A4","A5"),labels=c("Je n'ai jamais testé", "Très adaptée", "Plutôt adaptée", "Plutôt pas adaptée", "Pas adaptée du tout", "Sans opinion"))
# names(data)[211] <- "visiousages_seminaire"
# LimeSurvey Field type: A
data[, 212] <- as.character(data[, 212])
# attributes(data)$variable.labels[212] <- "[Conférence ou congrès avec plusieurs présentations] Actuellement, d'après votre expérience, pour quels usages la visioconférence ou l'audioconférence vous semblent-elles adaptées ?"
data[, 212] <- factor(data[, 212], levels=c("A7","A1","A2","A3","A4","A5"),labels=c("Je n'ai jamais testé", "Très adaptée", "Plutôt adaptée", "Plutôt pas adaptée", "Pas adaptée du tout", "Sans opinion"))
# names(data)[212] <- "visiousages_conf"
# LimeSurvey Field type: A
data[, 213] <- as.character(data[, 213])
# attributes(data)$variable.labels[213] <- "Depuis le confinement, votre expérience vous a-t-elle rendu·e plus favorable ou moins favorable à la visioconférence ou l'audioconférence ?"
data[, 213] <- factor(data[, 213], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Beaucoup plus favorable", "Un peu plus favorable", "Mon avis n'a pas changé", "Un peu moins favorable", "Beaucoup moins favorable", "Sans opinion"))
# names(data)[213] <- "visioapresconf"
# LimeSurvey Field type: A
data[, 214] <- as.character(data[, 214])
# attributes(data)$variable.labels[214] <- "[Gagner du temps] Imaginons une réunion d'une journée, située à 2h de votre domicile en voiture. Les raisons suivantes pourraient-elles vous pousser à privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 214] <- factor(data[, 214], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[214] <- "visioprivilegiee_temps"
# LimeSurvey Field type: A
data[, 215] <- as.character(data[, 215])
# attributes(data)$variable.labels[215] <- "[Limiter la fatigue due au déplacement] Imaginons une réunion d'une journée, située à 2h de votre domicile en voiture. Les raisons suivantes pourraient-elles vous pousser à privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 215] <- factor(data[, 215], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[215] <- "visioprivilegiee_fatigue"
# LimeSurvey Field type: A
data[, 216] <- as.character(data[, 216])
# attributes(data)$variable.labels[216] <- "[Limiter les frais de mission] Imaginons une réunion d'une journée, située à 2h de votre domicile en voiture. Les raisons suivantes pourraient-elles vous pousser à privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 216] <- factor(data[, 216], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[216] <- "visioprivilegiee_frais"
# LimeSurvey Field type: A
data[, 217] <- as.character(data[, 217])
# attributes(data)$variable.labels[217] <- "[Mieux concilier vos contraintes familiales et votre activité professionnelle] Imaginons une réunion d'une journée, située à 2h de votre domicile en voiture. Les raisons suivantes pourraient-elles vous pousser à privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 217] <- factor(data[, 217], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[217] <- "visioprivilegiee_famille"
# LimeSurvey Field type: A
data[, 218] <- as.character(data[, 218])
# attributes(data)$variable.labels[218] <- "[Limiter vos émissions de gaz à effet de serre] Imaginons une réunion d'une journée, située à 2h de votre domicile en voiture. Les raisons suivantes pourraient-elles vous pousser à privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 218] <- factor(data[, 218], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[218] <- "visioprivilegiee_emissions"
# LimeSurvey Field type: A
data[, 219] <- as.character(data[, 219])
# attributes(data)$variable.labels[219] <- "[Pouvoir faire autre chose en même temps que la réunion] Imaginons une réunion d'une journée, située à 2h de votre domicile en voiture. Les raisons suivantes pourraient-elles vous pousser à privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 219] <- factor(data[, 219], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[219] <- "visioprivilegiee_autrechose"
# LimeSurvey Field type: A
data[, 220] <- as.character(data[, 220])
# attributes(data)$variable.labels[220] <- "[Réunir facilement de nombreuses personnes très éloignées géographiquement] Imaginons une réunion d'une journée, située à 2h de votre domicile en voiture. Les raisons suivantes pourraient-elles vous pousser à privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 220] <- factor(data[, 220], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[220] <- "visioprivilegiee_reunir"
# LimeSurvey Field type: A
data[, 221] <- as.character(data[, 221])
# attributes(data)$variable.labels[221] <- "[Cela génère des problèmes techniques] Imaginons toujours une réunion d'une journée, située à 2h de votre domicile en voiture. Les problèmes suivants pourraient-ils vous empêcher de privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 221] <- factor(data[, 221], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[221] <- "visiopb_technique"
# LimeSurvey Field type: A
data[, 222] <- as.character(data[, 222])
# attributes(data)$variable.labels[222] <- "[Cela limite les aspects relationnels] Imaginons toujours une réunion d'une journée, située à 2h de votre domicile en voiture. Les problèmes suivants pourraient-ils vous empêcher de privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 222] <- factor(data[, 222], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[222] <- "visiopb_relation"
# LimeSurvey Field type: A
data[, 223] <- as.character(data[, 223])
# attributes(data)$variable.labels[223] <- "[Cela rend difficile d'écrire ou dessiner en discutant (schémas, formules, équations...)] Imaginons toujours une réunion d'une journée, située à 2h de votre domicile en voiture. Les problèmes suivants pourraient-ils vous empêcher de privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 223] <- factor(data[, 223], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[223] <- "visiopb_ecrire"
# LimeSurvey Field type: A
data[, 224] <- as.character(data[, 224])
# attributes(data)$variable.labels[224] <- "[C'est plus fatigant à suivre qu'une réunion en présentiel ] Imaginons toujours une réunion d'une journée, située à 2h de votre domicile en voiture. Les problèmes suivants pourraient-ils vous empêcher de privilégier la visioconférence ou l'audioconférence à un déplacement ? Si vous ne conduisez pas, supposez qu'un·e collègue vous emmène."
data[, 224] <- factor(data[, 224], levels=c("A1","A2","A3","A4","A5"),labels=c("Oui, beaucoup", "Oui, un peu", "Non, pas vraiment", "Non, pas du tout", "Sans opinion"))
# names(data)[224] <- "visiopb_fatigue"
# LimeSurvey Field type: A
data[, 225] <- as.character(data[, 225])
# attributes(data)$variable.labels[225] <- "La France s'est engagée à réduire d'un tiers ses émissions de gaz à effet de serre d'ici à 2030. Dans ce cadre, pensez-vous que :"
data[, 225] <- factor(data[, 225], levels=c("A1","A2","A3"),labels=c("La recherche doit montrer l'exemple", "La recherche doit réduire ses émissions d'un tiers", "La recherche peut bénéficier d'un statut dérogatoire"))
# names(data)[225] <- "solreducrech"
# LimeSurvey Field type: A
data[, 226] <- as.character(data[, 226])
# attributes(data)$variable.labels[226] <- "[Les vols en avion pour les conférences, réunions, congrès] Êtes-vous prêt·e à réduire vos émissions de gaz à effet de serre d'ici à 2030 dans les domaines suivants ? On ne tient pas compte ici des réductions que vous avez déjà réalisées par le passé."
data[, 226] <- factor(data[, 226], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[226] <- "solreducperso_conf"
# LimeSurvey Field type: A
data[, 227] <- as.character(data[, 227])
# attributes(data)$variable.labels[227] <- "[Les déplacements pour missions de terrain, d'observation ou de collecte de données (en avion, voiture ou bateau)] Êtes-vous prêt·e à réduire vos émissions de gaz à effet de serre d'ici à 2030 dans les domaines suivants ? On ne tient pas compte ici des réductions que vous avez déjà réalisées par le passé."
data[, 227] <- factor(data[, 227], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[227] <- "solreducperso_donnees"
# LimeSurvey Field type: A
data[, 228] <- as.character(data[, 228])
# attributes(data)$variable.labels[228] <- "[Les expériences et observations scientifiques (hors déplacements)] Êtes-vous prêt·e à réduire vos émissions de gaz à effet de serre d'ici à 2030 dans les domaines suivants ? On ne tient pas compte ici des réductions que vous avez déjà réalisées par le passé."
data[, 228] <- factor(data[, 228], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[228] <- "solreducperso_exp"
# LimeSurvey Field type: A
data[, 229] <- as.character(data[, 229])
# attributes(data)$variable.labels[229] <- "[L'équipement en postes informatiques et sa fréquence de renouvellement] Êtes-vous prêt·e à réduire vos émissions de gaz à effet de serre d'ici à 2030 dans les domaines suivants ? On ne tient pas compte ici des réductions que vous avez déjà réalisées par le passé."
data[, 229] <- factor(data[, 229], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[229] <- "solreducperso_info"
# LimeSurvey Field type: A
data[, 230] <- as.character(data[, 230])
# attributes(data)$variable.labels[230] <- "[Les déplacements domicile-travail en voiture, moto, scooter ou avion] Êtes-vous prêt·e à réduire vos émissions de gaz à effet de serre d'ici à 2030 dans les domaines suivants ? On ne tient pas compte ici des réductions que vous avez déjà réalisées par le passé."
data[, 230] <- factor(data[, 230], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[230] <- "solreducperso_domicile"
# LimeSurvey Field type: A
data[, 231] <- as.character(data[, 231])
# attributes(data)$variable.labels[231] <- "[En l'utilisant moins fréquemment ou moins intensivement] Êtes-vous prêt·e à réduire d'ici à 2030 les émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné à vos expériences et observations scientifiques, par les moyens suivants ?"
data[, 231] <- factor(data[, 231], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[231] <- "solreducmateriel_util"
# LimeSurvey Field type: A
data[, 232] <- as.character(data[, 232])
# attributes(data)$variable.labels[232] <- "[En utilisant ou en développant des dispositifs plus simples (low tech)] Êtes-vous prêt·e à réduire d'ici à 2030 les émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné à vos expériences et observations scientifiques, par les moyens suivants ?"
data[, 232] <- factor(data[, 232], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[232] <- "solreducmateriel_lowtech"
# LimeSurvey Field type: A
data[, 233] <- as.character(data[, 233])
# attributes(data)$variable.labels[233] <- "[En utilisant moins d'équipements dans les dispositifs expérimentaux] Êtes-vous prêt·e à réduire d'ici à 2030 les émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné à vos expériences et observations scientifiques, par les moyens suivants ?"
data[, 233] <- factor(data[, 233], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[233] <- "solreducmateriel_moins"
# LimeSurvey Field type: A
data[, 234] <- as.character(data[, 234])
# attributes(data)$variable.labels[234] <- "[En renouvelant le matériel moins fréquemment] Êtes-vous prêt·e à réduire d'ici à 2030 les émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné à vos expériences et observations scientifiques, par les moyens suivants ?"
data[, 234] <- factor(data[, 234], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Non", "Non concerné·e", "Sans opinion"))
# names(data)[234] <- "solreducmateriel_renouv"
# LimeSurvey Field type: A
data[, 235] <- as.character(data[, 235])
# attributes(data)$variable.labels[235] <- "[Diminuer la qualité de vos travaux (ou ceux de votre équipe)] Selon vous, quels seraient les risques liés à une politique de réduction des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné aux expériences et aux observations scientifiques ?"
data[, 235] <- factor(data[, 235], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[235] <- "solrisqreducmateriel_qual"
# LimeSurvey Field type: A
data[, 236] <- as.character(data[, 236])
# attributes(data)$variable.labels[236] <- "[Vous imposer un changement de thèmes de recherche] Selon vous, quels seraient les risques liés à une politique de réduction des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné aux expériences et aux observations scientifiques ?"
data[, 236] <- factor(data[, 236], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[236] <- "solrisqreducmateriel_themes"
# LimeSurvey Field type: A
data[, 237] <- as.character(data[, 237])
# attributes(data)$variable.labels[237] <- "[Réduire votre accès aux financements] Selon vous, quels seraient les risques liés à une politique de réduction des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné aux expériences et aux observations scientifiques ?"
data[, 237] <- factor(data[, 237], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[237] <- "solrisqreducmateriel_fin"
# LimeSurvey Field type: A
data[, 238] <- as.character(data[, 238])
# attributes(data)$variable.labels[238] <- "[Vous faire prendre du retard par rapport aux équipes concurrentes] Selon vous, quels seraient les risques liés à une politique de réduction des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné aux expériences et aux observations scientifiques ?"
data[, 238] <- factor(data[, 238], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[238] <- "solrisqreducmateriel_retard"
# LimeSurvey Field type: A
data[, 239] <- as.character(data[, 239])
# attributes(data)$variable.labels[239] <- "[Réduire votre nombre de publications] Selon vous, quels seraient les risques liés à une politique de réduction des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel destiné aux expériences et aux observations scientifiques ?"
data[, 239] <- factor(data[, 239], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[239] <- "solrisqreducmateriel_publi"
# LimeSurvey Field type: A
data[, 240] <- as.character(data[, 240])
# attributes(data)$variable.labels[240] <- "[Diminuer la qualité de vos travaux (ou ceux de votre équipe)] Selon vous, quels seraient les risques liés à la mise en place d'une politique de réduction des vols professionnels en avion dans la recherche ?"
data[, 240] <- factor(data[, 240], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[240] <- "solrisqreducavion_qual"
# LimeSurvey Field type: A
data[, 241] <- as.character(data[, 241])
# attributes(data)$variable.labels[241] <- "[Réduire votre accès aux financements] Selon vous, quels seraient les risques liés à la mise en place d'une politique de réduction des vols professionnels en avion dans la recherche ?"
data[, 241] <- factor(data[, 241], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[241] <- "solrisqreducavion_fin"
# LimeSurvey Field type: A
data[, 242] <- as.character(data[, 242])
# attributes(data)$variable.labels[242] <- "[Diminuer la diffusion de vos travaux (ou ceux de votre équipe)] Selon vous, quels seraient les risques liés à la mise en place d'une politique de réduction des vols professionnels en avion dans la recherche ?"
data[, 242] <- factor(data[, 242], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[242] <- "solrisqreducavion_diffusion"
# LimeSurvey Field type: A
data[, 243] <- as.character(data[, 243])
# attributes(data)$variable.labels[243] <- "[Vous gêner pour l'accès à certains terrains ou la collecte/production de certaines données] Selon vous, quels seraient les risques liés à la mise en place d'une politique de réduction des vols professionnels en avion dans la recherche ?"
data[, 243] <- factor(data[, 243], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[243] <- "solrisqreducavion_donnees"
# LimeSurvey Field type: A
data[, 244] <- as.character(data[, 244])
# attributes(data)$variable.labels[244] <- "[Réduire certains avantages que vous apporte votre métier (comme voyager et découvrir d'autres pays...)] Selon vous, quels seraient les risques liés à la mise en place d'une politique de réduction des vols professionnels en avion dans la recherche ?"
data[, 244] <- factor(data[, 244], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[244] <- "solrisqreducavion_avantages"
# LimeSurvey Field type: A
data[, 245] <- as.character(data[, 245])
# attributes(data)$variable.labels[245] <- "[Isoler la recherche française du reste du monde] Selon vous, quels seraient les risques liés à la mise en place d'une politique de réduction des vols professionnels en avion dans la recherche ?"
data[, 245] <- factor(data[, 245], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[245] <- "solrisqreducavion_isoler"
# LimeSurvey Field type: A
data[, 246] <- as.character(data[, 246])
# attributes(data)$variable.labels[246] <- "[Gêner l'insertion des jeunes chercheurs] Selon vous, quels seraient les risques liés à la mise en place d'une politique de réduction des vols professionnels en avion dans la recherche ?"
data[, 246] <- factor(data[, 246], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[246] <- "solrisqreducavion_insertion"
# LimeSurvey Field type: A
data[, 247] <- as.character(data[, 247])
# attributes(data)$variable.labels[247] <- "[Accroître la bureaucratie (application des règles, critères d'évaluation…)] Selon vous, quels seraient les risques liés à la mise en place d'une politique de réduction des vols professionnels en avion dans la recherche ?"
data[, 247] <- factor(data[, 247], levels=c("A1","A2","A3","A4","A5"),labels=c("C'est probable et c'est un problème", "C'est probable mais ce n'est pas un problème", "C'est peu probable", "Sans opinion", "Non concerné·e"))
# names(data)[247] <- "solrisqreducavion_bureaucratie"
# LimeSurvey Field type: A
data[, 248] <- as.character(data[, 248])
# attributes(data)$variable.labels[248] <- "[Financer le train même lorsque c'est plus cher ou allonge la durée de la mission] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 248] <- factor(data[, 248], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[248] <- "solinstit_train"
# LimeSurvey Field type: A
data[, 249] <- as.character(data[, 249])
# attributes(data)$variable.labels[249] <- "[Financer des initiatives de compensation carbone] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 249] <- factor(data[, 249], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[249] <- "solinstit_compensation"
# LimeSurvey Field type: A
data[, 250] <- as.character(data[, 250])
# attributes(data)$variable.labels[250] <- "[Dresser et diffuser régulièrement des bilans carbone/de gaz à effet de serre détaillés] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 250] <- factor(data[, 250], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[250] <- "solinstit_bilanges"
# LimeSurvey Field type: A
data[, 251] <- as.character(data[, 251])
# attributes(data)$variable.labels[251] <- "[Imposer une limite au nombre de vols en avion par personne] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 251] <- factor(data[, 251], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[251] <- "solinstit_limitevols"
# LimeSurvey Field type: A
data[, 252] <- as.character(data[, 252])
# attributes(data)$variable.labels[252] <- "[Ajouter les émissions de gaz à effet de serre parmi les principaux critères de sélection des projets à financer] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 252] <- factor(data[, 252], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[252] <- "solinstit_selection"
# LimeSurvey Field type: A
data[, 253] <- as.character(data[, 253])
# attributes(data)$variable.labels[253] <- "[Diminuer le poids des conférences et présentations à l'étranger dans les évaluations de carrière] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 253] <- factor(data[, 253], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[253] <- "solinstit_conf"
# LimeSurvey Field type: A
data[, 254] <- as.character(data[, 254])
# attributes(data)$variable.labels[254] <- "[Proscrire les vols en avion lorsque le trajet en train prend moins de 6 heures] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 254] <- factor(data[, 254], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[254] <- "solinstit_vols6h"
# LimeSurvey Field type: A
data[, 255] <- as.character(data[, 255])
# attributes(data)$variable.labels[255] <- "[Ne pas renouveler le matériel informatique qui fonctionne avant un minimum de 5 ans d'ancienneté] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 255] <- factor(data[, 255], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[255] <- "solinstit_info"
# LimeSurvey Field type: A
data[, 256] <- as.character(data[, 256])
# attributes(data)$variable.labels[256] <- "[Lors du renouvellement des équipements, privilégier ceux qui consomment moins d'énergie même s'ils sont plus coûteux] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 256] <- factor(data[, 256], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[256] <- "solinstit_equip"
# LimeSurvey Field type: A
data[, 257] <- as.character(data[, 257])
# attributes(data)$variable.labels[257] <- "[Lors de l'organisation d'événements, favoriser les prestataires proposant des menus locaux ou végétariens] Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?"
data[, 257] <- factor(data[, 257], levels=c("A1","A2","A3","A4"),labels=c("C'est prioritaire", "C'est secondaire", "Il ne faut pas le mettre en œuvre", "Sans opinion"))
# names(data)[257] <- "solinstit_vege"
# LimeSurvey Field type: A
data[, 258] <- as.character(data[, 258])
# attributes(data)$variable.labels[258] <- "Si le covoiturage était organisé à l'échelle de votre laboratoire ou plus largement de votre site, seriez-vous prêt·e à y recourir pour vos trajets domicile-travail ?"
data[, 258] <- factor(data[, 258], levels=c("A1","A2","A3","A4"),labels=c("Oui, certainement", "Oui, probablement", "Non, probablement pas", "Non, certainement pas"))
# names(data)[258] <- "solcovoit"
# LimeSurvey Field type: A
data[, 259] <- as.character(data[, 259])
# attributes(data)$variable.labels[259] <- "[Calculé tout ou partie de vos émissions de gaz à effet de serre (bilan carbone)] Au cours des 10 dernières années, avez-vous déjà..."
data[, 259] <- factor(data[, 259], levels=c("A1","A2","A3"),labels=c("Oui", "Non", "Je ne souhaite pas répondre"))
# names(data)[259] <- "dixannees_bilan"
# LimeSurvey Field type: A
data[, 260] <- as.character(data[, 260])
# attributes(data)$variable.labels[260] <- "[Consulté un rapport du Groupe d'experts intergouvernemental sur l'évolution du climat (GIEC) ou un résumé (hors article de presse)] Au cours des 10 dernières années, avez-vous déjà..."
data[, 260] <- factor(data[, 260], levels=c("A1","A2","A3"),labels=c("Oui", "Non", "Je ne souhaite pas répondre"))
# names(data)[260] <- "dixannees_giec"
# LimeSurvey Field type: A
data[, 261] <- as.character(data[, 261])
# attributes(data)$variable.labels[261] <- "[Adhéré ou fait un don à une association de défense de l'environnement] Au cours des 10 dernières années, avez-vous déjà..."
data[, 261] <- factor(data[, 261], levels=c("A1","A2","A3"),labels=c("Oui", "Non", "Je ne souhaite pas répondre"))
# names(data)[261] <- "dixannees_asso"
# LimeSurvey Field type: A
data[, 262] <- as.character(data[, 262])
# attributes(data)$variable.labels[262] <- "[Participé à une marche pour le climat] Au cours des 10 dernières années, avez-vous déjà..."
data[, 262] <- factor(data[, 262], levels=c("A1","A2","A3"),labels=c("Oui", "Non", "Je ne souhaite pas répondre"))
# names(data)[262] <- "dixannees_marche"
# LimeSurvey Field type: A
data[, 263] <- as.character(data[, 263])
# attributes(data)$variable.labels[263] <- "[Accordé une importance déterminante à l'écologie dans un vote] Au cours des 10 dernières années, avez-vous déjà..."
data[, 263] <- factor(data[, 263], levels=c("A1","A2","A3"),labels=c("Oui", "Non", "Je ne souhaite pas répondre"))
# names(data)[263] <- "dixannees_vote"
# LimeSurvey Field type: A
data[, 264] <- as.character(data[, 264])
# attributes(data)$variable.labels[264] <- "Combien avez-vous réalisé de vols aller-retour en avion en 2019 dans un cadre personnel ?"
data[, 264] <- factor(data[, 264], levels=c("A2","A3","A4","A5"),labels=c("Aucun aller-retour", "1 ou 2 allers-retours", "3 ou 4 allers-retours", "Plus de 5 allers-retours"))
# names(data)[264] <- "avionperso"
# LimeSurvey Field type: A
data[, 265] <- as.character(data[, 265])
# attributes(data)$variable.labels[265] <- "Avez-vous, dans les 5 dernières années, changé vos pratiques en matière de déplacements en avion dans un cadre personnel ?"
data[, 265] <- factor(data[, 265], levels=c("A2","A3","A4","A5","A6"),labels=c("Non", "Oui, je le prends beaucoup moins", "Oui, je le prends un peu moins", "Oui, je le prends un peu plus", "Oui, je le prends beaucoup plus"))
# names(data)[265] <- "avionpersochgt"
# LimeSurvey Field type: A
data[, 266] <- as.character(data[, 266])
# attributes(data)$variable.labels[266] <- "Au cours des 5 dernières années, avez-vous, pour des raisons environnementales, fait des efforts pour réduire ou maintenir basse votre consommation personnelle de certains éléments de la liste suivante : vêtements, viande, équipements high-tech, énergie nécessaire au chauffage de votre logement ?"
data[, 266] <- factor(data[, 266], levels=c("A1","A2","A3"),labels=c("Oui, beaucoup", "Oui, un peu", "Non"))
# names(data)[266] <- "effortsconso"
# LimeSurvey Field type: A
data[, 267] <- as.character(data[, 267])
# attributes(data)$variable.labels[267] <- "[La plupart des problèmes environnementaux peuvent être résolus par le développement de technologies plus performantes] A quel point êtes-vous d'accord avec les affirmations suivantes ?"
data[, 267] <- factor(data[, 267], levels=c("TFA","PA","PPA","PTA","SO"),labels=c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord", "Sans opinion"))
# names(data)[267] <- "opinionecolo_techno"
# LimeSurvey Field type: A
data[, 268] <- as.character(data[, 268])
# attributes(data)$variable.labels[268] <- "[Protéger l'environnement est plus important que protéger la croissance économique] A quel point êtes-vous d'accord avec les affirmations suivantes ?"
data[, 268] <- factor(data[, 268], levels=c("TFA","PA","PPA","PTA","SO"),labels=c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord", "Sans opinion"))
# names(data)[268] <- "opinionecolo_proteger"
# LimeSurvey Field type: A
data[, 269] <- as.character(data[, 269])
# attributes(data)$variable.labels[269] <- "[Il ne sert à rien que je fasse des efforts pour l\'environnement si les autres ne font pas de même] A quel point êtes-vous d'accord avec les affirmations suivantes ?"
data[, 269] <- factor(data[, 269], levels=c("TFA","PA","PPA","PTA","SO"),labels=c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord", "Sans opinion"))
# names(data)[269] <- "opinionecolo_efforts"
# LimeSurvey Field type: A
data[, 270] <- as.character(data[, 270])
# attributes(data)$variable.labels[270] <- "[Je suis d'accord pour que des contraintes réglementaires (quotas, interdictions) soient mises en place pour protéger l'environnement, même si cela limite mon confort] A quel point êtes-vous d'accord avec les affirmations suivantes ?"
data[, 270] <- factor(data[, 270], levels=c("TFA","PA","PPA","PTA","SO"),labels=c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord", "Sans opinion"))
# names(data)[270] <- "opinionecolo_contraintes"
# LimeSurvey Field type: A
data[, 271] <- as.character(data[, 271])
# attributes(data)$variable.labels[271] <- "[La décroissance est nécessaire pour faire face aux enjeux environnementaux] A quel point êtes-vous d'accord avec les affirmations suivantes ?"
data[, 271] <- factor(data[, 271], levels=c("TFA","PA","PPA","PTA","SO"),labels=c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord", "Sans opinion"))
# names(data)[271] <- "opinionecolo_decroissance"
# LimeSurvey Field type: A
data[, 272] <- as.character(data[, 272])
# attributes(data)$variable.labels[272] <- "[Si les choses continuent au rythme actuel, nous allons bientôt vivre une catastrophe écologique majeure] A quel point êtes-vous d'accord avec les affirmations suivantes ?"
data[, 272] <- factor(data[, 272], levels=c("TFA","PA","PPA","PTA","SO"),labels=c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord", "Sans opinion"))
# names(data)[272] <- "opinionecolo_cata"
# LimeSurvey Field type: A
data[, 273] <- as.character(data[, 273])
# attributes(data)$variable.labels[273] <- "[Ce type de catastrophe pourrait provoquer un effondrement de nos sociétés : les besoins de base (alimentation, énergie, santé, etc.) ne seront plus assurés pour la majorité de la population] A quel point êtes-vous d'accord avec les affirmations suivantes ?"
data[, 273] <- factor(data[, 273], levels=c("TFA","PA","PPA","PTA","SO"),labels=c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord", "Sans opinion"))
# names(data)[273] <- "opinionecolo_effondrement"
# LimeSurvey Field type: F
data[, 274] <- as.numeric(data[, 274])
# attributes(data)$variable.labels[274] <- "[Agence nationale de la recherche (ANR)] [Oui, comme membre] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 274] <- factor(data[, 274], levels=c(1,0),labels=c("1", "0"))
# names(data)[274] <- "projets_anr_m"
# LimeSurvey Field type: F
data[, 275] <- as.numeric(data[, 275])
# attributes(data)$variable.labels[275] <- "[Agence nationale de la recherche (ANR)] [Oui, comme (co-)responsable] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 275] <- factor(data[, 275], levels=c(1,0),labels=c("1", "0"))
# names(data)[275] <- "projets_anr_r"
# LimeSurvey Field type: F
data[, 276] <- as.numeric(data[, 276])
# attributes(data)$variable.labels[276] <- "[Agence nationale de la recherche (ANR)] [Non] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 276] <- factor(data[, 276], levels=c(1,0),labels=c("1", "0"))
# names(data)[276] <- "projets_anr_n"
# LimeSurvey Field type: F
data[, 277] <- as.numeric(data[, 277])
# attributes(data)$variable.labels[277] <- "[Autre financement public français] [Oui, comme membre] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 277] <- factor(data[, 277], levels=c(1,0),labels=c("1", "0"))
# names(data)[277] <- "projets_france_m"
# LimeSurvey Field type: F
data[, 278] <- as.numeric(data[, 278])
# attributes(data)$variable.labels[278] <- "[Autre financement public français] [Oui, comme (co-)responsable] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 278] <- factor(data[, 278], levels=c(1,0),labels=c("1", "0"))
# names(data)[278] <- "projets_france_r"
# LimeSurvey Field type: F
data[, 279] <- as.numeric(data[, 279])
# attributes(data)$variable.labels[279] <- "[Autre financement public français] [Non] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 279] <- factor(data[, 279], levels=c(1,0),labels=c("1", "0"))
# names(data)[279] <- "projets_france_n"
# LimeSurvey Field type: F
data[, 280] <- as.numeric(data[, 280])
# attributes(data)$variable.labels[280] <- "[Financement européen (European Research Council (ERC), H2020, etc.)] [Oui, comme membre] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 280] <- factor(data[, 280], levels=c(1,0),labels=c("1", "0"))
# names(data)[280] <- "projets_europe_m"
# LimeSurvey Field type: F
data[, 281] <- as.numeric(data[, 281])
# attributes(data)$variable.labels[281] <- "[Financement européen (European Research Council (ERC), H2020, etc.)] [Oui, comme (co-)responsable] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 281] <- factor(data[, 281], levels=c(1,0),labels=c("1", "0"))
# names(data)[281] <- "projets_europe_r"
# LimeSurvey Field type: F
data[, 282] <- as.numeric(data[, 282])
# attributes(data)$variable.labels[282] <- "[Financement européen (European Research Council (ERC), H2020, etc.)] [Non] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 282] <- factor(data[, 282], levels=c(1,0),labels=c("1", "0"))
# names(data)[282] <- "projets_europe_n"
# LimeSurvey Field type: F
data[, 283] <- as.numeric(data[, 283])
# attributes(data)$variable.labels[283] <- "[Autre financement public international] [Oui, comme membre] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 283] <- factor(data[, 283], levels=c(1,0),labels=c("1", "0"))
# names(data)[283] <- "projets_inter_m"
# LimeSurvey Field type: F
data[, 284] <- as.numeric(data[, 284])
# attributes(data)$variable.labels[284] <- "[Autre financement public international] [Oui, comme (co-)responsable] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 284] <- factor(data[, 284], levels=c(1,0),labels=c("1", "0"))
# names(data)[284] <- "projets_inter_r"
# LimeSurvey Field type: F
data[, 285] <- as.numeric(data[, 285])
# attributes(data)$variable.labels[285] <- "[Autre financement public international] [Non] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 285] <- factor(data[, 285], levels=c(1,0),labels=c("1", "0"))
# names(data)[285] <- "projets_inter_n"
# LimeSurvey Field type: F
data[, 286] <- as.numeric(data[, 286])
# attributes(data)$variable.labels[286] <- "[Financement privé (y compris fondation privée)] [Oui, comme membre] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 286] <- factor(data[, 286], levels=c(1,0),labels=c("1", "0"))
# names(data)[286] <- "projets_prive_m"
# LimeSurvey Field type: F
data[, 287] <- as.numeric(data[, 287])
# attributes(data)$variable.labels[287] <- "[Financement privé (y compris fondation privée)] [Oui, comme (co-)responsable] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 287] <- factor(data[, 287], levels=c(1,0),labels=c("1", "0"))
# names(data)[287] <- "projets_prive_r"
# LimeSurvey Field type: F
data[, 288] <- as.numeric(data[, 288])
# attributes(data)$variable.labels[288] <- "[Financement privé (y compris fondation privée)] [Non] Participez-vous à un ou plusieurs projets de recherche disposant d'un financement dédié (c'est-à-dire autre que les fonds propres de votre institution) ?"
data[, 288] <- factor(data[, 288], levels=c(1,0),labels=c("1", "0"))
# names(data)[288] <- "projets_prive_n"
# LimeSurvey Field type: A
data[, 289] <- as.character(data[, 289])
# attributes(data)$variable.labels[289] <- "[Votre poste principal en juin 2020 était-il hors de France ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 289] <- factor(data[, 289], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[289] <- "international_poste"
# LimeSurvey Field type: A
data[, 290] <- as.character(data[, 290])
# attributes(data)$variable.labels[290] <- "[Êtes-vous né·e dans un pays étranger ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 290] <- factor(data[, 290], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[290] <- "international_naiss"
# LimeSurvey Field type: A
data[, 291] <- as.character(data[, 291])
# attributes(data)$variable.labels[291] <- "[Avez-vous une nationalité étrangère (y compris double nationalité) ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 291] <- factor(data[, 291], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[291] <- "international_natio"
# LimeSurvey Field type: A
data[, 292] <- as.character(data[, 292])
# attributes(data)$variable.labels[292] <- "[Avez-vous suivi au moins un an de votre scolarité primaire ou secondaire hors de France ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 292] <- factor(data[, 292], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[292] <- "international_scol"
# LimeSurvey Field type: A
data[, 293] <- as.character(data[, 293])
# attributes(data)$variable.labels[293] <- "[Avez-vous étudié (études supérieures) au moins trois mois hors de France ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 293] <- factor(data[, 293], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[293] <- "international_etudes"
# LimeSurvey Field type: A
data[, 294] <- as.character(data[, 294])
# attributes(data)$variable.labels[294] <- "[Avez-vous fait un ou plusieurs post-docs hors de France ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 294] <- factor(data[, 294], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[294] <- "international_postdoc"
# LimeSurvey Field type: A
data[, 295] <- as.character(data[, 295])
# attributes(data)$variable.labels[295] <- "[Avez-vous travaillé (dans l'enseignement et/ou la recherche mais hors post-doc) au moins trois mois hors de France ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 295] <- factor(data[, 295], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[295] <- "international_travail"
# LimeSurvey Field type: A
data[, 296] <- as.character(data[, 296])
# attributes(data)$variable.labels[296] <- "[Êtes-vous actuellement engagé·e dans un programme de recherche international ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 296] <- factor(data[, 296], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[296] <- "international_prog"
# LimeSurvey Field type: A
data[, 297] <- as.character(data[, 297])
# attributes(data)$variable.labels[297] <- "[Êtes-vous activement impliqué·e (animation, bureau…) dans une association professionnelle non française ou internationale ?] Votre rapport à l'international :  Que vous soyez français·e ou étranger·e, les questions vous concernent telles quelles : par exemple, que vous soyez français·e ou italien·ne, si vous avez grandi en Italie, répondez que vous avez bien effectué votre scolarité hors de France."
data[, 297] <- factor(data[, 297], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[297] <- "international_asso"
# LimeSurvey Field type: F
data[, 298] <- as.numeric(data[, 298])
# attributes(data)$variable.labels[298] <- "Depuis 2017 inclus, combien avez-vous approximativement publié d'articles (en tant qu'auteur·e ou ou co-auteur·e) dans des revues à comité de lecture ?"
# names(data)[298] <- "nbpublis"
# LimeSurvey Field type: F
data[, 299] <- as.numeric(data[, 299])
# attributes(data)$variable.labels[299] <- "Parmi ces articles, combien approximativement ont été publiés en langue anglaise ?"
# names(data)[299] <- "nbpublisang"
# LimeSurvey Field type: A
data[, 300] <- as.character(data[, 300])
# attributes(data)$variable.labels[300] <- "Connaissez-vous approximativement votre h-index?"
data[, 300] <- factor(data[, 300], levels=c("Oui","Non","NC","NSP"),labels=c("Oui", "Non", "Non concerné·e", "Je ne suis pas certain de ce qu'est le h-index"))
# names(data)[300] <- "hindexconn"
# LimeSurvey Field type: F
data[, 301] <- as.numeric(data[, 301])
# attributes(data)$variable.labels[301] <- "Pouvez-vous indiquer la valeur approximative de votre h-index ?"
# names(data)[301] <- "hindex"
# LimeSurvey Field type: A
data[, 302] <- as.character(data[, 302])
# attributes(data)$variable.labels[302] <- "Êtes-vous titulaire d'une thèse de doctorat ?"
data[, 302] <- factor(data[, 302], levels=c("Oui","Non"),labels=c("Oui", "Non"))
# names(data)[302] <- "these"
# LimeSurvey Field type: F
data[, 303] <- as.numeric(data[, 303])
# attributes(data)$variable.labels[303] <- "En quelle année l\'avez-vous soutenue ?"
# names(data)[303] <- "theseannee"
# LimeSurvey Field type: A
data[, 304] <- as.character(data[, 304])
# attributes(data)$variable.labels[304] <- "Êtes-vous à un moment de votre carrière où vous cherchez à être promu, recruté ou titularisé ?"
data[, 304] <- factor(data[, 304], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[304] <- "carriere"
# LimeSurvey Field type: A
data[, 305] <- as.character(data[, 305])
# attributes(data)$variable.labels[305] <- "Avez vous le sentiment d'être..."
data[, 305] <- factor(data[, 305], levels=c("A2","A3","A4","A5","A6"),labels=c("Très bien payé·e", "Bien payé·e", "Correctement payé·e", "Mal payé·e", "Très mal payé·e"))
# names(data)[305] <- "paie"
# LimeSurvey Field type: A
data[, 306] <- as.character(data[, 306])
# attributes(data)$variable.labels[306] <- "Êtes-vous actuellement employé·e à plein temps ?"
data[, 306] <- factor(data[, 306], levels=c("Oui","Non"),labels=c("Oui", "Non"))
# names(data)[306] <- "tpsplein"
# LimeSurvey Field type: A
data[, 307] <- as.character(data[, 307])
# attributes(data)$variable.labels[307] <- "Quelle est votre quotité ?"
data[, 307] <- factor(data[, 307], levels=c("A2","A3","A4","A5","A6"),labels=c("90 %", "80 %", "70 %", "60 %", "50 % ou moins"))
# names(data)[307] <- "tpsquotite"
# LimeSurvey Field type: A
data[, 308] <- as.character(data[, 308])
# attributes(data)$variable.labels[308] <- "Au cours des 3 dernières années, avez-vous interrompu votre activité de recherche (en raison d'un congé maternité, congé maladie, mise en disponibilité) pour une durée supérieure à 3 mois ?"
data[, 308] <- factor(data[, 308], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[308] <- "conge"
# LimeSurvey Field type: A
data[, 309] <- as.character(data[, 309])
# attributes(data)$variable.labels[309] <- "Vivez-vous en couple ?"
data[, 309] <- factor(data[, 309], levels=c("A1","A2"),labels=c("Oui", "Non"))
# names(data)[309] <- "couple"
# LimeSurvey Field type: F
data[, 310] <- as.numeric(data[, 310])
# attributes(data)$variable.labels[310] <- "Combien avez-vous d'enfants ?"
# names(data)[310] <- "enfantsnb"
# LimeSurvey Field type: F
data[, 311] <- as.numeric(data[, 311])
# attributes(data)$variable.labels[311] <- "Quel est l'âge du plus jeune ?"
# names(data)[311] <- "enfantsage"
# LimeSurvey Field type: A
data[, 312] <- as.character(data[, 312])
# attributes(data)$variable.labels[312] <- "[Mère] Quel est le diplôme le plus élevé détenu par vos parents ?"
data[, 312] <- factor(data[, 312], levels=c("A002","A003","A004","A005","A006","A007","A008"),labels=c("Aucun diplôme", "CAP, BEP, BEPC ou équivalent", "Bac ou équivalent", "Bac +2 ou 3", "Bac +4 ou 5", "Doctorat", "Ne sait pas"))
# names(data)[312] <- "dippar_m"
# LimeSurvey Field type: A
data[, 313] <- as.character(data[, 313])
# attributes(data)$variable.labels[313] <- "[Père] Quel est le diplôme le plus élevé détenu par vos parents ?"
data[, 313] <- factor(data[, 313], levels=c("A002","A003","A004","A005","A006","A007","A008"),labels=c("Aucun diplôme", "CAP, BEP, BEPC ou équivalent", "Bac ou équivalent", "Bac +2 ou 3", "Bac +4 ou 5", "Doctorat", "Ne sait pas"))
# names(data)[313] <- "dippar_p"
# LimeSurvey Field type: A
data[, 314] <- as.character(data[, 314])
# attributes(data)$variable.labels[314] <- "[Mère] Quand vous aviez 18 ans, quel était le statut d'emploi de vos parents ?"
data[, 314] <- factor(data[, 314], levels=c("A2","A3","A4","A5","A6","A7","A8"),labels=c("Fonctionnaire ou salarié·e du public", "Salarié·e du privé", "À son compte ou libéral", "Au chômage", "Inactif/Inactive ou retraité·e", "Décédé·e", "Ne sait pas"))
# names(data)[314] <- "statutpar_m"
# LimeSurvey Field type: A
data[, 315] <- as.character(data[, 315])
# attributes(data)$variable.labels[315] <- "[Père] Quand vous aviez 18 ans, quel était le statut d'emploi de vos parents ?"
data[, 315] <- factor(data[, 315], levels=c("A2","A3","A4","A5","A6","A7","A8"),labels=c("Fonctionnaire ou salarié·e du public", "Salarié·e du privé", "À son compte ou libéral", "Au chômage", "Inactif/Inactive ou retraité·e", "Décédé·e", "Ne sait pas"))
# names(data)[315] <- "statutpar_p"
# LimeSurvey Field type: A
data[, 316] <- as.character(data[, 316])
# attributes(data)$variable.labels[316] <- "Si vous additionnez toutes les sources de revenus de votre foyer, pouvez-vous indiquer le montant du revenu mensuel net après impôts et prélèvement à la source de votre ménage en 2019 ? Tenir compte de toutes les rentrées d'argent de toutes les personnes de votre foyer : salaires et revenus professionnels, pensions de retraite, allocations diverses, produits éventuels de propriétés ou placements financiers, etc."
data[, 316] <- factor(data[, 316], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12"),labels=c("Moins de 1 500 euros par mois", "De 1 500 à 2 499 euros par mois", "De 2 500 à 3 499 euros par mois", "De 3 500 à 4 499 euros par mois", "De 4 500 à 5 999 euros par mois", "De 6 000 à 7 999 euros par mois", "De 8 000 à 9 999 euros par mois", "De 10 000 à 15 000 euros par mois", "Plus de 15 000 par mois", "Je ne souhaite pas répondre", "Je ne sais pas"))
# names(data)[316] <- "revenu"
# LimeSurvey Field type: A
data[, 317] <- as.character(data[, 317])
# attributes(data)$variable.labels[317] <- "[Quelle est votre commune de résidence ?] Les deux questions suivantes nous permettront de calculer la taille de l'agglomération, le type de commune (rural/urbain) ainsi la distance entre votre lieu de résidence et votre lieu de travail. Vous êtes libre de ne pas répondre si vous le désirez. Nous nous engageons à détruire les informations concernant la commune exacte dans un délai d'un an."
# names(data)[317] <- "communes_SaisieVille"
# LimeSurvey Field type: A
data[, 318] <- as.character(data[, 318])
# attributes(data)$variable.labels[318] <- "[Quelle est la commune où se situe votre lieu de travail ?] Les deux questions suivantes nous permettront de calculer la taille de l'agglomération, le type de commune (rural/urbain) ainsi la distance entre votre lieu de résidence et votre lieu de travail. Vous êtes libre de ne pas répondre si vous le désirez. Nous nous engageons à détruire les informations concernant la commune exacte dans un délai d'un an."
# names(data)[318] <- "communes_SaisieVilleTravail"
# LimeSurvey Field type: A
data[, 319] <- as.character(data[, 319])
# attributes(data)$variable.labels[319] <- "Accepteriez-vous qu'un chercheur/une chercheuse vous recontacte dans quelques mois ou plus pour répondre à d'autres questions sur les thèmes abordés dans ce questionnaire ?"
data[, 319] <- factor(data[, 319], levels=c("-oth-","A1"),labels=c("J'accepte d'être recontacté·e", "Je n'accepte pas d'être recontacté·e"))
# names(data)[319] <- "recontact"
# LimeSurvey Field type: A
data[, 320] <- as.character(data[, 320])
# attributes(data)$variable.labels[320] <- "[Autre] Accepteriez-vous qu'un chercheur/une chercheuse vous recontacte dans quelques mois ou plus pour répondre à d'autres questions sur les thèmes abordés dans ce questionnaire ?"
# names(data)[320] <- "recontact_other"
# LimeSurvey Field type: A
data[, 321] <- as.character(data[, 321])
# attributes(data)$variable.labels[321] <- "Commentaires éventuels :"
# names(data)[321] <- "commentaires"
# LimeSurvey Field type: A
data[, 322] <- as.character(data[, 322])
# attributes(data)$variable.labels[322] <- "Merci pour votre participation à ce questionnaire. Nous apprécions grandement le temps que vous nous avez consacré.      Nous vous proposons de finir par un petit quiz ! Sauriez-vous estimer les émissions de gaz à effet de serre de différentes pratiques professionnelles ? Les bonnes réponses vous seront indiquées juste après l'avoir rempli."
data[, 322] <- factor(data[, 322], levels=c("Oui","Non"),labels=c("Je réponds au quiz", "Je décline le quiz"))
# names(data)[322] <- "quiz"
# LimeSurvey Field type: F
data[, 323] <- as.numeric(data[, 323])
# attributes(data)$variable.labels[323] <- "A votre avis, quel niveau devraient atteindre les émissions par habitant de la planète en 2030 pour limiter le réchauffement à +1,5 °C en 2100 ? Pour information, les émissions de gaz à effet de serre par habitant de la planète sont aujourd'hui de 7 tonnes équivalent CO2 par an, et les émissions moyennes d'un·e Français·e de 11 tonnes (empreinte carbone)."
# names(data)[323] <- "quizempreinte"
# LimeSurvey Field type: A
data[, 324] <- as.character(data[, 324])
# attributes(data)$variable.labels[324] <- "[Parcourir pendant un an 50 km en voiture pour aller et revenir du travail (soit environ 12 000 km)] Quelle quantité d\'émissions de gaz à effet de serre (en kg équivalent CO2) représentent les actions suivantes (sachant que l'empreinte carbone d'un·e Français·e est en moyenne de 30 kg par jour) ?  Indiquez la valeur qui vous semble la plus proche."
data[, 324] <- factor(data[, 324], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14"),labels=c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg"))
# names(data)[324] <- "quizfacteurs_voiture"
# LimeSurvey Field type: A
data[, 325] <- as.character(data[, 325])
# attributes(data)$variable.labels[325] <- "[Faire un aller-retour Paris – New York en avion (soit environ 12 000 km) ] Quelle quantité d\'émissions de gaz à effet de serre (en kg équivalent CO2) représentent les actions suivantes (sachant que l'empreinte carbone d'un·e Français·e est en moyenne de 30 kg par jour) ?  Indiquez la valeur qui vous semble la plus proche."
data[, 325] <- factor(data[, 325], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14"),labels=c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg"))
# names(data)[325] <- "quizfacteurs_avion"
# LimeSurvey Field type: A
data[, 326] <- as.character(data[, 326])
# attributes(data)$variable.labels[326] <- "[Faire un aller-retour Paris-Marseille en TGV] Quelle quantité d\'émissions de gaz à effet de serre (en kg équivalent CO2) représentent les actions suivantes (sachant que l'empreinte carbone d'un·e Français·e est en moyenne de 30 kg par jour) ?  Indiquez la valeur qui vous semble la plus proche."
data[, 326] <- factor(data[, 326], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14"),labels=c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg"))
# names(data)[326] <- "quizfacteurs_TGV"
# LimeSurvey Field type: A
data[, 327] <- as.character(data[, 327])
# attributes(data)$variable.labels[327] <- "[Fabriquer un ordinateur portable neuf] Quelle quantité d\'émissions de gaz à effet de serre (en kg équivalent CO2) représentent les actions suivantes (sachant que l'empreinte carbone d'un·e Français·e est en moyenne de 30 kg par jour) ?  Indiquez la valeur qui vous semble la plus proche."
data[, 327] <- factor(data[, 327], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14"),labels=c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg"))
# names(data)[327] <- "quizfacteurs_ordi"
# LimeSurvey Field type: A
data[, 328] <- as.character(data[, 328])
# attributes(data)$variable.labels[328] <- "[Faire 3 heures de visioconférence avec votre ordinateur (pour une personne)] Quelle quantité d\'émissions de gaz à effet de serre (en kg équivalent CO2) représentent les actions suivantes (sachant que l'empreinte carbone d'un·e Français·e est en moyenne de 30 kg par jour) ?  Indiquez la valeur qui vous semble la plus proche."
data[, 328] <- factor(data[, 328], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14"),labels=c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg"))
# names(data)[328] <- "quizfacteurs_visio"
# LimeSurvey Field type: A
data[, 329] <- as.character(data[, 329])
# attributes(data)$variable.labels[329] <- "[Imprimer une thèse de 200 pages en 10 exemplaires, recto-verso] Quelle quantité d\'émissions de gaz à effet de serre (en kg équivalent CO2) représentent les actions suivantes (sachant que l'empreinte carbone d'un·e Français·e est en moyenne de 30 kg par jour) ?  Indiquez la valeur qui vous semble la plus proche."
data[, 329] <- factor(data[, 329], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14"),labels=c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg"))
# names(data)[329] <- "quizfacteurs_these"
# LimeSurvey Field type: A
data[, 330] <- as.character(data[, 330])
# attributes(data)$variable.labels[330] <- "[Produire un steak de bœuf de 150 g] Quelle quantité d\'émissions de gaz à effet de serre (en kg équivalent CO2) représentent les actions suivantes (sachant que l'empreinte carbone d'un·e Français·e est en moyenne de 30 kg par jour) ?  Indiquez la valeur qui vous semble la plus proche."
data[, 330] <- factor(data[, 330], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14"),labels=c("10 g", "100 g", "1 kg", "5 kg", "25 kg", "50 kg", "100 kg", "250 kg", "500 kg", "1 000 kg", "2 000 kg", "3 000 kg", "5 000 kg"))
# names(data)[330] <- "quizfacteurs_steak"
# Variable name was incorrect and was changed from  to q_ .

