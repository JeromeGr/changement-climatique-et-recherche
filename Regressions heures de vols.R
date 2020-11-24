library(texreg)

#Régressions sur les heures de vols

#Je vire ceux dont la ligne est totalement vide
#Peut être rajouter des conditions
# Et je vire le troll
climat$nbpublisang<-as.numeric(climat$nbpublisang)
climat<-climat %>% filter(!(sexe=="" & age=="" & statut=="" & employeur=="" & changclim=="" & preoccupe==""))
# Et je vire le troll (je sais pas pourquoi il faut rajouter de garder les NAs sinon on les perd)
climat<-climat %>% filter((nbpublisang!=666 | is.na(nbpublisang)))

################################
#Recodage pour la régression
freq(climat$solevolges.conf.)
climat$Evol_GesVol.conf.[climat$solevolges.conf.=="Été à peu près stables"]<-0
climat$Evol_GesVol.conf.[climat$solevolges.conf.=="Fortement augmenté"]<-3
climat$Evol_GesVol.conf.[climat$solevolges.conf.=="Un peu augmenté"]<-1
climat$Evol_GesVol.conf.[climat$solevolges.conf.=="Fortement diminué"]<--3
climat$Evol_GesVol.conf.[climat$solevolges.conf.=="Un peu diminué"]<--1
freq(climat$Evol_GesVol.conf.)

#REvenu : on agrège les catégories avec peu de monde
climat$revenuAgr<-climat$revenu
climat$revenuAgr[climat$revenu %in% c("De 10 000 à 15 000 euros par mois", "Plus de 15 000 par mois", "De 8 000 à 9 999 euros par mois")]<-"Au moins 8000 euros par mois"

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
  "Médecine, pharma, santé"="42 : Morphologie et morphogenèse",
  "Médecine, pharma, santé"="43 : Biophysique et imagerie médicale",
  "Médecine, pharma, santé"="44 : Biochimie, biologie cellulaire et moléculaire, physiologie et nutrition",
  "Médecine, pharma, santé"="45 : Microbiologie, maladies transmissibles et hygiène",
  "Médecine, pharma, santé"="46 : Santé publique, environnement et société",
  "Médecine, pharma, santé"="47 : Cancérologie, génétique, hématologie, immunologie",
  "Médecine, pharma, santé"="48 : Anesthésiologie, réanimation, médecine d'urgence, pharmacologie et thérapeutique",
  "Médecine, pharma, santé"="49 : Pathologie nerveuse et musculaire, pathologie mentale, handicap et rééducation",
  "Médecine, pharma, santé"="50 : Pathologie ostéo-articulaire, dermatologie et chirurgie plastique",
  "Médecine, pharma, santé"="51 : Pathologie cardiorespiratoire et vasculaire",
  "Médecine, pharma, santé"="52 : Maladies des appareils digestif et urinaire",
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
  "Biologie des populations et écologie"="67 : Biologie des populations et écologie",
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


#######################
#Modalités de référence dans les régressions
climat$sexe <- as.factor(climat$sexe)
climat$sexe <- relevel(climat$sexe, ref = "un homme")

climat$ageAgr <- as.factor(climat$ageAgr)
climat$ageAgr <- relevel(climat$ageAgr, ref = "50-54 ans")

climat$couple <- as.factor(climat$couple)
climat$couple <- relevel(climat$couple, ref = "Oui")

climat$revenuAgr <- as.factor(climat$revenuAgr)
climat$revenuAgr <- relevel(climat$revenuAgr, ref = "De 4 500 à 5 999 euros par mois")

climat$sitpro <- relevel(climat$sitpro, ref = "Maître·sse de conférences")

climat$discipline_agregee <- relevel(climat$discipline_agregee , ref = "Sciences (1)") 
climat$discipline_agr3 <- relevel(climat$discipline_agr3 , ref = "Physique")

climat$discipline <- as.factor(climat$discipline)
climat$discipline <- relevel(climat$discipline , ref = "25 : Mathématiques")

climat$carriere <- as.factor(climat$carriere)
climat$carriere <- relevel(climat$carriere , ref = "Non")

climat$Profin_Mb_Resp <- as.factor(climat$Profin_Mb_Resp)
climat$Profin_Mb_Resp <- relevel(climat$Profin_Mb_Resp , ref = "Ni membre ni resp d'un 1 projet financé")

climat$solinstit.limitevols. <- as.factor(climat$solinstit.limitevols.)
climat$solinstit.limitevols. <- relevel(climat$solinstit.limitevols. , ref = "C’est prioritaire")

climat$solinstit.vols6h. <- as.factor(climat$solinstit.vols6h.)
climat$solinstit.vols6h. <- relevel(climat$solinstit.vols6h. , ref = "C’est prioritaire")

climat$solinstit.train. <- as.factor(climat$solinstit.train.)
climat$solinstit.train. <- relevel(climat$solinstit.train. , ref = "C’est prioritaire")

climat$solrisqreducavion.qual. <- as.factor(climat$solrisqreducavion.qual.)
climat$solrisqreducavion.qual. <- relevel(climat$solrisqreducavion.qual. , ref = "C’est peu probable")
climat$solrisqreducavion.fin. <- as.factor(climat$solrisqreducavion.fin.)
climat$solrisqreducavion.fin. <- relevel(climat$solrisqreducavion.fin. , ref = "C’est peu probable")
climat$solrisqreducavion.diffusion. <- as.factor(climat$solrisqreducavion.diffusion.)
climat$solrisqreducavion.diffusion. <- relevel(climat$solrisqreducavion.diffusion. , ref = "C’est peu probable")
climat$solrisqreducavion.donnees. <- as.factor(climat$solrisqreducavion.donnees.)
climat$solrisqreducavion.donnees. <- relevel(climat$solrisqreducavion.donnees. , ref = "C’est peu probable")
climat$solrisqreducavion.avantages. <- as.factor(climat$solrisqreducavion.avantages.)
climat$solrisqreducavion.avantages. <- relevel(climat$solrisqreducavion.avantages. , ref = "C’est peu probable")
climat$solrisqreducavion.insertion. <- as.factor(climat$solrisqreducavion.insertion.)
climat$solrisqreducavion.insertion. <- relevel(climat$solrisqreducavion.insertion. , ref = "C’est peu probable")
climat$solrisqreducavion.isoler. <- as.factor(climat$solrisqreducavion.isoler.)
climat$solrisqreducavion.isoler. <- relevel(climat$solrisqreducavion.isoler. , ref = "C’est peu probable")
climat$solrisqreducavion.bureaucratie. <- as.factor(climat$solrisqreducavion.bureaucratie.)
climat$solrisqreducavion.bureaucratie. <- relevel(climat$solrisqreducavion.bureaucratie. , ref = "C’est peu probable")

climat$paie <- as.factor(climat$paie)
climat$paie <- relevel(climat$paie , ref = "Mal payé·e")

climat$employeur <- as.factor(climat$employeur)
climat$employeur <- relevel(climat$employeur , ref = "Une université")

##########################################
#Régressions
mean(climat$volshnum, na.rm=T)
freq(climat$sitpro)
#Tout le monde
#Premier doc "all staff"
res.reg1 <- lm(volshnum ~ sexe + ageAgr, data=climat)
res.reg2<- lm(volshnum ~ sexe + ageAgr  + sitpro, data=climat)
res.reg3<- lm(volshnum ~ sexe + ageAgr  + sitpro + revenuTete  , data=climat)
res.reg4<- lm(volshnum ~ sexe + ageAgr  + sitpro + enfantsnb + couple , data=climat)
res.reg5 <- lm(volshnum ~ sexe + ageAgr  + discipline_agr3 , data=climat)
res.reg6 <- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3, data=climat)
res.reg7 <- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + nbpublis, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + revenuTete + enfantsnb + couple , data=climat)


freq(climat$avionperso)

#Sous discipline Socio pour comparer enquête socio
climatSocio<-climat %>% filter(discipline_agr3=="Socio, démo")
res.reg9<- lm(volshnum ~ sexe + ageAgr  + sitpro + revenuTete + enfantsnb + couple , data=climatSocio)

htmlreg(list(res.reg1, res.reg2, res.reg3, res.reg4, res.reg5,res.reg6, res.reg7, res.reg8, res.reg9), 
        stars = c(0.001, 0.01, 0.05, 0.1), digits = 2, #single.row = TRUE,
        custom.header = list("All staff" = 1:8, "Socio" = 9),
        custom.coef.map = list("sexeune femme"= "Woman (ref = man)",
                               "sexeautre"="Sex : other",
                               "ageAgrMoins de 29 ans"= "less than 29 years old (Ref = 50-54 years old",
                               "ageAgr30-34 ans"= "30-34 years old",
                               "ageAgr35-39 ans"= "35-39 years old",
                               "ageAgr40-44 ans"= "40-44 years old",
                               "ageAgr45-49 ans"= "45-49 years old",
                               "ageAgr55-64 ans"= "55-64 years old",
                               "ageAgr65 ans et plus"= "More than 65 years old",
                               "sitproDirecteur·rice de recherche"="Directeur·rice de recherche (Ref = Maître·sse de conférences)",
                               "sitproProfesseur·e des universités"= "University professor ",
                               "sitproChargé·e de recherche"="Researcher",
                               "sitproMaître·sse de conférences"="Lecturer",
                               "sitproPost-doctorant·e"= "Post doctoral student",
                               "sitproATER"="ATER",
                               "sitproDoctorant·e contractuel·le"="contractual doctoral student",
                               "sitproDoctorant·e CIFRE"= "CIFRE contractual doctoral student",
                               "sitproIngénieur·e de recherche"="Research engineer",
                               "sitproIngénieur·e d'études"="Design engineer",
                               "sitproAssistant ingénieur·e"="Assistant engineer",
                               "sitproTechnicien·ne"="Technician",
                               "sitproChargé·e d’études/de mission"= "Research officer",
                               "sitproAdjoint·e technique"="Technical assistant",
                               "sitproAutre"="Other",
                               "discipline_agr3Droit, économie, gestion"="Droit, économie, gestion (Ref = physique)",
                               "discipline_agr3Autres lettres et sciences humaines"="Autres lettres et sciences humaines",
                               "discipline_agr3Archi/arts, anthropo ethno"="Archi/arts, anthropo ethno",
                               "discipline_agr3Socio, démo"="Socio, démo",
                               "discipline_agr3Histoire, géo, urba"="Histoire, géo, urba",
                               "discipline_agr3Mathématiques"="Mathématiques",
                               "discipline_agr3Informatique"="Informatique",
                               "discipline_agr3Chimie"="Chimie",
                               "discipline_agr3Astro, géologie"="Astro, géologie",
                               "discipline_agr3Météo, océano, physiqu environt"="Météo, océano, physiqu environt",
                               "discipline_agr3Médecine, pharma, santé"="Médecine, pharma, santé",
                               "discipline_agr3Génies : méca, info, élec, énergie"="Génies : méca, info, élec, énergie",
                               "discipline_agr3Biologie"="Biologie",
                               "discipline_agr3Biologie des populations et écologie"="Biologie des populations et écologie",
                               "nbpublis"="Number of publications in 2017-mid2020",
                               "revenuTete"="Revenu par individu du foyer",
                               "enfantsnb"= "nombre d'enfants",
                               "coupleNon"="ne vit pas en couple"),
        symbol = "+",
        caption = "Tableau 1 : Régressions linéaires multiples sur le nombre d'heures de vol en 2019", caption.above=TRUE, 
        single.row = TRUE, 
        #custom.gof.rows = NULL,
        file="/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Regressions duree de vol agr 2019, discip, sitpro, rev.doc")

#Employeur
res.reg8<- lm(volshnum ~ sexe + ageAgr  + employeur, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 , data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 +  employeur, data=climat)
summary(res.reg8)

#Pratiques perso avion
freq(climat$avionperso)
freq(climat$avionpersochgt)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro + paie, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro + avionperso, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro + avionperso + revenuTete, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro + avionpersochgt, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro + avionperso + avionpersochgt, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro + avionperso + avionpersochgt + revenuTete, data=climat)
summary(res.reg9)

#Score écolo
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + ScoreEcolo, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro + ScoreEcolo , data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro +discipline_agr3 + ScoreEcolo , data=climat)

#Etape dans la carrière, projet financé (type ; resp/membre),
res.reg1<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3, data=climat)
res.reg2<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + carriere , data=climat)
res.reg3<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + nbpublis, data=climat)
res.reg4<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + nbpublis + carriere, data=climat)
res.reg5<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive , data=climat)
res.reg6<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive + nbpublis , data=climat)
res.reg7<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive + nbpublis + carriere , data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + Profin_Mb_Resp , data=climat)
res.reg9<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + Profin_Mb_Resp + nbpublis + carriere , data=climat)
res.reg10<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + projets.anr_m + projets.anr_r + projets.france_m + projets.france_r + projets.europe_m + projets.europe_r + projets.inter_m + projets.inter_r +projets.prive_m + projets.prive_r, data=climat)
res.reg11<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + nbpublis + carriere + projets.anr_m + projets.anr_r + projets.france_m + projets.france_r + projets.europe_m + projets.europe_r + projets.inter_m + projets.inter_r +projets.prive_m + projets.prive_r, data=climat)

summary(res.reg10)

freq(climat$projet.anr_m)
freq(climat$sexe, na.rm=TRUE)
?freq
htmlreg(list(res.reg1, res.reg2, res.reg3, res.reg4, res.reg5,res.reg6, res.reg7, res.reg8, res.reg9, res.reg10, res.reg11), 
        stars = c(0.001, 0.01, 0.05, 0.1), digits = 2, #single.row = TRUE,
        custom.coef.map = list("sexeune femme"= "Woman (ref = man)",
                               "sexeautre"="Sex : other",
                               "ageAgrMoins de 29 ans"= "less than 29 years old (Ref = 50-54 years old",
                               "ageAgr30-34 ans"= "30-34 years old",
                               "ageAgr35-39 ans"= "35-39 years old",
                               "ageAgr40-44 ans"= "40-44 years old",
                               "ageAgr45-49 ans"= "45-49 years old",
                               "ageAgr55-64 ans"= "55-64 years old",
                               "ageAgr65 ans et plus"= "More than 65 years old",
                               "sitproDirecteur·rice de recherche"="Directeur·rice de recherche (Ref = Maître·sse de conférences)",
                               "sitproProfesseur·e des universités"= "University professor ",
                               "sitproChargé·e de recherche"="Researcher",
                               "sitproMaître·sse de conférences"="Lecturer",
                               "sitproPost-doctorant·e"= "Post doctoral student",
                               "sitproATER"="ATER",
                               "sitproDoctorant·e contractuel·le"="contractual doctoral student",
                               "sitproDoctorant·e CIFRE"= "CIFRE contractual doctoral student",
                               "sitproIngénieur·e de recherche"="Research engineer",
                               "sitproIngénieur·e d'études"="Design engineer",
                               "sitproAssistant ingénieur·e"="Assistant engineer",
                               "sitproTechnicien·ne"="Technician",
                               "sitproChargé·e d’études/de mission"= "Research officer",
                               "sitproAdjoint·e technique"="Technical assistant",
                               "sitproAutre"="Other",
                               "discipline_agr3Droit, économie, gestion"="Droit, économie, gestion (Ref = physique)",
                               "discipline_agr3Autres lettres et sciences humaines"="Autres lettres et sciences humaines",
                               "discipline_agr3Archi/arts, anthropo ethno"="Archi/arts, anthropo ethno",
                               "discipline_agr3Socio, démo"="Socio, démo",
                               "discipline_agr3Histoire, géo, urba"="Histoire, géo, urba",
                               "discipline_agr3Mathématiques"="Mathématiques",
                               "discipline_agr3Informatique"="Informatique",
                               "discipline_agr3Chimie"="Chimie",
                               "discipline_agr3Astro, géologie"="Astro, géologie",
                               "discipline_agr3Météo, océano, physiqu environt"="Météo, océano, physiqu environt",
                               "discipline_agr3Médecine, pharma, santé"="Médecine, pharma, santé",
                               "discipline_agr3Génies : méca, info, élec, énergie"="Génies : méca, info, élec, énergie",
                               "discipline_agr3Biologie"="Biologie",
                               "discipline_agr3Biologie des populations et écologie"="Biologie des populations et écologie",
                               "nbpublis"="Number of publications in 2017-mid2020",
                               "nbpublisang"="Number of publications in english in 2017-mid2020",
                               "particip_ANR"="Participe à projet(s) financé ANR",
                               "particip_Fr"="Participe à projet(s) av financt public Fr",
                               "particip_Europ"="Participe à projet(s) av financt europ",
                               "particip_Intern"="Participe à projet(s) av financt internation",
                               "particip_prive"="Participe à projet(s) av financt privé",
                               "Profin_Mb_RespMembre d'au moins 1 projet financé"="Membre de projet(s) financé(s) (ref = Ni mb ni responsable)",
                               "Profin_Mb_RespResponsable d'au moins 1 projet financé"="Responsable de projet(s) financé(s)",
                               "carriereOui"="Cherche à être promu, recrut, titularisé",
                               "projets.anr_mMembre projet ANR oui"="Membre projet(s) financt ANR (Ref = Ne participe pas)",
                               "projets.anr_rResponsable projet ANR oui"="Responsable projet(s) financt ANR",
                               "projets.france_mMembre projet France oui"="Membre projet(s) financt France (Ref = Ne participe pas)",
                               "projets.france_rResponsable projet France oui"="Responsable projet(s) financt France",
                               "projets.europe_mMembre projet européen oui"="Membre projet(s) financt européen (Ref = Ne participe pas)",
                               "projets.europe_rResponsable projet européen oui"="Responsable projet(s) financt européen",
                               "projets.inter_mMembre projet international oui"="Membre projet(s) financt international (Ref = Ne participe pas)",
                               "projets.inter_rResponsable projet international oui"="Responsable projet(s) financt international",
                               "projets.prive_mMembre projet privé oui"="Membre projet(s) financt privé (Ref = Ne participe pas)",
                               "projets.prive_rResponsable projet privé oui"="Responsable projet(s) financt privé",
                               "revenuTete"="Revenu par individu du foyer",
                               "enfantsnb"= "nombre d'enfants",
                               "coupleNon"="ne vit pas en couple"),
        symbol = "+",
        caption = "Tableau 2 : Régressions linéaires multiples sur le nombre d'heures de vol en 2019", caption.above=TRUE, 
        single.row = TRUE, 
        #custom.gof.rows = NULL,
        file="/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Reg duree de vol agr 2019, carr, publi, financt.doc")


#Corrélation avec les variables d'opinion
#Solutions
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solinstit.limitevols., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solinstit.vols6h., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solinstit.train., data=climat)
summary(res.reg8)
#Risques
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.qual., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.fin., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.diffusion., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.donnees., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.avantages., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.isoler., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.insertion., data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.bureaucratie., data=climat)

res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agr3 + solrisqreducavion.avantages. + paie, data=climat)
summary(res.reg8)



freq(climat$solinstit.train.)
freq(climat$solinstit.vols6h.)
rprop(table(climat$solinstit.train.))


rbind(prop.table(table(climat$solinstit.train.)), prop.table(table(climat$solinstit.vols6h.)))






#Evolution de la quantité de vols
res.reg8<- lm(Evol_GesVol.conf. ~ sexe + ageAgr  + sitpro + discipline_agr3 , data=climat)
res.reg8<- lm(Evol_GesVol.conf. ~ sexe + ageAgr  + sitpro + discipline_agr3 +volshnum, data=climat)

mean(climat$Evol_GesVol.conf., na.rm=T)


summary(res.reg8)


climat$Evol_GesVol.conf.

#Personnel en position de publier

climatPersPubli<-climat %>% filter(!(sitpro %in% c("Technicien·ne", "Adjoint·e technique", "Autre")))


#############################################################################@@@
#################################################################################@@@
#Rebus
"discipline_agregeeDroit, économie, gestion"="Droit, économie, gestion (Ref = Sciences (1))",
"discipline_agregeeLettres et sciences humaines (1)"="Lettres et sciences humaines (1)",
"discipline_agregeeMédecine, odontologie" ="Médecine, odontologie",
"discipline_agregeeSciences (2)"="Sciences (2)",
"discipline_agregeeLettres et sciences humaines (2)"="Lettres et sciences humaines (2)",
"discipline_agregeePharmacie"="Pharmacie",
"discipline_agregeeAutres santé"="Autres santé",

climat$Part_ANR_ERC[climat$projets.anr_r. %in% c(0, NA) & climat$projets.anr_m. %in% c(0, NA) & climat$projets.france_r. %in% c(0, NA) & climat$projets.france_m. %in% c(0, NA) 
                    & climat$projets.europe_r. %in% c(0, NA) & climat$projets.europe_m. %in% c(0, NA) & climat$projets.inter_r.  %in% c(0, NA) & climat$projets.inter_m. %in% c(0, NA) 
                    &   climat$projets.prive_r.  %in% c(0, NA) & climat$projets.prive_m. %in% c(0, NA)]<-"Ne participe à aucun projet financé"
climat$Part_ANR_ERC[(climat$projets.anr_r. ==1 | climat$projets.anr_m. ==1) & climat$projets.france_r. %in% c(0, NA) & climat$projets.france_m. %in% c(0, NA) 
                    & climat$projets.europe_r. %in% c(0, NA) & climat$projets.europe_m. %in% c(0, NA) & climat$projets.inter_r.  %in% c(0, NA) & climat$projets.inter_m. %in% c(0, NA) 
                    &   climat$projets.prive_r.  %in% c(0, NA) & climat$projets.prive_m. %in% c(0, NA)]<-"Seult finance ANR"
climat$Part_ANR_ERC[climat$projets.anr_r. %in% c(0, NA) & climat$projets.anr_m. %in% c(0, NA) & (climat$projets.france_r.==1 | climat$projets.france_m.==1) 
                    & climat$projets.europe_r. %in% c(0, NA) & climat$projets.europe_m. %in% c(0, NA) & climat$projets.inter_r.  %in% c(0, NA) & climat$projets.inter_m. %in% c(0, NA) 
                    &   climat$projets.prive_r.  %in% c(0, NA) & climat$projets.prive_m. %in% c(0, NA)]<-"Seult finance français (hors ANR)"
climat$Part_ANR_ERC[climat$projets.anr_r. %in% c(0, NA) & climat$projets.anr_m. %in% c(0, NA) & climat$projets.france_r. %in% c(0, NA) & climat$projets.france_m. %in% c(0, NA) 
                    & (climat$projets.europe_r. ==1 | climat$projets.europe_m. ==1) & climat$projets.inter_r.  %in% c(0, NA) & climat$projets.inter_m. %in% c(0, NA) 
                    &   climat$projets.prive_r.  %in% c(0, NA) & climat$projets.prive_m. %in% c(0, NA)]<-"Seult finance européen"
climat$Part_ANR_ERC[climat$projets.anr_r. %in% c(0, NA) & climat$projets.anr_m. %in% c(0, NA) & climat$projets.france_r. %in% c(0, NA) & climat$projets.france_m. %in% c(0, NA) 
                    & climat$projets.europe_r. %in% c(0, NA) & climat$projets.europe_m. %in% c(0, NA) & (climat$projets.inter_r. ==1 | climat$projets.inter_m. ==1) 
                    &   climat$projets.prive_r.  %in% c(0, NA) & climat$projets.prive_m. %in% c(0, NA)]<-"Seult finance internat (hors europe)"
climat$Part_ANR_ERC[climat$projets.anr_r. %in% c(0, NA) & climat$projets.anr_m. %in% c(0, NA) & climat$projets.france_r. %in% c(0, NA) & climat$projets.france_m. %in% c(0, NA) 
                    & climat$projets.europe_r. %in% c(0, NA) & climat$projets.europe_m. %in% c(0, NA) & climat$projets.inter_r.  %in% c(0, NA) & climat$projets.inter_m. %in% c(0, NA) 
                    &   (climat$projets.prive_r.==1 | climat$projets.prive_m. ==1)]<-"Seult finance privé"

#Regroupement participation à projet financé (attention, il y a une erreur : les 0 sont comptés comme des 1 avec ce code, or ils devraient être regroupés avec les NA)
#climat$Part_ANR_ERC[is.na(climat$projets.anr_r.) & is.na(climat$projets.anr_m.) & is.na(climat$projets.europe_r.) 
#                    & is.na(climat$projets.europe_m.)]<-"Ni financement ANR ou Europe"
#climat$Part_ANR_ERC[(!is.na(climat$projets.anr_r.) | !is.na(climat$projets.anr_m.)) & is.na(climat$projets.europe_r.) 
#                    & is.na(climat$projets.europe_m.)]<-"Projet ANR"
#climat$Part_ANR_ERC[is.na(climat$projets.anr_r.) & is.na(climat$projets.anr_m.) & (!is.na(climat$projets.europe_r.) 
#                    | !is.na(climat$projets.europe_m.))]<-"Projet européen"               
#climat$Part_ANR_ERC[(!is.na(climat$projets.anr_r.) | !is.na(climat$projets.anr_m.)) & (!is.na(climat$projets.europe_r.) 
#               | !is.na(climat$projets.europe_m.))]<-"Projet ANR et projet européen"             


"Part_ANR_ERCProjet ANR"="Participation projet ANR (Ref = ni projet ANR ni européen)",
"Part_ANR_ERCProjet européen"="Participation projet européen",
"Part_ANR_ERCProjet ANR et projet européen" ="Participation projet ANR et projet européen",
"revenuAgrDe 1 500 à 2 499 euros par mois"="1 500 à 2 499 euros par mois",
