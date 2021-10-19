library(texreg)
library(dplyr)
library(ggplot2)
library(GGally)

#Juste les vols extérieurs
climatRegr$volsnb_ext<-climatRegr$volsnb_tot-climatRegr$volsnbFrance

climatRegr_recherche <- climatRegr[!climatRegr$sitpro2 %in% c(
        "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
        "Chargé·e d'études/de mission","Adjoint·e technique",
        "Autre"
),]

################################
#Recodage pour la régression
freq(climatRegr$solevolges.conf)
climatRegr$Evol_GesVol.conf.[climatRegr$solevolges.conf=="Été à peu près stables"]<-0
climatRegr$Evol_GesVol.conf.[climatRegr$solevolges.conf=="Fortement augmenté"]<-3
climatRegr$Evol_GesVol.conf.[climatRegr$solevolges.conf=="Un peu augmenté"]<-1
climatRegr$Evol_GesVol.conf.[climatRegr$solevolges.conf=="Fortement diminué"]<--3
climatRegr$Evol_GesVol.conf.[climatRegr$solevolges.conf=="Un peu diminué"]<--1
freq(climatRegr$Evol_GesVol.conf)

#Nombre de vol en avion perso en numérique (approximation)

climatRegr_recherche$avionpersonum[climatRegr_recherche$avionperso=="Aucun aller-retour"]<-0
climatRegr_recherche$avionpersonum[climatRegr_recherche$avionperso=="1 ou 2 allers-retours"]<-1.5
climatRegr_recherche$avionpersonum[climatRegr_recherche$avionperso=="3 ou 4 allers-retours"]<-3.5
climatRegr_recherche$avionpersonum[climatRegr_recherche$avionperso=="Plus de 5 allers-retours"]<-6





##########################################
#Régressions
mean(climatRegr$volshnum, na.rm=T)
freq(climatRegr$sitpro)
#Tout le monde
#Premier doc "all staff"
res.reg1 <- lm(volshnum ~ sexe + ageAgr, data=climatRegr)
res.reg2<- lm(volshnum ~ sexe + ageAgr  + sitpro2, data=climatRegr)
res.reg3<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + revenuTete  , data=climatRegr)
res.reg4<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + enfantsnb + couple , data=climatRegr)
res.reg5 <- lm(volshnum ~ sexe + ageAgr  + discipline_agr3 , data=climatRegr)
res.reg6 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climatRegr)
res.reg7 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete + enfantsnb + couple , data=climatRegr)

#Employeur
res.reg8<- lm(volshnum ~ sexe + ageAgr  + employeur, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  employeur, data=climatRegr)
summary(res.reg8)

#Pratiques perso avion
freq(climatRegr$avionperso)
freq(climatRegr$avionpersochgt)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + paie, data=climatRegr)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionperso, data=climatRegr)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionperso + revenuTete, data=climatRegr)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionpersochgt, data=climatRegr)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt, data=climatRegr)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt + revenuTete, data=climatRegr)


#Nb de vol (et juste pour le personnel de recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionpersochgt , data=climatRegr_recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionperso , data=climatRegr_recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionpersochgt + revenuTete, data=climatRegr_recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt, data=climatRegr_recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt + revenuTete, data=climatRegr_recherche)




#Score écolo
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + ScoreEcolo, data=climatRegr)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + ScoreEcolo , data=climatRegr)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 +discipline_agr3 + ScoreEcolo , data=climatRegr)

#Etape dans la carrière, projet financé (type ; resp/membre),
res.reg1<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climatRegr)
res.reg2<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + carriere , data=climatRegr)
res.reg3<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis, data=climatRegr)
res.reg4<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis + carriere, data=climatRegr)
res.reg5<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive , data=climatRegr)
res.reg6<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive + nbpublis , data=climatRegr)
res.reg7<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive + nbpublis + carriere , data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + Profin_Mb_Resp , data=climatRegr)
res.reg9<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + Profin_Mb_Resp + nbpublis + carriere , data=climatRegr)
res.reg10<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2, data=climatRegr)
res.reg11<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis + carriere + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2, data=climatRegr)

summary(res.reg10)

freq(climatRegr$projet.anr_m)
freq(climatRegr$sexe, na.rm=TRUE)
?freq
htmlreg(list(res.reg1, res.reg2, res.reg3, res.reg4, res.reg5,res.reg6, res.reg7, res.reg8, res.reg9, res.reg10, res.reg11), 
        stars = c(0.001, 0.01, 0.05, 0.1), digits = 2, #single.row = TRUE,
        custom.coef.map = list("sexeune femme"= "Woman (ref = man)",
                               "sexeautre"="Sex : other",
                               "ageAgrMoins de 29 ans"= "less than 29 ans (Ref = 50-54 ans",
                               "ageAgr30-34 ans"= "30-34 years old",
                               "ageAgr35-39 ans"= "35-39 years old",
                               "ageAgr40-44 ans"= "40-44 years old",
                               "ageAgr45-49 ans"= "45-49 years old",
                               "ageAgr55-64 ans"= "55-64 years old",
                               "ageAgr65 ans et plus"= "More than 65 years old",
                               "sitpro2Directeur·rice de recherche"="Directeur·rice de recherche (Ref = Maître·sse de conférences)",
                               "sitpro2Professeur·e des universités"= "University professor ",
                               "sitpro2Chargé·e de recherche"="Researcher",
                               "sitpro2Maître·sse de conférences"="Lecturer",
                               "sitpro2Post-doctorant·e"= "Post doctoral student",
                               "sitpro2ATER"="ATER",
                               "sitpro2Doctorant·e contractuel·le"="contractual doctoral student",
                               "sitpro2Doctorant·e CIFRE"= "CIFRE contractual doctoral student",
                               "sitpro2Ingénieur·e de recherche"="Research engineer",
                               "sitpro2Ingénieur·e d'études"="Design engineer",
                               "sitpro2Assistant ingénieur·e"="Assistant engineer",
                               "sitpro2Technicien·ne"="Technician",
                               "sitpro2Chargé·e d'études/de mission"= "Research officer",
                               "sitpro2Adjoint·e technique"="Technical assistant",
                               "sitpro2Autre"="Other",
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
                               "projets.anr_m2Membre projet ANR oui"="Membre projet(s) financt ANR (Ref = Ne participe pas)",
                               "projets.anr_r2Responsable projet ANR oui"="Responsable projet(s) financt ANR",
                               "projets.france_m2Membre projet France oui"="Membre projet(s) financt France (Ref = Ne participe pas)",
                               "projets.france_r2Responsable projet France oui"="Responsable projet(s) financt France",
                               "projets.europe_m2Membre projet européen oui"="Membre projet(s) financt européen (Ref = Ne participe pas)",
                               "projets.europe_r2Responsable projet européen oui"="Responsable projet(s) financt européen",
                               "projets.inter_m2Membre projet international oui"="Membre projet(s) financt international (Ref = Ne participe pas)",
                               "projets.inter_r2Responsable projet international oui"="Responsable projet(s) financt international",
                               "projets.prive_m2Membre projet privé oui"="Membre projet(s) financt privé (Ref = Ne participe pas)",
                               "projets.prive_r2Responsable projet privé oui"="Responsable projet(s) financt privé",
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
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solinstit.limitevols, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solinstit.vols6h, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solinstit.train, data=climatRegr)

summary(res.reg8)


#Preoccupe
res.reg8<- lm(volshnum ~ preoccupe2, data=climatRegr_recherche)
res.reg8<- lm(volshnum ~ preoccupe2, data=climatRegr_recherche)
summary(res.reg8)
reglog3 <- MASS::glm.nb(volshnum ~ preoccupe2, data=climatRegr)
reglog3 <- MASS::glm.nb(volshnum ~ preoccupe2, data=climatRegr_recherche)
reglog3 <- MASS::glm.nb(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + preoccupe2, data=climatRegr_recherche)
summary(reglog3)


freq(climatRegr_recherche$preoccupe2)

#Risques
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.qual, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.fin, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.diffusion, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.donnees, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.avantages, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.isoler, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.insertion, data=climatRegr)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.bureaucratie, data=climatRegr)

res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.avantages + paie, data=climatRegr)
summary(res.reg8)



freq(climatRegr$solinstit.train)
freq(climatRegr$solinstit.vols6h)
rprop(table(climatRegr$solinstit.train))


rbind(prop.table(table(climatRegr$solinstit.train)), prop.table(table(climatRegr$solinstit.vols6h)))

#Réduction possible des GES perso

res.reg8<- lm(volshnum ~ solreducperso.conf, data=climatRegr_recherche)
summary(res.reg8)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solreducperso.conf, data=climatRegr_recherche)
summary(res.reg8)
climat$solreduc

#Comparaison avec les ordis
res.reg8<- lm(ordis.nbtotal~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solreducperso.info, data=climatRegr_recherche)
summary(res.reg8)
res.reg8<- lm(ordis.nbtotal~ solreducperso.info, data=climatRegr_recherche)
summary(res.reg8)

res.reg8<- lm(ordis.5anstotal~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solreducperso.info, data=climatRegr_recherche)
summary(res.reg8)
res.reg8<- lm(ordis.5anstotal~ solreducperso.info, data=climatRegr_recherche)
summary(res.reg8)

climat$solreducperso.info

#Evolution de la quantité de vols
res.reg8<- lm(Evol_GesVol.conf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climatRegr)
res.reg8<- lm(Evol_GesVol.conf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +volshnum, data=climatRegr)

mean(climatRegr$Evol_GesVol.conf, na.rm=T)


summary(res.reg8)


climatRegr$Evol_GesVol.conf.

#Personnel en position de publier

climatRegrPersPubli<-climatRegr %>% filter(!(sitpro %in% c("Technicien·ne", "Adjoint·e technique", "Autre")))


##########################################################################################################################################################
##########################################################################################################################################################
#Sur le nombre de vol (mai 2021)

#Distribution
ggplot(climatRegr_recherche, aes(x = volsnb)) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") +
        labs( x= "Nombre de vols aller-retour en 2019", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatRegrique et recherche/Figures, graphs/Nombre de vols aller-retour en 2019, distribution.pdf",
       width=9, height=5)

graphEffectif<-climatRegr_recherche %>% group_by(solevolges.conf, NumVague) %>%summarise(Effectif=n())

a<-tapply(climatRegr_recherche$volsnb, climatRegr_recherche$discipline_agr3, mean, na.rm=T)
b<-tapply(climatRegr_recherche$ScoreEcolo, climatRegr_recherche$discipline_agr3, mean, na.rm=T)
cor(a,b)*cor(a,b)

a<-tapply(climatRegr_recherche$volsnb, climatRegr_recherche$recheco, mean, na.rm=T)
b<-tapply(climatRegr_recherche$ScoreEcolo, climatRegr_recherche$recheco, mean, na.rm=T)
cor(a,b)


mean(climatRegr_recherche$volsnb, na.rm=T)
summary(reglog2)
exp(reglog2$coefficients)
reglog2 <- lm(volsnb ~ sexe +ageAgr , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe  + ageaccad_tranch2 , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + ageaccad_tranch2 , data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + Moinsavionperso, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +avionpersochgt, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +Moinsavionperso , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +Moinsavionperso +avionperso, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +avionpersochgt +avionperso, data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +Moinsavionconf , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +conffois5ans , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +conffois5ans + Moinsavionconf , data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + revenuTete, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + revenuparadulte, data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + revenuTete +avionperso, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + revenuTete +avionperso +ScoreEcolo, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + ScoreEcolo, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climatRegr_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere + malpaye, data=climatRegr_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr + volsnb , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr +  ScoreEcolo , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr +  revenuTete + ScoreEcolo , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr +  sitpro2 + discipline_agr3 +  revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche )




reglog2 <- lm(volsnb ~ sexe +ageAgr +  sitpro2 + discipline_agr3 + ScoreEcolo, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr +  sitpro2 + discipline_agr3*ScoreEcolo, data=climatRegr_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr +  sitpro2 + discipline_agr3 + ScoreEcolo2, data=climatRegr_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts + opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche )

freq(climatRegr_recherche$carriere)

summary(reglog2)
reglog2 <- lm(volsnb ~ sexe*enfantsnb_rec +ageAgr + sitpro2 ,data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3  + revenuTete,data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe*enfantsage_rec +ageAgr + sitpro2 ,data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climatRegr_recherche )

reglog2 <- lm(volshnum ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climatRegr_recherche )
reglog2 <- lm(volshnum ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climatRegr_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsduree_moy +nbpublistranch2 ,data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climatRegr_recherche )


reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche )

reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 +
                      international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche )


reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsduree_moy, data=climatRegr_recherche )
reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + couple, data=climatRegr_recherche )

summary(reglog2)


#The giga modèle complet avec variable statut/carrière/professionnel, sans var écolo assez peu signif

reglog2 <- lm(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                       international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                       avionperso, data=climatRegr_recherche)




reglog2 <- lm(volsduree_moy ~ sexe*enfantsnb_rec +ageAgr + sitpro2 ,data=climatRegr_recherche )
reglog2 <- lm(volsduree_moy ~ sexe*enfantsage_rec +ageAgr + sitpro2 ,data=climatRegr_recherche )

#library(MASS)
#Binomiale négative
reglog3 <- glm.nb(volsnb ~ sexe + ageAgr + ageaccad_tranch2  + sitpro2  + discipline_agr3, data=climatRegr_recherche )

reglog3 <- glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                      international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                      avionperso, data=climatRegr_recherche)

reglog3 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                          international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          avionperso + trav.metropole, data=climatRegr_recherche)

reglog3 <- glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                          international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          avionperso + trav.TUU2017, data=climatRegr_recherche)
reglog3 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                          international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          avionperso + trav.TAAV2017, data=climatRegr_recherche)

reglog3 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                                international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                                avionperso + trav.TAAV2017, data=climatRegr_recherche)
summary(reglog3)

freq(climatRegr$trav.metropole, total=T)

#Juste vols intérieurs
climatRegr_recherche_mod1<- climatRegr_recherche %>% filter(tiragemodule == "1")

reglog3 <- glm.nb(volsnbFrance ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                          international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          avionperso + trav.metropole, data=climatRegr_recherche_mod1)

reglog3 <- glm.nb(volsnbFrance ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                          international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          avionperso + trav.TUU2017, data=climatRegr_recherche_mod1)
reglog3 <- glm.nb(volsnbFrance ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                          international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          avionperso + trav.TAAV2017, data=climatRegr_recherche_mod1)

reglog3 <- glm.nb(volsnbFrance ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                          international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          avionperso + trav.TAAV2017, data=climatRegr_recherche_mod1)
summary(reglog3)

freq(climatRegr$trav.metropole)
freq(climatRegr$volsnb)

#Juste les vols extérieurs
#Attention aux conflits entre MASS et Dyplr pour select

reglog3 <- MASS::glm.nb(volsnb_ext ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                          international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          avionperso + trav.metropole, data=climatRegr_recherche)

reglog3 <- MASS::glm.nb(volsnb_ext ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +trav.metropole, data=climatRegr_recherche)

reglog3 <- MASS::glm.nb(volsnb_ext ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +trav.TUU2017, data=climatRegr_recherche)
reglog3 <- MASS::glm.nb(volsnb_ext ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +trav.TAAV2017, data=climatRegr_recherche)

reglog3 <- MASS::glm.nb(volsnb_ext ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                                international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                                avionperso + trav.TAAV2017, data=climatRegr_recherche)

reglog3 <- MASS::glm.nb(volsnb_ext ~ sexe + ageAgr +ageaccad_tranch2  + statut + employeur + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                                international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                                avionperso + trav.TAAV2017, data=climatRegr_recherche)




freq(climatRegr_recherche$trav.TAAV2017)

summary(reglog3)
##################
#Voler/pas voler depuis 3 ans

freq(climatRegr_recherche$vols_dicho3ans)
freq(climatRegr_recherche$vols2ans)

library(GGally)
library(ggeffects)
library(labelled)
library(gtsummary)

reglog1 <- glm(vols_dicho3ans ~ sexe + ageAgr  + sitpro2 , data=climatRegr_recherche, family=binomial())
tbl_regression(reglog2, exponentiate = T)

summary (reglog2)
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe  + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr +ageaccad_tranch2+ sitpro2 + discipline_agr3, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + recheco, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3  + Moinsavionperso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + avionpersochgt, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + avionperso, data=climatRegr_recherche, family=binomial())
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + avionperso + revenuTete, data=climatRegr_recherche, family=binomial())
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 +  revenuTete, data=climatRegr_recherche, family=binomial())

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr +ageaccad_tranch2 + sitpro2 + discipline_agr3 + carriere, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + malpaye, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere + malpaye, data=climatRegr_recherche, family=binomial(logit))

freq(climatRegr_recherche$preoccupe)
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3  + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + ageaccad_tranch2 + sitpro2 + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + ageaccad_tranch2 + sitpro2 + discipline_agr3 + effortsconso, data=climatRegr_recherche, family=binomial(logit))

summary (reglog2)

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + pluspreoccupe, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe + pluspreoccupe, data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 +  nbpublistranch2 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 +  nbpublistranch2 + malpaye +carriere + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))

summary(reglog2)

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3  + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe*enfantsnb_rec +ageAgr ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr +ageaccad_tranch2 + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho3ans ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2 + nbpublistranch2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho3ans ~ sexe + ageAgr+ageaccad_tranch2  + sitpro2 + discipline_agr3 +  international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))

summary (reglog2)

#The giga modèle complet avec variable statut/carrière/professionnel, sans var écolo assez peu signif

reglog2 <- glm(vols_dicho3ans ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                       international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                       avionperso, data=climatRegr_recherche, family=binomial(logit))





#Ne pas voler en 2019



summary (reglog2)

reglog2 <- glm(vols_dicho ~ sexe +ageAgr , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe  + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +Moinsavionperso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +avionpersochgt, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +avionpersochgt +avionperso, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere + malpaye, data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho ~ sexe +ageAgr + volsnb , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr +  ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + volsnb + revenuTete + ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))


summary(reglog2)

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3  + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe*enfantsnb_rec +ageAgr ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))

summary (reglog2)

reglog2 <- lm(ScoreEcolo ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +volsnb , data=climatRegr_recherche)



##########################
#########################@@
#Pour l'AFS


reg1 <- MASS::glm.nb(volsnb ~ sexe + ageAgr, data=climatRegr_recherche)
reg2 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3, data=climatRegr_recherche)
reg3 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2, data=climatRegr_recherche)
reg4 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                             international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche)
reg5 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                             international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                             avionperso + trav.metropole, data=climatRegr_recherche)
reg6 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                                international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                                trav.metropole, data=climatRegr_recherche)
reg7 <- MASS::glm.nb(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                             international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                             trav.metropole, data=climatRegr_recherche)

reg8 <- MASS::glm.nb(volsnbFrance ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                       international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                       trav.metropole, data=climatRegr_recherche)
volsnbFrance
summary (reg7)

htmlreg(list(reg1, reg2, reg3, reg4, reg5,reg6, reg7, reg8), 
        stars = c(0.001, 0.01, 0.05, 0.1), digits = 2, #single.row = TRUE,
        custom.header = list("Tous vols" = 1:7, "Vols intérieurs" = 8),
        custom.coef.map = list("sexeune femme"= "Femme (ref = Homme)",
                                "ageAgrMoins de 29 ans"= "Moins de 29 ans (Ref = 50-54 ans)",
                               "ageAgr30-34 ans"= "30-34 ans",
                               "ageAgr35-39 ans"= "35-39 ans",
                               "ageAgr40-44 ans"= "40-44 ans",
                               "ageAgr45-49 ans"= "45-49 ans",
                               "ageAgr55-64 ans"= "55-64 ans",
                               "ageAgr65 ans et plus"= "Plus de 65 ans",
                               "ageaccad_tranch2Pas de thèse"="Pas de thèse (Ref = 24-28 ans post-thèse)",
                               "ageaccad_tranch2(2,5]"= "2-5 ans post-thèse",
                                "ageaccad_tranch2(5,8]"= "5-8 ans post-thèse",
        "ageaccad_tranch2(8,13]"= "8-13 ans post-thèse",
        "ageaccad_tranch2(13,18]"= "13-18 ans post-thèse",
        "ageaccad_tranch2(18,23]"= "18-23 ans post-thèse",
        "ageaccad_tranch2(29,114]"= "Plus de 28 ans post-thèse",
        "sitpro2Directeur·rice de recherche"="Directeur·rice de recherche (Ref = Maître·sse de conférences)",
        "sitpro2Professeur·e des universités"= "Professeur·e des universités",
        "sitpro2Chargé·e de recherche"="Chargé·e de recherche",
        "sitpro2Maître·sse de conférences"="Maître·sse de conférences",
        "sitpro2Post-doctorant·e"= "Post-doctorant·e",
        "sitpro2ATER"="ATER",
        "sitpro2Doctorant·e contractuel·le"="Doctorant·e contractuel·le",
        "sitpro2Doctorant·e CIFRE"= "Doctorant·e CIFRE",
        "sitpro2Ingénieur·e de recherche"="Ingénieur·e de recherche",
        "sitpro2Ingénieur·e d'études"="Ingénieur·e d'études",
        "sitpro2Assistant ingénieur·e"="Assistant ingénieur·e",
        "sitpro2Technicien·ne"="Technicien·ne",
        "sitpro2Chargé·e d'études/de mission"= "Chargé·e d'études/de mission",
        "sitpro2Adjoint·e technique"="Adjoint·e technique",
        "sitpro2Autre"="Autre",
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
        "rechecoNon, mais je l'ai fait par le passé"="A participé à des recherches en lien avec écologie,\n envrt, climat (Ref=participe actuellement)",
        "rechecoNon"="Ne participe pas à des recherches en lien avec écologie, envrt, climat",
        "nbpublistranch2(0.5,2]"="1 à 2 publications ds revues comité lecture (Ref : aucune publi)",                     
        "nbpublistranch2(2,4]"="2 à 4 publications ds revues comité lecture", 
        "nbpublistranch2(4,7]"="5 à 7 publications ds revues comité lecture",
        "nbpublistranch2(7,12]"="8 à 12 publications ds revues comité lecture",
        "nbpublistranch2(12,20]"="13 à 20 publications ds revues comité lecture",
        "nbpublistranch2(20,40]"="21 à 40 publications ds revues comité lecture",
        "nbpublistranch2(40,180]"="Plus de 40 publications ds revues comité lecture",
        "carriereOui"="Cherche à être promu, recrut, titularisé",
        "projets.anr_m2Membre projet ANR oui"="Membre projet(s) financt ANR (Ref = Ne participe pas)",
        "projets.anr_r2Responsable projet ANR oui"="Responsable projet(s) financt ANR",
        "projets.france_m2Membre projet France oui"="Membre projet(s) financt France (Ref = Ne participe pas)",
        "projets.france_r2Responsable projet France oui"="Responsable projet(s) financt France",
        "projets.europe_m2Membre projet européen oui"="Membre projet(s) financt européen (Ref = Ne participe pas)",
        "projets.europe_r2Responsable projet européen oui"="Responsable projet(s) financt européen",
        "projets.inter_m2Membre projet international oui"="Membre projet(s) financt international (Ref = Ne participe pas)",
        "projets.inter_r2Responsable projet international oui"="Responsable projet(s) financt international",
        "projets.prive_m2Membre projet privé oui"="Membre projet(s) financt privé (Ref = Ne participe pas)",
        "projets.prive_r2Responsable projet privé oui"="Responsable projet(s) financt privé",
        "international.travailNon "="N'a pas travaillé hors de France en enseignt/recherche au moins 3 mois (hors post-doc)",
        "international.progNon"="N'est pas engagé·e dans un progr de recherche international",
        "international.assoNon"="Pas activement impliqué·e ds une asso pro non française ou internationale",
        "avionperso1 ou 2 allers-retours"="1 à 2 allers-retours en avion en 2019 ds cadre perso (Ref= zéro allers-retour)",
        "avionperso2 ou 4 allers-retours"="3 à 4 allers-retours en avion en 2019 ds cadre perso",
        "avionpersoPlus de 5 allers-retours"="Plus de 5 allers-retours en avion en 2019 ds cadre perso",
        "trav.metropoleLyon"= "Travaille dans la métropole de Lyon (Ref  = métropole Paris)",
        "trav.metropoleAix_Marseille"= "Travaille dans la métropole de Aix_Marseille",
        "trav.metropoleLille"= "Travaille dans la métropole de Lille",
        "trav.metropoleToulouse"= "Travaille dans la métropole de Toulouse",
        "trav.metropoleBordeaux"= "Travaille dans la métropole de Bordeaux",
        "trav.metropoleNantes"= "Travaille dans la métropole de Nantes",
        "trav.metropoleStrasbourg"= "Travaille dans la métropole de Strasbourg",
        "trav.metropoleMontpellier"= "Travaille dans la métropole de Montpellier",
        "trav.metropoleRennes"= "Travaille dans la métropole de Rennes",
        "trav.metropoleGrenoble"= "Travaille dans la métropole de Grenoble",
        "trav.metropoleRouen"= "Travaille dans la métropole de Rouen",
        "trav.metropoleNice"= "Travaille dans la métropole de Nice",
        "trav.metropoleToulon"= "Travaille dans la métropole de Toulon",
        "trav.metropoleTours"= "Travaille dans la métropole de Tours",
        "trav.metropoleNancy"= "Travaille dans la métropole de Nancy",
        "trav.metropoleAutre"= "Travaille dans la métropole de Autre"),
      
symbol = "+",
caption = "Tableau : Régressions binomiales négatives sur le nombre de vols en 2019", caption.above=TRUE, 
single.row = TRUE, 
#custom.gof.rows = NULL,
file="/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Reg binom neg, nombre de vol 2019.doc")

#Si on veut économiser des lignes du tableau, par exemple en virant l'âge/âge académique qu'est pas signif, mais en contrôlant quand même par l'âge
htmlreg(list(reg1, reg2, reg3, reg4, reg5,reg6, reg7, reg8), 
        stars = c(0.001, 0.01, 0.05, 0.1), digits = 2, #single.row = TRUE,
        custom.header = list("Tous vols" = 1:7, "Vols intérieurs" = 8),
        custom.coef.map = list("sexeune femme"= "Femme (ref = Homme)",
                               "sitpro2Directeur·rice de recherche"="Directeur·rice de recherche (Ref = Maître·sse de conférences)",
                               "sitpro2Professeur·e des universités"= "Professeur·e des universités",
                               "sitpro2Chargé·e de recherche"="Chargé·e de recherche",
                               "sitpro2Maître·sse de conférences"="Maître·sse de conférences",
                               "sitpro2Post-doctorant·e"= "Post-doctorant·e",
                               "sitpro2ATER"="ATER",
                               "sitpro2Doctorant·e contractuel·le"="Doctorant·e contractuel·le",
                               "sitpro2Doctorant·e CIFRE"= "Doctorant·e CIFRE",
                               "sitpro2Ingénieur·e de recherche"="Ingénieur·e de recherche",
                               "sitpro2Ingénieur·e d'études"="Ingénieur·e d'études",
                               "sitpro2Assistant ingénieur·e"="Assistant ingénieur·e",
                               "sitpro2Technicien·ne"="Technicien·ne",
                               "sitpro2Chargé·e d'études/de mission"= "Chargé·e d'études/de mission",
                               "sitpro2Adjoint·e technique"="Adjoint·e technique",
                               "sitpro2Autre"="Autre",
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
                               "rechecoNon, mais je l'ai fait par le passé"="A participé à des recherches en lien avec écologie,\n envrt, climat (Ref=participe actuellement)",
                               "rechecoNon"="Ne participe pas à des recherches en lien avec écologie, envrt, climat",
                               "nbpublistranch2(0.5,2]"="1 à 2 publications ds revues comité lecture (Ref : aucune publi)",                     
                               "nbpublistranch2(2,4]"="2 à 4 publications ds revues comité lecture", 
                               "nbpublistranch2(4,7]"="5 à 7 publications ds revues comité lecture",
                               "nbpublistranch2(7,12]"="8 à 12 publications ds revues comité lecture",
                               "nbpublistranch2(12,20]"="13 à 20 publications ds revues comité lecture",
                               "nbpublistranch2(20,40]"="21 à 40 publications ds revues comité lecture",
                               "nbpublistranch2(40,180]"="Plus de 40 publications ds revues comité lecture",
                               "carriereOui"="Cherche à être promu, recrut, titularisé",
                               "projets.anr_m2Membre projet ANR oui"="Membre projet(s) financt ANR (Ref = Ne participe pas)",
                               "projets.anr_r2Responsable projet ANR oui"="Responsable projet(s) financt ANR",
                               "projets.france_m2Membre projet France oui"="Membre projet(s) financt France (Ref = Ne participe pas)",
                               "projets.france_r2Responsable projet France oui"="Responsable projet(s) financt France",
                               "projets.europe_m2Membre projet européen oui"="Membre projet(s) financt européen (Ref = Ne participe pas)",
                               "projets.europe_r2Responsable projet européen oui"="Responsable projet(s) financt européen",
                               "projets.inter_m2Membre projet international oui"="Membre projet(s) financt international (Ref = Ne participe pas)",
                               "projets.inter_r2Responsable projet international oui"="Responsable projet(s) financt international",
                               "projets.prive_m2Membre projet privé oui"="Membre projet(s) financt privé (Ref = Ne participe pas)",
                               "projets.prive_r2Responsable projet privé oui"="Responsable projet(s) financt privé",
                               "international.travailNon "="N'a pas travaillé hors de France en enseignt/recherche au moins 3 mois (hors post-doc)",
                               "international.progNon"="N'est pas engagé·e dans un progr de recherche international",
                               "international.assoNon"="Pas activement impliqué·e ds une asso pro non française ou internationale",
                               "avionperso1 ou 2 allers-retours"="1 à 2 allers-retours en avion en 2019 ds cadre perso (Ref= zéro allers-retour)",
                               "avionperso2 ou 4 allers-retours"="3 à 4 allers-retours en avion en 2019 ds cadre perso",
                               "avionpersoPlus de 5 allers-retours"="Plus de 5 allers-retours en avion en 2019 ds cadre perso",
                               "trav.metropoleLyon"= "Travaille dans la métropole de Lyon (Ref  = métropole Paris)",
                               "trav.metropoleAix_Marseille"= "Travaille dans la métropole de Aix_Marseille",
                               "trav.metropoleLille"= "Travaille dans la métropole de Lille",
                               "trav.metropoleToulouse"= "Travaille dans la métropole de Toulouse",
                               "trav.metropoleBordeaux"= "Travaille dans la métropole de Bordeaux",
                               "trav.metropoleNantes"= "Travaille dans la métropole de Nantes",
                               "trav.metropoleStrasbourg"= "Travaille dans la métropole de Strasbourg",
                               "trav.metropoleMontpellier"= "Travaille dans la métropole de Montpellier",
                               "trav.metropoleRennes"= "Travaille dans la métropole de Rennes",
                               "trav.metropoleGrenoble"= "Travaille dans la métropole de Grenoble",
                               "trav.metropoleRouen"= "Travaille dans la métropole de Rouen",
                               "trav.metropoleNice"= "Travaille dans la métropole de Nice",
                               "trav.metropoleToulon"= "Travaille dans la métropole de Toulon",
                               "trav.metropoleTours"= "Travaille dans la métropole de Tours",
                               "trav.metropoleNancy"= "Travaille dans la métropole de Nancy",
                               "trav.metropoleAutre"= "Travaille dans la métropole de Autre"),
        
        symbol = "+",
        caption = "Tableau : Régressions binomiales négatives sur le nombre de vols en 2019", caption.above=TRUE, 
        single.row = TRUE, 
        #custom.gof.rows = NULL,
        file="/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Reg binom neg, nombre de vol 2019, nb ligne reduit.doc")


####################################Mai 2021###################
#A partir des valeurs précises du tableau
freq(climatRegr$volsnb_tot)
freq(climatRegr$volsdist_tot)
freq(climatRegr$tiragemodule)
freq(climatRegr$volshnum)
freq(climatRegr$sexe)
table(climatRegr$sexe)
cumsum(table(climatRegr$sexe))
cumsum(table(climatRegr$volshnum))
cumsum(table(climatRegr$volsnb_tot))
cumsum(table(climatRegr$volsnb))


#####################################################################################################################################@
#Evolution des émissions de GES pour des vols en avions pour des conf, réunions, congrès (et juste pour le personnel de recherche)

#Distribution
ggplot(climatRegr_recherche, aes(x = factor(solevolges.conf))) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        labs( x= "Evolution des GES ces 5 dernières années \n concernant les vols en avion pour les confs, réunions, congrès ", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatRegrique et recherche/Figures, graphs/Chgt des pratiques avion pro conf, nb de répondants par modalité.pdf",
       width=9, height=5)

#Comme on a changé l'intitué de la question avant la dernière vague (pour tenir compte du confinement), 
#on regarde si ça a eu un effet majeur

#Graphiques : changement des pratiques de vols en fonction de la vague de réponse


graphEffectif<-climatRegr_recherche %>% group_by(solevolges.conf, NumVague) %>%summarise(Effectif=n())
graphEffectif$NumVague <- ordered(graphEffectif$NumVague, levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

effectifsvague<-climatRegr_recherche %>% group_by(NumVague) %>%summarise(Effectifvague=n())
graphEffectif<-merge(graphEffectif, effectifsvague, by="NumVague", all =TRUE )
graphEffectif$PourcentEffectif<-graphEffectif$Effectif/graphEffectif$Effectifvague*100

ggplot(graphEffectif, aes(x = solevolges.conf, y = PourcentEffectif, fill = NumVague)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() + guides(fill=guide_legend(reverse = TRUE)) +
                scale_fill_viridis_d() +
                labs(y="% des répondants par vague", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")


#La même chose sans les nons réponses

graphEffectif<-climatRegr_recherche %>% filter(!is.na(solevolges.conf)) %>% group_by(solevolges.conf, NumVague) %>%summarise(Effectif=n())
graphEffectif$NumVague <- ordered(graphEffectif$NumVague, levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

effectifsvague<-climatRegr_recherche %>% filter(!is.na(solevolges.conf)) %>%group_by(NumVague) %>%summarise(Effectifvague=n())
graphEffectif<-merge(graphEffectif, effectifsvague, by="NumVague", all =TRUE )
graphEffectif$PourcentEffectif<-graphEffectif$Effectif/graphEffectif$Effectifvague*100

ggplot(graphEffectif, aes(x = solevolges.conf, y = PourcentEffectif, fill = NumVague)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() +  guides(fill=guide_legend(reverse = TRUE)) +
        scale_fill_viridis_d() +
        labs(y="% des répondants par vague (hors non-réponses)", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")

ggsave("/Users/jeromegreffion/Changement climatRegrique et recherche/Figures, graphs/Chgt des pratiques avion pro conf, pourcentage de répondants par vague, hors NA, répartition réponses.pdf",
       width=9, height=5)
freq(climatRegr_recherche$solevolges.conf)

#Prendre moins l'avion depuis 5 ans pour des confs, réunions, congrès


summary(reglog2)
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe  + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + recheco, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +Moinsavionperso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +avionpersochgt, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +avionpersochgt +avionperso, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere + malpaye, data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + volsnb , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr +  ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + volsnb + revenuTete + ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + pluspreoccupe, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 +preoccupe + pluspreoccupe, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))


summary(reglog2)

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + quizfacteurs.avionnum , data=climatRegr_recherche,family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + quizfacteurs.avionnum , data=climatRegr_recherche,family=binomial(logit))


summary(reglog2)

#Mutinomiale

library(nnet)
library(GGally)
library(ggeffects)
library(labelled)
library(gtsummary)
climatRegr_recherche$solevolges.conf<-fct_relevel(climatRegr_recherche$solevolges.conf ,"Été à peu près stables")

freq(climatRegr_recherche$solevolges.conf)
freq(climatRegr_recherche$docto)

#On supprime les non 
climatRegr_recherche2<-climatRegr_recherche %>% filter(!(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) | sexe!="Autre" | docto=="Non")



#Je vire les sexes "autres" qui tassent le graph de ggcoef à cause des écarts types énormes (les "autre" restent dans le graph, mais la catégorie doit être vide ; 
#elle reste dans les représentations graphiques sans doute à cause d'un level qq part
regm <- multinom(solevolges.conf ~ sexe , data = climatRegr_recherche, 
                 subset= !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) & !(sexe=="Autre") )

#Pour ne pas avoir les modalités vides représentées dans la représentation graphique ggcoef
climatRegr_recherche <- droplevels(climatRegr_recherche)
regm <- multinom(solevolges.conf ~ sexe , data = climatRegr_recherche, subset= solevolges.conf %in% c("Fortement augmenté", "Un peu augmenté" ,"Été à peu près stables", "Un peu diminué","Fortement diminué") )

regm <- multinom(solevolges.conf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data = climatRegr_recherche, 
                 subset= !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) & !(sexe=="Autre") &
                         docto=="Non" & !(sitpro2 %in% c("Doctorant·e CIFRE", "Doctorant·e contractuel·le")))

#Test avec climatRegr_recherche2 pour éviter d'avoir les levels vides (bof ça marche pas...)


regm <- multinom(solevolges.conf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data = droplevels( 
        filter(climatRegr_recherche, !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) | !(sexe=="Autre") |
        docto=="Non" | !(sitpro2 %in% c("Doctorant·e CIFRE", "Doctorant·e contractuel·le")))))
                                                                                                                  

regm <- multinom(solevolges.conf ~ sexe + ageaccad_tranch2  + sitpro2 + discipline_agr3 , data = climatRegr_recherche, 
                 subset= !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) & !(sexe=="Autre") &
                         docto=="Non" & !(sitpro2 %in% c("Doctorant·e CIFRE", "Doctorant·e contractuel·le")))
regm <- multinom(solevolges.conf ~ sexe + ageaccad_tranch2  + sitpro2 + discipline_agr3 , data = climatRegr_recherche, 
                 subset= !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) & !(sexe=="Autre") &
                         docto=="Non" & !(sitpro2 %in% c("Doctorant·e CIFRE", "Doctorant·e contractuel·le")) & ageaccad_tranch2!="Pas de thèse")

regm <- multinom(solevolges.conf ~ sexe + ageaccad_tranch2  + sitpro2 + discipline_agr3 +dixannees.marche , data = climatRegr_recherche, 
                 subset= !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) & !(sexe=="Autre") &
                         docto=="Non" & !(sitpro2 %in% c("Doctorant·e CIFRE", "Doctorant·e contractuel·le")) & ageaccad_tranch2!="Pas de thèse")

regm <- multinom(solevolges.conf ~ dixannees.marche , data = climatRegr_recherche, 
                 subset= !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) & !(sexe=="Autre") &
                         docto=="Non" & !(sitpro2 %in% c("Doctorant·e CIFRE", "Doctorant·e contractuel·le")) & ageaccad_tranch2!="Pas de thèse")

regm <- multinom(solevolges.conf ~ dixannees.marche , data = climatRegr_recherche, 
                 subset= !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) & !(sexe=="Autre") &
                         docto=="Non" & !(sitpro2 %in% c("Doctorant·e CIFRE", "Doctorant·e contractuel·le")) & ageaccad_tranch2!="Pas de thèse")



freq(climatRegr$ageaccad_tranch2)

regm <- multinom(solevolges.conf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data = climatRegr_recherche, subset= !(solevolges.conf %in% c("Non concerné·e", "Je ne sais pas")) )



regm <- multinom(solevolges.conf ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data = climatRegr_recherche2)

ggcoef_multinom(regm, exponentiate = TRUE)
summary(regm)
tbl_regression(regm, exponentiate = TRUE)
#http://larmarange.github.io/analyse-R/regression-logistique.html#r%C3%A9gression-logistique-multinomiale


freq(climatRegr$trav.dep)
freq(climatRegr$trav.CATEAAV2020)
freq(climatRegr$trav.TAAV2017)
freq(climatRegr$trav.TUU2017)

freq(climatRegr$res.TUU2017)
freq(climatRegr$res.TAAV2017)
freq(climatRegr$res.CATEAAV2020)
freq(climatRegr$res.metropole)
#####################################################################################################################################@
#Evolution des émissions de GES pour des vols en avions pour recueillir des données (et juste pour le personnel de recherche)

#Distribution
ggplot(climatRegr_recherche, aes(x = factor(solevolges.donnees))) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        labs( x= "Evolution des GES ces 5 dernières années \n concernant les vols en avion pour recueillir des données ", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatRegrique et recherche/Figures, graphs/Chgt des pratiques avion pro recueil des données, nb de répondants par modalité.pdf",
       width=9, height=5)

#Comme on a changé l'intitué de la question avant la dernière vague (pour tenir compte du confinement), 
#on regarde si ça a eu un effet majeur

#Graphiques : changement des pratiques de vols en fonction de la vague de réponse

#Variable avec uniquement la date

graphEffectif<-climatRegr_recherche %>% group_by(solevolges.donnees, NumVague) %>%summarise(Effectif=n())
graphEffectif$NumVague <- ordered(graphEffectif$NumVague, levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

effectifsvague<-climatRegr_recherche %>% group_by(NumVague) %>%summarise(Effectifvague=n())
graphEffectif<-merge(graphEffectif, effectifsvague, by="NumVague", all =TRUE )
graphEffectif$PourcentEffectif<-graphEffectif$Effectif/graphEffectif$Effectifvague*100

ggplot(graphEffectif, aes(x = solevolges.donnees, y = PourcentEffectif, fill = NumVague)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() + guides(fill=guide_legend(reverse = TRUE)) +
        scale_fill_viridis_d() +
        labs(y="% des répondants par vague", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")


#La même chose sans les nons réponses

graphEffectif<-climatRegr_recherche %>% filter(!is.na(solevolges.donnees)) %>% group_by(solevolges.donnees, NumVague) %>%summarise(Effectif=n())
graphEffectif$NumVague <- ordered(graphEffectif$NumVague, levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

effectifsvague<-climatRegr_recherche %>% filter(!is.na(solevolges.donnees)) %>%group_by(NumVague) %>%summarise(Effectifvague=n())
graphEffectif<-merge(graphEffectif, effectifsvague, by="NumVague", all =TRUE )
graphEffectif$PourcentEffectif<-graphEffectif$Effectif/graphEffectif$Effectifvague*100

ggplot(graphEffectif, aes(x = solevolges.donnees, y = PourcentEffectif, fill = NumVague)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() +  guides(fill=guide_legend(reverse = TRUE)) +
        scale_fill_viridis_d() +
        labs(y="% des répondants par vague (hors non-réponses)", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")


#Prendre moins l'avion/voiture/bateau depuis 5 ans pour le recueil de données


summary(reglog2)
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe  + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3  +Moinsavionperso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +Moinsavionperso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3  +avionpersochgt, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3  +avionpersochgt +avionperso, data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + volsnb , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr +  ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr +  revenuTete + ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))

summary(reglog2)
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))
summary(reglog2)

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(Moinsaviondonnees ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))


summary(reglog2)

#Contruction variable alternative (sans les "stables") : Prendre moins l'avion/voiture/bateau depuis 5 ans pour le recueil de données
#On ne tient pas compte des "stables"


summary(reglog2)
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe  + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + ageaccad_tranch2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3  +Moinsavionperso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +Moinsavionperso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3  +avionpersochgt, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3  +avionpersochgt +avionperso, data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + volsnb , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr +  ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr +  revenuTete + ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))

summary(reglog2)
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +  opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))
summary(reglog2)

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +nbpublistranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +hindextranch2 ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(Moinsaviondonnees2 ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climatRegr_recherche,family=binomial(logit))


summary(reglog2)


#####################################################################################################################################@
#Avion perso (et juste pour le personnel de recherche)

#Distribution nb de vols et changement pratiques

ggplot(climatRegr_recherche, aes(x = factor(avionperso))) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        labs( x= "Nombre de vols en avion dans la vie privé en 2019", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatRegrique et recherche/Figures, graphs/Nombre de vols avion vie privée, répartition réponses.pdf",
       width=9, height=5)


ggplot(climatRegr_recherche, aes(x = factor(avionpersochgt))) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        labs( x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatRegrique et recherche/Figures, graphs/Changement des pratiques avion vie privée, répartition réponses.pdf",
       width=9, height=5)

#Moyennes par catégorie de changement de pratiques : distribution
graphMoyenne<-climatRegr_recherche %>% group_by(avionpersochgt) %>%summarise(MoyenneVols = mean(avionpersonum, na.rm=T),
                                                                          Ecart = sd(avionpersonum, na.rm=T))

ggplot(graphMoyenne, aes(x = avionpersochgt, y = MoyenneVols, fill = avionpersochgt)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        geom_errorbar(
                aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
                width = .2, position = position_dodge(.9), size = 1
        ) + 
        scale_fill_viridis_d() +
        guides(fill = FALSE) +
        labs(y="Nombre moyens de vols en avion dans la vie privé en 2019", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")

ggsave("/Users/jeromegreffion/Changement climatRegrique et recherche/Figures, graphs/Nombre moyen de vols privés en fonction du degré d'évolution de la prise de l'avion.pdf",
       width=9, height=5)

#Si on enlève ceux qui sont à zéro vols (potentiellement des gens qui ne volent jamais et qui ne peuvent pas réduire)
graphMoyenne<-climatRegr_recherche %>% filter(avionpersonum>0) %>%
        group_by(avionpersochgt) %>%summarise(MoyenneVols = mean(avionpersonum, na.rm=T),
                                                                         Ecart = sd(avionpersonum, na.rm=T))

ggplot(graphMoyenne, aes(x = avionpersochgt, y = MoyenneVols, fill = avionpersochgt)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        geom_errorbar(
                aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
                width = .2, position = position_dodge(.9), size = 1
        ) + 
        scale_fill_viridis_d() +
        guides(fill = FALSE) +
        labs(y="Nombre moyens de vols en avion dans la vie privé en 2019\n pour ceux qui ont volé", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")

ggsave("/Users/jeromegreffion/Changement climatRegrique et recherche/Figures, graphs/Nombre moyen de vols privés en fonction du degré d'évolution de la prise de l'avion, pour ceux qui ont volé.pdf",
       width=9, height=5)


res.reg9 <- lm(avionpersonum ~ sexe + ageAgr , data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2  , data=climatRegr_recherche)

res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + avionpersochgt , data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + avionpersochgt +  revenuTete, data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe*enfantsage_rec + ageAgr  + sitpro2 + avionpersochgt , data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe*enfantsnb_rec + ageAgr  + sitpro2 + avionpersochgt , data=climatRegr_recherche)


res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + statutpar.p , data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + dippar.p , data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + statutpar.p +revenuparadulte , data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + dippar.p +revenuparadulte, data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + revenuTete, data=climatRegr_recherche)

res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2+ discipline_agr3 , data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2+ discipline_agr3 + revenuTete, data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2+ discipline_agr3 + revenuparadulte, data=climatRegr_recherche)


res.reg9 <- lm(avionpersonum ~   avionpersochgt , data=climatRegr_recherche)
res.reg9 <- lm(avionpersonum ~   avionpersochgt +scorecolo , data=climatRegr_recherche)

res.reg9 <- lm(revenuTete ~ sexe   , data=climatRegr_recherche)

summary(res.reg9)

mean(climatRegr_recherche$avionpersonum, na.rm=T)
freq(climatRegr_recherche$Moinsavionperso)
freq(climatRegr_recherche$avionpersochgt)
freq(climatRegr_recherche$ageAgr)
freq(climatRegr_recherche$revenuTete)


#Prendre moins l'avion depuis 5 ans dans la vie privée


reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + aucunvolperso , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))


reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + aucunvolperso + revenuTete + ScoreEcolo , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + aucunvolperso + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + aucunvolperso + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + pluspreoccupe, data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 +preoccupe + pluspreoccupe, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climatRegr_recherche, family=binomial(logit))
summary(reglog2)

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso ,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso + revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso + revenuTete,data=climatRegr_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + avionperso+ revenuTete,data=climatRegr_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + avionperso+ revenuTete,data=climatRegr_recherche, family=binomial(logit))


summary(reglog2)

freq(climatRegr_recherche$opinionecolo.decroissance)
climatRegr$volsnbtranch2

reglog2 <- lm(volsnb ~ solevolges.conf,data=climatRegr_recherche)
climatRegr_recherche$solevolges.conf<-fct_relevel(climatRegr_recherche$solevolges.conf ,"Été à peu près stables")

reglog2 <- glm(Moinsavionconf ~ carriere, data=climatRegr_recherche, family=binomial(logit))
summary(reglog2)


######
#REbus


freq(climatRegr$avionperso)

#Sous discipline Socio pour comparer enquête socio
climatRegrSocio<-climatRegr %>% filter(discipline_agr3=="Socio, démo")
res.reg9<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + revenuTete + enfantsnb + couple , data=climatRegrSocio)

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
                               "sitpro2Directeur·rice de recherche"="Directeur·rice de recherche (Ref = Maître·sse de conférences)",
                               "sitpro2Professeur·e des universités"= "University professor ",
                               "sitpro2Chargé·e de recherche"="Researcher",
                               "sitpro2Maître·sse de conférences"="Lecturer",
                               "sitpro2Post-doctorant·e"= "Post doctoral student",
                               "sitpro2ATER"="ATER",
                               "sitpro2Doctorant·e contractuel·le"="contractual doctoral student",
                               "sitpro2Doctorant·e CIFRE"= "CIFRE contractual doctoral student",
                               "sitpro2Ingénieur·e de recherche"="Research engineer",
                               "sitpro2Ingénieur·e d'études"="Design engineer",
                               "sitpro2Assistant ingénieur·e"="Assistant engineer",
                               "sitpro2Technicien·ne"="Technician",
                               "sitpro2Chargé·e d'études/de mission"= "Research officer",
                               "sitpro2Adjoint·e technique"="Technical assistant",
                               "sitpro2Autre"="Other",
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
        file="/Users/jeromegreffion/Dropbox/changement-climatRegrique-et-recherche/Resultats/Regressions duree de vol agr 2019, discip, sitpro, rev.doc")


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

climatRegr$Part_ANR_ERC[climatRegr$projets.anr_r %in% c(0, NA) & climatRegr$projets.anr_m %in% c(0, NA) & climatRegr$projets.france_r %in% c(0, NA) & climatRegr$projets.france_m %in% c(0, NA) 
                    & climatRegr$projets.europe_r %in% c(0, NA) & climatRegr$projets.europe_m %in% c(0, NA) & climatRegr$projets.inter_r  %in% c(0, NA) & climatRegr$projets.inter_m %in% c(0, NA) 
                    &   climatRegr$projets.prive_r  %in% c(0, NA) & climatRegr$projets.prive_m %in% c(0, NA)]<-"Ne participe à aucun projet financé"
climatRegr$Part_ANR_ERC[(climatRegr$projets.anr_r ==1 | climatRegr$projets.anr_m ==1) & climatRegr$projets.france_r %in% c(0, NA) & climatRegr$projets.france_m %in% c(0, NA) 
                    & climatRegr$projets.europe_r %in% c(0, NA) & climatRegr$projets.europe_m %in% c(0, NA) & climatRegr$projets.inter_r  %in% c(0, NA) & climatRegr$projets.inter_m %in% c(0, NA) 
                    &   climatRegr$projets.prive_r  %in% c(0, NA) & climatRegr$projets.prive_m %in% c(0, NA)]<-"Seult finance ANR"
climatRegr$Part_ANR_ERC[climatRegr$projets.anr_r %in% c(0, NA) & climatRegr$projets.anr_m %in% c(0, NA) & (climatRegr$projets.france_r==1 | climatRegr$projets.france_m==1) 
                    & climatRegr$projets.europe_r %in% c(0, NA) & climatRegr$projets.europe_m %in% c(0, NA) & climatRegr$projets.inter_r  %in% c(0, NA) & climatRegr$projets.inter_m %in% c(0, NA) 
                    &   climatRegr$projets.prive_r  %in% c(0, NA) & climatRegr$projets.prive_m %in% c(0, NA)]<-"Seult finance français (hors ANR)"
climatRegr$Part_ANR_ERC[climatRegr$projets.anr_r %in% c(0, NA) & climatRegr$projets.anr_m %in% c(0, NA) & climatRegr$projets.france_r %in% c(0, NA) & climatRegr$projets.france_m %in% c(0, NA) 
                    & (climatRegr$projets.europe_r ==1 | climatRegr$projets.europe_m ==1) & climatRegr$projets.inter_r  %in% c(0, NA) & climatRegr$projets.inter_m %in% c(0, NA) 
                    &   climatRegr$projets.prive_r  %in% c(0, NA) & climatRegr$projets.prive_m %in% c(0, NA)]<-"Seult finance européen"
climatRegr$Part_ANR_ERC[climatRegr$projets.anr_r %in% c(0, NA) & climatRegr$projets.anr_m %in% c(0, NA) & climatRegr$projets.france_r %in% c(0, NA) & climatRegr$projets.france_m %in% c(0, NA) 
                    & climatRegr$projets.europe_r %in% c(0, NA) & climatRegr$projets.europe_m %in% c(0, NA) & (climatRegr$projets.inter_r ==1 | climatRegr$projets.inter_m ==1) 
                    &   climatRegr$projets.prive_r  %in% c(0, NA) & climatRegr$projets.prive_m %in% c(0, NA)]<-"Seult finance internat (hors europe)"
climatRegr$Part_ANR_ERC[climatRegr$projets.anr_r %in% c(0, NA) & climatRegr$projets.anr_m %in% c(0, NA) & climatRegr$projets.france_r %in% c(0, NA) & climatRegr$projets.france_m %in% c(0, NA) 
                    & climatRegr$projets.europe_r %in% c(0, NA) & climatRegr$projets.europe_m %in% c(0, NA) & climatRegr$projets.inter_r  %in% c(0, NA) & climatRegr$projets.inter_m %in% c(0, NA) 
                    &   (climatRegr$projets.prive_r==1 | climatRegr$projets.prive_m ==1)]<-"Seult finance privé"

#Regroupement participation à projet financé (attention, il y a une erreur : les 0 sont comptés comme des 1 avec ce code, or ils devraient être regroupés avec les NA)
#climatRegr$Part_ANR_ERC[is.na(climatRegr$projets.anr_r) & is.na(climatRegr$projets.anr_m) & is.na(climatRegr$projets.europe_r) 
#                    & is.na(climatRegr$projets.europe_m)]<-"Ni financement ANR ou Europe"
#climatRegr$Part_ANR_ERC[(!is.na(climatRegr$projets.anr_r) | !is.na(climatRegr$projets.anr_m)) & is.na(climatRegr$projets.europe_r) 
#                    & is.na(climatRegr$projets.europe_m)]<-"Projet ANR"
#climatRegr$Part_ANR_ERC[is.na(climatRegr$projets.anr_r) & is.na(climatRegr$projets.anr_m) & (!is.na(climatRegr$projets.europe_r) 
#                    | !is.na(climatRegr$projets.europe_m))]<-"Projet européen"               
#climatRegr$Part_ANR_ERC[(!is.na(climatRegr$projets.anr_r) | !is.na(climatRegr$projets.anr_m)) & (!is.na(climatRegr$projets.europe_r) 
#               | !is.na(climatRegr$projets.europe_m))]<-"Projet ANR et projet européen"             


"Part_ANR_ERCProjet ANR"="Participation projet ANR (Ref = ni projet ANR ni européen)",
"Part_ANR_ERCProjet européen"="Participation projet européen",
"Part_ANR_ERCProjet ANR et projet européen" ="Participation projet ANR et projet européen",
"revenuAgrDe 1 500 à 2 499 euros par mois"="1 500 à 2 499 euros par mois",

