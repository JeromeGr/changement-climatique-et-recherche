library(texreg)

#Régressions sur les heures de vols

#Je vire ceux dont la ligne est totalement vide
#Peut être rajouter des conditions
climat$nbpublisang<-as.numeric(climat$nbpublisang)
climat<-climat %>% filter(!(sexe=="" & age=="" & statut=="" & employeur=="" & changclim=="" & preoccupe==""))
# Et je vire le troll (je sais pas pourquoi il faut rajouter de garder les NAs sinon on les perd)
climat<-climat %>% filter((nbpublisang!=666 | is.na(nbpublisang)))

################################
#Recodage pour la régression
freq(climat$solevolges.conf)
climat$Evol_GesVol.conf.[climat$solevolges.conf=="Été à peu près stables"]<-0
climat$Evol_GesVol.conf.[climat$solevolges.conf=="Fortement augmenté"]<-3
climat$Evol_GesVol.conf.[climat$solevolges.conf=="Un peu augmenté"]<-1
climat$Evol_GesVol.conf.[climat$solevolges.conf=="Fortement diminué"]<--3
climat$Evol_GesVol.conf.[climat$solevolges.conf=="Un peu diminué"]<--1
freq(climat$Evol_GesVol.conf)

#REvenu : on agrège les catégories avec peu de monde
climat$revenuAgr<-climat$revenu
climat$revenuAgr[climat$revenu %in% c("De 10 000 à 15 000 euros par mois", "Plus de 15 000 par mois", "De 8 000 à 9 999 euros par mois")]<-"Au moins 8000 euros par mois"


#Nombre de vol en avion perso en numérique (approximation)

climat_recherche$avionpersonum[climat_recherche$avionperso=="Aucun aller-retour"]<-0
climat_recherche$avionpersonum[climat_recherche$avionperso=="1 ou 2 allers-retours"]<-1.5
climat_recherche$avionpersonum[climat_recherche$avionperso=="3 ou 4 allers-retours"]<-3.5
climat_recherche$avionpersonum[climat_recherche$avionperso=="Plus de 5 allers-retours"]<-6


#S'estimer bien ou mal payé
freq(climat_recherche$paie)
climat_recherche$malpaye[climat_recherche$paie %in% c("Mal payé·e" , "Très mal payé·e")]<-"Oui"
climat_recherche$malpaye[climat_recherche$paie %in% c("Bien payé·e", "Correctement payé·e", "Très bien payé·e")]<-"Non"
climat_recherche$malpaye<-fct_relevel(climat_recherche$malpaye, "Non")


##########################################
#Régressions
mean(climat$volshnum, na.rm=T)
freq(climat$sitpro)
#Tout le monde
#Premier doc "all staff"
res.reg1 <- lm(volshnum ~ sexe + ageAgr, data=climat)
res.reg2<- lm(volshnum ~ sexe + ageAgr  + sitpro2, data=climat)
res.reg3<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + revenuTete  , data=climat)
res.reg4<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + enfantsnb + couple , data=climat)
res.reg5 <- lm(volshnum ~ sexe + ageAgr  + discipline_agr3 , data=climat)
res.reg6 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat)
res.reg7 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete + enfantsnb + couple , data=climat)


freq(climat$avionperso)

#Sous discipline Socio pour comparer enquête socio
climatSocio<-climat %>% filter(discipline_agr3=="Socio, démo")
res.reg9<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + revenuTete + enfantsnb + couple , data=climatSocio)

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
        file="/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Regressions duree de vol agr 2019, discip, sitpro, rev.doc")

#Employeur
res.reg8<- lm(volshnum ~ sexe + ageAgr  + employeur, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  employeur, data=climat)
summary(res.reg8)

#Pratiques perso avion
freq(climat$avionperso)
freq(climat$avionpersochgt)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + paie, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionperso, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionperso + revenuTete, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionpersochgt, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt + revenuTete, data=climat)


#Nb de vol (et juste pour le personnel de recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionpersochgt , data=climat_recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionperso , data=climat_recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionpersochgt + revenuTete, data=climat_recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt, data=climat_recherche)
res.reg9 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt + revenuTete, data=climat_recherche)




#Score écolo
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + ScoreEcolo, data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 + ScoreEcolo , data=climat)
res.reg9 <- lm(volshnum ~ sexe + ageAgr  + sitpro2 +discipline_agr3 + ScoreEcolo , data=climat)

#Etape dans la carrière, projet financé (type ; resp/membre),
res.reg1<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat)
res.reg2<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + carriere , data=climat)
res.reg3<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis, data=climat)
res.reg4<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis + carriere, data=climat)
res.reg5<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive , data=climat)
res.reg6<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive + nbpublis , data=climat)
res.reg7<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + particip_ANR + particip_Fr + particip_Europ + particip_Intern + particip_prive + nbpublis + carriere , data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + Profin_Mb_Resp , data=climat)
res.reg9<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + Profin_Mb_Resp + nbpublis + carriere , data=climat)
res.reg10<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2, data=climat)
res.reg11<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis + carriere + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2, data=climat)

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
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solinstit.limitevols, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solinstit.vols6h, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solinstit.train, data=climat)
summary(res.reg8)
#Risques
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.qual, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.fin, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.diffusion, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.donnees, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.avantages, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.isoler, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.insertion, data=climat)
res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.bureaucratie, data=climat)

res.reg8<- lm(volshnum ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + solrisqreducavion.avantages + paie, data=climat)
summary(res.reg8)



freq(climat$solinstit.train)
freq(climat$solinstit.vols6h)
rprop(table(climat$solinstit.train))


rbind(prop.table(table(climat$solinstit.train)), prop.table(table(climat$solinstit.vols6h)))






#Evolution de la quantité de vols
res.reg8<- lm(Evol_GesVol.conf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climat)
res.reg8<- lm(Evol_GesVol.conf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +volshnum, data=climat)

mean(climat$Evol_GesVol.conf, na.rm=T)


summary(res.reg8)


climat$Evol_GesVol.conf.

#Personnel en position de publier

climatPersPubli<-climat %>% filter(!(sitpro %in% c("Technicien·ne", "Adjoint·e technique", "Autre")))


##########################################################################################################################################################
##########################################################################################################################################################
#Sur le nombre de vol (mai 2021)

#Distribution
ggplot(climat_recherche, aes(x = volsnb)) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") +
        labs( x= "Nombre de vols aller-retour en 2019", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre de vols aller-retour en 2019, distribution.pdf",
       width=9, height=5)

mean(climat_recherche$volsnb, na.rm=T)
summary(reglog2)
reglog2 <- lm(volsnb ~ sexe +ageAgr , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe  + ageaccad_tranch2 , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + ageaccad_tranch2 , data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + Moinsavionperso, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +avionpersochgt, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +Moinsavionperso , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +Moinsavionperso +avionperso, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +avionpersochgt +avionperso, data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +Moinsavionconf , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +conffois5ans , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +conffois5ans + Moinsavionconf , data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + revenuTete, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + revenuparadulte, data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + revenuTete +avionperso, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + revenuTete +avionperso +ScoreEcolo, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 +ScoreEcolo, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere + malpaye, data=climat_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr + volsnb , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr +  ScoreEcolo , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr +  revenuTete + ScoreEcolo , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts + opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche )

freq(climat_recherche$carriere)

summary(reglog2)
reglog2 <- lm(volsnb ~ sexe*enfantsnb_rec +ageAgr + sitpro2 ,data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3  + revenuTete,data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe*enfantsage_rec +ageAgr + sitpro2 ,data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climat_recherche )

reglog2 <- lm(volshnum ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climat_recherche )
reglog2 <- lm(volshnum ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climat_recherche )


reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsduree_moy +nbpublistranch2 ,data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climat_recherche )


reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche )

reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 +
                      international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche )


reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsduree_moy, data=climat_recherche )
reglog2 <- lm(volsnb ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + couple, data=climat_recherche )


summary(reglog2)


#The giga modèle complet avec variable statut/carrière/professionnel, sans var écolo assez peu signif

reglog2 <- lm(volsnb ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                       international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                       avionperso, data=climat_recherche)


#Durée moyenne des vols effectués (hors module ?)
climat_recherche$volsduree_moy<-climat_recherche$volshnum/climat_recherche$volsnb
climat_recherche$volsduree_moy[climat_recherche$volsnb==0]<-0

reglog2 <- lm(volsduree_moy ~ sexe*enfantsnb_rec +ageAgr + sitpro2 ,data=climat_recherche )
reglog2 <- lm(volsduree_moy ~ sexe*enfantsage_rec +ageAgr + sitpro2 ,data=climat_recherche )





##################
#Voler/pas voler depuis 3 ans
climat_recherche$vols_dicho3ans <- ifelse(climat_recherche$volsnb=="0", ifelse(climat_recherche$vols2ans=="Non", "N'a pas volé en 3 ans", "A volé depuis 3 ans"),  "A volé depuis 3 ans")
climat_recherche$vols_dicho3ans[is.na(climat_recherche$volsnb)] <- NA

climat_recherche$vols_dicho3ans <- fct_relevel(climat_recherche$vols_dicho3ans, "N'a pas volé en 3 ans")
climat_recherche$vols_dicho3ans <- as_factor(climat_recherche$vols_dicho3ans)

freq(climat_recherche$vols_dicho3ans)
freq(climat_recherche$vols2ans)

library(GGally)
library(ggeffects)
library(labelled)
library(gtsummary)

reglog1 <- glm(vols_dicho3ans ~ sexe + ageAgr  + sitpro2 , data=climat_recherche, family=binomial())
tbl_regression(reglog2, exponentiate = T)

summary (reglog2)
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe  + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr +ageaccad_tranch2+ sitpro2 + discipline_agr3, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + recheco, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3  + Moinsavionperso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + avionpersochgt, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + avionperso, data=climat_recherche, family=binomial())
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + avionperso + revenuTete, data=climat_recherche, family=binomial())
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 +  revenuTete, data=climat_recherche, family=binomial())

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr +ageaccad_tranch2 + sitpro2 + discipline_agr3 + carriere, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + malpaye, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere + malpaye, data=climat_recherche, family=binomial(logit))

freq(climat_recherche$preoccupe)
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3  + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + ageaccad_tranch2 + sitpro2 + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + ageaccad_tranch2 + sitpro2 + discipline_agr3 + effortsconso, data=climat_recherche, family=binomial(logit))

summary (reglog2)

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + pluspreoccupe, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe + pluspreoccupe, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 +  nbpublistranch2 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 +  nbpublistranch2 + malpaye +carriere + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))

summary(reglog2)

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3  + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe*enfantsnb_rec +ageAgr ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr +ageaccad_tranch2 + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climat_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho3ans ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climat_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho3ans ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho3ans ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2 + nbpublistranch2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho3ans ~ sexe + ageAgr+ageaccad_tranch2  + sitpro2 + discipline_agr3 +  international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))

summary (reglog2)

#The giga modèle complet avec variable statut/carrière/professionnel, sans var écolo assez peu signif

reglog2 <- glm(vols_dicho3ans ~ sexe + ageAgr +ageaccad_tranch2  + sitpro2  + discipline_agr3 +recheco +carriere + nbpublistranch2 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 + 
                       international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                       avionperso, data=climat_recherche, family=binomial(logit))





#Ne pas voler en 2019
climat_recherche$vols_dicho <- ifelse(climat_recherche$volsnb=="0", "N'a pas volé en 2019", "A volé en 2019")
climat_recherche$vols_dicho[is.na(climat_recherche$volsnb)] <- NA

climat_recherche$vols_dicho <- fct_relevel(climat_recherche$vols_dicho, "N'a pas volé en 2019")
climat_recherche$vols_dicho <- as_factor(climat_recherche$vols_dicho)


summary (reglog2)

reglog2 <- glm(vols_dicho ~ sexe +ageAgr , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe  + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +Moinsavionperso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +avionpersochgt, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +avionpersochgt +avionperso, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere + malpaye, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho ~ sexe +ageAgr + volsnb , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr +  ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + volsnb + revenuTete + ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))


summary(reglog2)

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3  + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe*enfantsnb_rec +ageAgr ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +hindextranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climat_recherche, family=binomial(logit))


reglog2 <- glm(vols_dicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(vols_dicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))
reglog2 <- glm(vols_dicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))

summary (reglog2)



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

climat$Part_ANR_ERC[climat$projets.anr_r %in% c(0, NA) & climat$projets.anr_m %in% c(0, NA) & climat$projets.france_r %in% c(0, NA) & climat$projets.france_m %in% c(0, NA) 
                    & climat$projets.europe_r %in% c(0, NA) & climat$projets.europe_m %in% c(0, NA) & climat$projets.inter_r  %in% c(0, NA) & climat$projets.inter_m %in% c(0, NA) 
                    &   climat$projets.prive_r  %in% c(0, NA) & climat$projets.prive_m %in% c(0, NA)]<-"Ne participe à aucun projet financé"
climat$Part_ANR_ERC[(climat$projets.anr_r ==1 | climat$projets.anr_m ==1) & climat$projets.france_r %in% c(0, NA) & climat$projets.france_m %in% c(0, NA) 
                    & climat$projets.europe_r %in% c(0, NA) & climat$projets.europe_m %in% c(0, NA) & climat$projets.inter_r  %in% c(0, NA) & climat$projets.inter_m %in% c(0, NA) 
                    &   climat$projets.prive_r  %in% c(0, NA) & climat$projets.prive_m %in% c(0, NA)]<-"Seult finance ANR"
climat$Part_ANR_ERC[climat$projets.anr_r %in% c(0, NA) & climat$projets.anr_m %in% c(0, NA) & (climat$projets.france_r==1 | climat$projets.france_m==1) 
                    & climat$projets.europe_r %in% c(0, NA) & climat$projets.europe_m %in% c(0, NA) & climat$projets.inter_r  %in% c(0, NA) & climat$projets.inter_m %in% c(0, NA) 
                    &   climat$projets.prive_r  %in% c(0, NA) & climat$projets.prive_m %in% c(0, NA)]<-"Seult finance français (hors ANR)"
climat$Part_ANR_ERC[climat$projets.anr_r %in% c(0, NA) & climat$projets.anr_m %in% c(0, NA) & climat$projets.france_r %in% c(0, NA) & climat$projets.france_m %in% c(0, NA) 
                    & (climat$projets.europe_r ==1 | climat$projets.europe_m ==1) & climat$projets.inter_r  %in% c(0, NA) & climat$projets.inter_m %in% c(0, NA) 
                    &   climat$projets.prive_r  %in% c(0, NA) & climat$projets.prive_m %in% c(0, NA)]<-"Seult finance européen"
climat$Part_ANR_ERC[climat$projets.anr_r %in% c(0, NA) & climat$projets.anr_m %in% c(0, NA) & climat$projets.france_r %in% c(0, NA) & climat$projets.france_m %in% c(0, NA) 
                    & climat$projets.europe_r %in% c(0, NA) & climat$projets.europe_m %in% c(0, NA) & (climat$projets.inter_r ==1 | climat$projets.inter_m ==1) 
                    &   climat$projets.prive_r  %in% c(0, NA) & climat$projets.prive_m %in% c(0, NA)]<-"Seult finance internat (hors europe)"
climat$Part_ANR_ERC[climat$projets.anr_r %in% c(0, NA) & climat$projets.anr_m %in% c(0, NA) & climat$projets.france_r %in% c(0, NA) & climat$projets.france_m %in% c(0, NA) 
                    & climat$projets.europe_r %in% c(0, NA) & climat$projets.europe_m %in% c(0, NA) & climat$projets.inter_r  %in% c(0, NA) & climat$projets.inter_m %in% c(0, NA) 
                    &   (climat$projets.prive_r==1 | climat$projets.prive_m ==1)]<-"Seult finance privé"

#Regroupement participation à projet financé (attention, il y a une erreur : les 0 sont comptés comme des 1 avec ce code, or ils devraient être regroupés avec les NA)
#climat$Part_ANR_ERC[is.na(climat$projets.anr_r) & is.na(climat$projets.anr_m) & is.na(climat$projets.europe_r) 
#                    & is.na(climat$projets.europe_m)]<-"Ni financement ANR ou Europe"
#climat$Part_ANR_ERC[(!is.na(climat$projets.anr_r) | !is.na(climat$projets.anr_m)) & is.na(climat$projets.europe_r) 
#                    & is.na(climat$projets.europe_m)]<-"Projet ANR"
#climat$Part_ANR_ERC[is.na(climat$projets.anr_r) & is.na(climat$projets.anr_m) & (!is.na(climat$projets.europe_r) 
#                    | !is.na(climat$projets.europe_m))]<-"Projet européen"               
#climat$Part_ANR_ERC[(!is.na(climat$projets.anr_r) | !is.na(climat$projets.anr_m)) & (!is.na(climat$projets.europe_r) 
#               | !is.na(climat$projets.europe_m))]<-"Projet ANR et projet européen"             


"Part_ANR_ERCProjet ANR"="Participation projet ANR (Ref = ni projet ANR ni européen)",
"Part_ANR_ERCProjet européen"="Participation projet européen",
"Part_ANR_ERCProjet ANR et projet européen" ="Participation projet ANR et projet européen",
"revenuAgrDe 1 500 à 2 499 euros par mois"="1 500 à 2 499 euros par mois",

####################################Mai 2021###################
#A partir des valeurs précises du tableau
freq(climat$volsnb_tot)
freq(climat$volsdist_tot)
freq(climat$tiragemodule)
freq(climat$volshnum)
freq(climat$sexe)
table(climat$sexe)
cumsum(table(climat$sexe))
cumsum(table(climat$volshnum))
cumsum(table(climat$volsnb_tot))
cumsum(table(climat$volsnb))


#####################################################################################################################################@
#Evolution des émissions de GES pour des vols en avions pour des conf, réunions, congrès (et juste pour le personnel de recherche)

#Distribution
ggplot(climat_recherche, aes(x = factor(solevolges.conf))) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        labs( x= "Evolution des GES ces 5 dernières années \n concernant les vols en avion pour les confs, réunions, congrès ", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Chgt des pratiques avion pro conf, nb de répondants par modalité.pdf",
       width=9, height=5)

#Comme on a changé l'intitué de la question avant la dernière vague (pour tenir compte du confinement), 
#on regarde si ça a eu un effet majeur

#Graphiques : changement des pratiques de vols en fonction de la vague de réponse

#Variable avec uniquement la date
climat_recherche$dateDebut<-strftime(strptime(climat_recherche$startdate, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
climat_recherche$dateFin<-strftime(strptime(climat_recherche$datestamp, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")

climat_recherche$NumVague[climat_recherche$dateDebut<"2020-07-07"]<-"Après premier message"
climat_recherche$NumVague["2020-07-07"<=climat_recherche$dateDebut & climat_recherche$dateDebut<"2020-09-07"]<-"Après première relance"
climat_recherche$NumVague["2020-09-07"<=climat_recherche$dateDebut & climat_recherche$dateDebut<"2020-10-12"]<-"Après deuxième relance"
climat_recherche$NumVague["2020-10-12"<=climat_recherche$dateDebut & climat_recherche$dateDebut<"2020-11-16"]<-"Après troisième relance"
climat_recherche$NumVague["2020-11-16"<=climat_recherche$dateDebut]<-"Après quatrième relance"

climat_recherche$NumVague <- factor(climat_recherche$NumVague,
                                        levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

graphEffectif<-climat_recherche %>% group_by(solevolges.conf, NumVague) %>%summarise(Effectif=n())
graphEffectif$NumVague <- ordered(graphEffectif$NumVague, levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

effectifsvague<-climat_recherche %>% group_by(NumVague) %>%summarise(Effectifvague=n())
graphEffectif<-merge(graphEffectif, effectifsvague, by="NumVague", all =TRUE )
graphEffectif$PourcentEffectif<-graphEffectif$Effectif/graphEffectif$Effectifvague*100

ggplot(graphEffectif, aes(x = solevolges.conf, y = PourcentEffectif, fill = NumVague)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() + guides(fill=guide_legend(reverse = TRUE)) +
                scale_fill_viridis_d() +
                labs(y="% des répondants par vague", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")


#La même chose sans les nons réponses

graphEffectif<-climat_recherche %>% filter(!is.na(solevolges.conf)) %>% group_by(solevolges.conf, NumVague) %>%summarise(Effectif=n())
graphEffectif$NumVague <- ordered(graphEffectif$NumVague, levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

effectifsvague<-climat_recherche %>% filter(!is.na(solevolges.conf)) %>%group_by(NumVague) %>%summarise(Effectifvague=n())
graphEffectif<-merge(graphEffectif, effectifsvague, by="NumVague", all =TRUE )
graphEffectif$PourcentEffectif<-graphEffectif$Effectif/graphEffectif$Effectifvague*100

ggplot(graphEffectif, aes(x = solevolges.conf, y = PourcentEffectif, fill = NumVague)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() +  guides(fill=guide_legend(reverse = TRUE)) +
        scale_fill_viridis_d() +
        labs(y="% des répondants par vague (hors non-réponses)", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Chgt des pratiques avion pro conf, pourcentage de répondants par vague, hors NA, répartition réponses.pdf",
       width=9, height=5)
freq(climat_recherche$solevolges.conf)

#Contruction variable : Prendre moins l'avion depuis 5 ans pour des confs, réunions, congrès
climat_recherche$Moinsavionconf[climat_recherche$solevolges.conf %in% c("Fortement diminué", "Fortement diminué")]<-"Oui"
climat_recherche$Moinsavionconf[climat_recherche$solevolges.conf %in% c("Été à peu près stables", "Un peu augmenté", "Fortement augmenté")]<-"Non"
climat_recherche$Moinsavionconf<-as.factor(climat_recherche$Moinsavionconf)

summary(reglog2)
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe  + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + recheco, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +Moinsavionperso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +avionpersochgt, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +avionpersochgt +avionperso, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + carriere + malpaye, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + volsnb , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr +  ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + volsnb + revenuTete + ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + pluspreoccupe, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 +preoccupe + pluspreoccupe, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))


summary(reglog2)

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +nbpublistranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +hindextranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climat_recherche, family=binomial(logit))


reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))
reglog2 <- glm(Moinsavionconf ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))


summary(reglog2)

#####################################################################################################################################@
#Evolution des émissions de GES pour des vols en avions pour recueillir des données (et juste pour le personnel de recherche)

#Distribution
ggplot(climat_recherche, aes(x = factor(solevolges.donnees))) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        labs( x= "Evolution des GES ces 5 dernières années \n concernant les vols en avion pour recueillir des données ", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Chgt des pratiques avion pro recueil des données, nb de répondants par modalité.pdf",
       width=9, height=5)

#Comme on a changé l'intitué de la question avant la dernière vague (pour tenir compte du confinement), 
#on regarde si ça a eu un effet majeur

#Graphiques : changement des pratiques de vols en fonction de la vague de réponse

#Variable avec uniquement la date

graphEffectif<-climat_recherche %>% group_by(solevolges.donnees, NumVague) %>%summarise(Effectif=n())
graphEffectif$NumVague <- ordered(graphEffectif$NumVague, levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

effectifsvague<-climat_recherche %>% group_by(NumVague) %>%summarise(Effectifvague=n())
graphEffectif<-merge(graphEffectif, effectifsvague, by="NumVague", all =TRUE )
graphEffectif$PourcentEffectif<-graphEffectif$Effectif/graphEffectif$Effectifvague*100

ggplot(graphEffectif, aes(x = solevolges.donnees, y = PourcentEffectif, fill = NumVague)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() + guides(fill=guide_legend(reverse = TRUE)) +
        scale_fill_viridis_d() +
        labs(y="% des répondants par vague", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")


#La même chose sans les nons réponses

graphEffectif<-climat_recherche %>% filter(!is.na(solevolges.donnees)) %>% group_by(solevolges.donnees, NumVague) %>%summarise(Effectif=n())
graphEffectif$NumVague <- ordered(graphEffectif$NumVague, levels = c("Après premier message", "Après première relance", "Après deuxième relance", "Après troisième relance", "Après quatrième relance"))

effectifsvague<-climat_recherche %>% filter(!is.na(solevolges.donnees)) %>%group_by(NumVague) %>%summarise(Effectifvague=n())
graphEffectif<-merge(graphEffectif, effectifsvague, by="NumVague", all =TRUE )
graphEffectif$PourcentEffectif<-graphEffectif$Effectif/graphEffectif$Effectifvague*100

ggplot(graphEffectif, aes(x = solevolges.donnees, y = PourcentEffectif, fill = NumVague)) +
        geom_bar(stat = "identity", position = position_dodge(),
                 colour = "grey30") + coord_flip() +  guides(fill=guide_legend(reverse = TRUE)) +
        scale_fill_viridis_d() +
        labs(y="% des répondants par vague (hors non-réponses)", x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans")


#Contruction variable : Prendre moins l'avion/voiture/bateau depuis 5 ans pour le recueil de données
climat_recherche$Moinsaviondonnees[climat_recherche$solevolges.donnees %in% c("Fortement diminué", "Fortement diminué")]<-"Oui"
climat_recherche$Moinsaviondonnees[climat_recherche$solevolges.donnees %in% c("Été à peu près stables", "Un peu augmenté", "Fortement augmenté")]<-"Non"
climat_recherche$Moinsaviondonnees<-as.factor(climat_recherche$Moinsaviondonnees)

summary(reglog2)
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe  + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3  +Moinsavionperso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +Moinsavionperso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3  +avionpersochgt, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3  +avionpersochgt +avionperso, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + volsnb , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr +  ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr +  revenuTete + ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))

summary(reglog2)
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))
summary(reglog2)

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +nbpublistranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +hindextranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climat_recherche, family=binomial(logit))


reglog2 <- glm(Moinsaviondonnees ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))


summary(reglog2)

#Contruction variable alternative (sans les "stables") : Prendre moins l'avion/voiture/bateau depuis 5 ans pour le recueil de données
#On ne tient pas compte des "stables"
climat_recherche$Moinsaviondonnees2[climat_recherche$solevolges.donnees %in% c("Fortement diminué", "Fortement diminué")]<-"Oui"
climat_recherche$Moinsaviondonnees2[climat_recherche$solevolges.donnees %in% c("Un peu augmenté", "Fortement augmenté")]<-"Non"
climat_recherche$Moinsaviondonnees2<-as.factor(climat_recherche$Moinsaviondonnees2)


summary(reglog2)
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe  + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + ageaccad_tranch2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3  +Moinsavionperso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +Moinsavionperso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3  +avionpersochgt, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3  +avionpersochgt +avionperso, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + volsnb , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr +  ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr +  revenuTete + ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))

summary(reglog2)
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +  opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))
summary(reglog2)

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 + volsnb + revenuTete,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +nbpublistranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + hindextranch2 ,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + volsnb +hindextranch2 ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe +ageAgr + sitpro2 + discipline_agr3 + conffois5ans ,data=climat_recherche, family=binomial(logit))


reglog2 <- glm(Moinsaviondonnees2 ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsaviondonnees2 ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))
reglog2 <- glm(Moinsaviondonnees2 ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche,family=binomial(logit))


summary(reglog2)


#####################################################################################################################################@
#Avion perso (et juste pour le personnel de recherche)

#Distribution nb de vols et changement pratiques

ggplot(climat_recherche, aes(x = factor(avionperso))) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        labs( x= "Nombre de vols en avion dans la vie privé en 2019", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre de vols avion vie privée, répartition réponses.pdf",
       width=9, height=5)


ggplot(climat_recherche, aes(x = factor(avionpersochgt))) +
        geom_bar(stat = "count", position = position_dodge(),
                 colour = "grey30") + coord_flip() +
        labs( x= "Changement des pratiques de déplacement en avion\n dans la vie privé depuis 5 ans", y="nombre de répondants")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Changement des pratiques avion vie privée, répartition réponses.pdf",
       width=9, height=5)

#Moyennes par catégorie de changement de pratiques : distribution
graphMoyenne<-climat_recherche %>% group_by(avionpersochgt) %>%summarise(MoyenneVols = mean(avionpersonum, na.rm=T),
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

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre moyen de vols privés en fonction du degré d'évolution de la prise de l'avion.pdf",
       width=9, height=5)

#Si on enlève ceux qui sont à zéro vols (potentiellement des gens qui ne volent jamais et qui ne peuvent pas réduire)
graphMoyenne<-climat_recherche %>% filter(avionpersonum>0) %>%
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

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre moyen de vols privés en fonction du degré d'évolution de la prise de l'avion, pour ceux qui ont volé.pdf",
       width=9, height=5)


res.reg9 <- lm(avionpersonum ~ sexe + ageAgr , data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2  , data=climat_recherche)

res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + avionpersochgt , data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + avionpersochgt +  revenuTete, data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe*enfantsage_rec + ageAgr  + sitpro2 + avionpersochgt , data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe*enfantsnb_rec + ageAgr  + sitpro2 + avionpersochgt , data=climat_recherche)


res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + statutpar.p , data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + dippar.p , data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + statutpar.p +revenuparadulte , data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + dippar.p +revenuparadulte, data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2 + revenuTete, data=climat_recherche)

res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2+ discipline_agr3 , data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2+ discipline_agr3 + revenuTete, data=climat_recherche)
res.reg9 <- lm(avionpersonum ~ sexe + ageAgr  + sitpro2+ discipline_agr3 + revenuparadulte, data=climat_recherche)


res.reg9 <- lm(avionpersonum ~   avionpersochgt , data=climat_recherche)
res.reg9 <- lm(avionpersonum ~   avionpersochgt +scorecolo , data=climat_recherche)

res.reg9 <- lm(revenuTete ~ sexe   , data=climat_recherche)

summary(res.reg9)

mean(climat_recherche$avionpersonum, na.rm=T)
freq(climat_recherche$Moinsavionperso)
freq(climat_recherche$avionpersochgt)
freq(climat_recherche$ageAgr)
freq(climat_recherche$revenuTete)


#Prendre moins l'avion depuis 5 ans dans la vie privée
climat_recherche$Moinsavionperso[climat_recherche$avionpersochgt %in% c("Oui, je le prends beaucoup moins", "Oui, je le prends un peu moins")]<-"Oui"
climat_recherche$Moinsavionperso[climat_recherche$avionpersochgt %in% c("Oui, je le prends beaucoup plus", "Oui, je le prends un peu plus", "Non")]<-"Non"
climat_recherche$Moinsavionperso<-as.factor(climat_recherche$Moinsavionperso)

climat_recherche$aucunvolperso[climat_recherche$avionperso!="Aucun aller-retour" & !is.na(climat_recherche$avionperso)]<-"Non"
climat_recherche$aucunvolperso[climat_recherche$avionperso=="Aucun aller-retour"]<-"Oui"
freq(climat_recherche$aucunvolperso)

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + aucunvolperso , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + ScoreEcolo , data=climat_recherche, family=binomial(logit))


reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + aucunvolperso + revenuTete + ScoreEcolo , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + aucunvolperso + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + aucunvolperso + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso + revenuTete + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan + effortsconso, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + preoccupe, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + pluspreoccupe, data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 +preoccupe + pluspreoccupe, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + 
                       opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata +opinionecolo.effondrement, data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.efforts +
                       opinionecolo.cata + opinionecolo.techno + opinionecolo.proteger + opinionecolo.contraintes, data=climat_recherche, family=binomial(logit))
summary(reglog2)

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso ,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe*enfantsnb_rec +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso + revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe*enfantsage_rec +ageAgr + sitpro2 + discipline_agr3 + aucunvolperso + revenuTete,data=climat_recherche, family=binomial(logit))

reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + avionperso+ revenuTete,data=climat_recherche, family=binomial(logit))
reglog2 <- glm(Moinsavionperso ~ sexe +ageAgr + sitpro2 + avionperso+ revenuTete,data=climat_recherche, family=binomial(logit))


summary(reglog2)

freq(climat_recherche$opinionecolo.decroissance)

climat_recherche$aucunvolperso<- climat_recherche$avionperso!="Aucun aller-retour" & 


