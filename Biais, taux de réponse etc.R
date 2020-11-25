library(tidyr)
library(tidyverse)


####################################@
#RECODAGE
#Variable avec uniquement la date
climat$dateDebut<-strftime(strptime(climat$startdate, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
climat$dateFin<-strftime(strptime(climat$datestamp, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")

#Durée de remplissage
climat$datestamp1 <- as.POSIXct(climat$datestamp, format ="%Y-%m-%d %H:%M:%S")
climat$startdate1 <- as.POSIXct(climat$startdate, format ="%Y-%m-%d %H:%M:%S")
climat$TpsRempliMin<-as.numeric(difftime(climat$datestamp1, climat$startdate1,  units="mins"))
mean(climat$TpsRempliMin)
#Ca donne pas la même chose que ça : 
mean(climat$interviewtime)
1873/60

#Durée de remplissage sur le même jour
climat$TpsRemp_MmJour[climat$dateDebut==climat$dateFin]<-as.numeric(difftime(climat$datestamp1[climat$dateDebut==climat$dateFin], climat$startdate1[climat$dateDebut==climat$dateFin],  units="mins"))
mean(climat$TpsRemp_MmJour, na.rm=T)


#Dichotomie par vague
climat$NumVague[climat$dateDebut<"2020-07-07"]<-"Après premier message"
climat$NumVague["2020-07-07"<=climat$dateDebut & climat$dateDebut<"2020-09-07"]<-"Après première relance"
climat$NumVague["2020-09-07"<=climat$dateDebut & climat$dateDebut<"2020-10-12"]<-"Après deuxième relance"
climat$NumVague["2020-10-12"<=climat$dateDebut & climat$dateDebut<"2020-11-16"]<-"Après troisième relance"
climat$NumVague["2020-11-16"<=climat$dateDebut]<-"Après quatrième relance"

#Construction d'un "score écolo"
climat$ScoreEcolo<-0
climat$ScoreEcolo[climat$dixannees.bilan.=="Oui" & !is.na(climat$dixannees.bilan.)]<-climat$ScoreEcolo[climat$dixannees.bilan.=="Oui" & !is.na(climat$dixannees.bilan)]+1
climat$ScoreEcolo[climat$dixannees.giec.=="Oui" & !is.na(climat$dixannees.giec.)]<-climat$ScoreEcolo[climat$dixannees.giec.=="Oui" & !is.na(climat$dixannees.giec.)]+1
climat$ScoreEcolo[climat$dixannees.asso.=="Oui"& !is.na(climat$dixannees.asso.)]<-climat$ScoreEcolo[climat$dixannees.asso.=="Oui" & !is.na(climat$dixannees.asso.)]+1
climat$ScoreEcolo[climat$dixannees.marche.=="Oui" & !is.na(climat$dixannees.marche.)]<-climat$ScoreEcolo[climat$dixannees.marche.=="Oui" & !is.na(climat$dixannees.marche.)]+1
climat$ScoreEcolo[climat$dixannees.vote.=="Oui" & !is.na(climat$dixannees.vote.)]<-climat$ScoreEcolo[climat$dixannees.vote.=="Oui" & !is.na(climat$dixannees.vote.)]+1
climat$ScoreEcolo[is.na(climat$dixannees.bilan.) & is.na(climat$dixannees.giec.) &is.na(climat$dixannees.asso.) & is.na(climat$dixannees.marche.) & is.na(climat$dixannees.vote.)]<-NA

#Autre façon de calculer un score écolo
climat$ScoreEcoloPond<-0
climat$ScoreEcoloPond[climat$dixannees.bilan.=="Oui" & !is.na(climat$dixannees.bilan.)]<-climat$ScoreEcoloPond[climat$dixannees.bilan.=="Oui" & !is.na(climat$dixannees.bilan)]+2
climat$ScoreEcoloPond[climat$dixannees.giec.=="Oui" & !is.na(climat$dixannees.giec.)]<-climat$ScoreEcoloPond[climat$dixannees.giec.=="Oui" & !is.na(climat$dixannees.giec.)]+1
climat$ScoreEcoloPond[climat$dixannees.asso.=="Oui"& !is.na(climat$dixannees.asso.)]<-climat$ScoreEcoloPond[climat$dixannees.asso.=="Oui" & !is.na(climat$dixannees.asso.)]+2
climat$ScoreEcoloPond[climat$dixannees.marche.=="Oui" & !is.na(climat$dixannees.marche.)]<-climat$ScoreEcoloPond[climat$dixannees.marche.=="Oui" & !is.na(climat$dixannees.marche.)]+2
climat$ScoreEcoloPond[climat$dixannees.vote.=="Oui" & !is.na(climat$dixannees.vote.)]<-climat$ScoreEcoloPond[climat$dixannees.vote.=="Oui" & !is.na(climat$dixannees.vote.)]+1
climat$ScoreEcoloPond[is.na(climat$dixannees.bilan.) & is.na(climat$dixannees.giec.) &is.na(climat$dixannees.asso.) & is.na(climat$dixannees.marche.) & is.na(climat$dixannees.vote.)]<-NA

#Passage en numérique de la variable "inquiétude"
climat$preoccupeNum[climat$preoccupe=="Pas du tout préoccupé·e"]<-0
climat$preoccupeNum[climat$preoccupe=="Un peu préoccupé·e"]<-1
climat$preoccupeNum[climat$preoccupe=="Assez préoccupé·e"]<-2
climat$preoccupeNum[climat$preoccupe=="Très préoccupé·e"]<-3
climat$preoccupeNum[climat$preoccupe=="Extrêmement préoccupé·e"]<-4
climat$preoccupeNum[climat$preoccupe=="Un peu préoccupé·e"]<-1
climat$preoccupeNum[climat$preoccupe=="Un peu préoccupé·e"]<-1


#Temps partiel en numérique : porportion d'un temps complet (1 pour les temps complets)
climat$tpsquotiteNum[climat$tpsplein=="Oui"]<-1
climat<-tidyr::extract(climat, tpsquotite, "tpsquotiteVal", "(\\d+)", remove=FALSE)
climat$tpsquotiteVal<-as.numeric(climat$tpsquotiteVal)
climat$tpsquotiteNum<-climat$tpsquotiteVal/100

#Doctorants : numérique
climat$doctoNum[climat$docto=="Oui"]<-1
climat$doctoNum[climat$docto=="Non"]<-0


################################################
#Graphique : nombre d'ouverture du questionnaire en fonction de l'heure de la journée
plot(table(strftime(strptime(climat$startdate, "%Y-%m-%d %H:%M:%S"), "%H")), xlab ="Heure de la journée", ylab="Nb d'ouvertures du questionnaire")

#En excluant les jours de relances (il manque sans doute les relances des réponses partielles à exclure aussi ?)

climatSSrelances<-climat%>% filter (!(dateDebut %in% c("2020-06-29", "2020-06-30", "2020-07-07", "2020-09-07", "2020-10-12", "2020-11-16"  )))

plot(table(strftime(strptime(climatSSrelances$startdate, "%Y-%m-%d %H:%M:%S"), "%H")), xlab ="Heure de la journée", ylab="Nb d'ouvertures du questionnaire (hors jour relance)")


#Graphique : nombre d'ouvertures du questionnaire en fonction de la date de la journée

plot(table(strftime(strptime(climat$startdate, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")), ylab="Date d'ouverture du questionnaire")

#Autre version
TablDateDebut<-climat %>% group_by(dateDebut) %>% summarize(Effectif=n())

ggplot(TablDateDebut, aes(x=dateDebut, y=Effectif))+
  geom_bar(stat="identity")+
  scale_x_discrete(labels=NULL)+
  theme_classic()+
  xlab("Date d'ouverture du questionnaire") +
  scale_y_continuous(name="Nombre d'ouvertures")

#l'effet Marianne (pas d'effet sur la proportion par sexe ou léger ; un peu sur l'âge (les plus vieux))

climat$EffetMarianne[climat$dateDebut<"2020-10-12"]<-0
climat$EffetMarianne[climat$dateDebut>="2020-10-12"]<-1

res.reg1 <- lm(EffetMarianne ~ sexe , data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr + enfantsnb, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr + ScoreEcoloPond , data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr + preoccupeNum, data=climat)
res.reg1<- lm(EffetMarianne ~ preoccupeNum, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + sitpro , data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + sitpro + ScoreEcoloPond , data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + discipline_agr3, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + discipline_agr3 + ScoreEcoloPond, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + employeur, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + employeur + ScoreEcoloPond, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + tpsquotiteNum, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + interviewtime, data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + TpsRemp_MmJour  , data=climat)



summary(res.reg1)

#Préoccupé et vagues
mean(climat$preoccupeNum, na.rm=T)
res.reg1 <- lm(preoccupeNum ~ NumVague , data=climat)
res.reg1 <- lm(preoccupeNum ~ NumVague + sexe + ageAgr + sitpro, data=climat)
res.reg1 <- lm(preoccupeNum ~ sexe + ageAgr, data=climat)

#Revenu par tête et vagues
res.reg1 <- lm(revenuTete ~ NumVague , data=climat)

#Enfant et vagues
res.reg1 <- lm(enfantsnb ~ NumVague , data=climat)
res.reg1 <- lm(enfantsnb ~ NumVague + sexe + ageAgr , data=climat)
res.reg1 <- lm(enfantsnb ~ NumVague + sexe + ageAgr + sitpro, data=climat)
freq(climat$enfantsage, total=T)

#Nombre de publis
res.reg1 <- lm(nbpublis ~ NumVague , data=climat)
res.reg1 <- lm(enfantsnb ~ NumVague + sexe + ageAgr , data=climat)
res.reg1 <- lm(nbpublis ~ NumVague + sexe + ageAgr + sitpro, data=climat)



#Temps partiel et vague
res.reg1 <- lm(tpsquotiteNum ~ NumVague , data=climat)
freq(climat$tpsquotite)

#Doctorant et vague
res.reg1 <- lm(doctoNum ~ NumVague + ageAgr , data=climat)
summary(res.reg1)
freq(climat$docto)

#Score écolo et numéro de vague

res.reg1 <- lm(ScoreEcolo ~ NumVague , data=climat)
res.reg1 <- lm(ScoreEcolo ~ NumVague + sexe + ageAgr, data=climat)
res.reg1 <- lm(ScoreEcolo ~ sexe + ageAgr, data=climat)

#Score écolo pondéré
res.reg1 <- lm(ScoreEcoloPond ~ NumVague , data=climat)
res.reg1 <- lm(ScoreEcoloPond ~ NumVague + sexe + ageAgr, data=climat)



mean(climat$ScoreEcolo, na.rm=T)

freq(climat$dixannees.bilan.)
freq(climat$dixannees.giec.)
freq(climat$dixannees.asso.)
freq(climat$dixannees.marche.)
freq(climat$dixannees.vote.)

#Heures de vols et vagues
res.reg1 <- lm(volshnum ~ NumVague, data=climat)
summary(res.reg1)




freq(climat$avantMarianne)
climat$datedebut<-as.Date(as.character(climat$startdate), format="%d/%m/%Y")
