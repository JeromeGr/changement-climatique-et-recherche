library(tidyr)
library(tidyverse)


####################################@
mean(climat$TpsRempliMin)
#Ca donne pas la même chose que ça : 
mean(climat$interviewtime)
1873/60

mean(climat$TpsRemp_MmJour, na.rm=T)

#Passage en numérique de la variable "inquiétude"
climat$preoccupeNum[climat$preoccupe=="Pas du tout préoccupé·e"]<-0
climat$preoccupeNum[climat$preoccupe=="Un peu préoccupé·e"]<-1
climat$preoccupeNum[climat$preoccupe=="Assez préoccupé·e"]<-2
climat$preoccupeNum[climat$preoccupe=="Très préoccupé·e"]<-3
climat$preoccupeNum[climat$preoccupe=="Extrêmement préoccupé·e"]<-4
climat$preoccupeNum[climat$preoccupe=="Un peu préoccupé·e"]<-1
climat$preoccupeNum[climat$preoccupe=="Un peu préoccupé·e"]<-1



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
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + sitpro2 , data=climat)
res.reg1<- lm(EffetMarianne ~ sexe + ageAgr  + sitpro2 + ScoreEcoloPond , data=climat)
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
res.reg1 <- lm(preoccupeNum ~ NumVague + sexe + ageAgr + sitpro2, data=climat)
res.reg1 <- lm(preoccupeNum ~ sexe + ageAgr, data=climat)

#Revenu par tête et vagues
res.reg1 <- lm(revenuTete ~ NumVague , data=climat)

#Enfant et vagues
res.reg1 <- lm(enfantsnb ~ NumVague , data=climat)
res.reg1 <- lm(enfantsnb ~ NumVague + sexe + ageAgr , data=climat)
res.reg1 <- lm(enfantsnb ~ NumVague + sexe + ageAgr + sitpro2, data=climat)
freq(climat$enfantsage, total=T)

#Nombre de publis
res.reg1 <- lm(nbpublis ~ NumVague , data=climat)
res.reg1 <- lm(enfantsnb ~ NumVague + sexe + ageAgr , data=climat)
res.reg1 <- lm(nbpublis ~ NumVague + sexe + ageAgr + sitpro2, data=climat)



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

freq(climat$dixannees.bilan)
freq(climat$dixannees.giec)
freq(climat$dixannees.asso)
freq(climat$dixannees.marche)
freq(climat$dixannees.vote)

#Heures de vols et vagues
res.reg1 <- lm(volshnum ~ NumVague, data=climat)
summary(res.reg1)




freq(climat$avantMarianne)
climat$datedebut<-as.Date(as.character(climat$startdate), format="%d/%m/%Y")
