library(tidyr)
library(tidyverse)


####################################@
#RECODAGE
#Variable avec uniquement la date
climat$dateDebut<-strftime(strptime(climat$startdate, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")

#Dichotomie par vague
climat$NumVague[climat$dateDebut<"2020-07-07"]<-"Après premier message"
climat$NumVague["2020-07-07"<=climat$dateDebut & climat$dateDebut<"2020-09-07"]<-"Après première relance"
climat$NumVague["2020-09-07"<=climat$dateDebut & climat$dateDebut<"2020-10-12"]<-"Après deuxième relance"
climat$NumVague["2020-10-12"<=climat$dateDebut & climat$dateDebut<"2020-11-16"]<-"Après troisième relance"
climat$NumVague["2020-11-16"<=climat$dateDebut]<-"Après quatrième relance"

freq(climat$NumVague)
climat$ScoreEcolo<-0
climat$ScoreEcolo[climat$dixannees.bilan.=="Oui" & !is.na(climat$dixannees.bilan.)]<-climat$ScoreEcolo[climat$dixannees.bilan.=="Oui" & !is.na(climat$dixannees.bilan)]+1
climat$ScoreEcolo[climat$dixannees.giec.=="Oui" & !is.na(climat$dixannees.giec.)]<-climat$ScoreEcolo[climat$dixannees.giec.=="Oui" & !is.na(climat$dixannees.giec.)]+1
climat$ScoreEcolo[climat$dixannees.asso.=="Oui"& !is.na(climat$dixannees.asso.)]<-climat$ScoreEcolo[climat$dixannees.asso.=="Oui" & !is.na(climat$dixannees.asso.)]+1
climat$ScoreEcolo[climat$dixannees.marche.=="Oui" & !is.na(climat$dixannees.marche.)]<-climat$ScoreEcolo[climat$dixannees.marche.=="Oui" & !is.na(climat$dixannees.marche.)]+1
climat$ScoreEcolo[climat$dixannees.vote.=="Oui" & !is.na(climat$dixannees.vote.)]<-climat$ScoreEcolo[climat$dixannees.vote.=="Oui" & !is.na(climat$dixannees.vote.)]+1
climat$ScoreEcolo[is.na(climat$dixannees.bilan.) & is.na(climat$dixannees.giec.) &is.na(climat$dixannees.asso.) & is.na(climat$dixannees.marche.) & is.na(climat$dixannees.vote.)]<-NA


freq (climat$ScoreEcolo, total = T)
?freq

table(climat$ScoreEcolo)
freq(climat$dixannees.bilan.)
freq(climat$dixannees.giec.)
freq(climat$dixannees.asso.)
freq(climat$dixannees.marche.)
freq(climat$dixannees.vote, total = T)

###################################################@@
#Modalités de référence dans les régressions
climat$NumVague <- as.factor(climat$NumVague)
climat$NumVague <- relevel(climat$NumVague, ref = "Après premier message")

################################################
#Graphique : nombre d'ouverture du questionnaire en fonction de l'heure de la journée
plot(table(strftime(strptime(climat$startdate, "%Y-%m-%d %H:%M:%S"), "%H")), xlab ="Heure de la journée", ylab="Nb d'ouvertures du questionnaire")

#En excluant les jours de relances (il manque dans doute les relances des réponses partielles à exclure aussi ?)

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

climat$AvecMarianne[climat$dateDebut<"2020-10-12"]<-0
climat$AvecMarianne[climat$dateDebut>="2020-10-12"]<-1

res.reg1 <- lm(AvecMarianne ~ sexe , data=climat)
res.reg1<- lm(AvecMarianne ~ sexe + ageAgr, data=climat)
res.reg1<- lm(AvecMarianne ~ sexe + ageAgr  + sitpro + discipline_agr3, data=climat)

summary(res.reg1)

#Score écolo et numéro de vague

res.reg1 <- lm(ScoreEcolo ~ NumVague , data=climat)
res.reg1 <- lm(ScoreEcolo ~ NumVague + sexe + ageAgr, data=climat)
res.reg1 <- lm(ScoreEcolo ~ sexe + ageAgr, data=climat)


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
