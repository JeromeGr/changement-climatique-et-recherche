library(tidyr)
library(tidyverse)


####################################@
mean(climat$TpsRempliMin)
#Ca donne pas la même chose que ça : 
mean(climat$interviewtime)
1873/60

mean(climat$TpsRemp_MmJour, na.rm=T)
median(climat$TpsRemp_MmJour, na.rm=T)

#Médiane et moyenne pour ceux qui sont allés jusqu'à la fin
test<-climat%>% filter(lastpage==8)
median(test$TpsRemp_MmJour, na.rm=T)
mean(test$TpsRemp_MmJour, na.rm=T)


#Nombre de personnes ayant répondu avant la première relance
test<-climat%>% filter(climat$startdate<"2020-07-06")

?median
class(climat$lastpage)
freq(climat$lastpage)

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


#Le 6 juillet et le 15 octobre, on a fait une relance sur les incomplets
#Le 26 juin : premier envoi test
climatSSrelances2<-climat%>% filter (!(dateDebut %in% c("2020-06-26", "2020-06-29", "2020-06-30","2020-07-06" ,  "2020-07-07", "2020-09-07", "2020-10-12", "2020-10-15", "2020-11-16", "2020-11-24"  )))

freq(climat$dateDebut)



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

#Autres modèles (octobre 2021)
#Equivalent de l'effet Marianne mais plus propre (pas passage en numérique)
reglog3 <- glm(apres3erelance ~ sexe + ageAgr   + sitpro2  + discipline_agr3 , data=climatRegr, family=binomial(logit) )
reglog3 <- glm(apres3erelance ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso , data=climatRegr, family=binomial(logit) )

summary(reglog3)

#Par numéro de vague
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso + lastpage , data=climatRegr )

summary(res.reg1)

######################################
#Par date (c'est peut être le plus propre ?), ie le nombre de jours écoulés depuis lancement premier message
climatRegr$datedebut<-as.numeric(as.Date(climatRegr$dateDebut))-18438
freq(climat$datedebut)
mean(climatRegr$datedebut)

res.reg1 <- lm(datedebut ~ sexe + ageAgr, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr , data=climatRegr )
res.reg1 <- lm(datedebut ~ sitpro2 , data=climatRegr )

res.reg1 <- lm(datedebut ~ preoccupe2, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + preoccupe2, data=climatRegr )

res.reg1 <- lm(datedebut ~ dixannees.marche , data=climatRegr )
res.reg1 <- lm(datedebut ~ ageAgr + dixannees.marche , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche , data=climatRegr )

res.reg1 <- lm(datedebut ~ dixannees.vote , data=climatRegr )
res.reg1 <- lm(datedebut ~ dixannees.giec , data=climatRegr )
res.reg1 <- lm(datedebut ~ dixannees.asso , data=climatRegr )
res.reg1 <- lm(datedebut ~ dixannees.bilan , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.bilan , data=climatRegr )


res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso , data=climatRegr )

freq(climatRegr$sitpro2)

summary(res.reg1)

res.reg1 <- lm(datedebut ~ opinionecolo.cata , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.cata , data=climatRegr )

res.reg1 <- lm(datedebut ~ opinionecolo.decroissance , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.decroissance , data=climatRegr )

res.reg1 <- lm(datedebut ~ opinionecolo.proteger , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.proteger , data=climatRegr )

res.reg1 <- lm(datedebut ~ opinionecolo.efforts , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.efforts , data=climatRegr )

res.reg1 <- lm(datedebut ~ opinionecolo.contraintes , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.contraintes , data=climatRegr )

summary(res.reg1)

#Modèle total avec les variables d'opinion qui sont signif
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 
               + preoccupe2 + dixannees.marche+
                 opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                  opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr )


res.reg1 <- lm(datedebut ~ lastpage, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + lastpage, data=climatRegr )
res.reg1<- lm(datedebut ~TpsRemp_MmJour  , data=climatRegr)
res.reg1<- lm(datedebut ~ sexe + ageAgr  + TpsRemp_MmJour  , data=climatRegr)
res.reg1<- lm(datedebut ~ sexe + ageAgr  + interviewtime, data=climatRegr)
summary(res.reg1)


res.reg1<- lm(datedebut ~ enfantsnb, data=climatRegr)
res.reg1<- lm(datedebut ~ ageAgr + enfantsnb, data=climatRegr)
res.reg1<- lm(datedebut ~ ageAgr + enfantsage_rec, data=climatRegr)
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + enfantsage_rec, data=climatRegr )
res.reg1<- lm(datedebut ~ ageAgr + couple, data=climatRegr)
res.reg1<- lm(datedebut ~ sexe + ageAgr + enfantsnb, data=climatRegr)


res.reg1<- lm(datedebut ~ sexe + ageAgr  + employeur, data=climatRegr)

res.reg1<- lm(datedebut ~ tpsquotiteNum, data=climatRegr)
res.reg1<- lm(datedebut ~ sexe + ageAgr + sitpro2  + discipline_agr3  + tpsquotiteNum, data=climatRegr)

res.reg1<- lm(datedebut ~ tpsplein, data=climatRegr)

res.reg1<- lm(datedebut ~ sexe + ageAgr + sitpro2  + discipline_agr3 + malpaye, data=climatRegr)
res.reg1<- lm(datedebut ~ carriere, data=climatRegr)

res.reg1<- lm(datedebut ~ sexe + ageAgr + sitpro2  + discipline_agr3 + carriere, data=climatRegr)

summary(res.reg1)

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + nbpublistranch2, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + hindextranch2, data=climatRegr )

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + volshnum, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + Moinsavionperso, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + avionpersochgt, data=climatRegr )

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solevolges.conf, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + Moinsavionconf, data=climatRegr )

res.reg1 <- lm(datedebut ~  solreducrech, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducrech, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.donnees, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.conf, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.exp, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.info, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.domicile, data=climatRegr )

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.train, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.compensation, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.bilanges, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.limitevols, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.selection, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.vols6h, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.conf, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.info, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.equip, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.vege, data=climatRegr )




summary(res.reg1)
freq(climat$solinstit.vege)

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.anr, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.france, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.europe, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.inter, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.prive, data=climatRegr )

summary(res.reg1)

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.poste, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.natio, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.naiss, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.scol, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.etudes, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.postdoc, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.travail, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.prog, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.asso, data=climatRegr )


res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + 
                international.poste + international.natio +  international.naiss + international.scol + international.etudes + 
                 international.postdoc + international.travail + international.prog + international.asso, data=climatRegr )

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  dippar.m, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  dippar.p, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  statutpar.m, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  statutpar.p, data=climatRegr )

summary(res.reg1)

res.reg1 <- lm(datedebut ~ ageAgr , data=climatRegr )

#Modèle total
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  
                 preoccupe2 + opinionecolo.proteger + opinionecolo.contraintes +opinionecolo.cata+
                 solreducrech2 + solinstit.train + solinstit.limitevols + solinstit.bilanges + solinstit.vege +
                 dixannees.bilan +
                 solreducperso.donnees + solreducperso.exp +
                 malpaye + carriere + tpsplein + 
                 nbpublistranch2 + projets.inter + projets.prive +
                 international.scol + international.naiss + international.natio
                 , data=climatRegr )

freq(climat$solreducperso.exp)

#Modèle total réduit
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  
                 preoccupe2 + opinionecolo.proteger + opinionecolo.contraintes +opinionecolo.cata+
                 solreducrech2 + solinstit.train + solinstit.limitevols + solinstit.bilanges + solinstit.vege +
                 dixannees.bilan +
                 solreducperso.donnees + solreducperso.exp +
                 malpaye + carriere + tpsplein + 
                 projets.inter  +
                 international.scol + international.naiss + international.natio
               , data=climatRegr )
summary(res.reg1)

#Voir si on retrouve les mêmes effets d'âge pour la période post-rentrée

climatRegrsept<-climatRegr%>%filter(datedebut>70)
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 , data=climatRegrsept )
res.reg1 <- lm(datedebut ~ sexe + ageAgr    + discipline_agr3 +  employeur, data=climatRegrsept )

freq(climat$volsnbmoins2j)

#Avoir rempli le jour même (catégories pas propres : certains n'ont pas remplis le jour même et sont notés comme tel car on remplit un jour d'envoi de message)
climatRegr$remplijourenvoi<-as.factor(climatRegr$remplijourenvoi)
res.reg1 <- glm(remplijourenvoi ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + preoccupe2, data=climatRegr, family=binomial(logit) )
res.reg1 <- glm(remplijourenvoi ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso, data=climatRegr, family=binomial(logit) )
res.reg1 <- glm(remplijourenvoi ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                  opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata, data=climatRegr, family=binomial(logit) )

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

climat$datedebut<-as.numeric(as.Date(climat$dateDebut))-18438
freq(climat$datedebut)
