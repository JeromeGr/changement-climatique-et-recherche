library(tidyr)
library(tidyverse)
library(gtsummary)

########### 
#Taux de réponse complet
#lastpage = 1 : ça veut dire qu'on a ouvert la page 1 mais qu'on n'est pas allé jusqu'à la page 2

freq((!is.na(climat$lastpage) & climat$lastpage == 8) |
  (!is.na(climat$rechpub) & climat$rechpub == "Non"))
freq(climat$dippar.m, total =T)
#Non réponse à la dernière vraie question du questionnaire :
freq(climat$revenu, total =T)

climat$lastpagefin<-climat$lastpage
climat$lastpagefin[climat$lastpagefin==8&!(is.na(climat$revenu))]<-9
freq(climat$lastpagefin)

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

##########################

a <- cprop(table(climat$preoccupe2, climat$NumVague))
a <- as.data.frame(a[1:6,])
## Réordonnancement de a$Var1
a$Var1 <- factor(a$Var1,
                 levels = c(
                   "Pas du tout préoccupé·e", "Un peu préoccupé·e", "Sans opinion",
                   "Assez préoccupé·e", "Très préoccupé·e", "Extrêmement préoccupé·e"
                 )
)

ggplot(a) + geom_bar(aes(x=Var2, y=Freq, fill=Var1), stat="identity")+
  scale_fill_manual(
    values=c(
      "#74add1", "#fee090","light grey", "#fdae61", "#f46d43", "#d73027"))+
  theme(axis.text.x = element_text(angle = 90))

freq(climat$opinionecolo.cata)
a <- cprop(table(climat$opinionecolo.cata, climat$NumVague))
a <- as.data.frame(a[1:5,])
## Réordonnancement de a$Var1
a$Var1 <- factor(a$Var1,
                 levels = c(
                   "Pas du tout d'accord", "Plutôt pas d'accord",  "Sans opinion","Plutôt d'accord", "Tout à fait d'accord")
 )

ggplot(a) + geom_bar(aes(x=Var2, y=Freq, fill=Var1), stat="identity")+
  scale_fill_manual(
    values=c(
      "#74add1", "#fee090","light grey", "#fdae61", "#f46d43"))+
  theme(axis.text.x = element_text(angle = 90))


a <- cprop(table(climat$opinionecolo.contraintes, climat$NumVague))
a <- as.data.frame(a[1:6,])
a <- cprop(table(climat$opinionecolo.proteger, climat$NumVague))
a <- as.data.frame(a[1:6,])

a <- cprop(table(climat$opinionecolo.efforts, climat$NumVague))
a <- as.data.frame(a[1:6,])

a <- cprop(table(climat$solreducrech, climat$NumVague))
a <- as.data.frame(a[1:4,])

a <- cprop(table(climat$solinstit.limitevols, climat$NumVague))
a <- as.data.frame(a[1:4,])

a <- cprop(table(climat$solinstit.selection, climat$NumVague))
a <- as.data.frame(a[1:4,])

a <- cprop(table(climat$solinstit.conf, climat$NumVague))
a <- as.data.frame(a[1:4,])

freq(climat$opinionecolo.techno)
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
#############################################@
#Qui est allé jusqu'à la fin 
res.reg1 <- lm(lastpage ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + preoccupe2 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso , data=climatRegr )
res.reg1 <- lm(lastpage ~ preoccupe2 , data=climatRegr )
res.reg1 <- lm(lastpage ~ sexe + ageAgr   + preoccupe2 , data=climatRegr )
res.reg1 <- lm(lastpage ~ sexe + ageAgr + sitpro2  + discipline_agr4  + preoccupe2 , data=climatRegr )
res.reg1 <- lm(lastpage ~ sexe + ageAgr + sitpro2  + discipline_agr4  + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso , data=climatRegr )
res.reg1 <- lm(lastpage ~ sexe + ageAgr + sitpro2  + discipline_agr4  + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso + dixannees.bilan , data=climatRegr )

res.reg1 <- lm(lastpagefin ~ sexe + ageAgr + sitpro2  + discipline_agr4 + preoccupe2 , data=climatRegr )

res.reg1 <- lm(lastpagefin ~ sexe + ageAgr + sitpro2  + discipline_agr4  + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso + dixannees.bilan , data=climatRegr )
summary(res.reg1)

freq(climat$lastpage)
freq(climat$ordis.nbtotalpro)
######################################
#Par date (c'est peut être le plus propre ?), ie le nombre de jours écoulés depuis lancement premier message
freq(climat$datedebut)
mean(climatRegr$datedebut)

res.reg1 <- lm(datedebut ~ sexe + ageAgr, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr , data=climatRegr )
res.reg1 <- lm(datedebut ~ sitpro2 , data=climatRegr )

res.reg1 <- lm(datedebut ~ preoccupe2, data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + preoccupe2, data=climatRegr )

climatRegr$preoccupe2 <- relevel(climatRegr$preoccupe, ref = "Extrêmement préoccupé·e")


res.reg1 <- lm(datedebut ~ dixannees.marche , data=climatRegr )
res.reg1 <- lm(datedebut ~ ageAgr + dixannees.marche , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche , data=climatRegr )

res.reg1 <- lm(datedebut ~ dixannees.vote , data=climatRegr )
res.reg1 <- lm(datedebut ~ dixannees.giec , data=climatRegr )
res.reg1 <- lm(datedebut ~ dixannees.asso , data=climatRegr )
res.reg1 <- lm(datedebut ~ dixannees.bilan , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.bilan , data=climatRegr )


res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso + dixannees.bilan , data=climatRegr )

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + ScoreEcolo , data=climatRegr )
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + ScoreEcoloPond , data=climatRegr )

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
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr4 +  
                 preoccupe2 + opinionecolo.proteger + opinionecolo.contraintes +opinionecolo.cata+
                 solreducrech2 + solinstit.train + solinstit.limitevols + solinstit.bilanges + solinstit.vege +
                 dixannees.bilan +
                 solreducperso.donnees + solreducperso.exp +
                 malpaye + carriere + tpsplein + 
                 projets.inter  +
                 international.scol + international.naiss + international.natio
               , data=climatRegr )
summary(res.reg1)

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

res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline +  
                 preoccupe2 + opinionecolo.proteger + opinionecolo.contraintes +opinionecolo.cata+
                 solreducrech2 + solinstit.train + solinstit.limitevols + solinstit.bilanges + solinstit.vege +
                 dixannees.bilan +
                 solreducperso.donnees + solreducperso.exp +
                 malpaye + carriere + tpsplein + 
                 projets.inter  +
                 international.scol + international.naiss + international.natio
               , data=climatRegr )
summary(res.reg1)


res.reg6 <- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr4   + carriere + malpaye + international.naiss, data=climatRegr )
summary(res.reg6)

model_1 <- lm(vaguenum ~ sexe + ageAgr, data=climatRegr )
model_2 <- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr4 , data=climatRegr )

table_1 <- 
  tbl_regression(model_1) %>% 
  add_significance_stars() %>%
  add_glance_table(include = nobs)

table_2 <- 
  tbl_regression(model_2) %>% 
  add_significance_stars() %>%
  add_glance_table(include = nobs)

table_final <- 
  tbl_merge(list(table_1, table_2)) %>%
  # ensure the glance statistics are at the bottom of table
  modify_table_body(~.x %>% dplyr::arrange(row_type == "glance_statistic"))


tbl_regression(m, exponentiate=TRUE, add_estimate_to_reference_rows=TRUE,
               label=list(institut2 ~ "Institut",
                          delegation ~ "Délégation régionale",
                          type2 ~ "Statut",
                          sexe.x ~ "Sexe")) %>%
  modify_header(list(label ~ "**Variable**",
                     estimate ~ "**Odds ratio**",
                     ci ~ "**IC à 95%**",
                     p.value ~ "**p-value**")) %>%
  modify_footnote(everything() ~ NA, abbreviation = TRUE)


#Voir si on retrouve les mêmes effets d'âge pour la période post-rentrée

climatRegrsept<-climatRegr%>%filter(datedebut>70)
res.reg1 <- lm(datedebut ~ sexe + ageAgr   + sitpro2  + discipline_agr3 , data=climatRegrsept )
res.reg1 <- lm(datedebut ~ sexe + ageAgr    + discipline_agr3 +  employeur, data=climatRegrsept )

freq(climat$volsnb_moins2j)


##############################@
#A nouveau par numéro de vague (oct 2021)

mean(climat$vaguenum)
freq(climat$vaguenum)
res.reg1 <- lm(vaguenum ~ sexe + ageAgr, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr4 , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sitpro2 , data=climatRegr )

res.reg1 <- lm(vaguenum ~ preoccupe2, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + preoccupe2, data=climatRegr )

res.reg1 <- lm(vaguenum ~ dixannees.marche , data=climatRegr )
res.reg1 <- lm(vaguenum ~ ageAgr + dixannees.marche , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche , data=climatRegr )

res.reg1 <- lm(vaguenum ~ dixannees.vote , data=climatRegr )
res.reg1 <- lm(vaguenum ~ dixannees.giec , data=climatRegr )
res.reg1 <- lm(vaguenum ~ dixannees.asso , data=climatRegr )
res.reg1 <- lm(vaguenum ~ dixannees.bilan , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.bilan , data=climatRegr )

freq(climat$dixannees.vote)
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso , data=climatRegr )

summary(res.reg1)

res.reg1 <- lm(vaguenum ~ opinionecolo.cata , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.cata , data=climatRegr )

res.reg1 <- lm(vaguenum ~ opinionecolo.decroissance , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.decroissance , data=climatRegr )

res.reg1 <- lm(vaguenum ~ opinionecolo.proteger , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.proteger , data=climatRegr )

res.reg1 <- lm(vaguenum ~ opinionecolo.efforts , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.efforts , data=climatRegr )

res.reg1 <- lm(vaguenum ~ opinionecolo.contraintes , data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + opinionecolo.contraintes , data=climatRegr )

summary(res.reg1)

#Modèle total avec les variables d'opinion qui sont signif
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 
               + preoccupe2 + dixannees.marche+
                 opinionecolo.techno + opinionecolo.proteger + opinionecolo.efforts+ 
                 opinionecolo.contraintes +  opinionecolo.decroissance + opinionecolo.cata , data=climatRegr )


res.reg1 <- lm(vaguenum ~ lastpage, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + lastpage, data=climatRegr )
res.reg1<- lm(vaguenum ~TpsRemp_MmJour  , data=climatRegr)
res.reg1<- lm(vaguenum ~ sexe + ageAgr  + TpsRemp_MmJour  , data=climatRegr)
res.reg1<- lm(vaguenum ~ sexe + ageAgr  + interviewtime, data=climatRegr)
summary(res.reg1)


res.reg1<- lm(vaguenum ~ enfantsnb, data=climatRegr)
res.reg1<- lm(vaguenum ~ ageAgr + enfantsnb, data=climatRegr)
res.reg1<- lm(vaguenum ~ ageAgr + enfantsage_rec, data=climatRegr)
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + enfantsage_rec, data=climatRegr )
res.reg1<- lm(vaguenum ~ ageAgr + couple, data=climatRegr)
res.reg1<- lm(vaguenum ~ sexe + ageAgr + enfantsnb, data=climatRegr)


res.reg1<- lm(vaguenum ~ sexe + ageAgr  + employeur, data=climatRegr)

res.reg1<- lm(vaguenum ~ tpsquotiteNum, data=climatRegr)
res.reg1<- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr3  + tpsquotiteNum, data=climatRegr)

res.reg1<- lm(vaguenum ~ tpsplein, data=climatRegr)

res.reg1<- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr3 + malpaye, data=climatRegr)
res.reg1<- lm(vaguenum ~ carriere, data=climatRegr)

res.reg1<- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr3 + carriere, data=climatRegr)

summary(res.reg1)

res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + nbpublistranch2, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + hindextranch2, data=climatRegr )

res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + volshnum, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + Moinsavionperso, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + avionpersochgt, data=climatRegr )

res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solevolges.conf, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + Moinsavionconf, data=climatRegr )

res.reg1 <- lm(vaguenum ~  solreducrech, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducrech, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.donnees, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.conf, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.exp, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.info, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solreducperso.domicile, data=climatRegr )

res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.train, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.compensation, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.bilanges, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.limitevols, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.selection, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.vols6h, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.conf, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.info, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.equip, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + solinstit.vege, data=climatRegr )




summary(res.reg1)
freq(climat$solinstit.vege)

res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.anr, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.france, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.europe, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.inter, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  projets.prive, data=climatRegr )

summary(res.reg1)

res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.poste, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.natio, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.naiss, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.scol, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.etudes, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.postdoc, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.travail, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.prog, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  international.asso, data=climatRegr )


res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 + 
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + 
                 international.postdoc + international.travail + international.prog + international.asso, data=climatRegr )

res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  dippar.m, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  dippar.p, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  statutpar.m, data=climatRegr )
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr3 +  statutpar.p, data=climatRegr )

summary(res.reg1)


#Modèle total réduit
res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr4 +  
                 preoccupe2 + opinionecolo.proteger + opinionecolo.contraintes +opinionecolo.cata+
                 solreducrech2 + solinstit.train + solinstit.limitevols + solinstit.bilanges + solinstit.vege +
                 dixannees.bilan +
                 solreducperso.donnees + solreducperso.exp +
                 malpaye + carriere + tpsplein + 
                 projets.inter  +
                 international.scol + international.naiss + international.natio
               , data=climatRegr )
summary(res.reg6)

freq(climatRegr$solreducrech2)

res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr4 +  
                                  
                
                international.natio
               , data=climatRegr )


res.reg1 <- lm(vaguenum ~ sexe + ageAgr   + sitpro2  + discipline_agr4 +  
                 preoccupe2 + opinionecolo.proteger + opinionecolo.contraintes +opinionecolo.cata+
                 solreducrech2 + solinstit.train + solinstit.limitevols + solinstit.bilanges + solinstit.vege +
                 dixannees.bilan +
                 solreducperso.donnees + solreducperso.exp +
                 malpaye + carriere + tpsplein + 
                
                 international.natio
               , data=climatRegr )
summary(res.reg1)



#####################@@@@

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

#Tableau récapitulatif des régressions, pour Plos One

res.reg1 <- lm(vaguenum ~ sexe + ageAgr, data=climatRegr )
res.reg2 <- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr4 , data=climatRegr )
res.reg3 <- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr4 + preoccupe2 , data=climatRegr )
res.reg4 <- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr4 + solreducrech2, data=climatRegr )
res.reg5 <- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr4 + preoccupe2 + opinionecolo.cata+ opinionecolo.contraintes +       opinionecolo.proteger + solreducrech + solinstit.limitevols + solinstit.selection + solinstit.conf, data=climatRegr )
res.reg6 <- lm(vaguenum ~ sexe + ageAgr + sitpro2  + discipline_agr3 + nbpublistranch2 + tpsquotiteNum + carriere + international.naiss, data=climatRegr )

htmlreg(list(res.reg1, res.reg2, res.reg3, res.reg4, res.reg5,res.reg6), 
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
        single.row = TRUE )
