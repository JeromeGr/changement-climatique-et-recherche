library(texreg)
library(dplyr)
library(ggplot2)

# sous base seulement recherche
climat_recherche <- climat[!climat$sitpro2 %in% c(
  "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
  "Chargé·e d'études/de mission","Adjoint·e technique",
  "Autre"
),]
######################################################################################################################################################
######################################################################################################################################################
##########Hindex
#1710 personnes ont donné leur Hindex
freq(climat_recherche$hindex)
mean(climat_recherche$hindex, na.rm=T)
cumsum(table(climat_recherche$hindex))
climat_recherche$nbpublisang
srt(climat_recherche)

######Préparation régressions
#Il faut passer en numérique
climat_recherche$hindex<-as.numeric(climat_recherche$hindex)

#Attention : deux valeurs abérantes de hindex / et aussi nbpublis
climat_recherche<-climat_recherche %>% filter(hindex<300| is.na(hindex))

climat_recherche<-climat_recherche %>% filter(nbpublis<200| is.na(nbpublis))


#Répartition par tranche hindex/nbpublis
climat_recherche$hindextranch<-quant.cut(climat_recherche$hindex, 6)

freq(climat_recherche$hindex)

climat_recherche$nbpublistranch<-quant.cut(climat_recherche$nbpublis, 6)

#icut(climat_recherche, hindex)
climat_recherche$hindextranch2 <- cut(climat_recherche$hindex,
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      breaks = c(0, 8, 13, 18, 23, 30, 50, 80, 176)
)

climat_recherche$nbpublistranch2 <- cut(climat_recherche$nbpublis,
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      breaks = c(0, 0.5, 2, 4, 7, 12, 20, 40, 180)
)


#volsnb en tranche
climat_recherche$volsnbtranch<-quant.cut(climat_recherche$volsnb, 30)
climat_recherche$volsnbtranch2 <- cut(climat_recherche$volsnb,
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      breaks = c(0, 0.4, 1, 2, 3, 4, 5, 8, 65)
)

########################################################################################
#Représentation graphique répartition h_index/nbpublis

distrihindex<-climat_recherche %>% group_by(hindex) %>% summarize(nbpersonnes=n())


ggplot(distrihindex, aes(x=hindex, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()

#ggsave("/Users/jeromegreffion/Documents/Recherche/ANR Medici/Commission transparence/Chapitre livre Hélène/Illustrations/Hindex in 2020.pdf",
#       width=9, height=5)


distrinbpublis<-climat_recherche %>% group_by(nbpublis) %>% summarize(nbpersonnes=n())


ggplot(distrinbpublis, aes(x=nbpublis, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()



##############################################@@
#Qui connait son hindex ?
freq(climat_recherche$hindexconn, total = T)
freq(climat_recherche$hindexconnDicho)

#Construction de la variable dichotomique
climat_recherche$hindexconnDicho[climat_recherche$hindexconn=="Oui"]<-"Oui"
climat_recherche$hindexconnDicho[climat_recherche$hindexconn %in% c("Non", "Je ne suis pas certain de ce qu'est le h-index")]<-"Non"

#Nombre de conf à l'étranger les cinq dernières années
climat_recherche$conffois5ans<-ifelse(climat_recherche$conf!="Oui, dans les 5 dernières années", "Zéro fois", as.character(climat_recherche$conffois))


freq(climat_recherche$conf)

#Les références dans la régression
climat_recherche$hindexconnDicho<-as.factor(climat_recherche$hindexconnDicho)
climat_recherche$hindexconnDicho<-fct_relevel(climat_recherche$hindexconnDicho, "Non")

climat_recherche$dippar.p<-fct_relevel(climat_recherche$dippar.p, "Bac +4 ou 5")
climat_recherche$dippar.m<-fct_relevel(climat_recherche$dippar.m, "Bac +4 ou 5")

climat_recherche$statutpar.p<-fct_relevel(climat_recherche$statutpar.p, "Fonctionnaire ou salarié·e du public")
climat_recherche$statutpar.m<-fct_relevel(climat_recherche$statutpar.m, "Fonctionnaire ou salarié·e du public")

climat_recherche$conffois5ans<-fct_relevel(climat_recherche$conffois5ans, "Zéro fois")

climat_recherche$volsarrivee1pays<-fct_relevel(climat_recherche$volsarrivee1pays, "FR")


#On met le temps de vol à zéro pour ceux qui ont indiqué aucun vol
climat_recherche$volsh<-ifelse(!is.na(climat_recherche$volsnb) & climat_recherche$volsnb==0, "0h", as.character(climat_recherche$volsh))

#Les modalités de référence
climat_recherche$volsh<-fct_relevel(climat_recherche$volsh, "De 1h à 10h")


#########
##Variables tirées du tableau sur les vols

#On met le temps de vol à zéro pour ceux qui ont indiqué aucun vol et qui sont dans le module 1
climat_recherche$volsdist_tot<-ifelse(climat_recherche$volsnb==0 & climat_recherche$tiragemodule=="1", 0, climat_recherche$volsdist_tot)
#On récupère une quarantaine de personnes dans la case 0
#Il faudrait voir si la même manoeuvre ne peut pas être faite pour les sous catégories de volsdist_tot

#Idem pour le nombre de vol
climat_recherche$volsnb_tot<-ifelse(climat_recherche$volsnb==0 & climat_recherche$tiragemodule=="1", 0, climat_recherche$volsnb_tot)



#Durée moyenne des vols effectués
climat_recherche$volsduree_moy<-climat_recherche$volshnum/climat_recherche$volsnb
climat_recherche$volsduree_moy[climat_recherche$volsnb==0 & climat_recherche$tiragemodule=="1"]<-0


#Distance moyenne des vols effectués et déclaré dans le tableau
climat_recherche$volsdist_moy<-climat_recherche$volsdist_tot/climat_recherche$volsnb
climat_recherche$volsdist_moy[climat_recherche$volsnb==0 & climat_recherche$tiragemodule=="1"]<-0




#Quelle situation pro pour les non concernés ou incertains ?

climat_recherche %>% filter (hindexconn=="Non concerné·e") %>% group_by (sitpro2)  %>% count(sitpro2)
climat_recherche %>% filter (hindexconn=="Je ne suis pas certain de ce qu'est le h-index") %>% group_by (sitpro2)  %>% count(sitpro2)

#Régressions

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + revenuTete , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + revenuTete + enfantsnb + couple , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete , data=climat_recherche, family=binomial(logit))


reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  nbpublis , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  carriere , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  statutpar.p , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + ScoreEcolo + revenuTete , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + ScoreEcolo + revenuTete + paie , data=climat_recherche, family=binomial(logit))


reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche, family=binomial(logit))

reglog2 <- glm(hindexconnDicho ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche, family=binomial(logit))


reglog2 <- glm(hindexconnDicho ~ nbpublis, data=climat_recherche, family=binomial(logit))

summary(reglog2)


freq (climat_recherche$paie)



################################################################
#Hindex, regressions avec l'avion  : première approche, avec les données avions totales ()
freq(climat_recherche$volsnb)
mean(climat_recherche$volsnb, na.rm=T)



#Hors avions
res.reg1<- lm(hindex ~ sexe + ageAgr, data=climat_recherche)
res.reg1<- lm(hindex ~ sexe + ageAgr  + sitpro2, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)
res.reg1<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete  , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + revenuTete , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volshnum + revenuTete + enfantsnb + couple , data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete , data=climat_recherche)


res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  carriere , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  dippar.p , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + ScoreEcolo, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3  + malpaye , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + dixannees.vote, data=climat_recherche)


summary(res.reg1)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +nbpublis, data=climat_recherche)



res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.reseaux, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.collegues, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.cv, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme+
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.reseaux+
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.collegues+
                 international.postdoc + international.prog + international.asso, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme + nbpublis, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux +nbpublis, data=climat_recherche)

summary(res.reg1)

#intégrant les vols en avion
res.reg1 <- lm(hindex ~ sexe + ageAgr + volsnb, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb + nbpublis, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +volsnb, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +nbpublis +volsnb, data=climat_recherche)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnbtranch, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnbtranch2, data=climat_recherche)


res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsh, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsh + volsnb, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsduree_moy + volsnb, data=climat_recherche)


res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + vols2ans, data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb + vols2ans, data=climat_recherche)



res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 +aviontrain, data=climat_recherche)


res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + revenuTete , data=climat_recherche)
res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + avionperso +avionpersochgt, data=climat_recherche)



summary(res.reg1)

res.reg1 <- lm(hindex ~ sexe + ageAgr + sitpro2 + discipline_agr3 + quizfacteurs.voiture3 , data=climat_recherche)

freq(climat_recherche$quizfacteurs.voiture4)

####Hindex en tranche

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + hindextranch, data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + hindextranch2, data=climat_recherche)

summary(res.reg1)


#Création des variables nb de vols, par pays
climat_recherche$volsnbFrance<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="FR", 0, climat_recherche$volsnb1)+
                        ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="FR", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="FR", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="FR", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="FR", 0, climat_recherche$volsnb5)

climat_recherche$volsnbUSA<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="US", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="US", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="US", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="US", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="US", 0, climat_recherche$volsnb5)

climat_recherche$volsnbItalie<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="IT", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="IT", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="IT", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="IT", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="IT", 0, climat_recherche$volsnb5)

climat_recherche$volsnbAllemagne<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="DE", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="DE", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="DE", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="DE", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="DE", 0, climat_recherche$volsnb5)

climat_recherche$volsnbEspagne<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="ES", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="ES", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="ES", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="ES", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="ES", 0, climat_recherche$volsnb5)

climat_recherche$volsnbCanada<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="CA", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="CA", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="CA", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="CA", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="CA", 0, climat_recherche$volsnb5)

climat_recherche$volsnbGB<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="GB", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="GB", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="GB", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="GB", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="GB", 0, climat_recherche$volsnb5)

climat_recherche$volsnbChine<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="CN", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="CN", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="CN", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="CN", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="CN", 0, climat_recherche$volsnb5)

climat_recherche$volsnbJapon<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="JP", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="JP", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="JP", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="JP", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="JP", 0, climat_recherche$volsnb5)

climat_recherche$volsnbAutriche<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="AT", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="AT", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="AT", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="AT", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="AT", 0, climat_recherche$volsnb5)

climat_recherche$volsnbPortugal<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="PT", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="PT", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="PT", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="PT", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="PT", 0, climat_recherche$volsnb5)

climat_recherche$volsnbPologne<-ifelse(is.na(climat_recherche$volsarrivee1pays) | climat_recherche$volsarrivee1pays!="PL", 0, climat_recherche$volsnb1)+
  ifelse(is.na(climat_recherche$volsarrivee2pays) | climat_recherche$volsarrivee2pays!="PL", 0, climat_recherche$volsnb2)+
  ifelse(is.na(climat_recherche$volsarrivee3pays) | climat_recherche$volsarrivee3pays!="PL", 0, climat_recherche$volsnb3)+
  ifelse(is.na(climat_recherche$volsarrivee4pays) | climat_recherche$volsarrivee4pays!="PL", 0, climat_recherche$volsnb4)+
  ifelse(is.na(climat_recherche$volsarrivee5pays) | climat_recherche$volsarrivee5pays!="PL", 0, climat_recherche$volsnb5)


freq(climat_recherche$volsnbUSA)

#Peut être regarder le lien avec le pays de destination
freq(climat_recherche$volsarrivee1pays, sort="dec")
freq(climat_recherche$volsarrivee2pays, sort="dec")
?freq

###################
#Hindex : en détaillant à partir du détail des vols déclarés dans le module

#Calcul distance totale par motif de vol, par personne

climat_recherche$volsdist_totconf <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Conférence, présentation", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Conférence, présentation", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Conférence, présentation", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Conférence, présentation", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif5=="Conférence, présentation", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totsejrech <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Séjour de recherche", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Séjour de recherche", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Séjour de recherche", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Séjour de recherche", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Séjour de recherche", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totworkshop <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Réunion, workshop", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Réunion, workshop", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Réunion, workshop", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Réunion, workshop", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Réunion, workshop", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totcours <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Enseignement, formation, école d'été", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totterrain <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Terrain, production et recueil de données", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Terrain, production et recueil de données", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Terrain, production et recueil de données", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Terrain, production et recueil de données", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Terrain, production et recueil de données", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totfinanc <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Obtention de financements", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Obtention de financements", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Obtention de financements", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Obtention de financements", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Obtention de financements", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_toteval <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Évaluation de la recherche", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Évaluation de la recherche", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Évaluation de la recherche", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Évaluation de la recherche", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Évaluation de la recherche", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totjury <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Jury", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Jury", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Jury", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Jury", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Jury", climat_recherche$volsdist_tot5, 0)

climat_recherche$volsdist_totautre <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Autre", climat_recherche$volsdist_tot1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Autre", climat_recherche$volsdist_tot2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Autre", climat_recherche$volsdist_tot3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Autre", climat_recherche$volsdist_tot4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Autre", climat_recherche$volsdist_tot5, 0)

#Nombre de vols par motif

climat_recherche$volsnbconf <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Conférence, présentation", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Conférence, présentation", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Conférence, présentation", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Conférence, présentation", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif5=="Conférence, présentation", climat_recherche$volsnb5, 0)

climat_recherche$volsnbsejrech <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Séjour de recherche", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Séjour de recherche", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Séjour de recherche", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Séjour de recherche", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Séjour de recherche", climat_recherche$volsnb5, 0)

climat_recherche$volsnbworkshop <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Réunion, workshop", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Réunion, workshop", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Réunion, workshop", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Réunion, workshop", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Réunion, workshop", climat_recherche$volsnb5, 0)

climat_recherche$volsnbcours <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Enseignement, formation, école d'été", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Enseignement, formation, école d'été", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Enseignement, formation, école d'été", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Enseignement, formation, école d'été", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Enseignement, formation, école d'été", climat_recherche$volsnb5, 0)

climat_recherche$volsnbterrain <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Terrain, production et recueil de données", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Terrain, production et recueil de données", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Terrain, production et recueil de données", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Terrain, production et recueil de données", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Terrain, production et recueil de données", climat_recherche$volsnb5, 0)

climat_recherche$volsnbfinanc <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Obtention de financements", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Obtention de financements", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Obtention de financements", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Obtention de financements", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Obtention de financements", climat_recherche$volsnb5, 0)

climat_recherche$volsnbeval <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Évaluation de la recherche", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Évaluation de la recherche", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Évaluation de la recherche", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Évaluation de la recherche", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Évaluation de la recherche", climat_recherche$volsnb5, 0)

climat_recherche$volsnbjury <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Jury", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Jury", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Jury", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Jury", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Jury", climat_recherche$volsnb5, 0)

climat_recherche$volsnbautre <- ifelse(!is.na(climat_recherche$volsmotif1) & climat_recherche$volsmotif1=="Autre", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsmotif2) & climat_recherche$volsmotif2=="Autre", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsmotif3) & climat_recherche$volsmotif3=="Autre", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsmotif4) & climat_recherche$volsmotif4=="Autre", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsmotif5) & climat_recherche$volsmotif5=="Autre", climat_recherche$volsnb5, 0)




res.reg2<- lm(hindex ~ sexe + ageAgr, data=climat_recherche)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 , data=climat_recherche)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + volsdist_totconf, data=climat_recherche)

###########################Calcul en fonction des durées (il faudrait pondérer par le nombre d'aller-retour)

climat_recherche$volsnbmoins2j <- ifelse(!is.na(climat_recherche$volsjours1) & climat_recherche$volsjours1=="Moins de deux jours", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsjours2) & climat_recherche$volsjours2=="Moins de deux jours", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsjours3) & climat_recherche$volsjours3=="Moins de deux jours", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours4=="Moins de deux jours", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours5=="Moins de deux jours", climat_recherche$volsnb5, 0)

climat_recherche$volsnb_2j_1sem <- ifelse(!is.na(climat_recherche$volsjours1) & climat_recherche$volsjours1=="De deux jours à une semaine", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsjours2) & climat_recherche$volsjours2=="De deux jours à une semaine", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsjours3) & climat_recherche$volsjours3=="De deux jours à une semaine", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours4=="De deux jours à une semaine", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsjours5) & climat_recherche$volsjours5=="De deux jours à une semaine", climat_recherche$volsnb5, 0)

climat_recherche$volsnb_1sem_1mois <- ifelse(!is.na(climat_recherche$volsjours1) & climat_recherche$volsjours1=="De plus d'une semaine à un mois", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsjours2) & climat_recherche$volsjours2=="De plus d'une semaine à un mois", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsjours3) & climat_recherche$volsjours3=="De plus d'une semaine à un mois", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours4=="De plus d'une semaine à un mois", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsjours5) & climat_recherche$volsjours5=="De plus d'une semaine à un mois", climat_recherche$volsnb5, 0)

climat_recherche$volsnb_sup1mois <- ifelse(!is.na(climat_recherche$volsjours1) & climat_recherche$volsjours1=="Plus d'un mois", climat_recherche$volsnb1, 0) + 
  ifelse(!is.na(climat_recherche$volsjours2) & climat_recherche$volsjours2=="Plus d'un mois", climat_recherche$volsnb2, 0) +
  ifelse(!is.na(climat_recherche$volsjours3) & climat_recherche$volsjours3=="Plus d'un mois", climat_recherche$volsnb3, 0) +
  ifelse(!is.na(climat_recherche$volsjours4) & climat_recherche$volsjours4=="Plus d'un mois", climat_recherche$volsnb4, 0) +
  ifelse(!is.na(climat_recherche$volsjours5) & climat_recherche$volsjours5=="Plus d'un mois", climat_recherche$volsnb5, 0)



#Je retiens juste ceux du module 1 (pour éviter d'attribuer des distances=0 à ceux qui n'ont juste pas répondu au module)
climat_recherche_mod1<- climat_recherche %>% filter(tiragemodule == "1")

freq(climat_recherche_mod1$volsdist_totconf)
mean(climat_recherche$volsnb, na.rm=T)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_tot, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot + volsdist_moy, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbconf + volsnbsejrech + volsnbworkshop + volsnbcours + volsnbterrain + volsnbfinanc +
                volsnbeval + volsnbjury + volsnbautre, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbconf, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbsejrech, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbworkshop, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbcours, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbterrain, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbfinanc , data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbeval, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbjury, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbautre, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_2j_1sem , data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_1sem_1mois , data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_sup1mois, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)

summary(res.reg2)


freq(climat_recherche_mod1$volsdist_tot)



freq(climat_recherche$volsdist_totjury)

mean(climat_recherche$volsdist_totconf, na.rm=T)

freq(climat_recherche$volsmotif1)

freq(climat_recherche$volsdist_tot1)

freq(climat_recherche$volsmotif1)

mean(climat_recherche$volsdist_tot1, na.rm=T)

mean(climat_recherche$volsdistTot1, na.rm=T)
freq(climat_recherche$volsdistTotConf)





freq(climat_recherche$volsjours1)

######################################################################################################################################################
#################################################################################################################################
##########Publications depuis 2017
mean(climat_recherche$nbpublis, na.rm=T)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2  + discipline_agr3 + volsnb, data=climat_recherche)



#Hors avions
res.reg1<- lm(nbpublis ~ sexe + ageAgr, data=climat_recherche)
res.reg1<- lm(nbpublis ~ sexe + ageAgr  + sitpro2, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)
res.reg1<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete  , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + hindex, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + enfantsnb + couple , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  carriere , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  dippar.p , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + discipline_agr3 +  dippar.m , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 +  statutpar.m , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + ScoreEcolo, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3  + malpaye , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + dixannees.vote, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans, data=climat_recherche)


res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.reseaux, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.collegues, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.cv, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme+
               international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.reseaux+
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.collegues+
                 international.postdoc + international.prog + international.asso, data=climat_recherche)


res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme+
                 international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans + apportconf.travaux + apportconf.reseaux , data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans + apportconf.travaux + apportconf.reseaux +
                 apportconf.collegues+ apportconf.cv+ apportconf.tourisme, data=climat_recherche)


summary(res.reg1)

freq(climat_recherche$apportconf.collegues)


#intégrant les vols en avion
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + volsnb, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb + hindex, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +volsnb, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + conffois5ans +hindex +volsnb, data=climat_recherche)


res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnbtranch, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnbtranch2, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsh, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsh + volsnb, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsduree_moy + volsnb, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + vols2ans, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + volsnb + vols2ans, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 +aviontrain, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + revenuTete , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + avionperso +revenuTete , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + avionpersochgt, data=climat_recherche)

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + 
                 international.postdoc + international.prog + international.asso +volsnb, data=climat_recherche)

summary(res.reg1)
freq(climat$volsnb)

freq(climat_recherche$avionpersochgt)



####Nb publis en tranche

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch, data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2, data=climat_recherche)

summary(res.reg1)


freq(climat_recherche$nbpublistranch2)


#Je retiens juste ceux du module 1 (pour éviter d'attribuer des distances=0 à ceux qui n'ont juste pas répondu au module)
climat_recherche_mod1<- climat_recherche %>% filter(tiragemodule == "1")

freq(climat_recherche_mod1$volsdist_tot)
mean(climat_recherche$volsnb, na.rm=T)
mean(climat_recherche_mod1$volsdist_moy, na.rm=T)

summary(res.reg2)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_tot, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_moy, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot + volsdist_moy, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbconf + volsnbsejrech + volsnbworkshop + volsnbcours + volsnbterrain + volsnbfinanc +
                volsnbeval + volsnbjury + volsnbautre, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbconf, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbsejrech, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbworkshop, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbcours, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbterrain, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbfinanc , data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbeval, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbjury, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbautre, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_2j_1sem , data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_1sem_1mois , data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_sup1mois, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + volsnbconf + volsnbsejrech + volsnbworkshop + volsnbcours + volsnbterrain + volsnbfinanc +
                volsnbeval + volsnbjury + volsnbautre + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + 
                volsnbEspagne+ volsnbGB + volsnbChine + volsnbJapon+ volsnbAutriche + volsnbPortugal + volsnbPologne, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance , data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + 
                volsnb_sup1mois + volsnbFrance, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot + volsdist_moy +volsnbFrance, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ nbpublis, data=climat_recherche)


summary(res.reg2)

summary(res.reg2)






#####A partir des détails du module1

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + volsdist_tot, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + volsnb_tot, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre, data=climat_recherche_mod1)


res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)


res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre + volsnbmoins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)




summary(res.reg2)


######################################################################################################################################################
#################################################################################################################################
################Vols en avions

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublis + hindex, data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 + hindextranch2, data=climat_recherche)

summary(res.reg1)


######################################################################################################################################################
#################################################################################################################################
################Origine sociale
climat_recherche$PR_DR[climat_recherche$sitpro2 %in% c("Directeur·rice de recherche", "Professeur·e des universités")]<-"PR ou DR"
climat_recherche$PR_DR[climat_recherche$sitpro2 %in% c("Chargé·e de recherche", "Maître·sse de conférences")]<-"CR ou MCF"

#Les références dans la régression
climat_recherche$PR_DR<-as.factor(climat_recherche$PR_DR)
climat_recherche$PR_DR<-fct_relevel(climat_recherche$PR_DR, "CR ou MCF")



reglog2 <- glm(PR_DR ~ sexe + ageAgr + statutpar.p , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr  + statutpar.m , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr  + dippar.p , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr  + dippar.m , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr  + dippar.m , data=climat_recherche, family=binomial(logit))
reglog2 <- glm(PR_DR ~ sexe + ageAgr + couple + enfantsnb , data=climat_recherche, family=binomial(logit))



summary(reglog2)

freq(climat_recherche$PR_DR)

##########

res.reg1 <- lm(hindex ~ sexe + ageAgr, data=climat_recherche)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2, data=climat_recherche)
res.reg3<- lm(hindex ~ sexe + ageAgr  + sitpro2 + revenuTete  , data=climat_recherche)
res.reg4<- lm(hindex ~ sexe + ageAgr  + sitpro2 + enfantsnb + couple , data=climat_recherche)
res.reg5 <- lm(hindex ~ sexe + ageAgr  + discipline_agr3 , data=climat_recherche)
res.reg6 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)
res.reg7 <- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + nbpublis, data=climat_recherche)
res.reg8<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete + enfantsnb + couple , data=climat_recherche)


htmlreg(list(res.reg1, res.reg2, res.reg3, res.reg4, res.reg5,res.reg6, res.reg7, res.reg8), 
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
                               "revenuTete"="Revenu par individu du foyer",
                               "enfantsnb"= "nombre d'enfants",
                               "coupleNon"="ne vit pas en couple"),
        symbol = "+",
        caption = "Tableau 1 : Régressions linéaires multiples sur le hindex", caption.above=TRUE, 
        single.row = TRUE, 
        #custom.gof.rows = NULL,
        file="/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Regressions Hindex.doc")





