library(texreg)
library(dplyr)
library(ggplot2)
library(GGally)

# sous base seulement recherche
climat_recherche <- climat[!climat$sitpro2 %in% c(
  "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
  "Chargé·e d'études/de mission","Adjoint·e technique",
  "Autre"
),]

#Passer en numérique le hindex
climat_recherche$hindex<-as.numeric(climat_recherche$hindex)

#Elimination des quelques individus aux valeurs aberrantes
climat_recherche<-climat_recherche %>% filter(hindex<300| is.na(hindex))
climat_recherche<-climat_recherche %>% filter(nbpublis<200| is.na(nbpublis))

##########################################################################################################################################
#Recodage/création de variables/préparation des régressions (modalités de références)
#Nombre de conférences en numérique (approximation)

climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Zéro fois"]<-0
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Moins d'une fois par an"]<-0.5
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Une fois par an"]<-1
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Deux fois par an"]<-2
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Trois fois par an"]<-3
climat_recherche$conffois5ansnum[climat_recherche$conffois5ans=="Plus de trois fois par an"]<-4.5


##########################################################################################################################################
##########################################################################################################################################
##Exploration des distributions des variables d'intérêts et des liens entre elles, hors régressions
#hindex, nombre de publis et nombre de vols


#Publis
distrinbpublis<-climat_recherche %>% group_by(nbpublis) %>% summarize(nbpersonnes=n())

ggplot(distrinbpublis, aes(x=nbpublis, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+
  labs(y="nombre d'enquêtés", x="Nombre de publications depuis 2017,")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution nbpublis.pdf",
       width=9, height=5)

distrinbpublis<-climat_recherche %>% group_by(nbpublistranch2) %>% summarize(nbpersonnes=n())

ggplot(distrinbpublis, aes(x=nbpublistranch2, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+ 
  labs(y="nombre d'enquêtés", x="Nombre de publications depuis 2017, en tranche")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution nbpublis en tranche.pdf",
       width=9, height=5)

#Hindex

distrihindex<-climat_recherche %>% group_by(hindex) %>% summarize(nbpersonnes=n())

ggplot(distrihindex, aes(x=hindex, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+
  labs(y="nombre d'enquêtés", x="h-index")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution Hindex.pdf",
       width=9, height=5)

distrihindex<-climat_recherche %>% group_by(hindextranch2) %>% summarize(nbpersonnes=n())

ggplot(distrihindex, aes(x=hindextranch2, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+ 
  labs(y="nombre d'enquêtés", x="h-index en tranche")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution Hindex en tranche.pdf",
       width=9, height=5)


#Distrib du log du h-index
climat_recherche$hindexln<-log(climat_recherche$hindex)
freq(climat_recherche$hindexln)
distrihindexln<-climat_recherche %>% filter(hindexln>=0)%>% group_by(hindexln) %>% summarize(nbpersonnes=n())

ggplot(distrihindexln, aes(x=hindexln, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()


#Nb vols

distrivolsnb<-climat_recherche %>% group_by(volsnb) %>% summarize(nbpersonnes=n())

ggplot(distrivolsnb, aes(x=volsnb, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+
  labs(y="nombre d'enquêtés", x="Nombre de vols en 2019,")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution volsnb.pdf",
       width=9, height=5)

distrivolsnb<-climat_recherche %>% group_by(volsnbtranch2) %>% summarize(nbpersonnes=n())

ggplot(distrivolsnb, aes(x=volsnbtranch2, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+ 
  labs(y="nombre d'enquêtés", x="Nombre de vols en 2019, en tranche")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution volsnb en tranche.pdf",
       width=9, height=5)

#Nbconférences lors des 5 dernières années

districonffois5ans<-climat_recherche %>% group_by(conffois5ans) %>% summarize(nbpersonnes=n())

ggplot(districonffois5ans, aes(x=conffois5ans, y=nbpersonnes))+
  geom_bar(stat="identity", fill="steelblue", width=0.5)+
  theme_minimal()+ 
  labs(y="nombre d'enquêtés", x="Nombre de participations à des conférences à l'étranger, les 5 dernières années")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Distribution du nombre de participation conf étranger.pdf",
       width=9, height=5)

freq(climat_recherche$conffois5ans)

#Distribution par discipline et sexe, boxplot
ggplot(climat_recherche)+
  geom_boxplot(aes(y=nbpublis, x=discipline_agr3))+coord_flip()


ggplot(climat_recherche)+
  geom_boxplot(aes(y=nbpublis, x=discipline_agr3, color=sexe))+coord_flip()

ggplot(climat_recherche)+
  geom_boxplot(aes(y=hindex, x=discipline_agr3))+coord_flip()

ggplot(climat_recherche)+
  geom_boxplot(aes(y=hindex, x=discipline_agr3, color=sexe))+coord_flip()

####Distribution des vols par tranche de nombre de publis et de h-index

#Boxplot
ggplot(climat_recherche)+
  geom_boxplot(aes(y=volsnb, x=nbpublistranch2))+
labs(y="Nombre moyens de vols en avion en 2019", x= "Nombre de publications 2017-mi2020")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Boxplot, nombre de vols en fonction du nombre de publis en tranches.pdf",
       width=9, height=5)


ggplot(climat_recherche)+
  geom_boxplot(aes(y=nbpublistranch2, x=volsnb))+coord_flip()
ggplot(climat_recherche)+
  geom_boxplot(aes(y=hindextranch2, x=volsnb))+coord_flip()

ggplot(climat_recherche, aes(x = , y = volsnb, fill = hindextranch2)) +
  geom_boxplot(colour = "grey30") + 
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0, NA),
                     expand = c(0, 0)) +
  guides(fill = FALSE)

#Moyennes
graphMoyenne<-climat_recherche %>% group_by(nbpublistranch2) %>%summarise(MoyenneVols = mean(volsnb, na.rm=T),
                                                                           Ecart = sd(volsnb, na.rm=T))

ggplot(graphMoyenne, aes(x = nbpublistranch2, y = MoyenneVols, fill = nbpublistranch2)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  guides(fill = FALSE) +
  labs(y="Nombre moyens de vols en avion en 2019", x= "Nombre de publications 2017-mi2020")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre moyen de vols en fonction du nombre de publis en tranches, moyennes.pdf",
       width=9, height=5)

graphMoyenne<-climat_recherche %>% group_by(hindextranch2) %>%summarise(MoyenneVols = mean(volsnb, na.rm=T),
                                                                          Ecart = sd(volsnb, na.rm=T))

ggplot(graphMoyenne, aes(x = hindextranch2, y = MoyenneVols, fill = hindextranch2)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") + +
  geom_errorbar(
    aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  guides(fill = FALSE) +
  labs(y="Nombre moyens de vols en avion en 2019", x= "H-index")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre moyen de vols en fonction du hindex en tranches, moyennes.pdf",
       width=9, height=5)



graphMoyenne<-climat_recherche %>% group_by(volsnbtranch2) %>%summarise(MoyennePublis = mean(nbpublis, na.rm=T),
                                                                          Ecart = sd(nbpublis, na.rm=T))

ggplot(graphMoyenne, aes(x = volsnbtranch2, y = MoyennePublis, fill = volsnbtranch2)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyennePublis - Ecart, ymax = MoyennePublis+ Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  guides(fill = FALSE) +
  labs(y="Nombre moyens de publications 2017-mi2020", x= "Nombre de voyages en avion en 2019")


####Distribution du nombre de conférences par nombre de publis et h-index
#Nombre de conf en numérique (conversion approx)


graphMoyenne<-climat_recherche %>% group_by(nbpublistranch2) %>%summarise(Moyenneconf = mean(conffois5ansnum, na.rm=T),
                                                                          Ecart = sd(conffois5ansnum, na.rm=T))

ggplot(graphMoyenne, aes(x = nbpublistranch2, y = Moyenneconf, fill = nbpublistranch2)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = Moyenneconf - Ecart, ymax = Moyenneconf + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  guides(fill = FALSE) +
  labs(y="Nombre annuel moyen de conférences à l'étranger ces 5 dernières années", x= "Nombre de publications 2017-mi2020")

ggsave("/Users/jeromegreffion/Changement climatique et recherche/Figures, graphs/Nombre moyen de conférences à l'étranger en fonction du nombre de publis en tranches, moyennes.pdf",
       width=9, height=5)


#Distinction entre les écolos (angle changement climatique) et les autres dans les distributions

#Ecolo angle changement climatique : inquiets/chgt pratiques avion/marche climat
climat_recherche$ecolochgtclimat<-ifelse(climat_recherche$dixannees.marche=="Oui" & 
                                           climat_recherche$avionpersochgt %in% c("Oui, je le prends beaucoup moins", "Oui, je le prends un peu moins") & 
                                           climat_recherche$preoccupe=="Extrêmement préoccupé·e", "Ecolo préoccupé et agissant", "Autre")

#Trop peu de monde dans la tranche sup des publis quand on prend que les écolos (il faut agrandir les tranches)
climat_recherche$nbpublistranch3 <- cut(climat_recherche$nbpublis,
                                        include.lowest = TRUE,
                                        right = TRUE,
                                        breaks = c(0, 0.5, 2, 4, 7, 12, 20, 40)
)

graphMoyenneEcolo<-climat_recherche %>% filter(!is.na(nbpublistranch3) & !is.na(ecolochgtclimat)) %>%
  group_by(nbpublistranch3,ecolochgtclimat) %>% summarise(MoyenneVols = mean(volsnb, na.rm=T),
                                                                          Ecart = sd(volsnb, na.rm=T), Effectifs = n())

ggplot(graphMoyenneEcolo, aes(x = nbpublistranch3, y = MoyenneVols, fill = ecolochgtclimat)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  labs(y="Nombre moyens de vols en avion en 2019", x= "Nombre de publications 2017-mi2020")


graphMoyenneEcolo<-climat_recherche %>% group_by(volsnbtranch2,ecolochgtclimat ) %>%summarise(MoyennePublis = mean(nbpublis, na.rm=T),
                                                                        Ecart = sd(nbpublis, na.rm=T), Effectifs=n())

ggplot(graphMoyenneEcolo, aes(x = volsnbtranch2, y = MoyennePublis, fill = ecolochgtclimat)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyennePublis - Ecart, ymax = MoyennePublis+ Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  labs(y="Nombre moyens de publications 2017-mi2020", x= "Nombre de voyages en avion en 2019")


#Pour avoir des crochets autour des effectifs
Tabgraph$Effectif<-paste("[", Tabgraph$Effectif, "]", sep="")

Tabgraph %>% filter(Av_SansTop8 %in% c("Moy_ParMembre", "Moy_ParMembre_8mb")) %>%
  ggplot(aes(x=AnneeEntree, y=Moy_av, fill=Av_SansTop8, order=Av_SansTop8))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Effectif), position=position_dodge(width=0.9), vjust=-0.4, size=3) +
  theme_classic()+
  scale_fill_manual(name = "",
                    breaks= c("Moy_ParMembre", "Moy_ParMembre_8mb"),
                    values=c("black", "grey"),
                    labels= c("Moy_ParMembre"="All members [headcount]" ,
                              "Moy_ParMembre_8mb"="Minus top 9 beneficiaries\n of all TCs since 2000 [headcount]"))+
  xlab("Date of TC entry") +
  ylab("Average amount received\n between 2013 and mid-2019 (euros)")+
  theme(legend.position = c(0.70, 0.85))

ggsave("/Users/jeromegreffion/Documents/Recherche/ANR Medici/Commission transparence/Chapitre livre Hélène/Illustrations/Average amount by date of TC entrance, non normalisé, en barre, années regroup.pdf",
       width=9, height=5)


#Ecolo en général (pas forcemment angle changt climatique) : fort "score écolo"
climat_recherche$scorecoloaggr<-ifelse(climat_recherche$ScoreEcoloPond>5, "Elevé", "Non élevé")

graphMoyenneEcolo<-climat_recherche %>% group_by(nbpublistranch2,scorecoloaggr) %>%summarise(MoyenneVols = mean(volsnb, na.rm=T),
                                                                                               Ecart = sd(volsnb, na.rm=T))

ggplot(graphMoyenneEcolo, aes(x = nbpublistranch2, y = MoyenneVols, fill = scorecoloaggr)) +
  geom_bar(stat = "identity", position = position_dodge(),
           colour = "grey30") +
  geom_errorbar(
    aes(ymin = MoyenneVols - Ecart, ymax = MoyenneVols + Ecart),
    width = .2, position = position_dodge(.9), size = 1
  ) + 
  scale_fill_viridis_d() +
  labs(y="Nombre moyens de vols en avion en 2019", x= "Nombre de publications 2017-mi2020")


climat$score


######################################################################################################################################################
######################################################################################################################################################
##########Hindex
#1710 personnes ont donné leur Hindex
freq(climat_recherche$hindex, total = T)
mean(climat_recherche$hindex, na.rm=T)

##############################################@@
#Qui connait son hindex ?
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




freq(climat_recherche$volsnbUSA)

#Peut être regarder le lien avec le pays de destination
freq(climat_recherche$volsarrivee1pays, sort="dec")
freq(climat_recherche$volsarrivee2pays, sort="dec")
?freq

###################
#Hindex : en détaillant à partir du détail des vols déclarés dans le module

res.reg2<- lm(hindex ~ sexe + ageAgr, data=climat_recherche)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 , data=climat_recherche)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + volsdist_totconf, data=climat_recherche)


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

res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_moins2j, data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_2j_1sem , data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_1sem_1mois , data=climat_recherche_mod1)
res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_sup1mois, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_moins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)


res.reg2<- lm(hindex ~ sexe + ageAgr  + sitpro2 + volsdist_totconf + volsdist_totsejrech + volsdist_totworkshop + volsdist_totcours + volsdist_totterrain + volsdist_totfinanc + volsdist_totfinanc +
                volsdist_toteval + volsdist_totjury + volsdist_totautre + volsnb_moins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)

summary(res.reg2)


freq(climat_recherche_mod1$volsdist_tot)
mean(climat_recherche$volsdistTot1, na.rm=T)


######################################################################################################################################################
#################################################################################################################################
##########Publications depuis 2017
mean(climat_recherche$nbpublis, na.rm=T)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2  + discipline_agr3 + volsnb, data=climat_recherche)



#Hors avions
res.reg1<- lm(nbpublis ~ sexe + ageAgr, data=climat_recherche)
res.reg1<- lm(nbpublis ~ sexe + ageAgr + ageaccad_tranch2, data=climat_recherche)

res.reg1<- lm(nbpublis ~ sexe + ageAgr  + sitpro2, data=climat_recherche)
res.reg1<- lm(nbpublis ~ sexe + ageAgr + ageaccad_tranch2  + sitpro2, data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat_recherche)
res.reg1<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + revenuTete  , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + hindex, data=climat_recherche)



climat_recherche$ageAgr<-fct_relevel(climat_recherche$ageAgr, "35-39 ans")

res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + enfantsnb + couple , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + enfantsnb , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe*enfantsnb_rec + ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe*enfantsnb_rec*ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe + enfantsnb_rec*ageAgr  + sitpro2 + discipline_agr3 , data=climat_recherche)
res.reg1 <- lm(nbpublis ~ sexe*enfantsage_rec + ageAgr  + sitpro2 + discipline_agr3 + couple , data=climat_recherche)

summary(res.reg1)



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


res.reg1 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme, data=climat_recherche)
summary(res.reg1)

res.reg2 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme + conffois5ans, data=climat_recherche)

res.reg2 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme + conffois5ans + apportconf.tourisme:conffois5ans, data=climat_recherche)

res.reg2 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme:conffois5ans, data=climat_recherche)
res.reg2 <- lm(nbpublis ~ sexe + ageAgr + sitpro2 + discipline_agr3 + apportconf.tourisme*conffois5ans, data=climat_recherche)

summary(res.reg2)
ggcoef_model(res.reg2)

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

res.reg1 <- lm(nbpublis ~ sexe + ageAgr + volsnb + ecolochgtclimat, data=climat_recherche)



summary(res.reg1)
freq(climat$volsnb)

freq(climat_recherche$avionpersochgt)

#Binomiale négative

reglog3 <- glm.nb(nbpublis ~ sexe + ageAgr  + sitpro2 + ageaccad_tranch2 + discipline_agr3, data=climat_recherche )

summary(reglog3)


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
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_moins2j, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_2j_1sem , data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_1sem_1mois , data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_sup1mois, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_moins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + volsnbconf + volsnbsejrech + volsnbworkshop + volsnbcours + volsnbterrain + volsnbfinanc +
                volsnbeval + volsnbjury + volsnbautre + volsnb_moins2j + volsnb_2j_1sem + volsnb_1sem_1mois + volsnb_sup1mois, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + 
                volsnbEspagne+ volsnbGB + volsnbChine + volsnbJapon+ volsnbAutriche + volsnbPortugal + volsnbPologne, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1)
res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance , data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_moins2j + volsnb_2j_1sem + volsnb_1sem_1mois + 
                volsnb_sup1mois + volsnbFrance, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnb_tot + volsdist_moy +volsnbFrance, data=climat_recherche_mod1)

res.reg2<- lm(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + trav.TUU2017+ volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1)

res.reg2<- lm(hindex ~ nbpublis, data=climat_recherche)

summary(res.reg2)


library(MASS)
#Binomiale négative
reglog3 <- glm.nb(nbpublis ~ sexe + ageAgr + ageaccad_tranch2  + sitpro2 + discipline_agr3 + trav.TUU2017+ volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1 )
reglog3 <- glm.nb(nbpublis ~ sexe + ageAgr + ageaccad_tranch2  + sitpro2 + discipline_agr3 + trav.TAAV2017+ volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1 )

reglog3 <- glm.nb(nbpublis ~ sexe + ageAgr + ageaccad_tranch2  + sitpro2 + discipline_agr3 + trav.metropole + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1 )
reglog3 <- glm.nb(nbpublis ~ sexe + ageAgr + ageaccad_tranch2  + sitpro2 + discipline_agr3  + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1 )


reglog3 <- glm.nb(nbpublis ~ trav.TUU2017, data=climat_recherche_mod1 )
reglog3 <- glm.nb(nbpublis ~ trav.TAAV2017, data=climat_recherche_mod1 )


reglog3 <- glm.nb(nbpublis ~ sexe + ageAgr  + sitpro2 + discipline_agr3 + volsnbFrance +volsnbUSA + volsnbItalie + volsnbAllemagne + volsnbCanada + volsnbEspagne+ volsnbGB, data=climat_recherche_mod1 )

summary(reglog3)
freq(climat_recherche_mod1$volsnbFrance)
#################################################################################################################@
#Nombre de publis : premières explorations (réunion du 4 juin)

tapply(climat_recherche$nbpublis, climat_recherche$discipline_agr3, mean, na.rm=T)

reglog2 <- lm(nbpublis ~ sexe + ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche )
reglog2 <- lm(nbpublis ~ sexe +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche )

reglog2 <- lm(nbpublis ~ sexe +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 +enfantsnb , data=climat_recherche )
reglog2 <- lm(nbpublis ~ sexe*enfantsnb_rec +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche )
reglog2 <- lm(nbpublis ~ sexe*enfantsage_rec +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche )
reglog2 <- lm(nbpublis ~ sexe:enfantsage_rec +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche )


summary(reglog2)


reglog2 <- glm(nbpublis ~ sexe +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche, family = poisson )

reglog2 <- glm(nbpublis ~ sexe +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche, family = quasipoisson() )

reglog2 <- glm(nbpublis ~ sexe*enfantsnb_rec +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche, family = quasipoisson() )


library(MASS)
#Binomiale négative (Dans Xie) : peut être la meilleure solution (le plus flexible)
reglog3 <- glm.nb(nbpublis ~ sexe +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche )

reglog3 <- glm.nb(nbpublis ~ sexe*enfantsnb_rec +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche )


summary(reglog3)

#Log et régression (Steven Stack, Gender, research) Il garde juste les gens qui ont une thèse depuis plus de 4 ans (et il regarde les publis depuis 5 ans)
#Intérêt du Poisson : gérer les zéros
reglog2 <- lm(log(nbpublis) ~ sexe +ageAgr+ ageaccad_tranch2+ sitpro2 + discipline_agr3 , data=climat_recherche )

cbind(reglog2$coefficients, reglog3$coefficients)

#Contrôler par le % de femmes dans chaque discipline

#Réf pour la loi de poisson : article de Xie
#Article qui utilise loi de poisson Rotolo, When Does Centrality Matter? (pour Weighted Citation Index)

cor(fitted(reglog2),reglog2$y)
cor(fitted(reglog2),reglog2$model$nbpublis )
b<-tapply(climat_recherche$ScoreEcolo, climat_recherche$discipline_agr3, mean, na.rm=T)

#Uiliser boostrap ?
freq(climat_recherche$discipline_agr3)


reglog2 <- lm(nbpublis ~ sexe +ageAgr + ageaccad_tranch2 +  sitpro2 + discipline_agr3 +  revenuTete, data=climat_recherche )
reglog2 <- lm(nbpublis ~ sexe +ageAgr + ageaccad_tranch2 +  sitpro2 + discipline_agr3 + avionperso+  revenuTete, data=climat_recherche )
reglog2 <- lm(nbpublis ~ sexe +ageAgr + ageaccad_tranch2 +  sitpro2 + discipline_agr3 + avionperso, data=climat_recherche )
reglog2 <- lm(nbpublis ~ sexe +ageAgr + ageaccad_tranch2 +  sitpro2 + discipline_agr3 + avionperso + volsnbtranch2 + international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso + revenuTete, data=climat_recherche )

reglog2 <- lm(nbpublis ~ sexe +ageAgr + ageaccad_tranch2*discipline_agr3 + international.natio +  international.naiss + international.scol + international.etudes + dippar.p + dippar.m, data=climat_recherche )

summary(reglog2)

#Mutinomiale

library(nnet)
regm <- multinom(parents_3mod ~ sexe_rec + AGEREVQ_rec, data = rp17sample)
ggcoef_multinom(
  regm,
  exponentiate = TRUE
)



######################################################################################################################################################
#################################################################################################################################
################Vols en avions

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublis + hindex, data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch2 + hindextranch2, data=climat_recherche)

#Lien avec être plus ou moins écolo
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublis + ecolochgtclimat, data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + ecolochgtclimat, data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch3 + ecolochgtclimat, data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 + nbpublistranch3 + dixannees.marche +preoccupe + avionpersochgt  , data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  dixannees.marche +preoccupe + avionpersochgt  , data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  dixannees.marche +preoccupe + avionpersochgt +chgtpratique  , data=climat_recherche)
res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  dixannees.marche +preoccupe + avionpersochgt +effortsconso  , data=climat_recherche)

res.reg1 <- lm(volsnb ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  dixannees.marche + dixannees.giec + dixannees.vote + dixannees.asso +dixannees.bilan , data=climat_recherche)

summary(res.reg1)
climat$chgt

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



########################
#Ressources. Sur les produits de variables et analyses des interactions entre deux variables :
#http://larmarange.github.io/analyse-R/effets-d-interaction.html
#https://commonweb.unifr.ch/artsdean/pub/gestens/f/as/files/4665/9547_131825.pdf
