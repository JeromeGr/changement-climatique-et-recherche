#0,4% de climatosceptiques : var peu intéressante pour l'analyse ;  0,5% sans opinion
freq(climat$changclim)

freq(climat$acthum)



#Unique cause contre grande rôle
climat$UniqCause[climat$acthum=="Oui, elles en sont l'unique cause"]<-"Activités humaines unique cause"
climat$UniqCause[climat$acthum=="Oui, elles jouent un grand rôle"]<-"Activités humaines grand rôle"

climat$UniqCause <- as.factor(climat$UniqCause)
climat$UniqCause <- relevel(climat$UniqCause, ref = "Activités humaines grand rôle")


climat$UniqCauseNum[climat$acthum=="Oui, elles en sont l'unique cause"]<-1
climat$UniqCauseNum[climat$acthum=="Oui, elles jouent un grand rôle"]<-0


res.reg6 <- lm(UniqCauseNum ~ sexe + ageAgr, data=climat)
res.reg6 <- lm(UniqCauseNum ~ sexe + ageAgr  + discipline_agr3, data=climat)
res.reg6 <- lm(UniqCauseNum ~ sexe + ageAgr  + sitpro2, data=climat)
res.reg6 <- lm(UniqCauseNum ~ sexe + ageAgr  + sitpro2 + discipline_agr3, data=climat)

summary(res.reg6)
