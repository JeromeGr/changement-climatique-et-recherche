####Revenu par tête

reg2 <- lm(revenuTete ~ sexe + ageAgr, data=climat_recherche)
reg2 <- lm(revenuTete ~ sexe + ageAgr + sitpro2 + discipline_agr3, data=climat_recherche)
reg2 <- lm(revenuTete ~ sexe + ageAgr + sitpro2 + discipline_agr3 + paie, data=climat_recherche)
summary(reg2)


#S'estimer bien ou mal payé
freq(climat_recherche$paie)
climat$malpaye[climat$paie %in% c("Mal payé·e" , "Très mal payé·e")]<-"Oui"
climat$malpaye[climat$paie %in% c("Bien payé·e", "Correctement payé·e", "Très bien payé·e")]<-"Non"
climat$malpaye<-fct_relevel(climat$malpaye, "Non")

# sous base seulement recherche
climat_recherche <- climat[!climat$sitpro2 %in% c(
  "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
  "Chargé·e d'études/de mission","Adjoint·e technique",
  "Autre"
),]

freq(climat_recherche$opinionecolo.techno)
climat_recherche$opinionecolo.techno<-fct_relevel(climat_recherche$opinionecolo.techno, "Plutôt pas d'accord")

reg2 <- glm(malpaye ~ sexe + ageAgr, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + revenuTete, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 + revenuTete, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 + couple + statutpar.p + dippar.p + ScoreEcolo + enfantsnb, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 + couple + statutpar.p + dippar.p + ScoreEcolo + enfantsnb + revenuTete, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 + carriere + revenuTete, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 + projets.prive_m2 + projets.prive_r2, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  nbpublis, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 +  tpsplein, data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 + couple , data=climat_recherche, family=binomial(logit))
reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 + enfantsage , data=climat_recherche, family=binomial(logit))

reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 + preoccupe , data=climat_recherche, family=binomial(logit))

reg2 <- glm(malpaye ~ sexe + ageAgr + sitpro2 + discipline_agr3 + opinionecolo.decroissance + opinionecolo.techno + opinionecolo.proteger + revenuTete + effortsconso , data=climat_recherche, family=binomial(logit))


summary(reg2)
