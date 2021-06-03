####Revenu par tête

reg2 <- lm(revenuTete ~ sexe + ageAgr, data=climat_recherche)
reg2 <- lm(revenuTete ~ sexe + ageaccad_tranch2, data=climat_recherche)
reg2 <- lm(revenuTete ~ sexe + ageAgr + sitpro2 + discipline_agr3, data=climat_recherche)
reg2 <- lm(revenuTete ~ sexe + ageAgr + sitpro2 + discipline_agr3 + paie, data=climat_recherche)
summary(reg2)


####Revenu par adulte

climat_recherche$nbadultes<-1+climat_recherche$couple1

climat_recherche$revenuparadulte[climat_recherche$revenu=="Moins de 1 500 euros par mois" & !is.na(climat_recherche$revenu) ]<-750/climat_recherche$nbadultes[climat_recherche$revenu=="Moins de 1 500 euros par mois" & !is.na(climat_recherche$revenu)]
climat_recherche$revenuparadulte[climat_recherche$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat_recherche$revenu)]<-2000/climat_recherche$nbadultes[climat_recherche$revenu=="De 1 500 à 2 499 euros par mois" & !is.na(climat_recherche$revenu)]
climat_recherche$revenuparadulte[climat_recherche$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat_recherche$revenu)]<-3000/climat_recherche$nbadultes[climat_recherche$revenu=="De 2 500 à 3 499 euros par mois" & !is.na(climat_recherche$revenu)]
climat_recherche$revenuparadulte[climat_recherche$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat_recherche$revenu)]<-4000/climat_recherche$nbadultes[climat_recherche$revenu=="De 3 500 à 4 499 euros par mois" & !is.na(climat_recherche$revenu)]
climat_recherche$revenuparadulte[climat_recherche$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat_recherche$revenu)]<-7000/climat_recherche$nbadultes[climat_recherche$revenu=="De 6 000 à 7 999 euros par mois" & !is.na(climat_recherche$revenu)]
climat_recherche$revenuparadulte[climat_recherche$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat_recherche$revenu)]<-9000/climat_recherche$nbadultes[climat_recherche$revenu=="De 8 000 à 9 999 euros par mois" & !is.na(climat_recherche$revenu)]
climat_recherche$revenuparadulte[climat_recherche$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat_recherche$revenu)]<-12500/climat_recherche$nbadultes[climat_recherche$revenu=="De 10 000 à 15 000 euros par mois" & !is.na(climat_recherche$revenu)]
climat_recherche$revenuparadulte[climat_recherche$revenu=="Plus de 15 000 par mois" & !is.na(climat_recherche$revenu)]<-20000/climat_recherche$nbadultes[climat_recherche$revenu=="Plus de 15 000 par mois" & !is.na(climat_recherche$revenu)]


reg2 <- lm(revenuparadulte ~ sexe + ageAgr, data=climat_recherche)
reg2 <- lm(revenuparadulte ~ sexe + ageaccad_tranch2, data=climat_recherche)
reg2 <- lm(revenuparadulte ~ sexe + ageAgr + sitpro2 + discipline_agr3, data=climat_recherche)
reg2 <- lm(revenuparadulte ~ sexe + ageAgr + sitpro2 + discipline_agr3 + paie, data=climat_recherche)
reg2 <- lm(revenuparadulte ~ sexe + ageAgr + sitpro2 + dippar.p , data=climat_recherche)
reg2 <- lm(revenuparadulte ~ sexe + ageAgr + sitpro2 + statutpar.p , data=climat_recherche)
reg2 <- lm(revenuparadulte ~ sexe + ageAgr + sitpro2 + dippar.m , data=climat_recherche)


summary(reg2)

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
