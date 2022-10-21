library(lmerTest)
library(lme4)

# Examen de la distribution des unités dans tout Labintel
# labintel <- read.csv2("../Labintel/fiches.csv", fileEncoding="ISO-8859-1")
# tab <- table(labintel$unite)
# summary(c(tab))
# table(labintel$unite %in% names(tab[tab <= 5]))
# labintel$nbpers.unite <- tab[labintel$unite]
# summary(c(labintel$nbpers.unite))

climatRegr <- subset(climatRegr, !sitpro2 %in% c(
    "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
    "Chargé·e d'études/de mission","Adjoint·e technique",
    "Autre"))

# climatRegr <- group_by(climatRegr, unite.labintel) %>%
#     filter(n() > 1)
# 
# climatRegr <- group_by(climatRegr, discipline) %>%
#     filter(n() > 1)


climatRegr <- mutate(climatRegr,
                     nbpublis2=pmin(nbpublis, 100),
                     tourisme=startsWith(as.character(apportconf.tourisme), "Oui"),
                     avionpersonum=recode(avionperso,
                                          "Aucun aller-retour"=0,
                                          "1 ou 2 allers-retours"=1.5,
                                          "3 ou 4 allers-retours"=2.5,
                                          "Plus de 5 allers-retours"=6),
                     chgtpratiquenum=as.numeric(fct_relevel(chgtpratique, "Non, pas du tout d'accord",
                                                            "Non, plutôt pas d'accord", "Sans opinion",
                                                            "Oui, plutôt d'accord",
                                                            "Oui, tout à fait d'accord")) - 1,
                     preoccupe2num=as.numeric(fct_relevel(preoccupe2, "Pas du tout préoccupé·e", "Sans opinion",
                                                          "Un peu préoccupé·e", "Assez préoccupé·e", 
                                                          "Très préoccupé·e", "Extrêmement préoccupé·e")) - 1)

# Création des variables par labo et par discipline
climatRegr <- group_by(climatRegr, unite.labintel) %>%
    mutate(volshnum_labo=mean(volshnum, na.rm=TRUE),
           nbpublis2_labo=mean(nbpublis2, na.rm=TRUE),
           ScoreEcolo_labo=mean(ScoreEcolo, na.rm=TRUE),
           ScoreInternational_labo=mean(ScoreInternational, na.rm=TRUE),
           tourisme_labo=mean(tourisme, na.rm=TRUE),
           avionpersonum_labo=mean(avionpersonum, na.rm=TRUE),
           chgtpratiquenum_labo=mean(chgtpratiquenum, na.rm=TRUE),
           preoccupe2num_labo=mean(preoccupe2num, na.rm=TRUE)) %>%
    ungroup()

climatRegr <- group_by(climatRegr, discipline) %>%
    mutate(volshnum_disc=mean(volshnum, na.rm=TRUE),
           nbpublis2_disc=mean(nbpublis2, na.rm=TRUE),
           ScoreEcolo_disc=mean(ScoreEcolo, na.rm=TRUE),
           ScoreInternational_disc=mean(ScoreInternational, na.rm=TRUE),
           tourisme_disc=mean(tourisme, na.rm=TRUE),
           avionpersonum_disc=mean(avionpersonum, na.rm=TRUE),
           chgtpratiquenum_disc=mean(chgtpratiquenum, na.rm=TRUE),
           preoccupe2num_disc=mean(preoccupe2num, na.rm=TRUE)) %>%
    ungroup()

# Création de la variable d'écart au labo et à la discipline
climatRegr <- mutate(climatRegr,
                     volshnum_c=volshnum - volshnum_labo - volshnum_disc + mean(volshnum, na.rm=TRUE),
                     nbpublis2_c=nbpublis2 - nbpublis2_labo - nbpublis2_disc + mean(nbpublis2, na.rm=TRUE),
                     ScoreEcolo_c=ScoreEcolo - ScoreEcolo_labo - ScoreEcolo_disc + mean(ScoreEcolo, na.rm=TRUE),
                     ScoreInternational_c=ScoreInternational - ScoreInternational_labo - ScoreInternational_disc + mean(ScoreInternational, na.rm=TRUE),
                     tourisme_c=tourisme - tourisme_labo - tourisme_disc + mean(tourisme, na.rm=TRUE),
                     avionpersonum_c=avionpersonum - avionpersonum_labo - avionpersonum_disc + mean(avionpersonum, na.rm=TRUE),
                     chgtpratiquenum_c=chgtpratiquenum - chgtpratiquenum_labo - chgtpratiquenum_disc + mean(chgtpratiquenum, na.rm=TRUE),
                     preoccupe2num_c=preoccupe2num - preoccupe2num_labo - preoccupe2num_disc + mean(preoccupe2num, na.rm=TRUE),

                     volshnum_clabo=volshnum - volshnum_labo,
                     nbpublis2_clabo=nbpublis2 - nbpublis2_labo,
                     ScoreEcolo_clabo=ScoreEcolo - ScoreEcolo_labo,
                     ScoreInternational_clabo=ScoreInternational - ScoreInternational_labo,
                     tourisme_clabo=tourisme - tourisme_labo,
                     avionpersonum_clabo=avionpersonum - avionpersonum_labo,
                     chgtpratiquenum_clabo=chgtpratiquenum - chgtpratiquenum_labo,
                     preoccupe2num_clabo=preoccupe2num - preoccupe2num_labo)

# Régression linéaire avec distribution normale (incorrect)
reg1 <- lmer(volshnum ~ sexe + ageAgr + sitpro2 +
                 nbpublis2_c + nbpublis2_labo + nbpublis2_disc +
                 chgtpratiquenum_c + chgtpratiquenum_labo + chgtpratiquenum_disc +
                 chgtpratiquenum_c*nbpublis2_c +
                 (1 | unite.labintel) + (1 | discipline),
             data=climatRegr)

summary(reg1)

# Corrélation intra-classe
performance::icc(reg1)

# Calcul plus fiable des erreurs-types (mais long)
# summary(reg1, ddf="Kenward-Roger")


# Régression avec distribution binomiale négative, effet multiplicatif
reg2 <- glmer.nb(volshnum ~ sexe + ageAgr + sitpro2 +
                 nbpublis2_c + nbpublis2_labo + nbpublis2_disc +
                 chgtpratiquenum_c + chgtpratiquenum_labo + chgtpratiquenum_disc +
                 chgtpratiquenum_c*nbpublis2_c +
                 (1 | unite.labintel) + (1 | discipline),
             data=climatRegr)


# Représentation de l'interaction entre publis et volonté de changer les pratiques
library(ggeffects)
mydf <- ggeffect(reg2, terms = c("nbpublis2_c", "chgtpratiquenum_c [-7,-4,-2.5,-0.4]"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE) +
    labs(
        y = get_y_title(mydf),
        x = get_x_title(mydf),
        colour = get_legend_title(mydf)
    )


# Test de différentes distributions
library(glmmTMB)

form <- volshnum ~ sexe + ageAgr + sitpro2 +
    nbpublis2_c + nbpublis2_labo + nbpublis2_disc +
    ScoreEcolo_c + ScoreEcolo_labo + ScoreEcolo_disc +
    (1 | unite.labintel) + (1 | discipline)

dat <- drop_na(climatRegr, all.vars(form))
#dat$volsdist_tot <- round(dat$volsdist_tot)

reggenpois <- glmmTMB(form,
                      family="genpois",
                      data=dat,
                      control=glmmTMBControl(parallel=4))

reggenpoiszi <- glmmTMB(form,
                        ziformula=~ .,
                        family="genpois",
                        data=dat,
                        control=glmmTMBControl(parallel=4))

# Estimation échoue
# reggammazi <- glmmTMB(form,
#                       ziformula=~ .,
#                       family="ziGamma",
#                       data=dat,
#                       control=glmmTMBControl(parallel=4))

regnegbin <- glmmTMB(form,
                     family="nbinom2",
                     data=dat,
                     control=glmmTMBControl(parallel=4))

regnegbinzi <- glmmTMB(form,
                       ziformula=~ .,
                       family="nbinom2",
                       data=dat,
                       control=glmmTMBControl(parallel=4))

regtweedie <- glmmTMB(form,
                      family="tweedie",
                      data=dat,
                      control=glmmTMBControl(parallel=4))

bbmle::AICtab(reggenpois, reggenpoiszi, regnegbin, regnegbinzi, regtweedie)

summary(regtweedie)

qqnorm(residuals(regtweedie))
qqline(residuals(regtweedie))

# Examen des résidus
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
library(DHARMa)

sim <- simulateResiduals(regtweedie)
plot(sim, quantreg=TRUE)
plotResiduals(sim, dat$nbpublis2_c)
plotResiduals(sim, dat$chgtpratiquenum_c)
testCategorical(sim, dat$ageAgr)
testCategorical(sim, droplevels(dat$sitpro2))
testDispersion(sim)
testZeroInflation(sim)


# Analyses avec distribution Tweedie
regtweedie <- glmmTMB(volshnum ~ sexe + ageAgr + sitpro2 +
                          nbpublis2_c + nbpublis2_labo + nbpublis2_disc +
                          chgtpratiquenum_c + chgtpratiquenum_labo + chgtpratiquenum_disc +
                          ScoreEcolo_c + ScoreEcolo_labo + ScoreEcolo_disc +
                          #preoccupe2num_c + preoccupe2num_labo + preoccupe2num_disc +
                          avionpersonum_c + avionpersonum_labo + avionpersonum_disc +
                          ScoreInternational_c + ScoreInternational_labo + ScoreInternational_disc +
                          #international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          #projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 +
                          ScoreEcolo_c*ScoreInternational_c + ScoreEcolo_c*ScoreInternational_labo + ScoreEcolo_c*ScoreInternational_disc +
                          nbpublis2_c*ScoreEcolo_c + nbpublis2_c*ScoreEcolo_labo + nbpublis2_c*ScoreEcolo_disc +
                      (1 | unite.labintel) + (1 | discipline),
                      family="tweedie",
                      data=climatRegr,
                      control=glmmTMBControl(parallel=4))

regtweedie <- glmmTMB(volshnum ~ sexe + ageAgr + sitpro2 +
                          nbpublis2_c + nbpublis2_labo + nbpublis2_disc +
                          chgtpratiquenum_c + chgtpratiquenum_labo + chgtpratiquenum_disc +
                          ScoreEcolo_c + ScoreEcolo_labo + ScoreEcolo_disc +
                          #preoccupe2num_c + preoccupe2num_labo + preoccupe2num_disc +
                          avionpersonum_c + avionpersonum_labo + avionpersonum_disc +
                          ScoreInternational_c + ScoreInternational_labo + ScoreInternational_disc +
                          #international.poste + international.natio +  international.naiss + international.scol + international.etudes + international.postdoc + international.travail + international.prog + international.asso +
                          #projets.anr_m2 + projets.anr_r2 + projets.france_m2 + projets.france_r2 + projets.europe_m2 + projets.europe_r2 + projets.inter_m2 + projets.inter_r2 +projets.prive_m2 + projets.prive_r2 +
                          ScoreEcolo_c*ScoreInternational_c + ScoreEcolo_c*ScoreInternational_labo + ScoreEcolo_c*ScoreInternational_disc +
                          nbpublis2_c*ScoreEcolo_c + nbpublis2_c*ScoreEcolo_labo + nbpublis2_c*ScoreEcolo_disc +
                          carriere*ScoreInternational_c + carriere*ScoreInternational_labo + carriere*ScoreInternational_disc +
                          (1 | unite.labintel) + (1 | discipline),
                      family="tweedie",
                      data=climatRegr,
                      control=glmmTMBControl(parallel=4))
summary(regtweedie)

library(ggeffects)
mydf <- ggeffect(regtweedie, terms = c("ScoreEcolo_c [quart2]", "ScoreInternational_disc [quart2]"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE) +
    scale_y_log10() +
    labs(
        y = get_y_title(mydf),
        x = get_x_title(mydf),
        colour = get_legend_title(mydf)
    )
