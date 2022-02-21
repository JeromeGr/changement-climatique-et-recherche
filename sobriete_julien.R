# graphiques article sobriété

names(climat)[50:100]
freq(climat$solreducmateriel.moins)
freq(climat$solreducmateriel.lowtech)
freq(climat$solreducmateriel.util)
freq(climat$solreducmateriel.renouv)
freq(climat$solrisqreducmateriel.fin)
freq(climat$sol)
freq(climat$discipline_agr4)
freq(climat$sexe)

dispositex <- climat_recherche[climat_recherche$solreducmateriel.moins!="Non concerné·e" &
                       climat_recherche$solreducmateriel.lowtech!="Non concerné·e" &
                       climat_recherche$solreducmateriel.util!="Non concerné·e" &
                       climat_recherche$solreducmateriel.renouv!="Non concerné·e" &
                       climat_recherche$solrisqreducmateriel.qual!="Non concerné·e" &
                       climat_recherche$solrisqreducmateriel.themes!="Non concerné·e" &
                       climat_recherche$solrisqreducmateriel.fin!="Non concerné·e" &
                       climat_recherche$solrisqreducmateriel.retard!="Non concerné·e" &
                       climat_recherche$solrisqreducmateriel.publi!="Non concerné·e"&
                         climat_recherche$solreducmateriel.moins!="Sans opinion" &
                         climat_recherche$solreducmateriel.lowtech!="Sans opinion" &
                         climat_recherche$solreducmateriel.util!="Sans opinion" &
                         climat_recherche$solreducmateriel.renouv!="Sans opinion" &
                         climat_recherche$solrisqreducmateriel.qual!="Sans opinion" &
                         climat_recherche$solrisqreducmateriel.themes!="Sans opinion" &
                         climat_recherche$solrisqreducmateriel.fin!="Sans opinion" &
                         climat_recherche$solrisqreducmateriel.retard!="Sans opinion" &
                         climat_recherche$solrisqreducmateriel.publi!="Sans opinion",]

a <- lprop(as.table(rbind(table(dispositex$solreducmateriel.lowtech),
            table(dispositex$solreducmateriel.util),
            table(dispositex$solreducmateriel.moins),
            table(dispositex$solreducmateriel.renouv))))[1:4,]
rownames(a) <- c(
  "lowtech", "utilisation", "moins", "renouvellement"
)

copie(t(a))
# à mettre en relation avec l'ACM 



freq(climat$solrisqreducavion.qual)
freq(climat$solrisqreducavion.avantages)
freq(climat$solrisqreducavion.fin)
freq(climat$solrisqreducavion.diffusion)
freq(climat$solrisqreducavion.donnees)
freq(climat$solrisqreducavion.isoler)
freq(climat$solrisqreducavion.bureaucratie)
freq(climat$solrisqreducavion.insertion)


base_acm <- na.omit(dispositex[c(
                   "solrisqreducmateriel.qual",
                   "solrisqreducmateriel.themes",
                   "solrisqreducmateriel.fin",
                   "solrisqreducmateriel.retard",
                   "solrisqreducmateriel.publi",
                   "solrisqreducavion.qual",
                   "solrisqreducavion.avantages",
                   "solrisqreducavion.fin",
                   "solrisqreducavion.diffusion",
                   "solrisqreducavion.donnees",
                   "solrisqreducavion.isoler",
                   "solrisqreducavion.bureaucratie",
                   "solrisqreducavion.insertion",
                   "discipline_agr4",
                   "sitpro", "sexe")
                   ])

base_acm <- base_acm[base_acm$solrisqreducavion.qual != "Non concerné·e" &
                       base_acm$solrisqreducavion.avantages != "Non concerné·e" &
                       base_acm$solrisqreducavion.fin != "Non concerné·e" &
                       base_acm$solrisqreducavion.diffusion != "Non concerné·e" &
                       base_acm$solrisqreducavion.donnees != "Non concerné·e" &
                       base_acm$solrisqreducavion.isoler != "Non concerné·e" &
                       base_acm$solrisqreducavion.bureaucratie != "Non concerné·e" &
                       base_acm$solrisqreducavion.insertion != "Non concerné·e"&
                       base_acm$solrisqreducavion.qual != "Sans opinion" &
                       base_acm$solrisqreducavion.avantages != "Sans opinion" &
                       base_acm$solrisqreducavion.fin != "Sans opinion" &
                       base_acm$solrisqreducavion.diffusion != "Sans opinion" &
                       base_acm$solrisqreducavion.donnees != "Sans opinion" &
                       base_acm$solrisqreducavion.isoler != "Sans opinion" &
                       base_acm$solrisqreducavion.bureaucratie != "Sans opinion" &
                       base_acm$solrisqreducavion.insertion != "Sans opinion",]


#Sur la sous pop des confctionnaires et CDI etc, réduire ou pas fabrication du matos
freq(base_acm$solrisqreducmateriel.retard)



library(FactoMineR)
library(explor)
library(factoextra)

names(base_acm)

acm <- MCA(base_acm, quali.sup=14:16)
acm <- MCA(base_acm, quali.sup=14:16)
explor(acm)
fviz_mca_var(acm, select.var="sexe",
             ggtheme = theme_minimal())
plot(acm, choix = "ind", invisible = c("var","ind"))




# qui utilise du matériel ----

freq(climat$solinstit.equip)
freq(climat$materiel.aucun)

table(climat$discipline_agr4, climat$materiel.aucun)


# les labos doivent ils réduirent l'utilisation de matos selon si on utilise ou pas
cprop(table(climat$solinstit.equip, climat$materiel.aucun, useNA = "always"))
cprop(table(climat$solreducperso.exp, climat$materiel.aucun, useNA = "always"))
cprop(table(climat$solreducperso.exp, climat$materiel.aucun))


# pas de lien. C'est intéressant. 
#!!!


# creation base_equipement ----


# je filtre pour ne garder que ceux qui n'utilisent pas aucun équipement et qui sont concernés
#variable materiel.aucun = non
climat_equip <- subset(climat, materiel ==TRUE)

# Parmi eux, combien sont prêts à réduire (variables solreducmateriel) !!! Un tiers de NA, 12% de non concernés 5,5% de sans opinion

freq(climat_equip$discipline_agr4)
freq(climat_equip$solreducperso.exp)
freq(climat_equip$solreducmateriel.lowtech)
freq(climat_equip$solreducmateriel.util)
freq(climat_equip$solreducmateriel.moins)
freq(climat_equip$solreducmateriel.renouv)

# chelous <- subset(climat_equip, discipline_agr4 %in% c(
#   "Droit, économie, gestion", "Autres lettres et sciences humaines",
#   "Histoire, géo, urba, anthropo"
# ))


freq(climat$materiel)

tablea <- cbind(
freq(climat_equip$solreducperso.exp)[,3],
freq(climat_equip$solreducmateriel.lowtech)[,3],
freq(climat_equip$solreducmateriel.util)[,3],
freq(climat_equip$solreducmateriel.moins)[,3],
freq(climat_equip$solreducmateriel.renouv)[,3]
)[1:6,]

colnames(tablea) <- c("Experiences en général", "Lowtech", "Moindre utilisation", "Moindre équipement", "Moindre renouvellement")


rownames(tablea) <- rownames(
  freq(climat_equip$solreducperso.exp)[1:6,])
tablea


freq(climat$solreducmateriel.util)
freq(climat_equip$solreducmateriel.util)
freq(climat_equip$solreducmateriel.moins)
freq(climat_equip$solreducmateriel.renouv)
cprop(table(climat_equip$solrisqreducmateriel.qual, climat_equip$solreducperso.exp))
cprop(table(climat_equip$solrisqreducmateriel.publi, climat_equip$solreducperso.exp))


# ambition de réduction par discipline ----

lprop(table(climat_equip$discipline_agr4, climat_equip$solreducperso.exp)[4:14,1:4])
a <- lprop(table(climat_equip$discipline_agr4, climat_equip$solreducperso.exp)[4:14,1:4])[-12,]
ordre <- names(sort(a[,4]))

# dat <- rbind(data.frame(variable="Les expériences et observations\nscientifiques", valeur=climat$solreducperso.exp),
#              data.frame(variable="Les déplacements pour missions de terrain,\nd'observation ou de collecte de données", valeur=climat$solreducperso.donnees),
#              data.frame(variable="Les déplacements domicile-travail\nen voiture, moto, scooter ou avion", valeur=climat$solreducperso.domicile),
#              data.frame(variable="L'équipement en postes informatiques\net sa fréquence de renouvellement", valeur=climat$solreducperso.info),
#              data.frame(variable="Les vols en avion\npour les conférences, réunions, congrès", valeur=climat$solreducperso.conf)) %>%
#   mutate(variable=factor(variable, levels=unique(variable)),
#          valeur=fct_relevel(valeur, c("Oui, d'au moins un tiers", "Oui, mais de moins d'un tiers", "Non, car elles sont déjà très basses", "Sans opinion", "Non")))
# 

  ggplot(as.data.frame(a[,-5]), aes(x=Var1, y=Freq, fill=fct_rev(Var2))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#d7191c", "#3690c0", "#66bd63", "#1a9641")) +
  scale_y_continuous(labels=scales::percent_format(),
                     sec.axis=sec_axis(~ 1-., labels=scales::percent_format())) +
    scale_x_discrete(limits=ordre)+
  coord_flip() + labs(fill="") +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(reverse = TRUE, nrow=3, byrow=TRUE)) +
  labs(y="Proportion des répondant·es", x="",
       title="Êtes-vous prêt·e à réduire vos émissions de gaz à effet de serre\nd’ici à 2030 dans les domaines suivants ?") 
# espace des craintes----

  
  

dispositex <- climat_equip[climat_equip$solreducmateriel.moins!="Non concerné·e" &
                                 climat_equip$solreducmateriel.lowtech!="Non concerné·e" &
                                 climat_equip$solreducmateriel.util!="Non concerné·e" &
                                 climat_equip$solreducmateriel.renouv!="Non concerné·e" &
                                 climat_equip$solrisqreducmateriel.qual!="Non concerné·e" &
                                 climat_equip$solrisqreducmateriel.themes!="Non concerné·e" &
                                 climat_equip$solrisqreducmateriel.fin!="Non concerné·e" &
                                 climat_equip$solrisqreducmateriel.retard!="Non concerné·e" &
                                 climat_equip$solrisqreducmateriel.publi!="Non concerné·e"&
                                 climat_equip$solreducmateriel.moins!="Sans opinion" &
                                 climat_equip$solreducmateriel.lowtech!="Sans opinion" &
                                 climat_equip$solreducmateriel.util!="Sans opinion" &
                                 climat_equip$solreducmateriel.renouv!="Sans opinion" &
                                 climat_equip$solrisqreducmateriel.qual!="Sans opinion" &
                                 climat_equip$solrisqreducmateriel.themes!="Sans opinion" &
                                 climat_equip$solrisqreducmateriel.fin!="Sans opinion" &
                                 climat_equip$solrisqreducmateriel.retard!="Sans opinion" &
                                 climat_equip$solrisqreducmateriel.publi!="Sans opinion",]

base_acm <- na.omit(dispositex[c(
  "solrisqreducmateriel.qual",
  "solrisqreducmateriel.themes",
  "solrisqreducmateriel.fin",
  "solrisqreducmateriel.retard",
  "solrisqreducmateriel.publi",
  "solrisqreducavion.qual",
  "solrisqreducavion.avantages",
  "solrisqreducavion.fin",
  "solrisqreducavion.diffusion",
  "solrisqreducavion.donnees",
  "solrisqreducavion.isoler",
  "solrisqreducavion.bureaucratie",
  "solrisqreducavion.insertion",
  "discipline_agr4",
  "sitpro", "sexe", "solreducperso.exp")
  ])

base_acm <- base_acm[base_acm$solrisqreducavion.qual != "Non concerné·e" &
                       base_acm$solrisqreducavion.avantages != "Non concerné·e" &
                       base_acm$solrisqreducavion.fin != "Non concerné·e" &
                       base_acm$solrisqreducavion.diffusion != "Non concerné·e" &
                       base_acm$solrisqreducavion.donnees != "Non concerné·e" &
                       base_acm$solrisqreducavion.isoler != "Non concerné·e" &
                       base_acm$solrisqreducavion.bureaucratie != "Non concerné·e" &
                       base_acm$solrisqreducavion.insertion != "Non concerné·e"&
                       base_acm$solrisqreducavion.qual != "Sans opinion" &
                       base_acm$solrisqreducavion.avantages != "Sans opinion" &
                       base_acm$solrisqreducavion.fin != "Sans opinion" &
                       base_acm$solrisqreducavion.diffusion != "Sans opinion" &
                       base_acm$solrisqreducavion.donnees != "Sans opinion" &
                       base_acm$solrisqreducavion.isoler != "Sans opinion" &
                       base_acm$solrisqreducavion.bureaucratie != "Sans opinion" &
                       base_acm$solrisqreducavion.insertion != "Sans opinion",]


#Sur la sous pop des confctionnaires et CDI etc, réduire ou pas fabrication du matos
freq(base_acm$solrisqreducmateriel.retard)



library(FactoMineR)
library(explor)
library(factoextra)

names(base_acm)

acm <- MCA(base_acm, quali.sup=14:17)
acm <- MCA(base_acm, quali.sup=14:16)
explor(acm)
fviz_mca_var(acm, select.var="sexe",
             ggtheme = theme_minimal())
plot(acm, choix = "ind", invisible = c("var","ind"))


#espace par acp ----


  
a <- cbind(
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducmateriel.qual))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducmateriel.themes))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducmateriel.fin))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducmateriel.retard))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducmateriel.publi))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducavion.qual))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducavion.avantages))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducavion.fin))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducavion.diffusion))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducavion.donnees))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducavion.isoler))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducavion.bureaucratie))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducavion.insertion))[4:14,1:3],
lprop(table(climat_equip$discipline_agr4, climat_equip$solreducperso.exp))[4:14,c(1,2,3,4)]
)



a
colnames(a) <- c(
  "materiel.qual_probleme",
  "materiel.qual_pas_probleme",
  "materiel.qual_peu_probable",
  "materiel.themes_probleme",
  "materiel.themes_pas_probleme",
  "materiel.themes_peu_probable",
  "materiel.fin_probleme",
  "materiel.fin_pas_probleme",
  "materiel.fin_peu_probable",
  "materiel.retard_probleme",
  "materiel.retard_pas_probleme",
  "materiel.retard_peu_probable",
  "materiel.publi_probleme",
  "materiel.publi_pas_probleme",
  "materiel.publi_peu_probable",
  "avion.qual_probleme",
  "avion.qual_pas_probleme",
  "avion.qual_peu_probable",
  "avion.avantages_probleme",
  "avion.avantages_pas_probleme",
  "avion.avantages_peu_probable",
  "avion.fin_probleme",
  "avion.fin_pas_probleme",
  "avion.fin_peu_probable",
  "avion.diffusion_probleme",
  "avion.diffusion_pas_probleme",
  "avion.diffusion_peu_probable",
  "avion.donnees_probleme",
  "avion.donnees_pas_probleme",
  "avion.donnees_peu_probable",
  "avion.isoler_probleme",
  "avion.isoler_pas_probleme",
  "avion.isoler_peu_probable",
  "avion.bureaucratie_probleme",
  "avion.bureaucratie_pas_probleme",
  "avion.bureaucratie_peu_probable",
  "avion.insertion_probleme",
  "avion.insertion_pas_probleme",
  "avion.insertion_peu_probable",
  "Reduire_exp_beaucoup",
  "Reduire_exp_un_peu",
  "Reduire_exp_Non_deja_basses",
  "Reduire_exp_Non"
)
dim(a)
acp <- PCA(a,
           # quali.sup=1:4, 
           quanti.sup=16:43, 
           graph=FALSE)
# plot(acp, invisible="ind")
explor::explor(acp)

library(factoextra)
quali.sup <- as.data.frame(acp$quali.sup$coord)
quali.sup$name <- rownames(quali.sup)
quali.sup$var <- rep(rownames(acp$quali.sup$eta2),
                     sapply(rownames(acp$quali.sup$eta2),
                            function(x) length(unique(dat[,x]))))
quali.sup <- rbind(quali.sup, cbind(as.data.frame(acp$var$coord),
                                    name=rownames(acp$var$coord), var="Actives"))

library(ggrepel)

fviz_pca_var(acp, axes=c(1, 2), geom="arrow", title="") +
  geom_text_repel(data=quali.sup, aes(Dim.1, Dim.2, label=name, color=var),
                  min.segment.length=0, size=2, max.overlaps=100) +
  guides(color="none") +
  scale_color_brewer(palette="Set1")

fviz_pca_var(acp, axes=c(1, 3), geom="arrow", title="") +
  geom_text_repel(data=quali.sup, aes(Dim.1, Dim.3, label=name, color=var),
                  min.segment.length=0, size=2, max.overlaps=100) +
  guides(color="none") +
  scale_color_brewer(palette="Set1")

fviz_pca_var(acp, axes=c(2, 3), geom="arrow", title="") +
  geom_text_repel(data=quali.sup, aes(Dim.2, Dim.3, label=name, color=var),
                  min.segment.length=0, size=2, max.overlaps=100) +
  guides(color="none") +
  scale_color_brewer(palette="Set1")



tapply(climat$ScoreEcolo, climat$discipline_agr3, sd, na.rm=TRUE)
tapply(climat$ScoreEcolo, climat$discipline_agr3, "var", na.rm=TRUE)
var(climat$ScoreEcolo, na.rm=TRUE)


# régression sous population équipements ----


# On vire les disciplines out ----

climat_equip <- subset(climat_equip, !discipline_agr4 %in% c("Droit, économie, gestion",
                                                             "Autres lettres et sciences humaines",
                                                             "Histoire, géo, urba, anthropo")) %>% droplevels()
freq(climat_equip$discipline_agr4)

# régression sur le fait de dire que c'est un problème de réduire la qualité


## Recodage de climat_equip$solrisqreducmateriel.qual en climat_equip$risque_materiel_qualite_dicho
climat_equip$risque_materiel_qualite_dicho <- fct_recode(climat_equip$solrisqreducmateriel.qual,
  "Problème" = "C'est probable et c'est un problème",
  "pas_pb_ou_improbable" = "C'est probable mais ce n'est pas un problème",
  "pas_pb_ou_improbable" = "C'est peu probable",
  NULL = "Sans opinion",
  NULL = "Non concerné·e"
)

levels(climat_equip$risque_materiel_qualite_trois)

climat_equip$risque_materiel_qualite_trois <- fct_recode(climat_equip$solrisqreducmateriel.qual,
                                                         "Problème" = "C'est probable et c'est un problème",
                                                         "pas_pb" = "C'est probable mais ce n'est pas un problème",
                                                         "improbable" = "C'est peu probable",
                                                         NULL = "Sans opinion",
                                                         NULL = "Non concerné·e"
)


climat_equip$discipline_agr4 <- relevel(climat_equip$discipline_agr4, "Chimie")




# tris croisés ----
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducmateriel.qual))

library(nnet)
regm1 <- multinom(risque_materiel_qualite_trois ~ 
                   solreducperso.exp,
                 data=climat_equip)
regm1bis <- multinom(risque_materiel_qualite_trois ~ 
                       discipline_agr4,
                  data=climat_equip)
regm2 <- multinom(risque_materiel_qualite_trois ~ 
                    solreducperso.exp+discipline_agr4,
                  data=climat_equip)

regm3 <- multinom(risque_materiel_qualite_trois ~ 
                    solreducperso.exp+discipline_agr4+ScoreEcolo,
                  data=climat_equip)
regm4 <- multinom(risque_materiel_qualite_trois ~ 
                    solreducperso.exp+discipline_agr4+ScoreEcolo+
                    materiel.couteux+
                    materiel.trescouteux+
                    materiel.tgir+
                    materiel.info+
                    materiel.extensif+
                    materiel.petit,
                  data=climat_equip)



# library(GGally)
ggcoef_multinom(
  regm4,
  exponentiate = TRUE
)



reg1 <- glm(risque_materiel_qualite_dicho ~ 
              solreducperso.exp,
            data=climat_equip, family=binomial)
reg2 <- glm(risque_materiel_qualite_trois ~ 
                    solreducperso.exp+discipline_agr4,
                  data=climat_equip, family=binomial)

reg3 <- glm(risque_materiel_qualite_trois ~ 
                    solreducperso.exp+discipline_agr4+ScoreEcolo,
                  data=climat_equip, family=binomial)


ggcoef_model(
  reg3,
  exponentiate = TRUE
)

ggcoef_compare(list("Modèle 1"=reg2, "Modèle 2"=reg3), 
               conf.level = 0.90, 
               exponentiate = T)







# la même chose sur les publis ----


## Recodage de climat_equip$solrisqreducmateriel.qual en climat_equip$risque_materiel_qualite_dicho
climat_equip$risque_materiel_publi_dicho <- fct_recode(climat_equip$solrisqreducmateriel.publi,
                                                         "Problème" = "C'est probable et c'est un problème",
                                                         "pas_pb_ou_improbable" = "C'est probable mais ce n'est pas un problème",
                                                         "pas_pb_ou_improbable" = "C'est peu probable",
                                                         NULL = "Sans opinion",
                                                         NULL = "Non concerné·e"
)


climat_equip$risque_materiel_publi_trois <- fct_recode(climat_equip$solrisqreducmateriel.publi,
                                                         "Problème" = "C'est probable et c'est un problème",
                                                         "pas_pb" = "C'est probable mais ce n'est pas un problème",
                                                         "improbable" = "C'est peu probable",
                                                         NULL = "Sans opinion",
                                                         NULL = "Non concerné·e"
)

levels(climat_equip$risque_materiel_publi_trois)

climat_equip$discipline_agr4 <- relevel(climat_equip$discipline_agr4, "Chimie")




# tris croisés
lprop(table(climat_equip$discipline_agr4, climat_equip$solrisqreducmateriel.publi))

library(nnet)
regm1 <- multinom(risque_materiel_publi_trois ~ 
                    solreducperso.exp,
                  data=climat_equip)
regm1bis <- multinom(risque_materiel_publi_trois ~ 
                       discipline_agr4,
                     data=climat_equip)
regm2 <- multinom(risque_materiel_publi_trois ~ 
                    solreducperso.exp+discipline_agr4,
                  data=climat_equip)

regm3 <- multinom(risque_materiel_publi_trois ~ 
                    solreducperso.exp+discipline_agr4+ScoreEcolo,
                  data=climat_equip)
regm4 <- multinom(risque_materiel_publi_trois ~ 
                    solreducperso.exp+discipline_agr4+ScoreEcolo+
                    materiel.couteux+
                    materiel.trescouteux+
                    materiel.tgir+
                    materiel.info+
                    materiel.extensif+
                    materiel.petit,
                  data=climat_equip)




# library(GGally)
ggcoef_multinom(
  regm4,
  exponentiate = TRUE
)


# régression sur le fait de répondre non, je réduirai pas ----


## Recodage de climat_equip$solreducperso.exp en climat_equip$solreducperso.exp_dicho_sans_basses
climat_equip$solreducperso.exp_dicho_sans_basses <- fct_recode(climat_equip$solreducperso.exp,
  "Oui" = "Oui, d'au moins un tiers",
  "Oui" = "Oui, mais de moins d'un tiers",
  NULL = "Non, car elles sont déjà très basses",
  NULL = "Non concerné·e",
  NULL = "Sans opinion"
)

## Recodage de climat_equip$solreducperso.exp en climat_equip$solreducperso.exp_dicho_sans_basses
climat_equip$solreducperso.exp_quatre <- fct_recode(climat_equip$solreducperso.exp,
                                                               NULL = "Non concerné·e",
                                                               NULL = "Sans opinion"
)







# tris croisés
lprop(table(climat_equip$discipline_agr4, climat_equip$solreducperso.exp_dicho_sans_basses))

library(nnet)
regm1 <- multinom(solreducperso.exp_quatre ~ 
                    solreducperso.exp,
                  data=climat_equip)
regm1bis <- multinom(solreducperso.exp_quatre ~ 
                       discipline_agr4,
                     data=climat_equip)
regm2 <- multinom(solreducperso.exp_quatre ~ 
                    solreducperso.exp+discipline_agr4,
                  data=climat_equip)

regm3 <- multinom(solreducperso.exp_quatre ~ 
                    solreducperso.exp+discipline_agr4+ScoreEcolo,
                  data=climat_equip)
regm4 <- multinom(solreducperso.exp_quatre ~ 
                    discipline_agr4+ScoreEcolo+
                    materiel.couteux+
                    materiel.trescouteux+
                    materiel.tgir+
                    materiel.info+
                    materiel.extensif+
                    materiel.petit,
                  data=climat_equip[climat_equip$discipline_agr4!="Mathématiques",])




# library(GGally)
ggcoef_multinom(
  regm4,
  exponentiate = TRUE
)





# croisement écolo/réduction ----
rbind(tapply(climat$ScoreEcolo, climat$solreducrech, mean, na.rm=T),
tapply(climat$ScoreEcolo, climat$solreducrech, sd, na.rm=T))%>%copie()


rbind(
tapply(climat$ScoreEcolo, climat$solreducperso.conf, mean, na.rm=T),
tapply(climat$ScoreEcolo, climat$solreducperso.conf, sd, na.rm=T),
tapply(climat$ScoreEcolo, climat$solreducperso.donnees, mean, na.rm=T),
tapply(climat$ScoreEcolo, climat$solreducperso.donnees, sd, na.rm=T),
tapply(climat$ScoreEcolo, climat$solreducperso.exp, mean, na.rm=T),
tapply(climat$ScoreEcolo, climat$solreducperso.exp, sd, na.rm=T),
tapply(climat$ScoreEcolo, climat$solreducperso.info, mean, na.rm=T),
tapply(climat$ScoreEcolo, climat$solreducperso.info, sd, na.rm=T))[1:8,]%>%copie()


# croisement écolo/réduction croisé avec variables ----
lprop(table(climat$solreducrech3, climat$sexe))
lprop(table(climat$sitpro_reduite, climat$solreducrech3))%>%copie()

irec(climat$sitpro_reduite)

## Recodage de climat$sitpro_reduite en climat$sitpro_reduite_rec
climat$sitpro_reduite_rec <- fct_recode(climat$sitpro_reduite,
  "Rang A" = "Directeur·rice de recherche",
  "Rang A" = "Professeur·e des universités",
  "Rang B" = "Chargé·e de recherche",
  "Rang B" = "Maître·sse de conférences",
  "Ingénieur·e et technicien·ne" = "Ingénieur·e de recherche",
  "Doctorant·e ou post-doc" = "Post-doctorant·e",
  "Doctorant·e ou post-doc" = "ATER",
  "Doctorant·e ou post-doc" = "Doctorant·e contractuel·le",
  "Ingénieur·e et assistant·e" = "Ingénieur·e d'études",
  NULL = "Chargé·e d'études/de mission",
  "Ingénieur·e et technicien·ne" = "Assistant ingénieur·e",
  "Ingénieur·e et technicien·ne" = "Technicien·ne",
  "Ingénieur·e et technicien·ne" = "Adjoint·e technique",
  NULL = "Autre"
)

lprop(table(climat$discipline_agr5, climat$solreducrech3))%>%copie()

# solutions pour réduire ----

lprop(table())

a <- rbind(
  tapply(climat$ScoreEcolo, climat$solinstit.conf, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.conf, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.limitevols, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.limitevols, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.vols6h, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.vols6h, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.train, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.train, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.compensation, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.compensation, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.bilanges, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.bilanges, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.info, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.info, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.equip, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.equip, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.vege, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.vege, sd, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.selection, mean, na.rm=T),
  tapply(climat$ScoreEcolo, climat$solinstit.selection, sd, na.rm=T)
)
rownames(a) <- c("conf","conf",
                 "limitevols","limitevols",
                 "vols6h","vols6h",
                 "train","train",
                 "compensation","compensation",
                 "bilanges","bilanges",
                 "info","info",
                 "equip","equip",
                 "vege","vege"
)
copie(a)

copie(rbind(tapply(climat$ScoreEcolo, climat$solinstit.selection, mean, na.rm=T),
tapply(climat$ScoreEcolo, climat$solinstit.selection, sd, na.rm=T)))

freq(climat$avionpersochgt)
freq(climat$raison)
freq(climat$effortsconso)
freq(climat$dixannees.giec)
freq(climat$dixannees.bilan)
freq(climat$dixannees.marche)


# quoi réduire, où réduire ----

a <- t(cbind(
  freq(climat$solinstit.conf)[-5,],
  freq(climat$solinstit.limitevols)[-5,3],
  freq(climat$solinstit.vols6h)[-5,3],
  freq(climat$solinstit.train)[-5,3],
  freq(climat$solinstit.compensation)[-5,3],
  freq(climat$solinstit.bilanges)[-5,3],
  freq(climat$solinstit.info)[-5,3],
  freq(climat$solinstit.equip)[-5,3],
  freq(climat$solinstit.vege)[-5,3],
  freq(climat$solinstit.selection)[-5,3]
)[,-c(1:2)])

a
rownames(a) <- c("conf",
                 "limitevols",
                 "vols6h",
                 "train",
                 "compensation",
                 "bilanges",
                 "info",
                 "equip",
                 "vege", "selection")
a
copie(a)



climat$score_solinstit.conf <- as.numeric(climat$solinstit.conf)
climat$score_solinstit.limitevols <- as.numeric(climat$solinstit.limitevols)
climat$score_solinstit.vols6h <- as.numeric(climat$solinstit.vols6h)
climat$score_solinstit.train <- as.numeric(climat$solinstit.train)
climat$score_solinstit.compensation <- as.numeric(climat$solinstit.compensation)
climat$score_solinstit.bilanges <- as.numeric(climat$solinstit.bilanges)
climat$score_solinstit.info <- as.numeric(climat$solinstit.info)
climat$score_solinstit.equip <- as.numeric(climat$solinstit.equip)
climat$score_solinstit.vege <- as.numeric(climat$solinstit.vege)
climat$score_solinstit.selection <- as.numeric(climat$solinstit.selection)

climat$score_solinstit.conf[climat$solinstit.conf == 4] <- NA
climat$score_solinstit.limitevols[climat$solinstit.limitevols == 4] <- NA
climat$score_solinstit.vols6h[climat$solinstit.vols6h == 4] <- NA
climat$score_solinstit.train[climat$solinstit.train == 4] <- NA
climat$score_solinstit.compensation[climat$solinstit.compensation == 4] <- NA
climat$score_solinstit.bilanges[climat$solinstit.bilanges == 4] <- NA
climat$score_solinstit.info[climat$solinstit.info == 4] <- NA
climat$score_solinstit.equip[climat$solinstit.equip == 4] <- NA
climat$score_solinstit.vege[climat$solinstit.vege == 4] <- NA
climat$score_solinstit.selection[climat$solinstit.selection == 4] <- NA

a <- cbind(
  rbind(
    mean(climat$score_solinstit.conf, na.rm=T),
    mean(climat$score_solinstit.limitevols, na.rm=T),
    mean(climat$score_solinstit.vols6h, na.rm=T),
    mean(climat$score_solinstit.train, na.rm=T),
    mean(climat$score_solinstit.compensation, na.rm=T),
    mean(climat$score_solinstit.bilanges, na.rm=T),
    mean(climat$score_solinstit.info, na.rm=T),
    mean(climat$score_solinstit.equip, na.rm=T),
    mean(climat$score_solinstit.vege, na.rm=T),
    mean(climat$score_solinstit.selection, na.rm=T)
  ),
  rbind(
    sd(climat$score_solinstit.conf, na.rm=T),
    sd(climat$score_solinstit.limitevols, na.rm=T),
    sd(climat$score_solinstit.vols6h, na.rm=T),
    sd(climat$score_solinstit.train, na.rm=T),
    sd(climat$score_solinstit.compensation, na.rm=T),
    sd(climat$score_solinstit.bilanges, na.rm=T),
    sd(climat$score_solinstit.info, na.rm=T),
    sd(climat$score_solinstit.equip, na.rm=T),
    sd(climat$score_solinstit.vege, na.rm=T),
    sd(climat$score_solinstit.selection, na.rm=T)
  )
)


rownames(a) <- c("conf",
                 "limitevols",
                 "vols6h",
                 "train",
                 "compensation",
                 "bilanges",
                 "info",
                 "equip",
                 "vege", "selection")
a
colnames(a) <- c("Moyenne", "Ecart-type")

copie(a)


# réduire de plus ou moins d'un tiers



a <- t(cbind(
  freq(climat$solreducperso.conf[!climat$solreducperso.conf %in% c("Sans opinion", "Non concerné·e")])[-c(6:7),],
  freq(climat$solreducperso.donnees[!climat$solreducperso.donnees %in% c("Sans opinion", "Non concerné·e")])[-c(6:7),3],
  freq(climat$solreducperso.exp[!climat$solreducperso.exp %in% c("Sans opinion", "Non concerné·e")])[-c(6:7),3],
  freq(climat$solreducperso.info[!climat$solreducperso.info %in% c("Sans opinion", "Non concerné·e")])[-c(6:7),3]
)[,-c(1:2)])


# avec les non concernés
# a <- t(cbind(
#   freq(climat$solreducperso.conf)[-c(6:7),],
#   freq(climat$solreducperso.donnees)[-c(6:7),3],
#   freq(climat$solreducperso.exp)[-c(6:7),3],
#   freq(climat$solreducperso.info)[-c(6:7),3]
# )[,-c(1:2)])


a
rownames(a) <- c("Vols pour les conférences",
                 "Vols pour le recueil des données",
                 "Matériel expériences et observations",
                 "Matériel informatique")
a
copie(a)



# par discipline

# a <- rbind(
#   tapply(climat$score_solinstit.conf, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.conf, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.limitevols, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.limitevols, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.vols6h, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.vols6h, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.train, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.train, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.compensation, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.compensation, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.bilanges, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.bilanges, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.info, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.info, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.equip, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.equip, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.vege, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.vege, climat$discipline_agr4, sd, na.rm=T),
#   tapply(climat$score_solinstit.selection, climat$discipline_agr4, mean, na.rm=T),
#   tapply(climat$score_solinstit.selection, climat$discipline_agr4, sd, na.rm=T)
# )

a <- t(cbind(
tapply(climat$score_solinstit.conf, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.limitevols, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.vols6h, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.train, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.compensation, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.bilanges, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.info, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.equip, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.vege, climat$discipline_agr4, mean, na.rm=T),
tapply(climat$score_solinstit.selection, climat$discipline_agr4, mean, na.rm=T)
))



rownames(a) <- c("conf",
                 "limitevols",
                 "vols6h",
                 "train",
                 "compensation",
                 "bilanges",
                 "info",
                 "equip",
                 "vege", "selection")

a
copie(a)

# croisement écolo/réduction par discipline----
tapply(climat$ScoreEcolo, paste(climat$discipline_agr4, climat$solreducrech), mean, na.rm=T)
tapply(climat$ScoreEcolo, paste(climat$discipline_agr4, climat$solreducperso.conf), mean, na.rm=T)
tapply(climat$ScoreEcolo, paste(climat$discipline_agr4, climat$solreducperso.donnees), mean, na.rm=T)
tapply(climat$ScoreEcolo, paste(climat$discipline_agr4, climat$solreducperso.exp, mean), na.rm=T)
tapply(climat$ScoreEcolo, paste(climat$discipline_agr4, climat$solreducperso.info), mean, na.rm=T)




#regression ordinale ----
freq(climat$preoccupe2)
freq(climat$ageAgr)
freq(climat$discipline_agr5)
freq(climat$sitpro)
freq(climat$sexe)

irec(climat$preoccupe2)

## Recodage de climat$preoccupe2 en climat$preoccupe2_rec
climat$preoccupe2_rec <- fct_recode(climat$preoccupe2,
  NULL = "Sans opinion"
)

reg <- glm(formula = preoccupe2 ~sexe+ageAgr+sitpro+discipline_agr5, 
           data=climat, family = binomial)

library(nnet)
regm1 <- multinom(preoccupe2 ~sexe+ageAgr+sitpro+discipline_agr5, 
                  data=climat)
summary(regm1)

library(GGally)
ggcoef_multinom(regm1)
library(ordinal)

freq(climat$sexe)

climat$sexenf <- paste(climat$sexe, climat$enfantsnb_rec)
irec(climat$sexenf)
## Recodage de climat$sexenf en climat$sexenf_rec
climat$sexenf_rec <- fct_recode(climat$sexenf,
  NULL = "Autre 1",
  NULL = "NA 0",
  NULL = "Autre 2 ou plus",
  NULL = "Autre NA",
  NULL = "NA NA",
  NULL = "Autre 0",
  NULL = "NA 2 ou plus",
  NULL = "NA 1"
)
rego <- clm(preoccupe2_rec ~ sexenf_rec +ageAgr+sitpro+discipline_agr5
            , data=climat)
summary(rego)



# dipôme des parents et discipline

freq(climat$dippar.m)
freq(climat$dippar.p)

cprop(table(climat$dippar.p, climat$discipline_agr5))
## Recodage de climat$dippar.m en climat$dippar.m_rec
climat$dippar.m_rec <- fct_recode(climat$dippar.m,
  "Inf bac +2" = "Aucun diplôme",
  "Inf bac +2" = "CAP, BEP, BEPC ou équivalent",
  "Inf bac +2" = "Bac ou équivalent",
  "Bac +2 ou plus" = "Bac +2 ou 3",
  "Bac +2 ou plus" = "Bac +4 ou 5",
  "Bac +2 ou plus" = "Doctorat",
  NULL = "Ne sait pas"
)
climat$dippar.p_rec <- fct_recode(climat$dippar.p,
                                  "Inf bac +2" = "Aucun diplôme",
                                  "Inf bac +2" = "CAP, BEP, BEPC ou équivalent",
                                  "Inf bac +2" = "Bac ou équivalent",
                                  "Bac +2 ou plus" = "Bac +2 ou 3",
                                  "Bac +2 ou plus" = "Bac +4 ou 5",
                                  "Bac +2 ou plus" = "Doctorat",
                                  NULL = "Ne sait pas"
)

a <- cprop(table(climat$dippar.p_rec, climat$discipline_agr5))[-3, -17]
b <- cprop(table(climat$dippar.m_rec, climat$discipline_agr5))[-3, -17]
c <- cbind(a[2,], b[2,])
colnames(c) <- c("diplome_mere", "diplome_pere")
c <- as.data.frame(c)
c$discipline <- rownames(c)
library(ggrepel)
ggplot(c, aes(x=diplome_mere, y=diplome_pere, label = discipline))+geom_point()+
  geom_label_repel()+xlim(30,70)+ylim(30,70)


## Recodage de climat$dippar.m en climat$dippar.m5_rec
climat$dippar.m5_rec <- fct_recode(climat$dippar.m,
                                  "Inf bac +4" = "Aucun diplôme",
                                  "Inf bac +4" = "CAP, BEP, BEPC ou équivalent",
                                  "Inf bac +4" = "Bac ou équivalent",
                                  "Inf bac +4" = "Bac +2 ou 3",
                                  "Bac +4 ou plus" = "Bac +4 ou 5",
                                  "Bac +4 ou plus" = "Doctorat",
                                  NULL = "Ne sait pas"
)
climat$dippar.p5_rec <- fct_recode(climat$dippar.p,
                                   "Inf bac +4" = "Aucun diplôme",
                                   "Inf bac +4" = "CAP, BEP, BEPC ou équivalent",
                                   "Inf bac +4" = "Bac ou équivalent",
                                   "Inf bac +4" = "Bac +2 ou 3",
                                   "Bac +4 ou plus" = "Bac +4 ou 5",
                                   "Bac +4 ou plus" = "Doctorat",
                                   NULL = "Ne sait pas"
)

a <- cprop(table(climat$dippar.p5_rec, climat$discipline_agr5))[-3, -17]
b <- cprop(table(climat$dippar.m5_rec, climat$discipline_agr5))[-3, -17]
c <- cbind(a[2,], b[2,])
colnames(c) <- c("diplome_mere", "diplome_pere")
c <- as.data.frame(c)
c$discipline <- rownames(c)
library(ggrepel)
ggplot(c, aes(x=diplome_mere, y=diplome_pere, label = discipline))+geom_point()+
  geom_label_repel()+xlim(0,70)+ylim(0,70)

copie(c)       




climat$discipline_agr5 <- fct_relevel(climat$discipline_agr5, "Physique")
climat$sitpro <- fct_relevel(climat$sitpro, "Maître·sse de conférences")

rego <- clm(preoccupe2_rec ~ sexe +dippar.m5_rec+ageAgr+sitpro+discipline_agr5
            , data=climat)
summary(rego)

rego <- clm(preoccupe2_rec ~ sexe +dippar.m+ageAgr+sitpro+discipline_agr5
            , data=climat)
rego <- clm(preoccupe2_rec ~ sexe +ageAgr+sitpro+discipline_agr5
            , data=climat)
summary(rego)

exp(rego$coefficients)


freq(climat$solreducrech)
class(climat$reducrechexemp)
rego <- clm(as.factor(solreducrech) ~ sexe +dippar.m+ageAgr+sitpro+discipline_agr5
            , data=climat)
summary(rego)
rego <- clm(as.factor(solreducrech) ~ sexe +dippar.p+ageAgr+sitpro+discipline_agr5
            , data=climat)
summary(rego)



freq(climat$preoccupe2)
freq(climat$opinionecolo.techno)
freq(climat$opinionecolo.decroissance)
freq(climat$materiel.tgir)
freq(climat$solreducrech)

(materiel.tgir == "Oui" | materiel.extensif == "Oui" | materiel.trescouteux == "Oui" | materiel.couteux == "Oui", na.rm=TRUE)


climat_reduit <- subset(climat, !discipline_agr5 %in% c(
  "Droit, gestion",
  "Autres sciences sociales",
  "Économie",
  "Lettres",
  "Histoire, anthropologie"
))
mean(climat_reduit$materiel.tgir == "Oui" | 
       climat_reduit$materiel.extensif == "Oui" | 
       climat_reduit$materiel.trescouteux == "Oui" | 
       climat_reduit$materiel.couteux == "Oui", na.rm=TRUE)

lapply(list(group_by(climat, discipline_agr5),
            group_by(climat, sitpro_reduite),
            group_by(climat, ageAgr2),
            group_by(climat, sexe)),
       function(df) {
         variable <- colnames(df)[[group_cols(data=df)]]
         res <- summarize(df,
                          "Réduire de plus d'un tiers"=mean(solreducrech3 == "Réduire de plus d'un tiers", na.rm=TRUE) * 100,
                          #"Réduire d'un tiers"=mean(solreducrech3 == "Réduire d'un tiers", na.rm=TRUE) * 100,
                          #"Réduire de moins d'un tiers"=mean(solreducrech3 == "Réduire de moins d'un tiers", na.rm=TRUE) * 100,
                          "Très ou extrêmement préoccupé"=mean(preoccupe2 == "Très préoccupé·e" | preoccupe2 == "Extrêmement préoccupé·e", na.rm=TRUE) * 100,
                          "Confiant dans la technologie"=mean(opinionecolo.techno == "Tout à fait d'accord" | opinionecolo.techno == "Plutôt d'accord", na.rm=TRUE) * 100,
                          "Tout à fait pour la décroissance"=mean(opinionecolo.decroissance == "Tout à fait d'accord", na.rm=TRUE) * 100,
                          "Utilise du matériel lourd"=mean(materiel.tgir == "Oui" | materiel.extensif == "Oui" | materiel.trescouteux == "Oui" | materiel.couteux == "Oui", na.rm=TRUE) * 100)
         rename(res, modalites=1)
       }) %>%
  bind_rows() %>%
  drop_na(modalites) %>%
  mutate(across(where(is.numeric), ~ round(.x))) %>%
  as.matrix() %>%
  knitr::kable()


with(climat, freq(materiel.tgir == "Oui" | materiel.extensif == "Oui" | materiel.trescouteux == "Oui" | materiel.couteux == "Oui"))



climat$volsdist_totconfreu<-climat$volsdist_totconf+
  climat$volsdist_totworkshop + 
  climat$volsdist_totjury + 
  climat$volsdist_totfinanc +
  climat$volsdist_toteval


summary(climat$volsdist_totterrain)
summary(climat$volsdist_totconfreu)

?summary

## Recodage de climat$volsdist_totterrain en climat$volsdist_totterrain_rec
climat$volsdist_totterrain_rec <- cut(climat$volsdist_totterrain,
  include.lowest = FALSE,
  right = TRUE,
  breaks = c(0,1000, 10000, 1000000)
)

climat$volsdist_totterrain_tranches<- cut(climat$volsdist_totconfreu,
                                         include.lowest = TRUE,
                                         right = TRUE,
                                         breaks = c(0, 0.1, 1500, 7000, 30000, 100000))

climat$volsdist_totconfreu_tranch2<- cut(climat$volsdist_totconfreu,
                                         include.lowest = TRUE,
                                         right = TRUE,
                                         breaks = c(0, 0.1, 1500, 7000, 30000, 100000))




## Recodage de climat$sitpro en climat$sitpro_reduite
climat$sitpro_reduite <- fct_recode(climat$sitpro,
                                    "DR et PU" = "Directeur·rice de recherche",
                                    "DR et PU" = "Professeur·e des universités",
                                    "CR et MCF" = "Chargé·e de recherche",
                                    "CR et MCF" = "Maître·sse de conférences",
                                    "ITA" = "Ingénieur·e de recherche",
                                    "Doc et post-doc" = "Post-doctorant·e",
                                    "Doc et post-doc" = "ATER",
                                    "Doc et post-doc" = "Doctorant·e contractuel·le",
                                    "Doc et post-doc" = "Doctorant·e CIFRE",
                                    "ITA" = "Ingénieur·e d'études",
                                    "ITA" = "Chargé·e d'études/de mission",
                                    "ITA" = "Assistant ingénieur·e",
                                    "ITA" = "Technicien·ne",
                                    "ITA" = "Adjoint·e technique",
                                    NULL = "Autre"
)

freq(climat$solreducperso.info[climat$solreducperso.info!="Non concerné·e"])
freq(climat$solreducperso.donnees[climat$solreducperso.donnees!="Non concerné·e"])
freq(climat$solreducperso.conf[climat$solreducperso.conf!="Non concerné·e"])
freq(climat$solreducperso.exp[climat$solreducperso.exp!="Non concerné·e"])

freq(climat$solevolges.conf)
freq(climat$sitpro_reduite)
freq(climat$chgtpratique)
freq(climat$solrisqreducavion.isoler)
freq(climat$solrisqreducavion.qual)
freq()

copie(
rbind(
lprop(table(climat$volsdist_totconfreu_tranch2, climat$solreducperso.conf)[,c(1,2,4)]),
lprop(table(climat$solevolges.conf, climat$solreducperso.conf)[,c(1,2,4)]),
lprop(table(climat$sitpro_reduite, climat$solreducperso.conf)[,c(1,2,4)]),
lprop(table(climat$chgtpratique, climat$solreducperso.conf)[,c(1,2,4)]),
lprop(table(climat$solrisqreducavion.isoler, climat$solreducperso.conf)[,c(1,2,4)]),
lprop(table(climat$solrisqreducavion.qual, climat$solreducperso.conf)[,c(1,2,4)])
)
)

tapply(climat$volsdist_totconfreu, climat$solreducperso.conf, mean, na.rm=T)
tapply(climat$volsdist_totterrain, climat$solreducperso.donnees, mean, na.rm=T)

lprop(table(climat$volsdist_totterrain_rec, climat$solreducperso.donnees))
