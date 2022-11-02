library(ggrepel)
library(paletteer)
library(GGally)
library(ggpol)
library("gridExtra")
library("cowplot")

source("recodages.R", encoding="UTF-8")

# Recodages en plus pour l'occasion ----


climat$enfsexe <- paste(climat$sexe, climat$enfantsnb_rec)
## Recodage de climat$enfsexe
climat$enfsexe <- fct_recode(climat$enfsexe,
                             NULL = "Homme NA",
                             NULL = "Femme NA",
                             NULL = "NA 1",
                             NULL = "NA 0",
                             NULL = "NA 2 ou plus",
                             NULL = "NA NA"
)

climat$enfagesexe <- paste(climat$sexe, climat$enfantsage_rec)
## Recodage de climat$enfsexe
# irec(climat$enfagesexe)
## Recodage de climat$enfagesexe
climat$enfagesexe <- fct_recode(climat$enfagesexe,
                                NULL = "Homme NA",
                                NULL = "Femme NA",
                                NULL = "NA Entre 5 et 15 ans",
                                NULL = "NA Sans enfant",
                                NULL = "NA moins de 5 ans",
                                NULL = "NA NA",
                                NULL = "NA Plus de 15 ans"
)



#indicateurs de compétition

freq(climat$projets.anr_r2)
freq(climat$projets.anr_m2)
freq(climat$projets.europe_r2)
freq(climat$carriere)
freq(climat$reliquat.avion)
freq(climat$reliquat.ordi)

freq(climat$sitpro)



## Recodage de climat_recherche$nbpublisang en climat_recherche$nbpublisang_tranch
climat$nbpublisang_tranch <- cut(climat$nbpublisang,
                                 include.lowest = TRUE,
                                 right = TRUE,
                                 breaks = c(0, 1, 3, 5, 8, 13, 1300)
)



## Recodage de climat$nbpublis en climat$nbpublis_rec
climat$nbpublis_tranch3 <- cut(climat$nbpublis,
                               include.lowest = FALSE,
                               right = TRUE,
                               breaks = c(0, 1, 3, 6, 10, 1300)
)


## Recodage de climat$hindex en climat$hindex_rec
climat$hindex_tranch3 <- cut(climat$hindex,
                             include.lowest = FALSE,
                             right = TRUE,
                             breaks = c(0, 10, 15, 20.6000000000001, 29, 6000)
)



# sous base seulement recherche
climat_recherche <- climat[!climat$sitpro %in% c(
  "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
  "Chargé·e d'études/de mission","Adjoint·e technique",
  "Autre"
),]
climat_recherche <- climat_recherche[climat_recherche$sexe!="Autre",]




climat_recherche$chgtpratiquenum=as.numeric(fct_relevel(
  climat_recherche$chgtpratique, "Non, pas du tout d'accord",
  "Non, plutôt pas d'accord", "Sans opinion",
  "Oui, plutôt d'accord",
  "Oui, tout à fait d'accord")) - 1



# recodages milan multiniveaux ----


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
                                                          "Très préoccupé·e", "Extrêmement préoccupé·e")) - 1,
                     solreducrechnum=as.numeric(fct_rev(solreducrech)) - 1)

# Création des variables par labo et par discipline
climatRegr <- group_by(climatRegr, unite.labintel) %>%
  mutate(volshnum_labo=mean(volshnum, na.rm=TRUE),
         nbpublis2_labo=mean(nbpublis2, na.rm=TRUE),
         ScoreEcolo_labo=mean(ScoreEcolo, na.rm=TRUE),
         ScoreInternational_labo=mean(ScoreInternational, na.rm=TRUE),
         tourisme_labo=mean(tourisme, na.rm=TRUE),
         avionpersonum_labo=mean(avionpersonum, na.rm=TRUE),
         chgtpratiquenum_labo=mean(chgtpratiquenum, na.rm=TRUE),
         preoccupe2num_labo=mean(preoccupe2num, na.rm=TRUE),
         solreducrechnum_labo=mean(solreducrechnum, na.rm=TRUE)) %>%
  ungroup()

climatRegr <- group_by(climatRegr, discipline) %>%
  mutate(volshnum_disc=mean(volshnum, na.rm=TRUE),
         nbpublis2_disc=mean(nbpublis2, na.rm=TRUE),
         ScoreEcolo_disc=mean(ScoreEcolo, na.rm=TRUE),
         ScoreInternational_disc=mean(ScoreInternational, na.rm=TRUE),
         tourisme_disc=mean(tourisme, na.rm=TRUE),
         avionpersonum_disc=mean(avionpersonum, na.rm=TRUE),
         chgtpratiquenum_disc=mean(chgtpratiquenum, na.rm=TRUE),
         preoccupe2num_disc=mean(preoccupe2num, na.rm=TRUE),
         solreducrechnum_disc=mean(solreducrechnum, na.rm=TRUE)) %>%
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
                     solreducrechnum_c=solreducrechnum - solreducrechnum_labo - solreducrechnum_disc + mean(solreducrechnum, na.rm=TRUE),
                     
                     volshnum_clabo=volshnum - volshnum_labo,
                     nbpublis2_clabo=nbpublis2 - nbpublis2_labo,
                     ScoreEcolo_clabo=ScoreEcolo - ScoreEcolo_labo,
                     ScoreInternational_clabo=ScoreInternational - ScoreInternational_labo,
                     tourisme_clabo=tourisme - tourisme_labo,
                     avionpersonum_clabo=avionpersonum - avionpersonum_labo,
                     chgtpratiquenum_clabo=chgtpratiquenum - chgtpratiquenum_labo,
                     preoccupe2num_clabo=preoccupe2num - preoccupe2num_labo,
                     solreducrechnum_c=solreducrechnum - solreducrechnum_labo)



## Recodage de climatRegr$volsnbtranch en climatRegr$volsnb_dicho
climatRegr$volsnb_dicho <- climatRegr$volsnbtranch %>%
  fct_recode(
    "0" = "[0,0.5)",
    "1" = "[0.5,1)",
    "1" = "[1,2)",
    "1" = "[2,3)",
    "11" = "[3,4)",
    "1" = "[4,5)",
    "1" = "[5,7)",
    "1" = "[7,65]"
  )




# Recodages avion pro et perso ----
## Recodage de climatRegr$vols_dicho en climatRegr$vols_dicho_rec
climatRegr$vols_dicho_rec <- climatRegr$vols_dicho %>%
  fct_recode(
    "Pro_Non" = "N'a pas volé en 2019",
    "Pro_Oui" = "A volé en 2019"
  )
## Recodage de climatRegr$avionperso en climatRegr$avionperso_rec
climatRegr$avionperso_rec <- climatRegr$avionperso %>%
  fct_recode(
    "Perso_Non" = "Aucun aller-retour",
    "Perso_Oui" = "1 ou 2 allers-retours",
    "Perso_Oui" = "3 ou 4 allers-retours",
    "Perso_Oui" = "Plus de 5 allers-retours"
  )

prop(table(climatRegr$avionperso_rec, climatRegr$vols_dicho_rec))
climatRegr$avion_pro_perso <- paste(climatRegr$avionperso_rec, 
                                    climatRegr$vols_dicho_rec)

## Recodage de climatRegr$avion_pro_perso
climatRegr$avion_pro_perso <- climatRegr$avion_pro_perso %>%
  fct_recode(
    NULL = "NA NA",
    NULL = "NA Pro_Non",
    NULL = "NA Pro_Oui",
    NULL = "Perso_Non NA",
    NULL = "Perso_Oui NA"
  )



## Recodage de climatRegr$preoccupe2 en climatRegr$preoccupe2_4 (4 modalités)
climatRegr$preoccupe2_4 <- climatRegr$preoccupe2 %>%
  fct_recode(
    "Extrêmement" = "Extrêmement préoccupé·e",
    "Très" = "Très préoccupé·e",
    "Assez" = "Assez préoccupé·e",
    "Un peu ou pas du tout" = "Un peu préoccupé·e",
    "Un peu ou pas du tout" = "Pas du tout préoccupé·e",
    NULL = "Sans opinion"
  )

## Réordonnancement de climatRegr$preoccupe2_4 
climatRegr$preoccupe2_4 <- climatRegr$preoccupe2_4 %>%
  fct_relevel(
    "Un peu ou pas du tout", "Assez", "Très", "Extrêmement"
  )


## Réordonnancement de climatRegr$preoccupe2_4 en climatRegr$preoccupe2_4_rev
climatRegr$preoccupe2_4_rev <- climatRegr$preoccupe2_4 %>%
  fct_relevel(
    "Un peu ou pas du tout", "Assez", "Très", "Extrêmement"
  )

## Recodage de climat$preoccupe2 en climat$preoccupe2_rec (on vire les sans opinions)
climatRegr$preoccupe2_rec <- fct_recode(climatRegr$preoccupe2,
                                        NULL = "Sans opinion"
)

## Réordonnancement de climatRegr$preoccupe2_rec en climatRegr$preoccupe2_rec_rev
climatRegr$preoccupe2_rec_rev <- climatRegr$preoccupe2_rec %>%
  fct_relevel(
    "Extrêmement préoccupé·e", "Très préoccupé·e", "Assez préoccupé·e",
    "Un peu préoccupé·e", "Pas du tout préoccupé·e"
  )





# Traitements ----
# sur climatRegr (seulement recherche)

# Avion perso et pro ----

# tris à plat

freq(climatRegr$avionperso)
freq(climatRegr$avion_pro_perso)


# Graphique tri croisé ----

a <- lprop(table(climatRegr$preoccupe2_4, climatRegr$avionperso_rec))
b <- lprop(table(climatRegr$preoccupe2_4, climatRegr$vols_dicho_rec))
avion <- cbind(a[1:4, 2], b[1:4, 2])
colnames(avion) <- c("Privé", "Pro")
class(avion)
avion <- as.data.frame(as.table(avion))
avion_2 = 
  tibble(Freq = c(-80, 0, 0, 80),
         Var2 = c("Privé", "Privé", "Pro", "Pro"), 
         Var1=levels(avion$Var1)) # pour définir la longueur de l'axe des abscisses


avion$Freq = ifelse(avion$Var2 == "Privé", avion$Freq * -1, avion$Freq)
ggplot(avion, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", color="black", fill="black")+
  geom_blank(data = avion_2,
             mapping = aes(y = Freq))  + 
  facet_share(~Var2, dir = "h", scales = "free", reverse_num = T) +
  coord_flip()+ 
  labs(x="", y = "%", fill="") +
  theme(legend.position = "bottom")+theme_classic()+theme(
    plot.title = element_blank(),axis.title.y = element_blank())

# pas réussi à réduire cet espace bizarre à gauche


# avec le score écolo ----


a <- lprop(table(climatRegr$ScoreEcolo, climatRegr$avionperso_rec))
b <- lprop(table(climatRegr$ScoreEcolo, climatRegr$vols_dicho_rec))
avion <- cbind(a[1:6, 2], b[1:6, 2])
colnames(avion) <- c("Privé", "Pro")
class(avion)
avion <- as.data.frame(as.table(avion))
avion_2 = 
  tibble(Freq = c(-80, 0, 0, 80, 0, 80),
         Var2 = c("Privé", "Privé", "Pro", "Pro", "Pro", "Pro"), 
         Var1=levels(avion$Var1)) # pour définir la longueur de l'axe des abscisses


avion$Freq = ifelse(avion$Var2 == "Privé", avion$Freq * -1, avion$Freq)
ggplot(avion, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", color="black", fill="black")+
  geom_blank(data = avion_2,
             mapping = aes(y = Freq))  + 
  facet_share(~Var2, dir = "h", scales = "free", reverse_num = T) +
  coord_flip()+ 
  labs(x="", y = "%", fill="") +
  theme(legend.position = "bottom")+theme_classic()+theme(
    plot.title = element_blank(),axis.title.y = element_blank())



# avec la décroissance ----


a <- lprop(table(climatRegr$opinionecolo.decroissance, climatRegr$avionperso_rec))
b <- lprop(table(climatRegr$opinionecolo.decroissance, climatRegr$vols_dicho_rec))
avion <- cbind(a[1:4, 2], b[1:4, 2])
colnames(avion) <- c("Privé", "Pro")
class(avion)
avion <- as.data.frame(as.table(avion))
avion_2 = 
  tibble(Freq = c(-80, 0, 0, 80),
         Var2 = c("Privé", "Privé", "Pro", "Pro"), 
         Var1=levels(avion$Var1)) # pour définir la longueur de l'axe des abscisses


avion$Freq = ifelse(avion$Var2 == "Privé", avion$Freq * -1, avion$Freq)
ggplot(avion, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", color="black", fill="black")+
  geom_blank(data = avion_2,
             mapping = aes(y = Freq))  + 
  facet_share(~Var2, dir = "h", scales = "free", reverse_num = T) +
  coord_flip()+ 
  labs(x="", y = "%", fill="") +
  theme(legend.position = "bottom")+theme_classic()+theme(
    plot.title = element_blank(),axis.title.y = element_blank())


# avec les changements profonds ----


a <- lprop(table(climatRegr$chgtpratique, climatRegr$avionperso_rec))
b <- lprop(table(climatRegr$chgtpratique, climatRegr$vols_dicho_rec))
avion <- cbind(a[1:4, 2], b[1:4, 2])
colnames(avion) <- c("Privé", "Pro")
class(avion)
avion <- as.data.frame(as.table(avion))
avion_2 = 
  tibble(Freq = c(-80, 0, 0, 80),
         Var2 = c("Privé", "Privé", "Pro", "Pro"), 
         Var1=levels(avion$Var1)) # pour définir la longueur de l'axe des abscisses


avion$Freq = ifelse(avion$Var2 == "Privé", avion$Freq * -1, avion$Freq)
ggplot(avion, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", color="black", fill="black")+
  geom_blank(data = avion_2,
             mapping = aes(y = Freq))  + 
  facet_share(~Var2, dir = "h", scales = "free", reverse_num = T) +
  coord_flip()+ 
  labs(x="", y = "%", fill="") +
  theme(legend.position = "bottom")+theme_classic()+theme(
    plot.title = element_blank(),axis.title.y = element_blank())


# Même graphique par sexe ----

## pour les Femmes

a <- lprop(table(climatRegr$preoccupe2_4[climatRegr$sexe=="Femme"], 
                 climatRegr$avionperso_rec[climatRegr$sexe=="Femme"]))
b <- lprop(table(climatRegr$preoccupe2_4[climatRegr$sexe=="Femme"], 
                 climatRegr$vols_dicho_rec[climatRegr$sexe=="Femme"]))
avion <- cbind(a[1:4, 2], b[1:4, 2])
colnames(avion) <- c("Privé", "Pro")
class(avion)
avion <- as.data.frame(as.table(avion))
avion_2 = 
  tibble(Freq = c(-80, 0, 0, 80),
         Var2 = c("Privé", "Privé", "Pro", "Pro"), 
         Var1=levels(avion$Var1)) # pour définir la longueur de l'axe des abscisses


avion$Freq = ifelse(avion$Var2 == "Privé", avion$Freq * -1, avion$Freq)
ggplot(avion, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", color="black", fill="black")+
  geom_blank(data = avion_2,
             mapping = aes(y = Freq))  + 
  facet_share(~Var2, dir = "h", scales = "free", reverse_num = T) +
  coord_flip()+ 
  labs(x="", y = "%", fill="") +
  theme(legend.position = "bottom")+theme_classic()+theme(
    plot.title = element_blank(),axis.title.y = element_blank())

# pas réussi à réduire cet espace bizarre à gauche


## pour les Hommes

a <- lprop(table(climatRegr$preoccupe2_4[climatRegr$sexe=="Homme"], 
                 climatRegr$avionperso_rec[climatRegr$sexe=="Homme"]))
b <- lprop(table(climatRegr$preoccupe2_4[climatRegr$sexe=="Homme"], 
                 climatRegr$vols_dicho_rec[climatRegr$sexe=="Homme"]))
avion <- cbind(a[1:4, 2], b[1:4, 2])
colnames(avion) <- c("Privé", "Pro")
class(avion)
avion <- as.data.frame(as.table(avion))
avion_2 = 
  tibble(Freq = c(-80, 0, 0, 80),
         Var2 = c("Privé", "Privé", "Pro", "Pro"), 
         Var1=levels(avion$Var1)) # pour définir la longueur de l'axe des abscisses


avion$Freq = ifelse(avion$Var2 == "Privé", avion$Freq * -1, avion$Freq)
ggplot(avion, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", color="black", fill="black")+
  geom_blank(data = avion_2,
             mapping = aes(y = Freq))  + 
  facet_share(~Var2, dir = "h", scales = "free", reverse_num = T) +
  coord_flip()+ 
  labs(x="", y = "%", fill="") +
  theme(legend.position = "bottom")+theme_classic()+theme(
    plot.title = element_blank(),axis.title.y = element_blank())

# pas réussi à réduire cet espace bizarre à gauche


# Les deux sexes ensemble

a <- lprop(table(climatRegr$preoccupe2_4[climatRegr$sexe=="Homme"], 
                 climatRegr$avionperso_rec[climatRegr$sexe=="Homme"]))
b <- lprop(table(climatRegr$preoccupe2_4[climatRegr$sexe=="Homme"], 
                 climatRegr$vols_dicho_rec[climatRegr$sexe=="Homme"]))
c <- lprop(table(climatRegr$preoccupe2_4[climatRegr$sexe=="Femme"], 
                 climatRegr$avionperso_rec[climatRegr$sexe=="Femme"]))
d <- lprop(table(climatRegr$preoccupe2_4[climatRegr$sexe=="Femme"], 
                 climatRegr$vols_dicho_rec[climatRegr$sexe=="Femme"]))
avion_hommes <- cbind(a[1:4, 2], b[1:4, 2])
avion_femmes <- cbind(c[1:4, 2], d[1:4, 2])

colnames(avion_hommes) <- c("Privé", "Pro")
colnames(avion_femmes) <- c("Privé", "Pro")

class(avion)
avion_hommes <- as.data.frame(as.table(avion_hommes))
avion_femmes <- as.data.frame(as.table(avion_femmes))
avion_hommes$sexe <- "Homme"
avion_femmes$sexe <- "Femme"
avion <- rbind(avion_femmes, avion_hommes)
avion_2 = 
  tibble(Freq = c(-80, 0, 0, 80),
         Var2 = c("Privé", "Privé", "Pro", "Pro"), 
         Var1=levels(avion$Var1), 
         sexe=1) # pour définir la longueur de l'axe des abscisses


avion$Freq = ifelse(avion$Var2 == "Privé", avion$Freq * -1, avion$Freq)
ggplot(avion, aes(x = Var1, y = Freq, fill = sexe, group=sexe, color=sexe)) +
  geom_bar(stat = "identity", color="black", fill="black")+
  geom_blank(data = avion_2,
             mapping = aes(y = Freq))  + 
  facet_share(~Var2, dir = "h", scales = "free", reverse_num = T) +
  coord_flip()+ 
  labs(x="", y = "%", fill="") +
  theme(legend.position = "bottom")+theme_classic()+theme(
    plot.title = element_blank(),axis.title.y = element_blank())

avion_2 = 
  tibble(Freq = c(-70, 0, 0, 70, -70, 0, 0, 70),
         Var2 = c("Privé", "Privé", "Pro", "Pro", "Privé", "Privé", "Pro", "Pro"), 
         Var1=c(levels(avion$Var1), levels(avion$Var1)), 
         sexe=c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme", "Homme", "Femme")) # pour définir la longueur de l'axe des abscisses
ggplot(avion, aes(x = Var1, y = Freq, fill = sexe)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black")+
  geom_blank(data = avion_2,
             mapping = aes(y = Freq))  +
  facet_share(~Var2, dir = "h", scales = "free", reverse_num = T) +
  scale_fill_grey(start=0, end=1)+
  coord_flip()+ 
  labs(x="", y = "%", fill="") +
  theme(legend.position = "bottom")+theme_classic()+theme(
    plot.title = element_blank(),axis.title.y = element_blank())+
  geom_label(aes(label = paste(" (",round(abs(Freq)),"%)", sep=""), group=sexe), 
             fill="white", colour = "black", 
             position= position_dodge(1), 
             size = 3)
# pas réussi à réduire cet espace bizarre à gauche




# Régressions logistiques : comparaison de l'effet de l'inquiétude sur les vols pros et persos----

# recodages spécifique pour la représentation graphique de la régression ----
## Réordonnancement de climatRegr$preoccupe2_rec en climatRegr$preoccupe2_rec_rev
climatRegr$preoccupe2_rec_rev <- climatRegr$preoccupe2_rec %>%
  fct_relevel(
    "Extrêmement préoccupé·e",  "Pas du tout préoccupé·e",
    "Un peu préoccupé·e", "Assez préoccupé·e",
    "Très préoccupé·e"
  )


# contrôle par sexe et sitpro ----

reg1 <- glm(vols_dicho_rec~preoccupe2_rec_rev+sexe+sitpro2, data=climatRegr, 
            family = binomial())
reg2 <- glm(avionperso_rec~preoccupe2_rec_rev+sexe+sitpro2, data=climatRegr, 
            family = binomial())
ggcoef_compare(list("Vols pro"=reg1, "Vols perso"=reg2), 
               conf.level = 0.9, exponentiate = T)


# contrôle par sexe, sitpro et discipline ----

## Réordonnancement de climatRegr$preoccupe2_rec en climatRegr$preoccupe2_rec_rev
climatRegr$preoccupe2_rec_rev <- climatRegr$preoccupe2_rec %>%
  fct_relevel(
    "Extrêmement préoccupé·e",
    "Très préoccupé·e",  "Assez préoccupé·e",
    "Un peu préoccupé·e", 
    "Pas du tout préoccupé·e"
  )

reg1 <- glm(vols_dicho_rec~preoccupe2_rec_rev+sexe+sitpro2
            +discipline_agr5, data=climatRegr, 
            family = binomial())
reg2 <- glm(avionperso_rec~preoccupe2_rec_rev+sexe+sitpro2
            +discipline_agr5, data=climatRegr, 
            family = binomial())
ggcoef_compare(list("Vols pro"=reg1, "Vols perso"=reg2),
               conf.level = 0.9, exponentiate = T)
a <- ggcoef_model(reg1, include = "preoccupe2_rec_rev")
b <- ggcoef_model(reg2, include = "preoccupe2_rec_rev")
plot_grid(a,b)


#essais avec le score d'internationalisation

reg1 <- glm(vols_dicho_rec~preoccupe2_rec_rev+sexe+sitpro2+
              ScoreInternational+nbpublistranch+
              discipline_agr5, data=climatRegr, 
            family = binomial())
reg2 <- glm(avionperso_rec~preoccupe2_rec_rev+sexe+sitpro2+
              ScoreInternational+nbpublistranch+
              discipline_agr5
              , data=climatRegr, 
            family = binomial())
ggcoef_compare(list("Vols pro"=reg1, "Vols perso"=reg2), 
               conf.level = 0.9, exponentiate = T)
freq(climatRegr$nbpublistranch)

# Renoncer à une conférence internationale (en cours) ----


## Recodage de climatRegr$renoncedep.env en climatRegr$renoncedep.env_dicho1
climatRegr$renoncedep.env_dicho1 <- climatRegr$renoncedep.env %>%
  fct_recode(
    "Oui_determinant" = "Oui, c'était une raison déterminante",
    "Non" = "Oui, mais c'était une raison secondaire"
  )
climatRegr$renoncedep.env_dicho1 <- fct_relevel(climatRegr$renoncedep.env_dicho1,
                                                "Non")


## Réordonnancement de climatRegr$preoccupe2_4
climatRegr$preoccupe2_4 <- climatRegr$preoccupe2_4 %>%
  fct_relevel(
    "Extrêmement", "Très", "Assez", "Un peu ou pas du tout"
  )
reg1 <- glm(renoncedep.env_dicho1 ~ preoccupe2_4+sexe+sitpro2 +
              volsnbtranch2, data=climatRegr,
            family = binomial())
ggcoef_model(reg1, exponentiate = T)
# permet de voir que les inquiets, ils renoncent plus 
# et que donc, s'ils étaient pas inquiets, ils auraient encore plus de vols


freq(climatRegr$ScoreEcolo_m)
reg1 <- glm(renoncedep.env_dicho1 ~ discipline_agr5+
              sexe+
              sitpro2 +
              volsnbtranch2, data=climatRegr,
            family = binomial())
ggcoef_model(reg1, exponentiate = T)







# install.packages("remotes")
# remotes::install_github("glmmTMB/glmmTMB/glmmTMB@master")

# stats desc -----


freq(climat_recherche$solreducrech3)
a <- lapply(list(group_by(climatRegr, sexe),
                 group_by(climatRegr, nbpublistranch),
                 group_by(climatRegr, preoccupe2_4),
                 group_by(climatRegr, as.character(ScoreEcolo)),
                 group_by(climatRegr, sitpro2),
                 group_by(climatRegr, volsnbtranch)
),
function(df) {
  variable <- colnames(df)[[group_cols(data=df)]]
  res <- summarize(df, renoncer=mean(renoncedep.env=="Oui, c'était une raison déterminante",
                                            na.rm=TRUE),
                   variable=variable)
  rename(res, modalites=1)
}) %>%
  bind_rows() %>%
  drop_na(modalites) 

a
a$modalites <- paste(a$variable, a$modalites)
labels <- a$modalites
ggplot(a,aes(y=100*renoncer, x=fct_rev(modalites), 
             label=modalites, color=variable, 
             shape=variable)) +
  geom_point() + 
  geom_segment( aes(x=modalites, xend=modalites, 
                    y=0, yend=100*renoncer))+ coord_flip()+
  theme_minimal()+
  guides(color=guide_none(), shape=guide_none()) +
  labs(x="", y="Distance totale parcourue")+ 
  scale_x_discrete(limits=rev(labels))



a <- lapply(list(group_by(climatRegr, sexe),
                 group_by(climatRegr, sitpro2),
                 group_by(climatRegr, discipline_agr5),
                 group_by(climatRegr, carriere),
                 group_by(climatRegr, nbpublistranch),
                 group_by(climatRegr, volsnbtranch)
),
function(df) {
  variable <- colnames(df)[[group_cols(data=df)]]
  res <- summarize(df, renoncer=mean(renoncedep.env=="Oui, c'était une raison déterminante",
                                     na.rm=TRUE),
                   variable=variable)
  rename(res, modalites=1)
}) %>%
  bind_rows() %>%
  drop_na(modalites) 

a
a$modalites <- paste(a$variable, a$modalites)
labels <- a$modalites
ggplot(a,aes(y=100*renoncer, x=fct_rev(modalites), 
             label=modalites, color=variable, 
             shape=variable)) +
  geom_point() + 
  geom_segment( aes(x=modalites, xend=modalites, 
                    y=0, yend=100*renoncer))+ coord_flip()+
  theme_minimal()+
  guides(color=guide_none(), shape=guide_none()) +
  labs(x="", y="Distance totale parcourue")+ 
  scale_x_discrete(limits=rev(labels))


a <- lapply(list(group_by(climatRegr, opinionecolo.decroissance),
                 group_by(climatRegr, opinionecolo.techno),
                 group_by(climatRegr, dixannees.bilan),
                 group_by(climatRegr, dixannees.giec),
                 group_by(climatRegr, dixannees.asso),
                 group_by(climatRegr, dixannees.marche),
                 group_by(climatRegr, dixannees.vote),
                 group_by(climatRegr, preoccupe2_4),
                 group_by(climatRegr, chgtpratique),
                 group_by(climatRegr, solreducrech),
                 group_by(climatRegr, solreducperso.conf)
),
function(df) {
  variable <- colnames(df)[[group_cols(data=df)]]
  res <- summarize(df, renoncer=mean(renoncedep.env=="Oui, c'était une raison déterminante",
                                     na.rm=TRUE),
                   variable=variable)
  rename(res, modalites=1)
}) %>%
  bind_rows() %>%
  drop_na(modalites) 

a$modalites <- paste(a$variable, a$modalites)
labels <- a$modalites
ggplot(a,aes(y=100*renoncer, x=fct_rev(modalites), 
             label=modalites, color=variable)) +
  geom_point(shape=1) + 
  geom_segment(aes(x=modalites, xend=modalites, 
                    y=0, yend=100*renoncer))+
  coord_flip()+
  theme_minimal()+
  guides(color=guide_none(), shape=guide_none()) +
  labs(x="", y="Renoncer à une conf internationale 
       pour des raisons écolos (raison déterminante)")+ 
  scale_x_discrete(limits=rev(labels))



# Examen de la variable score internationalisation ----

freq(climatRegr$international.poste)
freq(climatRegr$international.naiss)
freq(climatRegr$international.natio)
freq(climatRegr$international.scol)
freq(climatRegr$international.etudes)
freq(climatRegr$international.postdoc)
freq(climatRegr$international.travail)
freq(climatRegr$international.prog)
freq(climatRegr$international.asso)


a <- rbind(
lprop(table(climatRegr$international.poste, climatRegr$vols_dicho_rec))[-3,2],
lprop(table(climatRegr$international.naiss, climatRegr$vols_dicho_rec))[-3,2],
lprop(table(climatRegr$international.natio, climatRegr$vols_dicho_rec))[-3,2],
lprop(table(climatRegr$international.scol, climatRegr$vols_dicho_rec))[-3,2],
lprop(table(climatRegr$international.etudes, climatRegr$vols_dicho_rec))[-3,2],
lprop(table(climatRegr$international.postdoc, climatRegr$vols_dicho_rec))[-3,2],
lprop(table(climatRegr$international.travail, climatRegr$vols_dicho_rec))[-3,2],
lprop(table(climatRegr$international.prog, climatRegr$vols_dicho_rec))[-3,2],
lprop(table(climatRegr$international.asso, climatRegr$vols_dicho_rec))[-3,2]
)
rownames(a) <- c(
  "poste",
  "naiss",
  "natio",
  "scol",
  "etudes",
  "postdoc",
  "travail",
  "prog",
  "asso"
)
# colnames(a) <- paste("pro_", colnames(a), sep="")
a <- as.data.frame(as.table(a))
a$type <- "pro"

b <- rbind(
  lprop(table(climatRegr$international.poste, climatRegr$avionperso_rec))[-3,2],
  lprop(table(climatRegr$international.naiss, climatRegr$avionperso_rec))[-3,2],
  lprop(table(climatRegr$international.natio, climatRegr$avionperso_rec))[-3,2],
  lprop(table(climatRegr$international.scol, climatRegr$avionperso_rec))[-3,2],
  lprop(table(climatRegr$international.etudes, climatRegr$avionperso_rec))[-3,2],
  lprop(table(climatRegr$international.postdoc, climatRegr$avionperso_rec))[-3,2],
  lprop(table(climatRegr$international.travail, climatRegr$avionperso_rec))[-3,2],
  lprop(table(climatRegr$international.prog, climatRegr$avionperso_rec))[-3,2],
  lprop(table(climatRegr$international.asso, climatRegr$avionperso_rec))[-3,2]
)
rownames(b) <- c(
  "poste",
  "naiss",
  "natio",
  "scol",
  "etudes",
  "postdoc",
  "travail",
  "prog",
  "asso"
)
# colnames(b) <- paste("perso_", colnames(b), sep="")
b <- as.data.frame(as.table(b))
b$type <- "perso"

c <- rbind(a,b)
as.data.frame(as.table(c))

ggplot(c, aes(y=Freq, x=Var1, fill=Var2))+
  geom_bar(stat="identity", position=position_dodge(), 
           color="black")+
  facet_grid(~type)+
  coord_flip()+
  scale_fill_grey(start=0, end=1)


# avec les heures de vol ----

freq(climat$volsnbtranch)


a <- rbind(
  lprop(table(climatRegr$international.poste, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$international.naiss, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$international.natio, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$international.scol, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$international.etudes, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$international.postdoc, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$international.travail, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$international.prog, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$international.asso, climatRegr$volsnbtranch))[1:2,-9]
)

rownames(a) <- paste(c(
  "poste","poste",
  "naiss","naiss",
  "natio","natio",
  "scol","scol",
  "etudes","etudes",
  "postdoc","postdoc",
  "travail","travail",
  "prog","prog",
  "asso", "asso"
),rownames(a))

# colnames(a) <- paste("pro_", colnames(a), sep="")
a <- as.data.frame(as.table(a))


ggplot(a, aes(y=Freq, x=Var1, fill=Var2))+
  geom_bar(stat="identity", position=position_fill(), 
           color="black")+
  coord_flip()+
  scale_fill_grey(start=1, end=0)



# Examen des variables du score écolo ----

freq(climatRegr$dixannees.bilan)
freq(climatRegr$dixannees.giec)
freq(climatRegr$dixannees.asso)
freq(climatRegr$dixannees.marche)
freq(climatRegr$dixannees.vote)



a <- rbind(
  lprop(table(climatRegr$dixannees.bilan, climatRegr$vols_dicho_rec))[1:2,2],
  lprop(table(climatRegr$dixannees.giec, climatRegr$vols_dicho_rec))[1:2,2],
  lprop(table(climatRegr$dixannees.asso, climatRegr$vols_dicho_rec))[1:2,2],
  lprop(table(climatRegr$dixannees.marche, climatRegr$vols_dicho_rec))[1:2,2],
  lprop(table(climatRegr$dixannees.vote, climatRegr$vols_dicho_rec))[1:2,2]
)
rownames(a) <- c(
  "bilan",
  "giec",
  "asso",
  "marche",
  "vote"
)
a <- as.data.frame(as.table(a))
a$type <- "pro"

b <- rbind(
  lprop(table(climatRegr$dixannees.bilan, climatRegr$avionperso_rec))[1:2,2],
  lprop(table(climatRegr$dixannees.giec, climatRegr$avionperso_rec))[1:2,2],
  lprop(table(climatRegr$dixannees.asso, climatRegr$avionperso_rec))[1:2,2],
  lprop(table(climatRegr$dixannees.marche, climatRegr$avionperso_rec))[1:2,2],
  lprop(table(climatRegr$dixannees.vote, climatRegr$avionperso_rec))[1:2,2]
)
rownames(b) <- c(
  "bilan",
  "giec",
  "asso",
  "marche",
  "vote"
)
b <- as.data.frame(as.table(b))
b$type <- "perso"

c <- rbind(a,b)


ggplot(c, aes(y=Freq, x=Var1, fill=Var2))+
  geom_bar(stat="identity", position=position_dodge(), 
           color="black")+
  facet_grid(~type)+
  coord_flip()+
  scale_fill_grey(start=0, end=1)


# avec les heures de vol


a <- rbind(
  lprop(table(climatRegr$dixannees.bilan, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$dixannees.giec, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$dixannees.asso, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$dixannees.marche, climatRegr$volsnbtranch))[1:2,-9],
  lprop(table(climatRegr$dixannees.vote, climatRegr$volsnbtranch))[1:2,-9]
)

rownames(a) <- paste(c(
  "bilan", "bilan",
  "giec", "giec",
  "asso", "asso",
  "marche", "marche",
  "vote", "vote"
),rownames(a))

# colnames(a) <- paste("pro_", colnames(a), sep="")
a <- as.data.frame(as.table(a))


ggplot(a, aes(y=Freq, x=Var1, fill=Var2))+
  geom_bar(stat="identity", position=position_fill(), 
           color="black")+
  coord_flip()+
  scale_fill_grey(start=1, end=0)


# variable de synthèse écolo ----

freq(climatRegr$opinionecolo.decroissance)
freq(climatRegr$opinionecolo.cata)
freq(climatRegr$opinionecolo.techno)
freq(climatRegr$opinionecolo.proteger)
freq(climatRegr$opinionecolo.efforts)
freq(climatRegr$opinionecolo.contraintes)
freq(climatRegr$opinionecolo.effondrement)



climatRegr <- mutate(climatRegr,
                     opinionecolo.decroissance_num=as.numeric(
                       fct_relevel(
                         opinionecolo.decroissance, 
                         "Pas du tout d'accord", 
                         "Plutôt pas d'accord", 
                         "Sans opinion", 
                         "Plutôt d'accord",
                         "Tout à fait d'accord")) - 1,
                     opinionecolo.cata_num=as.numeric(
                       fct_relevel(
                         opinionecolo.cata, 
                         "Pas du tout d'accord", 
                         "Plutôt pas d'accord", 
                         "Sans opinion", 
                         "Plutôt d'accord",
                         "Tout à fait d'accord")) - 1,
                     opinionecolo.techno_num=as.numeric(
                       fct_relevel(
                         opinionecolo.techno, 
                         "Pas du tout d'accord", 
                         "Plutôt pas d'accord", 
                         "Sans opinion", 
                         "Plutôt d'accord",
                         "Tout à fait d'accord")) - 1,
                     opinionecolo.proteger_num=as.numeric(
                       fct_relevel(
                         opinionecolo.proteger, 
                         "Pas du tout d'accord", 
                         "Plutôt pas d'accord", 
                         "Sans opinion", 
                         "Plutôt d'accord",
                         "Tout à fait d'accord")) - 1,
                     opinionecolo.efforts_num=as.numeric(
                       fct_relevel(
                         opinionecolo.efforts, 
                         "Pas du tout d'accord", 
                         "Plutôt pas d'accord", 
                         "Sans opinion", 
                         "Plutôt d'accord",
                         "Tout à fait d'accord")) - 1,
                     opinionecolo.contraintes_num=as.numeric(
                       fct_relevel(
                         opinionecolo.contraintes, 
                         "Pas du tout d'accord", 
                         "Plutôt pas d'accord", 
                         "Sans opinion", 
                         "Plutôt d'accord",
                         "Tout à fait d'accord")) - 1,
                     opinionecolo.effondrement_num=as.numeric(
                       fct_relevel(
                         opinionecolo.effondrement, 
                         "Pas du tout d'accord", 
                         "Plutôt pas d'accord", 
                         "Sans opinion", 
                         "Plutôt d'accord",
                         "Tout à fait d'accord")) - 1)

base_acp <- climatRegr[,
                       c(
                         
                         "opinionecolo.decroissance_num",
                         "opinionecolo.cata_num",
                         "opinionecolo.techno_num",
                         "opinionecolo.proteger_num",
                         "opinionecolo.efforts_num",
                         "opinionecolo.contraintes_num",
                         "opinionecolo.effondrement_num"
                       )]


# ACP ----
library(FactoMineR)
acp <- PCA(base_acp)


base_acp <- climatRegr[,
                       c(
                         
                         "opinionecolo.decroissance_num",
                         "opinionecolo.cata_num",
                         "opinionecolo.techno_num",
                         "opinionecolo.proteger_num",
                         "opinionecolo.efforts_num",
                         "opinionecolo.contraintes_num",
                         "opinionecolo.effondrement_num",
                         "preoccupe2num", 
                         "ScoreEcolo"
                       )]

acp <- PCA(base_acp)


base_acp <- climatRegr[,
                       c("opinionecolo.decroissance_num",
                         "opinionecolo.cata_num",
                         "opinionecolo.techno_num",
                         "opinionecolo.proteger_num",
                         "opinionecolo.efforts_num",
                         "opinionecolo.contraintes_num",
                         "opinionecolo.effondrement_num",
                         "preoccupe2num", 
                         "ScoreEcoloPond"
                       )]

acp <- PCA(base_acp)

# avec un score écolo juste avec les trois variables d'implication politique

# On suppose que si au moins une case a été cochée, les autres sont "Non"
climatRegr$ScoreEcolo_reduit<-0
climatRegr$ScoreEcolo_reduit[climatRegr$dixannees.asso=="Oui"& !is.na(climatRegr$dixannees.asso)]<-climatRegr$ScoreEcolo[climatRegr$dixannees.asso=="Oui" & !is.na(climatRegr$dixannees.asso)]+1
climatRegr$ScoreEcolo_reduit[climatRegr$dixannees.marche=="Oui" & !is.na(climatRegr$dixannees.marche)]<-climatRegr$ScoreEcolo[climatRegr$dixannees.marche=="Oui" & !is.na(climatRegr$dixannees.marche)]+1
climatRegr$ScoreEcolo_reduit[climatRegr$dixannees.vote=="Oui" & !is.na(climatRegr$dixannees.vote)]<-climatRegr$ScoreEcolo[climatRegr$dixannees.vote=="Oui" & !is.na(climatRegr$dixannees.vote)]+1
climatRegr$ScoreEcolo_reduit[is.na(climatRegr$dixannees.asso) & is.na(climatRegr$dixannees.marche) & is.na(climatRegr$dixannees.vote)]<-NA


base_acp <- climatRegr[,
                       c("opinionecolo.decroissance_num",
                         "opinionecolo.cata_num",
                         "opinionecolo.techno_num",
                         "opinionecolo.proteger_num",
                         "opinionecolo.efforts_num",
                         "opinionecolo.contraintes_num",
                         "opinionecolo.effondrement_num",
                         "preoccupe2num", 
                         "ScoreEcolo_reduit", 
                         "ScoreEcolo"
                       )]

acp <- PCA(base_acp, quanti.sup = 10)


# En décomposant les dimensions du score écolo

## Recodage de climatRegr$dixannees.marche en climatRegr$dixannees.marche_num
climatRegr$dixannees.marche_num <- climatRegr$dixannees.marche %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()

climatRegr$dixannees.asso_num <- climatRegr$dixannees.asso %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()
climatRegr$dixannees.giec_num <- climatRegr$dixannees.giec %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()
climatRegr$dixannees.vote_num <- climatRegr$dixannees.vote %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()
climatRegr$dixannees.bilan_num <- climatRegr$dixannees.bilan %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()

# acp


base_acp <- climatRegr[,
                       c("opinionecolo.decroissance_num",
                         "opinionecolo.cata_num",
                         "opinionecolo.techno_num",
                         "opinionecolo.proteger_num",
                         "opinionecolo.efforts_num",
                         "opinionecolo.contraintes_num",
                         "opinionecolo.effondrement_num",
                         "preoccupe2num",
                         "dixannees.marche_num", 
                         "dixannees.giec_num", 
                         "dixannees.vote_num", 
                         "dixannees.asso_num", 
                         "dixannees.bilan_num",
                         "ScoreEcolo_reduit", 
                         "ScoreEcolo", 
                         "ScoreEcoloPond" 
                       )]

acp <- PCA(base_acp, quanti.sup = 14:16)


base_acp <- climatRegr[,
                       c("dixannees.marche_num", 
                         "dixannees.giec_num", 
                         "dixannees.vote_num", 
                         "dixannees.asso_num", 
                         "dixannees.bilan_num",
                         "ScoreEcolo_reduit", 
                         "ScoreEcolo", 
                         "ScoreEcoloPond" 
                       )]

acp <- PCA(base_acp, quanti.sup = 6:8)


# ACP distances par motif ----



summary(climatRegr$volsdist_totconf)
summary(climatRegr$volsdist_totterrain)
summary(climatRegr$volsdist_totcours)
summary(climatRegr$volsdist_totsejrech)
summary(climatRegr$volsdist_totjury)
summary(climatRegr$volsdist_totworkshop)
summary(climatRegr$volsdist_toteval)
summary(climatRegr$volsdist_totfinanc)

base_acp <- climatRegr[,c(
  "volsdist_totconf",
  "volsdist_totterrain",
  "volsdist_totcours",
  "volsdist_totsejrech",
  "volsdist_totjury",
  "volsdist_totworkshop",
  "volsdist_toteval",
  "volsdist_totfinanc"
)]
PCA(base_acp)
PCA(base_acp, scale.unit=TRUE)


a <- tapply(climat$volsdist_totterrain, 
            climat$enfagesexe, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$enfagesexe, mean, na.rm=T)
c <- tapply(climat$volsdist_totcours, 
            climat$enfagesexe, mean, na.rm=T)
d <- tapply(climat$volsdist_totsejrech, 
            climat$enfagesexe, mean, na.rm=T)
e <- tapply(climat$volsdist_totjury, 
            climat$enfagesexe, mean, na.rm=T)
f <- tapply(climat$volsdist_totworkshop, 
            climat$enfagesexe, mean, na.rm=T)
g <- tapply(climat$volsdist_toteval, 
            climat$enfagesexe, mean, na.rm=T)
h <- tapply(climat$volsdist_totfinanc, 
            climat$enfagesexe, mean, na.rm=T)

motifs <- cbind(a, b, c, d, e, f, g, h)
colnames(motifs) <- c("confs", "terrain", "cours", "sejourrech", "jury", "workshop",
                      "eval", "financ")
library(FactoMineR)
library(explor)
library(factoextra)

acp <- PCA(motifs, scale.unit=F)


fviz_pca_biplot(acp, repel = T)+ xlim(-2.5, 4) + ylim (-3, 3)
fviz_pca_biplot(acp, repel = T)+ xlim(-2.5, 4) + ylim (-3, 3)

fviz_pca_ind(acp, repel = T)
fviz_pca_biplot(acp, axes = c(1,3), repel = T)+ xlim(-2.5, 4) + ylim (-3, 3)



