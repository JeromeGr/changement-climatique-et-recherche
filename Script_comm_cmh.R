source("recodages.R", encoding = "UTF-8")

library(glmmTMB)
library(tidyverse)
library(gtsummary)
library(GGally)
library(viridis)
library(wesanderson)
library(broom.mixed)


# recodages repris du script multiniveaux (tout n'est pas utile) -----


climatRegr_r <- mutate(climatRegr_r,
                       sexenum=if_else(sexe == "Femme", 1, 0),
                       carrierenum=if_else(carriere == "Oui", 1, 0),
                       nbpublis2=pmin(nbpublis, 100),
                       tourisme=startsWith(as.character(apportconf.tourisme), "Oui"),
                       revenuTete=revenuTete/1000,
                       parbacp5=dippar.p == "Bac +4 ou 5" | dippar.p == "Doctorat" |
                         dippar.m == "Bac +4 ou 5" | dippar.m == "Doctorat",
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
                       paienum=as.numeric(fct_relevel(paie, c("Très bien payé·e", "Bien payé·e", "Correctement payé·e", "Mal payé·e", "Très mal payé·e"))),
                       solreducrechnum=as.numeric(fct_rev(solreducrech)) - 1,
                       projet=Profin_Mb_Resp == "Membre d'au moins 1 projet financé" | Profin_Mb_Resp == "Responsable d'au moins 1 projet financé",
                       projet_fr=particip_ANR | particip_Fr,
                       projet_intern=particip_Europ | particip_Intern)

# climatRegr <- group_by(climatRegr, unite.labintel) %>%
#     filter(n() > 1)
# 
# climatRegr <- group_by(climatRegr, discipline) %>%
#     filter(n() > 1)


# ACP score écolo

climatRegr_r <- mutate(climatRegr_r,
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

climatRegr_r$dixannees.marche_num <- climatRegr_r$dixannees.marche %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()

climatRegr_r$dixannees.asso_num <- climatRegr_r$dixannees.asso %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()
climatRegr_r$dixannees.giec_num <- climatRegr_r$dixannees.giec %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()
climatRegr_r$dixannees.vote_num <- climatRegr_r$dixannees.vote %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()
climatRegr_r$dixannees.bilan_num <- climatRegr_r$dixannees.bilan %>%
  fct_recode(
    "1" = "Oui",
    "0" = "Non",
    NULL = "Je ne souhaite pas répondre"
  ) %>%
  as.character() %>%
  as.numeric()

base_acp <- climatRegr_r[,
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
                           "dixannees.bilan_num"
                         )]

# Ne pas imputer les personnes qui ont répondu à moins de la moitié des questions
manquant_acp <- rowSums(is.na(base_acp)) > 6
base_acp_complete <- missMDA::imputePCA(base_acp[!manquant_acp,], 1)$completeObs
acp <- FactoMineR::PCA(base_acp_complete)
climatRegr_r$ScoreEcoloACP <- NA
climatRegr_r$ScoreEcoloACP[!manquant_acp] <- acp$ind$coord[,1]

# Liste des variables à centrer par labo/discipline
vars_centrer <- c("nbpublis2", "ScoreEcolo", "ScoreEcoloACP",
                  "ScoreInternational", "ScoreInternational_perso", "ScoreInternational_pro", "tourisme",
                  "avionpersonum", "chgtpratiquenum", "preoccupe2num", "solreducrechnum",
                  "projet", "projet_fr", "projet_intern",
                  "revenuTete", "paienum", "parbacp5")

# Liste des variables à moyenner par labo/discipline
vars <- c(vars_centrer, "sexenum", "carrierenum")

# Standardiser les variables individuelles pour que leur écart-type soit égal à 1
climatRegr_r[vars_centrer] <- scale(climatRegr_r[vars_centrer])

# Création des moyennes par labo (_labo) et des variables centrées par labo (_clabo)
climatRegr_r <- group_by(climatRegr_r, unite.labintel) %>%
  mutate(across(all_of(vars), ~ mean(.x, na.rm=TRUE), .names="{.col}_labo"),
         across(all_of(vars), ~ .x - mean(.x, na.rm=TRUE), .names="{.col}_clabo")) %>%
  ungroup()

# Création des moyennes par discipline (_discipline) et des variables centrées par discipline (_cdisc)
climatRegr_r <- group_by(climatRegr_r, discipline) %>%
  mutate(across(all_of(vars), ~ mean(.x, na.rm=TRUE), .names="{.col}_disc"),
         across(all_of(vars), ~ .x - mean(.x, na.rm=TRUE), .names="{.col}_cdisc")) %>%
  ungroup()

# Création des variables centrées par labo et discipline (_c)
# On rajoute la moyenne de la variable d'origine
# pour que leur moyenne soit à 0 alors qu'on a soustrait deux fois la moyenne
climatRegr_r[paste0(vars, "_c")] <-
  select(climatRegr_r, all_of(vars)) -
  select(climatRegr_r, all_of(paste0(vars, "_labo"))) -
  select(climatRegr_r, all_of(paste0(vars, "_disc"))) +
  transmute(climatRegr_r, across(all_of(vars), ~ mean(.x, na.rm=TRUE)))

# Standardiser les variables au niveau labo et discipline
# pour que l'écart-type des moyennes par labo/discipline soit égal à 1
sd_labo <- select(climatRegr_r, unite.labintel, all_of(paste0(vars, "_labo"))) %>%
  group_by(unite.labintel)  %>%
  summarize(across(everything(), ~ mean(.x, na.rm=TRUE))) %>%
  select(-unite.labintel) %>%
  apply(2, sd, na.rm=T)
climatRegr_r[names(sd_labo)] <- sweep(climatRegr_r[names(sd_labo)], 2, sd_labo, "/")

sd_disc <- select(climatRegr_r, discipline, all_of(paste0(vars, "_disc"))) %>%
  group_by(discipline)  %>%
  summarize(across(everything(), ~ mean(.x, na.rm=TRUE))) %>%
  select(-discipline) %>%
  apply(2, sd, na.rm=T)
climatRegr_r[names(sd_disc)] <- sweep(climatRegr_r[names(sd_disc)], 2, sd_disc, "/")

# Standardiser les variables individuelles centrées pour que leur écart-type soit égal à 1
climatRegr_r[c(paste0(vars, "_c"), paste0(vars, "_clabo"), paste0(vars, "_cdisc"))] <-
  scale(climatRegr_r[c(paste0(vars, "_c"), paste0(vars, "_clabo"), paste0(vars, "_cdisc"))])



# Recodages avion pro et perso ----
## Recodage de climatRegr$vols_dicho en climatRegr$vols_dicho_rec
climatRegr_r$vols_dicho_rec <- climatRegr_r$vols_dicho %>%
  fct_recode(
    "Pro_Non" = "N'a pas volé en 2019",
    "Pro_Oui" = "A volé en 2019"
  )

## Recodage de climatRegr$avionperso en climatRegr$avionperso_rec
climatRegr_r$avionperso_rec <- climatRegr_r$avionperso %>%
  fct_recode(
    "Perso_Non" = "Aucun aller-retour",
    "Perso_Oui" = "1 ou 2 allers-retours",
    "Perso_Oui" = "3 ou 4 allers-retours",
    "Perso_Oui" = "Plus de 5 allers-retours"
  )


# Statistiques descriptives vols par métropole ----


a <- tapply(climatRegr_r$volsnb, climatRegr_r$trav.metropole, mean, na.rm=T)
b <- tapply(climatRegr_r$volshnum, climatRegr_r$trav.metropole, mean, na.rm=T)
c <- table(climatRegr_r$trav.metropole)

table <- cbind(a,b,c)
table <- as.data.frame(table)
ggplot(table, aes(x=a, y=b, label=rownames(table), size=c))+
  geom_point()+geom_text_repel(max.overlaps = Inf)+
  xlim(0,4)+ylim(0,16)+
  labs(x="Nombre de vols (moyenne)", 
       y="Heures de vol (moyenne)", 
       size="Effectif par métropole")+
  theme_minimal()


a <- tapply(climatRegr_r$volsnb, climatRegr_r$trav_zone_dom, mean, na.rm=T)
b <- tapply(climatRegr_r$volshnum, climatRegr_r$trav_zone_dom, mean, na.rm=T)
c <- table(climatRegr_r$trav_zone_dom)

table <- cbind(a,b,c)
table <- as.data.frame(table)
ggplot(table, aes(x=a, y=b, label=rownames(table), size=c))+
  geom_point()+geom_text_repel(max.overlaps = Inf)+
  xlim(0,4)+ylim(0,16)+
  labs(x="Nombre de vols (moyenne)", 
       y="Heures de vol (moyenne)", 
       size="Effectif par zone")+
  theme_minimal()


# Modèle simple avec métropole de travail et programmes de recherche ----

# heures de vol
reg0 <- glmmTMB(volshnum ~ sexe + ageAgr + ageaccad_tranch2 +
                  sitpro2  + discipline_agr5 + recheco + 
                  carriere + nbpublistranch2 +  
                  projets.anr_m2 + projets.anr_r2 + 
                  projets.france_m2 + projets.france_r2 + 
                  projets.europe_m2 + 
                  projets.europe_r2 + projets.inter_m2 + 
                  projets.inter_r2 +projets.prive_m2 + 
                  projets.prive_r2 + 
                  # ScoreInternational_pro+
                  trav.metropole,
                family="tweedie",
                data=climatRegr_r,
                control=glmmTMBControl(parallel=4))

#nombre de vols

library(labelled)



# représentation du modèle ----
var_label(climatRegr_r$sexe) <- "Sexe"
var_label(climatRegr_r$nbpublistranch2) <- "Nombre de publications"
var_label(climatRegr_r$carriere) <- "Moment clé de la carrière"
var_label(climatRegr_r$projets.anr_m2) <- "Membre projet ANR"
  var_label(climatRegr_r$projets.anr_r2) <- "Responsable projet ANR"
  var_label(climatRegr_r$projets.france_m2) <- "Membre projet France"
  var_label(climatRegr_r$projets.france_r2) <- "Responsable projet France"
  var_label(climatRegr_r$projets.europe_m2) <- "Membre projet européen"
  var_label(climatRegr_r$projets.europe_r2) <- "Responsable projet européen"
  var_label(climatRegr_r$projets.inter_m2) <- "Membre projet international"
  var_label(climatRegr_r$projets.inter_r2) <- "Responsable projet international"
  var_label(climatRegr_r$projets.prive_m2) <- "Membre projet privé"
  var_label(climatRegr_r$projets.prive_r2) <- "Reponsable projet privé"

climatRegr_r$carriere_tit <- climatRegr_r$carriere_tit %>%
  fct_relevel(
    "Non", "Oui"
  )

climatRegr_r <-  climatRegr_r %>% set_variable_labels(
  sexe = "Sexe",
  nbpublistranch2 = "Nombre de publications",
  carriere = "Moment clé de la carrière",
  projets.anr_m2 = "Membre projet ANR",
  projets.anr_r2 = "Responsable projet ANR",
  projets.france_m2 = "Membre projet France",
  projets.france_r2 = "Responsable projet France",
  projets.europe_m2 = "Membre projet européen",
  projets.europe_r2 = "Responsable projet européen",
  projets.inter_m2 = "Membre projet international",
  projets.inter_r2 = "Responsable projet international",
  projets.prive_m2 = "Membre projet privé",
  projets.prive_r2 = "Reponsable projet privé", 
  ScoreEcoloACP="Sensibilité écologique", 
  carriere_tit= "Moment clé de la carrière"
)
# 


reg1 <- glmmTMB(volsnb ~ sexe + ageAgr + ageaccad_tranch2 +
                  sitpro2  + discipline_agr5 + 
                  carriere_tit+ 
                  nbpublistranch2 +  
                  projets.anr_m2 + projets.anr_r2 + 
                  projets.france_m2 + projets.france_r2 + 
                  projets.europe_m2 + 
                  projets.europe_r2 + projets.inter_m2 + 
                  projets.inter_r2 +projets.prive_m2 + 
                  projets.prive_r2 +
                  ScoreEcoloACP+
                  # ScoreInternational_pro+
                  trav.metropole,
                family="tweedie",
                data=climatRegr_r,
                control=glmmTMBControl(parallel=4))

# ggcoef_model(reg1, exponentiate = T, include=
#                c("sexe", 
#                  "sitpro2",
#                  "discipline_agr5"))
ggcoef_model(reg1, exponentiate = T, include=
               c("carriere_tit", 
                 "ScoreEcoloACP",
                 "nbpublistranch2",
                 "projets.anr_m2",
                 "projets.anr_r2" , 
                 "projets.france_m2",
                 "projets.france_r2" , 
                 "projets.europe_m2",
                 "projets.europe_r2",
                 "projets.inter_m2" , 
                 "projets.inter_r2",
                 "projets.prive_m2" , 
                 "projets.prive_r2"), 
             show_p_values = F, 
             return_data = F)
library(broom.helpers)
?ggcoef_model
summary(reg1)
exp(-0.14059 )



# graphique nuage de points


lapply(list(group_by(climat, discipline_agr5),
            group_by(climat, sitpro_reduite),
            group_by(climat, ageAgr2),
            group_by(climat, sexe)),
       function(df) {
         variable <- colnames(df)[[group_cols(data=df)]]
         res <- summarize(df, preoccupe=mean(preoccupe2 == "Extrêmement préoccupé·e" | preoccupe2 == "Très préoccupé·e", na.rm=TRUE),
                          heures_vol=mean(volshnum, na.rm=TRUE), variable=variable)
         rename(res, modalites=1)
       }) %>%
  bind_rows() %>%
  drop_na(modalites) %>%
  ggplot(aes(preoccupe, heures_vol, 
             label=modalites, 
             color=variable, 
             shape=variable)) +
  geom_point(size=2) +
  geom_text_repel(size=4) + #min.segment.length=0
  scale_x_continuous(labels=scales::percent) +
  # scale_y_continuous(labels=scales::percent) +
  scale_color_paletteer_d("ggsci::category10_d3") +
  guides(color=guide_none(), shape=guide_none()) +
  labs(x="Très ou extrêmement préoccupé·e",
       y="Nombre d'heures de vol") +
  theme_minimal()

climatRegr_r$solreducperso.conf
lapply(list(group_by(climat, discipline_agr5),
            group_by(climat, sitpro_reduite)#,
            # group_by(climat, ageAgr2),
            # group_by(climat, sexe)
            ),
       function(df) {
         variable <- colnames(df)[[group_cols(data=df)]]
         res <- summarize(df, preoccupe=mean(preoccupe2 == "Extrêmement préoccupé·e" | preoccupe2 == "Très préoccupé·e", na.rm=TRUE),
                          reduire_vols=mean(solreducperso.conf == "Oui, d'au moins un tiers" , na.rm=TRUE), variable=variable)
         rename(res, modalites=1)
       }) %>%
  bind_rows() %>%
  drop_na(modalites) %>%
  ggplot(aes(y=preoccupe, x=reduire_vols, 
             label=modalites, 
             color=variable, 
             shape=variable)) +
  geom_point(size=2) +
  geom_text_repel(size=4, max.overlaps = Inf) + #min.segment.length=0
  scale_x_continuous(labels=scales::percent, limits = c(0.3,0.6)) +
  scale_y_continuous(labels=scales::percent, limits = c(0.6,NA)) +
  scale_color_paletteer_d("ggthemes::calc") +
  guides(color=guide_none(), shape=guide_none()) +
  labs(y="Très ou extrêmement préoccupé·e",
       x="Réduire ses vols d'au moins un tiers") +
  theme_minimal()


# regressions heure et nombre de vols en un modèle

a <- tidy(reg0, conf.int = T, exponentiate=T, intercept=F)
b <- tidy(reg1, conf.int = T, exponentiate=T, intercept=F)
a$modele <- "heures"
b$modele <- "nombre"

a$term
a <- a[!startsWith(a$term, "ageaccad_tranch2"),]
a <- a[!startsWith(a$term, "ageAgr"),]
a <- a[!startsWith(a$term, "nbpublistranch2"),]
a <- a[!startsWith(a$term, "(Intercept)"),]
a <- a[!startsWith(a$term, "trav.metropole"),]

b <- b[!startsWith(b$term, "ageaccad_tranch2"),]
b <- b[!startsWith(b$term, "ageAgr"),]
b <- b[!startsWith(b$term, "nbpublistranch2"),]
b <- b[!startsWith(b$term, "(Intercept)"),]
b <- b[!startsWith(b$term, "trav.metropole"),]


table <- rbind(a,b)

table <- as.data.frame(table)
colnames(table)
table$shape <- ifelse(table$p.value<=0.1, "", "non")
library(wesanderson)
ggplot(table, aes(x=estimate, y=term, group=modele, 
                  color=modele, 
                  shape=group)) +
  geom_pointrange(
    aes(xmin =  conf.low, xmax = conf.high, 
        color = modele,
        shape=paste(modele, shape)),
    position = position_dodge(0.4)
  )+geom_vline(xintercept =1)+
  guides(color = guide_legend(reverse = TRUE),shape=F)+
  theme_minimal()+
  scale_color_manual(values=wes_palette(n=2, name="GrandBudapest1"))+
  labs(x="", y="", color="Modèle")+
  scale_shape_manual(values=c(1, 16, 2, 17 ))


?tbl_cross
climatRegr_r %>%
  tbl_cross(row = "sitpro", col = "vols_dicho", percent = "row", missing = "no")

# corrélation des raisons de n'avoir pas pris le train ----

freq(climatRegr_r$aviontrainraisons)
table(climatRegr_r$aviontrainraisons_num.cher,
      climatRegr_r$aviontrainraisons.cher)

climatRegr_r <- mutate(climatRegr_r,
                       aviontrainraisons_num.admin=
                         recode(aviontrainraisons.admin,
                                "Non"=0,
                                "Oui"=1),
                       aviontrainraisons_num.rapide=
                         recode(aviontrainraisons.rapide,
                                "Non"=0,
                                "Oui"=1),
                       aviontrainraisons_num.cher=
                         recode(aviontrainraisons.cher,
                                "Non"=0,
                                "Oui"=1),
                       aviontrainraisons_num.pratique=
                         recode(aviontrainraisons.pratique,
                                "Non"=0,
                                "Oui"=1),
                       aviontrainraisons_num.aime=
                         recode(aviontrainraisons.aime,
                                "Non"=0,
                                "Oui"=1),
                       aviontrainraisons_num.nuit=
                         recode(aviontrainraisons.nuit,
                                "Non"=0,
                                "Oui"=1),
                       aviontrainraisons_num.miles=
                         recode(aviontrainraisons.miles,
                                "Non"=0,
                                "Oui"=1),
                       aviontrainraisons_num.paspose=
                         recode(aviontrainraisons.paspose,
                                "Non"=0,
                                "Oui"=1))
                                           

str(climat$trainavion)

raisons <- climatRegr_r %>%
  filter(trainavion %in% c("Oui, plusieurs fois", "Oui, une fois"))%>%
  select(starts_with("aviontrainraisons_num.")) 
raisons[is.na(raisons)] <- 0
mcor <- cor(raisons)
mcor

library(corrplot)
corrplot(mcor, type="upper", tl.col="black", tl.srt=45)




# Disciplines : % de femmes et vols ----

a <- climatRegr_r %>% group_by(discipline_agr5) %>% 
  summarise(moyenne_volsh = mean(volshnum, na.rm = TRUE),
            moyenne_score_inter = mean(ScoreInternational_pro, na.rm = TRUE),
            tx_femmes = 100*mean(sexe=="Femme", na.rm=T)) %>% 
  select(discipline_agr5, moyenne_volsh, tx_femmes,moyenne_score_inter)
ggplot(a[c(1:16),], 
       aes(x=tx_femmes, y=moyenne_volsh, label=discipline_agr5, 
           color=moyenne_score_inter))+
  geom_point(size=3)+geom_text_repel(max.overlaps = Inf)+
  labs(x="% de femmes", 
       y="Heures de vols (moyenne)", 
       color="Score international", 
       size="Score international")+
  theme_minimal()+xlim(0,100)+ylim(0,22)+
  scale_color_viridis(option="inferno", begin=0.3, end=0.8) 

# Modèles ----



# Programme de Damien pour faire les slides de la présentation au CMH du 17 fev 23


# préambules
library(dplyr)
library(tidyverse)
library(ggplot2)
# tinytex::install_tinytex() 
library(tinytex)# pour le rmarkdown en PDF (il faut latec)
library(broom)
library(broom.helpers)
library(gtsummary)
library(GGally)
library(questionr)


# lancement de la base 
load("climat.RData")
source("recodages.R", encoding = "UTF-8")

#climatRegr_r$sitproD`Doctorant·e` <- climatRegr_r == c("ATER", "Doctorant·e contractuel·le", "Doctorant·e CIFRE")

climat$sitpro <- factor(climat$sitpro,
                        levels = c(
                          "Directeur·rice de recherche", "Professeur·e des universités",
                          "Chargé·e de recherche", "Maître·sse de conférences",
                          "Ingénieur·e de recherche", "Post-doctorant·e",
                          "ATER", "Doctorant·e contractuel·le", "Doctorant·e CIFRE",
                          "Ingénieur·e d'études", "Chargé·e d'études/de mission",
                          "Assistant ingénieur·e", "Technicien·ne",
                          "Adjoint·e technique", "Autre"
                        ))

climatRegr_r$sitproD <- as.character(climatRegr_r$sitpro)

climatRegr_r$sitproD[climatRegr_r$sitproD %in% c("ATER", "Doctorant·e contractuel·le", "Doctorant·e CIFRE")] <- "Doctorant·e"
#climatRegr_r$sitproD == "ATER" | climatRegr_r$sitproD == "Doctorant·e contractuel·le" | climatRegr_r$sitproD == "Doctorant·e CIFRE" <- "Doctorant·e"

climatRegr_r$sitproD <- factor(climatRegr_r$sitproD,
                               levels = c(
                                 "Directeur·rice de recherche", "Professeur·e des universités",
                                 "Chargé·e de recherche", "Maître·sse de conférences",
                                 "Ingénieur·e de recherche", "Post-doctorant·e",
                                 "Doctorant·e"
                               ))


# table(climatRegr_r$sitproD)

# Croisement sitpro * vols(O/N)--------- 
climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "vols_dicho", percent = "row", missing = "no",
            label = list(sitproD ~ "Statut Professionnel", vols_dicho ~"Avoir volé en 2019"))
# test pour Julien 
climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "vols_dicho", percent = "row", missing = "no", statistic = "{p}%", margin = "column")


climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "vols_dicho", percent = "row", missing = "no", statistic = "{p}%")


# Croisement sitpro * Nb heures ----------
climatRegr_r$volshcat <- with(climatRegr_r,
                              case_when(volsnb == 0 ~ "0h",
                                        volsh == "De 1h à 10h" ~ "De 1h à 10h",
                                        volsh == "De 11h à 20h" ~ "De 11h à 20h",
                                        volsh == "De 20h à 50h" ~ "De 20h à 50h",
                                        volsh == "Plus de 50h" ~ "Plus de 50h"))
climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "volshcat", percent = "row", missing = "no")

climatRegr_r$volshcatr <- with(climatRegr_r,
                               case_when(volsnb == 0 ~ "Pas volé",
                                         volsh == "De 1h à 10h" ~ "De 1h à 20h",
                                         volsh == "De 11h à 20h" ~ "De 1h à 20h",
                                         volsh == "De 20h à 50h" ~ "Plus de 20h",
                                         volsh == "Plus de 50h" ~ "Plus de 20h"))

climatRegr_r$volshcatr <- factor(climatRegr_r$volshcatr,
                                 levels = c("Pas volé", "De 1h à 20h","Plus de 20h"))


climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "volshcatr", percent = "row", missing = "no", 
            label = list(sitproD ~ "Statut Professionnel", volshcatr ~"Nombre d'heures de vol"))

# vols sur 3 ans -------
table(climatRegr_r$vols_dicho3ans)

climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "vols_dicho3ans", percent = "row", missing = "no", 
            label = list(sitproD ~ "Statut Professionnel", vols_dicho3ans ~"Avoir volé au cours des 3 derniers années"))

# nombre de vols -----
climatRegr_r$volsnbtranch3 <- cut(climatRegr_r$volsnb,
                                  include.lowest = TRUE,
                                  right = TRUE,
                                  breaks = c(0, 0.4, 2, 4, 65)
)
table(climatRegr_r$volsnbtranch3)

climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "volsnbtranch3", percent = "row", missing = "no", 
            label = list(sitproD ~ "Statut Professionnel", volsnbtranch3 ~"Nombre d'Aller simples en avion"))

climatRegr_r$volsnbtranch3 <- as.character(climatRegr_r$volsnbtranch3)
climatRegr_r$volsnbtranch3 <- with(climatRegr_r,
                                   case_when(volsnbtranch3 == "[0,0.4]" ~ "Pas volé",
                                             volsnbtranch3 == "(0.4,2]" ~ "1 à 2 vols",
                                             volsnbtranch3 == "(2,4]" ~ "3 à 4 vols ",
                                             volsnbtranch3 == "(4,65]" ~ "Plus de 4 vols"
                                   ))
climatRegr_r$volsnbtranch3 <- factor(climatRegr_r$volsnbtranch3,
                                     levels = c("Pas volé", "1 à 2 vols","3 à 4 vols ", "Plus de 4 vols")) #pour trier les réponses

climatRegr_r %>%
  tbl_cross(row = "sitproD", col = "volsnbtranch3", percent = "row", missing = "no", 
            label = list(sitproD ~ "Statut Professionnel", volsnbtranch3 ~"Nombre d'Aller simples en avion"))

#### variables à regarder 







####Création de variables à partir des données du module sur l'avion LIGNE 846



# TO DO : le même tableau mais avec un nbvol en catégoriel + le même tableau mais avec heures de vols en catégoriel 



#### tests  
