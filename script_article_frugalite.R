source("recodages.R")

library(ggrepel)
library(paletteer)



### Section 1

# Figure 1 : Degré de préoccupation concernant le changement climatique
# et volonté de réduire les émissions de GES de la recherche de plus d’un tiers d’ici à 2030,
# par discipline, statut, âge et sexe

lapply(list(group_by(climat, discipline_agr5),
            group_by(climat, sitpro_reduite),
            group_by(climat, ageAgr2),
            group_by(climat, sexe)),
       function(df) {
           variable <- colnames(df)[[group_cols(data=df)]]
           res <- summarize(df, preoccupe=mean(preoccupe2 == "Extrêmement préoccupé·e" | preoccupe2 == "Très préoccupé·e", na.rm=TRUE),
                            reduire=mean(solreducrech3 == "Réduire de plus d'un tiers", na.rm=TRUE), variable=variable)
           rename(res, modalites=1)
       }) %>%
    bind_rows() %>%
    drop_na(modalites) %>%
    ggplot(aes(preoccupe, reduire, label=modalites, color=variable, shape=variable)) +
    geom_point(size=2) +
    geom_text_repel(size=4) + #min.segment.length=0
    scale_x_continuous(labels=scales::percent) +
    scale_y_continuous(labels=scales::percent) +
    scale_color_paletteer_d("ggsci::category10_d3") +
    guides(color=guide_none(), shape=guide_none()) +
    labs(x="Très ou extrêmement préoccupé·e",
         y="Réduire les émissions de la recherche de plus d'un tiers") +
    theme_minimal()

# Tableau 1 : Volonté de réduire les émissions de GES de la recherche,
# opinions concernant l’écologie et la technologie,
# et utilisation de matériel lourd (en %)
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



### Section 2

## 2.1

# Tableau 2 : Volonté de réduire ses émissions individuelles de GES de plus d’un tiers (en %)
lapply(list(group_by(climat, discipline_agr5),
            group_by(climat, sitpro_reduite),
            group_by(climat, ageAgr2),
            group_by(climat, sexe)),
       function(df) {
           variable <- colnames(df)[[group_cols(data=df)]]
           res <- summarize(df,
                            total=mean(na_if(solreducrech3, "Non concerné·e") == "Réduire de plus d'un tiers", na.rm=TRUE) * 100,
                            conf=mean(na_if(solreducperso.conf, "Non concerné·e") == "Oui, d'au moins un tiers", na.rm=TRUE) * 100,
                            donnees=mean(na_if(solreducperso.donnees, "Non concerné·e") == "Oui, d'au moins un tiers", na.rm=TRUE) * 100,
                            exp=mean(na_if(solreducperso.exp, "Non concerné·e") == "Oui, d'au moins un tiers", na.rm=TRUE) * 100,
                            info=mean(na_if(solreducperso.info, "Non concerné·e") == "Oui, d'au moins un tiers", na.rm=TRUE) * 100,
                            effectifs=sum(!is.na(solreducperso.conf)))
           rename(res, modalites=1)
       }) %>%
    bind_rows() %>%
    drop_na(modalites) %>%
    mutate(across(where(is.numeric), ~ round(.x))) %>%
    as.matrix() %>%
    knitr::kable()

# Tableau avec la même structure que le précédent mais pour les différents types de matériel
# (utilisé seulement en note de bas de page)
lapply(list(group_by(climat, discipline_agr5),
            group_by(climat, sitpro_reduite),
            group_by(climat, ageAgr2),
            group_by(climat, sexe)),
       function(df) {
           variable <- colnames(df)[[group_cols(data=df)]]
           res <- summarize(df,
                            lowtech=mean(na_if(solreducmateriel.lowtech, "Non concerné·e") == "Oui, d'au moins un tiers", na.rm=TRUE) * 100,
                            moins=mean(na_if(solreducmateriel.moins, "Non concerné·e") == "Oui, d'au moins un tiers", na.rm=TRUE) * 100,
                            renouv=mean(na_if(solreducmateriel.renouv, "Non concerné·e") == "Oui, d'au moins un tiers", na.rm=TRUE) * 100,
                            util=mean(na_if(solreducmateriel.util, "Non concerné·e") == "Oui, d'au moins un tiers", na.rm=TRUE) * 100,
                            effectifs=sum(!is.na(solreducmateriel.lowtech)))
           rename(res, modalites=1)
       }) %>%
    bind_rows() %>%
    drop_na(modalites) %>%
    mutate(across(where(is.numeric), ~ round(.x))) %>%
    as.matrix() %>%
    knitr::kable()


a <- t(cbind(
  freq(climat$solreducperso.conf[!climat$solreducperso.conf %in% c("Sans opinion", "Non concerné·e")])[-c(6:7),],
  freq(climat$solreducperso.donnees[!climat$solreducperso.donnees %in% c("Sans opinion", "Non concerné·e")])[-c(6:7),3],
  freq(climat$solreducperso.exp[!climat$solreducperso.exp %in% c("Sans opinion", "Non concerné·e")])[-c(6:7),3],
  freq(climat$solreducperso.info[!climat$solreducperso.info %in% c("Sans opinion", "Non concerné·e")])[-c(6:7),3]
)[,-c(1:2)])

rownames(a) <- c("Vols pour les conférences",
                 "Vols pour le recueil des données",
                 "Matériel expériences et observations",
                 "Matériel informatique")
a


## 2.2

with(filter(climat, materiel),
     cprop(table(solreducperso.exp, materiel.couteux == "Oui" | materiel.trescouteux == "Oui" | materiel.tgir == "Oui" | materiel.extensif == "Oui",
                 exclude=c(NA, "Non concerné·e"))))

with(filter(climat, volsdist_totconfreu >= 7000),
     cprop(table(preoccupe2, solreducperso.conf,
                 exclude=c(NA, "Non concerné·e"))))

with(filter(climat, volsdist_totconfreu >= 7000),
     cprop(table(preoccupe2, startsWith(as.character(solreducperso.conf), "Oui"),
                 exclude=c(NA, "Non concerné·e"))))

with(filter(climat, materiel.trescouteux == "Oui" | materiel.tgir == "Oui" | materiel.extensif == "Oui"),
     cprop(table(preoccupe2, solreducperso.exp, 
                 exclude=c(NA, "Non concerné·e"))))

cprop(table(climat$solrisqreducavion.qual,
            cut(climat$volsdist_tot, c(0, 1, 7000, Inf), right=FALSE, dig.lab=5)))

with(filter(climat, materiel),
     cprop(table(solrisqreducmateriel.qual,
                 materiel.couteux == "Oui" | materiel.trescouteux == "Oui" | materiel.tgir == "Oui" | materiel.extensif == "Oui")))


# Informatique
cprop(table(climat$solreducperso.info,
            cut(climat$ordis.nbtotal, c(1, 2, 3, 4, 10, Inf), right=FALSE),
            exclude=c(NA, "Non concerné·e")))
cprop(table(climat$solreducperso.info,
            cut(climat$ordis.indisptotal, c(1, 2, 3, 4, 10, Inf), right=FALSE),
            exclude=c(NA, "Non concerné·e")))

cprop(table(climat$solreducperso.info,
            cut(climat$ordis.nbtotal, c(1, 2, 3, 4, 10, Inf), right=FALSE),
            climat$ordis.indisptotal < climat$ordis.nbtotal,
            exclude=c(NA, "Non concerné·e"))[,,"FALSE"])
cprop(table(climat$solreducperso.info,
            cut(climat$ordis.nbtotal, c(1, 2, 3, 4, 10, Inf), right=FALSE),
            climat$ordis.indisptotal < climat$ordis.nbtotal,
            exclude=c(NA, "Non concerné·e"))[,,"TRUE"])


## 2.3

# Figure 2 : Taux de soutien (« prioritaire » ou « secondaire »)
# aux deux solutions institutionnelles les plus clivantes
# par discipline, statut, âge et sexe
lapply(list(group_by(climat, discipline_agr5),
            group_by(climat, sitpro_reduite),
            group_by(climat, ageAgr2),
            group_by(climat, sexe)),
       function(df) {
           variable <- colnames(df)[[group_cols(data=df)]]
           res <- summarize(df, vols=mean(solinstit.limitevols == "C'est prioritaire" | solinstit.limitevols == "C'est secondaire", na.rm=TRUE),
                            selection=mean(solinstit.selection == "C'est prioritaire" | solinstit.selection == "C'est secondaire", na.rm=TRUE),
                            variable=variable)
           rename(res, modalites=1)
       }) %>%
    bind_rows() %>%
    drop_na(modalites) %>%
    ggplot(aes(vols, selection, label=modalites, color=variable, shape=variable)) +
    geom_point(size=2) +
    geom_text_repel(size=4) + #min.segment.length=0
    scale_x_continuous(labels=scales::percent) +
    scale_y_continuous(labels=scales::percent) +
    scale_color_paletteer_d("ggsci::category10_d3") +
    guides(color=guide_none(), shape=guide_none()) +
    labs(x="Limite au nombre de vols en avion par personne",
         y="Émissions carbone parmi les principaux critères de sélection") +
    theme_minimal()
