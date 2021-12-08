library(tidyverse)
library(questionr)

ech <- read.csv("../Labintel/échantillon.csv")

erreurs1 <- read.table("../Labintel/Erreurs enquete.labos1point5@services.cnrs.fr 06-07-20.txt")
erreurs2 <- read.table("../Labintel/Erreurs questionnaire.labos1point5@services.cnrs.fr 06-07-20.txt")

climat <- read.csv("~/Private/results-survey113464_021220.csv", fileEncoding="UTF-8", na.strings="")


# Quelques adresses en doublon (que Sympa ne garde qu'une fois)
ech <- filter(ech, !duplicated(courriel))

# Sympa met les adresses en minuscules
stopifnot(all(c(erreurs1$V1, erreurs2$V1) %in% tolower(ech$courriel)))
ech <- filter(ech, !tolower(courriel) %in% c(erreurs1$V1, erreurs2$V1))

ech <- left_join(ech, climat, by=c("courriel"="email"))
stopifnot(all(climat$courriel %in% ech$email))

# Taux d'ouvertures du questionnaire
freq(!is.na(ech$lastpage))
# Equivalent à :
# freq(ech$courriel %in% climat$email)

# Taux de réponses complètes et partielles (validation de la page 1 au moins)
freq(!is.na(ech$lastpage) & ech$lastpage >= 1)

# Taux de réponses complètes
# La seconde condition permet de tenir compte
# des personnes hors-champ qui ont commencé à répondre
freq((!is.na(ech$lastpage) & ech$lastpage == 8) |
         (!is.na(ech$rechpub) & ech$rechpub == "Non"))

# Taux de réponse à la première page par statut et institut
lprop(table(ech$type,
            !is.na(ech$lastpage) & ech$lastpage >= 1))

ech$institut2 <- sapply(ech$institut,
                        function(x) strsplit(x, ",", fixed=TRUE)[[1]][[1]])
ech$institut2[substr(ech$institut2, 1, 3) == "DGD"] <- "Présidence et Direction générale"
ech$institut2[substr(ech$institut2, 1, 3) == "PDT"] <- "Présidence et Direction générale"
ech$institut2 <- fct_reorder(factor(ech$institut2),
                             !(!is.na(ech$lastpage) & ech$lastpage >= 1),
                             .fun=mean)

lprop(table(ech$institut2,
            !is.na(ech$lastpage) & ech$lastpage >= 1))

lprop(table(ech$delegation,
            !is.na(ech$lastpage) & ech$lastpage >= 1))

lprop(table(ech$type,
            !is.na(ech$lastpage) & ech$lastpage >= 1))

lprop(table(ech$sexe.x,
            !is.na(ech$lastpage) & ech$lastpage >= 1))

ech$type <- relevel(factor(ech$type), "Chercheur")
ech$sexe.x <- relevel(factor(ech$sexe.x), "Homme")

ech$type2 <- fct_recode(ech$type, "Autre personnel"="Chercheur",
                        "Autre personnel"="Enseignant-chercheur",
                        "Autre personnel"="Ingénieur",
                        "Technicien"="Technicien/administratif")
ech$type2 <- factor(ech$type2, levels=c("Directeur de recherche", "Professeur", "Chargé de recherche",
                                        "Maître de conférences", "Post-doctorant",
                                        "Doctorant", "Ingénieur de recherche",
                                        "Ingénieur d'études", "Assistant ingénieur", "Technicien",
                                        "Temporaire sur CDD", "Autre personnel"))
ech$type2[ech$categorie %in% c("DR2, D.R.T.2.C.",
                               "DR1, D.R.T.1.C. (&eacute;m&eacute;ritat)", "DET, Directeur d'études", 
                               "DR1, D.R.T.1.C.",
                               "DR2, D.R.T.2.C. (&eacute;m&eacute;ritat)", "PHYS1, Physicien de 1ère classe", 
                               "DRCE, D.R.T.C.E. (&eacute;m&eacute;ritat)", 
                               "DAD1, Dir.rec.ass.1C",
                               "DRCE, D.R.T.C.E.", "DAD2, Dir.rec.ass.2C",
                               "DADR, Dir.rec.ass.CE", "AST2, Astronome de 2 classe", "DRC, D.R.C.", 
                               "DE2, Directeur d'études de 2 classe",
                               "AST1, Astronome de 1 classe", "PHYS2, Physicien de 2ème classe", 
                               "CL, Chef de laboratoire", "DREM, Directeur de recherche émérite (&eacute;m&eacute;ritat)", 
                               "PHYSEM, Physicien émérite (&eacute;m&eacute;ritat)", 
                               "DECE, Directeur d'études de classe exceptionnelle", 
                               "DET, Directeur d'études (&eacute;m&eacute;ritat)", "MR, Maître de recherche",
                               "PHYSCE, Physicien de classe exceptionnelle",
                               "ASTCE, Astronome de classe exceptionelle")] <- "Directeur de recherche"
ech$type2[ech$categorie %in% c("P, Professeur",
                               "PU, Professeur des universités",
                               "PEM, Professeur émérite (&eacute;m&eacute;ritat)",
                               "PUCE, Professeur des universités de classe exceptionnelle", 
                               "PU, Professeur des universités",
                               "PU2, Professeur des universités 2ème classe", 
                               "PU1, Professeur des universités 1ère classe", 
                               "PU, Professeur des universités (&eacute;m&eacute;ritat)", 
                               "PUPH2, Professeur des universités - Praticien hospitalier 2ème classe", 
                               "PUPH1, Professeur des universités - Praticien hospitalier 1ère classe", 
                               "PHU1, Professeur hospitalo universitaire 1ère classe",
                               "P1, Professeur 1ère classe", 
                               "P2, Professeur 2ème classe",
                               "PUP2, Professeur des universités de pharmacie 2ème classe", 
                               "PUPHCE, Professeur des universités - Praticien hospitalier classe exceptionnelle", 
                               "PEA, Professeur des écoles d'architecture",
                               "PAST, Professeur associé à temps partiel",
                               "PHC, Professeur hors classe", 
                               "PU1, Professeur des universités 1ère classe (&eacute;m&eacute;ritat)", 
                               "PHU2, Professeur hospitalo universitaire 2ème classe",
                               "P, Professeur (&eacute;m&eacute;ritat)", 
                               "PU2, Professeur des universités 2ème classe (&eacute;m&eacute;ritat)", 
                               "PUAS, Professeur des universités associé",
                               "PEM, Professeur émérite", 
                               "PUM2, Professeur des universités 2ème classe de médecine", 
                               "PUCE, Professeur des universités de classe exceptionnelle (&eacute;m&eacute;ritat)", 
                               "PHC, Professeur hors classe (&eacute;m&eacute;ritat)",
                               "PUMCE, Professeur des universités de médecine classe exceptionnelle", 
                               "PUAS2, Professeur des universités associé 2ème classe")] <- "Professeur"
ech$type2[ech$categorie %in% c("CRCN, Cha.rec.cla.nor", "CR2, C.R.T.2.C.", 
                               "CR1, C.R.T.1.C.", "CRHC, Cha.rec.h.clas",
                               "ASTA, Astronome adjoint",
                               "PHYSA, Physicien adjoint de classe normale",
                               "CRC, C.R.C.",
                               "CRA2, C.R.2. associé",
                               "DA1C, C.R. ass 1",
                               "CRA1, C.R.1. associé", 
                               "PHYSAH, Physicien adjoint hors classe")] <- "Chargé de recherche"
ech$type2[ech$categorie %in% c("MCHC, Maître de conférences  hors classe", "MC, Maître de conférences des universités classe normale", 
                               "MCUPH0, Maître de conférences des universités - Praticien hospitalier hors classe", 
                               "MCUPH1, Maître de conférences des universités - Praticien hospitalier 1ère classe", 
                               "MCUPH2, Maître de conférences des universités - Praticien hospitalier 2ème classe", 
                               "MAEA, Maître assistant des écoles d'architecture", 
                               "MCHU, Maître de conférences hospitalo universitaire",
                               "MCP1, Maître de conférence pharmacie de 1ère classe", 
                               "MCP2, Maître de conférence pharmacie de 2ème classe",
                               "MA, Maître assistant")] <- "Maître de conférences"
ech$type2[ech$categorie %in% c("IECN, Ing.etu.cla.nor", "IE2, Ing.etu.2clas",
                               "IEHC, Ing.etu.h.clas", "IE, Ingénieur d'études",
                               "IE1C, ing.étud.1c")] <- "Ingénieur d'études"
ech$type2[ech$categorie == "AI, Ass. ingénieur"] <- "Assistant ingénieur"

lprop(table(ech$type2,
            !is.na(ech$lastpage) & ech$lastpage >= 1))


m <- glm(!is.na(lastpage) & lastpage >= 1 ~ institut2 + delegation + type2 + sexe.x,
         data=ech, family=binomial)
#stargazer::stargazer(m, type="html", out="~/tmp/out.html", apply.coef=exp, ci=TRUE)
library(gtsummary)
tbl_regression(m, exponentiate=TRUE, add_estimate_to_reference_rows=TRUE,
               label=list(institut2 ~ "Institut",
                          delegation ~ "Délégation régionale",
                          type2 ~ "Statut",
                          sexe.x ~ "Sexe")) %>%
    modify_header(list(label ~ "**Variable**",
                       estimate ~ "**Odds ratio**",
                       ci ~ "**IC à 95%**",
                       p.value ~ "**p-value**")) %>%
    modify_footnote(everything() ~ NA, abbreviation = TRUE) %>%
    add_significance_stars(hide_ci=FALSE, hide_se=TRUE)
