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

# Taux de réponses complètes et partielles (ouverture de la page 1 au moins)
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

m <- glm(!is.na(lastpage) & lastpage >= 1 ~ institut2 + delegation + type + sexe.x, data=ech, family=binomial)
stargazer(m, type="html", out="~/tmp/out.html", apply.coef=exp, ci=TRUE)
