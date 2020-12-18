# On réalise un calage sur marges direct
# La modélisation de la non-réponse dans une première étape séparée est inutile
# car elle se fonderait sur les mêmes variables que le calage (voir Sautory)
# Références :
# https://www.unine.ch/files/live/sites/statistics/files/shared/documents/laguennec3.doc (Sautory)
# https://www.insee.fr/fr/statistiques/fichier/2838097/5-les-methodes-de-calage.pdf
# https://faculty.washington.edu/tlumley/survey-jsm-nup.pdf
# https://tophcito.blogspot.com/2014/04/survey-computing-your-own-post.html
# https://bookdown.org/jespasareig/Book_How_to_weight_a_survey/

library(tidyverse)
library(survey)

fiches <- read.csv2("../Labintel/fiches.csv",
                    fileEncoding="WINDOWS-1252",
                    na.strings=c("NA", "&nbsp;"),
                    row.names=1)

# Très peu de non renseignés, on les mélange avec la plus grande délégation
fiches$delegation[is.na(fiches$delegation)] <- "02, Paris-Centre"
fiches$institut2 <- sapply(fiches$institut,
                           function(x) strsplit(x, " (", fixed=TRUE)[[1]][[1]])
fiches$institut2 <- fct_collapse(fiches$institut2,
                                 "DG-PDT"=c("DGD-I", "DGD-R", "DGD-S", "PDT"))

# On met les Autre personnel en dernier avant de supprimer les doublons
# pour garder la fiche qui donne le plus d'information
# Idem pour les autres variables, les NA vont à la fin
# On garde les fiches sans adresse courriel
fiches <- arrange(fiches, recode(type, "Autre personnel"="Z"), institut2, delegation, sexe)
fiches <- subset(fiches, !duplicated(courriel) | courriel == "")

names(fiches) <- paste0(names(fiches), ".labintel")
stopifnot(all(climat$email %in% fiches$courriel.labintel))
climat <- left_join(climat, fiches, by=c("email"="courriel.labintel"))

# Anonymisation
climat <- select(climat, !any_of(c("num.labintel", "nom.labintel",
                                   "dateentree.labintel", "fonction.labintel",
                                   "emploitype.labintel")))
# Attribution d'un identifiant aléatoire à chaque unité
climat$appartenance.labintel <- as.integer(factor(climat$appartenance.labintel,
                                           levels=sample(unique(climat$appartenance.labintel))))
climat$unite.labintel <- as.integer(factor(climat$unite.labintel,
                                           levels=sample(unique(climat$unite.labintel))))

# Ne supprimer qu'après la fusion avec climat, sinon certaines adresses courriel peuvent manquer
fiches <- subset(fiches, !duplicated(paste(nom.labintel, categorie.labintel)))

svyclimat.unweighted <- svydesign(ids=~1, weights=~1, data=climat)

# Calage sur marges
svyclimat.r <- rake(svyclimat.unweighted,
                    list(~sexe.labintel, ~type.labintel, ~institut2.labintel, ~delegation.labintel),
                    with(fiches, list(table(sexe.labintel), table(type.labintel), table(institut2.labintel),
                                   table(delegation.labintel))))

# Post-stratification utilisant l'ensemble des combinaisons (et pas seulement les marges)
# L'avertissement indique que certaines combinaisons de variables
# ne se retrouvent pas parmi les répondants (d'où partial=TRUE)
# svyclimat.ps <- postStratify(svyclimat.unweighted,
#                              ~ sexe.labintel + type.labintel + institut2.labintel + delegation.labintel,
#                              xtabs(~ sexe.labintel + type.labintel + institut2.labintel + delegation.labintel, fiches),
#                              partial=TRUE)

# Analyse des variations des poids
summary(weights(svyclimat.r))
x <- cbind(svyclimat.r$variables, w=weights(svyclimat.r))
group_by(x, sexe.labintel) %>% summarize(mean(w))
group_by(x, type.labintel) %>% summarize(mean(w))
group_by(x, institut2.labintel) %>% summarize(mean(w))
group_by(x, delegation.labintel) %>% summarize(mean(w))

# Pour limiter les poids extrêmes
# (ne fonctionne pas pour svyclimat.ps, sans doute à cause de la présence de combinaisons vides)
# svyclimat <- trimWeights(svyclimat.r,
#                          lower=quantile(weights(svyclimat.r), 0.05),
#                          upper=quantile(weights(svyclimat.r), 0.95))
# 
# summary(weights(svyclimat))

svyclimat <- svyclimat.r
climat$poids <- weights(svyclimat.r)

round(cbind(prop.table(svytable(~ sitpro, svyclimat.unweighted)),
            prop.table(svytable(~ sitpro, svyclimat))) * 100, 1)

svyciprop(~ I(preoccupe == "Extrêmement préoccupé·e"), svyclimat.unweighted)
svyciprop(~ I(preoccupe == "Extrêmement préoccupé·e"), svyclimat)

svyciprop(~ I(grepl("^La recherche publique doit montrer l'exemple", solreducrech)), svyclimat.unweighted)
svyciprop(~ I(grepl("^La recherche publique doit montrer l'exemple", solreducrech)), svyclimat)

svymean(~ nbpublis, na.rm=TRUE,
        subset(svyclimat.unweighted,
               sitpro %in% c("Professeur·e des universités", "Directeur·rice de recherche",
                             "Maître·sse de conférences", "Chargé·e de recherche")))
svymean(~ nbpublis, na.rm=TRUE,
        subset(svyclimat,
               sitpro %in% c("Professeur·e des universités", "Directeur·rice de recherche",
                             "Maître·sse de conférences", "Chargé·e de recherche")))

svymean(~ volsnb, na.rm=TRUE,
        subset(svyclimat.unweighted, sitpro %in% c("Professeur·e des universités", "Directeur·rice de recherche",
                                                   "Maître·sse de conférences", "Chargé·e de recherche")))
svymean(~ volsnb, na.rm=TRUE,
        subset(svyclimat, sitpro %in% c("Professeur·e des universités", "Directeur·rice de recherche",
                                        "Maître·sse de conférences", "Chargé·e de recherche")))


# Autre approche avec le paquet icarus de l'Insee
# library(icarus)
# 
# tabs <- with(fiches, list(table(sexe.labintel), table(type.labintel), table(institut2.labintel), table(delegation.labintel)))
# nms <- sapply(tabs, function(x) names(dimnames(x))[[1]])
# len <- sapply(tabs, length)
# mat <- matrix(0, length(tabs), max(len)+2)
# for(i in 1:nrow(mat)) {
#     mat[i, 1] <- nms[i]
#     mat[i, 2] <- len[i]
#     mat[i, 3:(2+len[i])] <- tabs[[i]]
# }
# 
# dat <- climat[nms]
# dat$w <- 1
# 
# # Même chose qu'avec survey
# calr <- calibration(dat, mat, colWeights="w", method="raking", calibTolerance=1e-3)
# stopifnot(cor(calr, weights(svyclimat.r)) > 0.99)
# 
# # Avec pénalisation des poids extrêmes
# # Ne converge jamais avec method="raking"
# calp <- calibration(dat, mat, colWeights="w", method="linear", calibTolerance=1e-3,
#                     gap=1.3, costs=rep(1, nrow(mat)), popTotal=nrow(fiches))
# stopifnot(cor(calp, weights(svyclimat.r)) > 0.9)
