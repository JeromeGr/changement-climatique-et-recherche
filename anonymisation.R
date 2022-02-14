climat <- read.csv("~/Private/results-survey_113464_R_data_file_151220.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")

# Supprimer les points à la fin des noms de variables
names(climat) <- gsub("\\.$", "", names(climat))

# Charger les intitulés des questions comme labels des variables 
questions <- read.csv("intitulés des questions.csv", row.names="code")
rownames(questions) <- gsub("\\[", ".", gsub("\\]$", "", rownames(questions)))
for(var in names(climat)) {
    attr(climat[[var]], "label") <-
        gsub("’", "'", questions[var, "question"])
}

source("Aéroports/aéroports.R")

source("codes_postaux/recodage_des_codes_postaux.R")

source("pondération.R")

attr(climat$res.dep, "label") <- "Département du lieu de résidence"
attr(climat$res.AAV2020, "label") <- "Aire d'attraction des villes du lieu de résidence"
attr(climat$res.TAAV2017, "label") <- "Tranche d'aire d'attraction des villes du lieu de résidence"
attr(climat$res.TDAAV2017, "label") <- "Tranche détaillée d'aire d'attraction des villes du lieu de résidence"
attr(climat$res.TUU2017, "label") <- "Tranche d'unité urbaine du lieu de résidence"
attr(climat$res.TDUU2017, "label") <- "Tranche détaillée d'unité urbaine du lieu de résidence"
attr(climat$res.CATEAAV2020, "label") <- "Catégorie de commune dans l'aire d'attraction des villes du lieu de résidence"

attr(climat$trav.dep, "label") <- "Département du lieu de travail"
attr(climat$trav.AAV2020, "label") <- "Aire d'attraction des villes du lieu de travail"
attr(climat$trav.TAAV2017, "label") <- "Tranche d'aire d'attraction des villes du lieu de travail"
attr(climat$trav.TDAAV2017, "label") <- "Tranche détaillée d'aire d'attraction des villes du lieu de travail"
attr(climat$trav.TUU2017, "label") <- "Tranche d'unité urbaine du lieu de travail"
attr(climat$trav.TDUU2017, "label") <- "Tranche détaillée d'unité urbaine du lieu de travail"
attr(climat$trav.CATEAAV2020, "label") <- "Catégorie de commune dans l'aire d'attraction des villes du lieu de travail"

attr(climat$sexe.labintel, "label") <- "Sexe dans Labintel"
attr(climat$unite.labintel, "label") <- "Unité de rattachement dans Labintel (anonymisée)"
attr(climat$appartenance.labintel, "label") <- "Établissement employeur dans Labintel (anonymisé)"
attr(climat$type.labintel, "label") <- "Type de personnel dans Labintel"
attr(climat$nature.labintel, "label") <- "Nature de personnel dans Labintel"
attr(climat$categorie.labintel, "label") <- "Catégorie de personnel dans Labintel"
attr(climat$bap.labintel, "label") <- "BAP dans Labintel"
attr(climat$institut.labintel, "label") <- "Institut dans Labintel"
attr(climat$delegation.labintel, "label") <- "Délégation régionale dans Labintel"
attr(climat$sections.labintel, "label") <- "Sections CNU dans Labintel"
attr(climat$institut2.labintel, "label") <- "Institut dans Labintel (abrégé)"
attr(climat$type2.labintel, "label") <- "Type de personnel dans Labintel (regroupé)"
attr(climat$poids, "label") <- "Pondération individuelle calculée par calage sur Labintel"

for(i in 1:5) {
    attr(climat[[paste0("volsjours", i)]], "label") <- sprintf("Temps travaillé sur place pour le trajet %i", i)
    attr(climat[[paste0("volsmotif", i)]], "label") <- sprintf("Motif principal du trajet %i", i)
    attr(climat[[paste0("volsmotifsec", i)]], "label") <- sprintf("Motif secondaire du trajet %i", i)
    attr(climat[[paste0("volsnb", i)]], "label") <- sprintf("Nombre d'allers-retours réalisés pour le trajet %i", i)
    attr(climat[[paste0("volsdist", i)]], "label") <- sprintf("Distance en km d'un aller-retour pour le trajet %i", i)
    attr(climat[[paste0("volsges", i)]], "label") <- sprintf("Émissions de GES d'un aller-retour pour le trajet %i", i)
    attr(climat[[paste0("volsh", i)]], "label") <- sprintf("Durée en heures d'un aller-retour pour le trajet %i", i)
    attr(climat[[paste0("volstrain3h", i)]], "label") <- sprintf("Trajet %i faisable en 3h de train", i)
    attr(climat[[paste0("volstrain6h", i)]], "label") <- sprintf("Trajet %i faisable en 6h de train", i)
    attr(climat[[paste0("volsdepart", i, "pays")]], "label") <- sprintf("Pays de départ du trajet %i", i)
    attr(climat[[paste0("volsarrivee", i, "pays")]], "label") <- sprintf("Pays d'arrivée du trajet %i", i)
}

attr(climat$volstrain3h1, "label") <- "Trajet 1 faisable en 3h de train"

attr(climat$volstrain3h2, "label") <- "Trajet 2 faisable en 3h de train"
attr(climat$volstrain3h3, "label") <- "Trajet 3 faisable en 3h de train"
attr(climat$volstrain3h4, "label") <- "Trajet 4 faisable en 3h de train"
attr(climat$volstrain3h5, "label") <- "Trajet 5 faisable en 3h de train"
attr(climat$volstrain6h1, "label") <- "Trajet 1 faisable en 6h de train"
attr(climat$volstrain6h2, "label") <- "Trajet 2 faisable en 6h de train"
attr(climat$volstrain6h3, "label") <- "Trajet 3 faisable en 6h de train"
attr(climat$volstrain6h4, "label") <- "Trajet 4 faisable en 6h de train"
attr(climat$volstrain6h5, "label") <- "Trajet 5 faisable en 6h de train"


excl <- c("id", "email", "startlanguage", "seed", "token",
          "volstitre1", "volstitre2", "volstitre3", "volstitre4", "volstitre5",
          "volsdepart1", "volsdepart2", "volsdepart3", "volsdepart4", "volsdepart5",
          "volsarrivee1", "volsarrivee2", "volsarrivee3", "volsarrivee4", "volsarrivee5",
          "volsdepart1code", "volsdepart2code", "volsdepart3code", "volsdepart4code", "volsdepart5code",
          "volsarrivee1code", "volsarrivee2code", "volsarrivee3code", "volsarrivee4code", "volsarrivee5code",
          "textedomtrav",
          "communes.SaisieVille", "communes.SaisieVilleTravail",
          "res.codeinsee", "trav.codeinsee", "res.cp", "trav.cp",
          "recontact.other")

climat <- select(climat, !any_of(excl) &
                     !starts_with("volstexte") &
                     (!ends_with("Time", ignore.case=FALSE) | starts_with("groupTime")))
svyclimat$variables <- svyclimat$variables[setdiff(names(climat), "poids")]

save(climat, file="climat.RData")
save(svyclimat, file="svyclimat.RData")
write.csv(climat, file="climat.csv",
          row.names=FALSE, na="", fileEncoding="UTF-8")
