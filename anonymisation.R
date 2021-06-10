source("importation limesurvey.R")
climat <- data
rm(data)

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
                     !ends_with("Time", ignore.case=FALSE))
svyclimat$variables <- svyclimat$variables[setdiff(names(climat), "poids")]

save(climat, file="climat.RData")
save(svyclimat, file="svyclimat.RData")
write.csv(climat, file="climat.csv",
          row.names=FALSE, na="", fileEncoding="UTF-8")
