


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