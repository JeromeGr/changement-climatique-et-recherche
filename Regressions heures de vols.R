#Régressions sur les heures de vols

#Je vire ceux qui n'ont rien répondu au sexe (souvent des questionnaires vides)
#Idéalement, il faudrait plutôt virer ceux qui n'ont rien répondu nulle part (ou presque)

climat<-climat %>% filter (sexe!="")

################################
#Recodage pour la régression
climat$anr

#Regroupement catégories d'âge
freq(climat$ageAgr)
freq(climat$age)
climat$ageAgr<-climat$age
climat$ageAgr[climat$age %in% c("70 ans ou plus", "65-69 ans")]<-"65 ans et plus"
climat$ageAgr[climat$age %in% c("18-24 ans", "25-29 ans")]<-"Moins de 29 ans"
climat$ageAgr[climat$age %in% c("55-59 ans", "60-64 ans")]<-"55-64 ans"

#Regroupement participation à projet financé
climat$Part_ANR_ERC[is.na(climat$projets.anr_r.) & is.na(climat$projets.anr_m.) & is.na(climat$projets.europe_r.) 
                    & is.na(climat$projets.europe_m.)]<-"Ni financement ANR ou Europe"
climat$Part_ANR_ERC[(!is.na(climat$projets.anr_r.) | !is.na(climat$projets.anr_m.)) & is.na(climat$projets.europe_r.) 
                    & is.na(climat$projets.europe_m.)]<-"Projet ANR"
climat$Part_ANR_ERC[is.na(climat$projets.anr_r.) & is.na(climat$projets.anr_m.) & (!is.na(climat$projets.europe_r.) 
                    | !is.na(climat$projets.europe_m.))]<-"Projet européen"               
climat$Part_ANR_ERC[(!is.na(climat$projets.anr_r.) | !is.na(climat$projets.anr_m.)) & (!is.na(climat$projets.europe_r.) 
                    | !is.na(climat$projets.europe_m.))]<-"Projet ANR et projet européen"             

freq(climat$Part_ANR_ERC)
#######################
#Je fixe les modalités de référence dans les régressions
climat$sexe <- as.factor(climat$sexe)
climat$sexe <- relevel(climat$sexe, ref = "un homme")

climat$ageAgr <- as.factor(climat$ageAgr)
climat$ageAgr <- relevel(climat$ageAgr, ref = "50-54 ans")

climat$sitpro <- relevel(climat$sitpro, ref = "Maître·sse de conférences")

climat$discipline_agregee <- relevel(climat$discipline_agregee , ref = "Sciences (1)") 

freq(climat$volshnum)
freq(climat)

##########################################
#Régressions



#Tout le monde
res.reg1 <- lm(volshnum ~ sexe + ageAgr, data=climat)
res.reg2 <- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agregee , data=climat)
res.reg3 <- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agregee + Part_ANR_ERC , data=climat)

summary(res.reg3)
#Personnel amené à publier

climatPersPubli<-climat %>% filter(!(sitpro %in% c("Technicien·ne", "Adjoint·e technique", "Autre")))

res.Perspubli1 <- lm(volshnum ~ sexe + ageAgr, data=climatPersPubli)
res.Perspubli2 <- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agregee , data=climatPersPubli)
res.Perspubli3 <- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agregee + nbpublis, data=climatPersPubli)
res.Perspubli4 <- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agregee + Part_ANR_ERC, data=climatPersPubli)
res.Perspubli5<- lm(volshnum ~ sexe + ageAgr  + sitpro + discipline_agregee + nbpublis + Part_ANR_ERC, data=climatPersPubli)


summary(res.Perspubli4)

library(texreg)

htmlreg(list(res.reg1, res.reg2, res.reg3, res.Perspubli1, res.Perspubli2, res.Perspubli3, res.Perspubli4, res.Perspubli5), 
        stars = c(0.001, 0.01, 0.05, 0.1), digits = 2, #single.row = TRUE,
        custom.header = list("All staff" = 1:3, "Publishing staff" = 4:8),
        custom.coef.map = list("sexeune femme"= "Woman (ref = man)",
                               "sexeautre"="Sex : other",
                               "ageAgrMoins de 29 ans"= "less than 29 years old (Ref = 50-54 years old",
                               "ageAgr30-34 ans"= "30-34 years old",
                               "ageAgr35-39 ans"= "35-39 years old",
                               "ageAgr40-44 ans"= "40-44 years old",
                               "ageAgr45-49 ans"= "45-49 years old",
                               "ageAgr55-64 ans"= "55-64 years old",
                               "ageAgr65 ans et plus"= "More than 65 years old",
                               "sitproDirecteur·rice de recherche"="Directeur·rice de recherche (Ref = Maître·sse de conférences)",
                               "sitproProfesseur·e des universités"= "University professor ",
                               "sitproChargé·e de recherche"="Researcher",
                               "sitproMaître·sse de conférences"="Lecturer",
                               "sitproPost-doctorant·e"= "Post doctoral student",
                               "sitproATER"="ATER",
                               "sitproDoctorant·e contractuel·le"="contractual doctoral student",
                               "sitproDoctorant·e CIFRE"= "CIFRE contractual doctoral student",
                               "sitproIngénieur·e de recherche"="Research engineer",
                               "sitproIngénieur·e d'études"="Design engineer",
                               "sitproAssistant ingénieur·e"="Assistant engineer",
                               "sitproTechnicien·ne"="Technician",
                               "sitproChargé·e d’études/de mission"= "Research officer",
                               "sitproAdjoint·e technique"="Technical assistant",
                               "sitproAutre"="Other",
                               "discipline_agregeeDroit, économie, gestion"="Droit, économie, gestion (Ref = Sciences (1))",
                               "discipline_agregeeLettres et sciences humaines (1)"="Lettres et sciences humaines (1)",
                               "discipline_agregeeMédecine, odontologie" ="Médecine, odontologie",
                               "discipline_agregeeSciences (2)"="Sciences (2)",
                               "discipline_agregeeLettres et sciences humaines (2)"="Lettres et sciences humaines (2)",
                               "discipline_agregeePharmacie"="Pharmacie",
                               "discipline_agregeeAutres santé"="Autres santé",
                               "nbpublis"="Number of publications in 2017-mid2020",
                               "Part_ANR_ERCProjet ANR"="Participation projet ANR (Ref = ni projet ANR ni européen)",
                               "Part_ANR_ERCProjet européen"="Participation projet européen",
                               "Part_ANR_ERCProjet ANR et projet européen" ="Participation projet ANR et projet européen"),
        symbol = "+",
        caption ="",
        #caption = "Tableau 5 : Régressions linéaires multiples sur le nombre d'heures de vol en 2019", caption.above=TRUE, 
        single.row = TRUE, 
        #custom.gof.rows = NULL,
        file="/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Regressions duree de vol agregee 2019.doc")

