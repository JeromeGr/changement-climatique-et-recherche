# chargement de la base, des packages et des recodages
source("recodages.R")

# Déterminants socio-professionnels élémentaires des vols ----

# j'ai fait ça à l'arrage pendant la formation. 
# Si vous avez une syntaxe plus économe pour faire ce truc, ça m'intéresse, 
# parce que là c'est pas hyper automatisable

# construction d'un tableau des proportions, sans les marges et les réponses nulles
a <- rbind(
  lprop(table(climat$sitpro,climat$vols_dicho))[-16,-3],
  lprop(table(climat$sexe,climat$vols_dicho))[3:4,-3],
  lprop(table(climat$docto,climat$vols_dicho))[2:3,-3],
  lprop(table(climat$statut,climat$vols_dicho))[-c(2,8),-3]
)
a <- as.data.frame(as.table(a))# on transforme le truc en un objet dataframe 
# pour qu'il soit avalable par ggplot. Je pense qu'on doit pouvoir écrire ça mieux

ggplot(a[a$Var2=="vol",], 
       aes(x=fct_rev(Var1), y = Freq))+
  geom_segment(
    aes(x=fct_rev(Var1), xend=fct_rev(Var1), 
        y=0, yend=Freq), color="grey")+
  geom_point( color="steelblue", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("% de volants")+
  geom_hline(yintercept=50.2,  colour = "red") +
  coord_flip()+
  labs(caption="La ligne rouge indique la valeur sur la totalité des répondants")


# vols par discipline ----
## discipline détaillée

a <-lprop(table(climat$discipline,climat$vols_dicho))[-69,-3]
a <- as.data.frame(as.table(a))
ggplot(a[a$Var2=="vol",],
       aes(x=reorder(Var1, Freq), y = Freq))+
  geom_segment(
    aes(x=reorder(Var1, Freq), xend=reorder(Var1, Freq), 
        y=0, yend=Freq), color="grey")+
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("% de volants")+
  geom_hline(yintercept=50.2,  colour = "red") +
  coord_flip()+
  labs(caption="La ligne rouge indique la valeur sur la totalité des répondants")

# discipline agrégée

a <-lprop(table(climat$discipline_agregee,climat$vols_dicho))[-10,-3]
a <- as.data.frame(as.table(a))
ggplot(a[a$Var2=="vol",],
       aes(x=reorder(Var1, Freq), y = Freq))+
  geom_segment(
    aes(x=reorder(Var1, Freq), xend=reorder(Var1, Freq), 
        y=0, yend=Freq), color="grey")+
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("% de volants")+
  geom_hline(yintercept=50.2,  colour = "red") +
  coord_flip()+
  labs(caption="La ligne rouge indique la valeur sur la totalité des répondants")

# hindex et vols ----

# nuage de points hindex/nb de vols
ggplot(climat,aes(x=hindex, 
                  y=volsnb))+
  geom_point()+ 
  geom_smooth(method=lm)+
  xlim(0,100)+ylim(0,51)


# programmes de recherche (ANR and co) et vols ----

# on construit un grand vecteur (une liste) avec les proportions qui nous intéressent
projets <- rbind(
  lprop(table(climat$projets.anr_r.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.anr_m.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.anr_n.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.france_r.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.france_m.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.france_n.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.europe_r.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.europe_m.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.europe_n.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.inter_r.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.inter_m.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.inter_n.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.prive_r.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.prive_m.,climat$vols_dicho))[1,2],
  lprop(table(climat$projets.prive_n.,climat$vols_dicho))[1,2])

# on met des intitulés clairs et dans l'ordre de la liste ci dessus
a <- c(
  "ANR_responsable", "ANR_membre", "ANR_non",
  "france_responsable", "france_membre", "france_non",
  "europe_responsable", "europe_membre", "europe_non",
  "inter_responsable", "inter_membre", "inter_non",
  "prive_responsable", "prive_membre", "prive_non"
)

# on associe 
projets <- as.data.frame(projets, row.names = a)
# je préfères que les intitulés soient dans une colonne 
#plutôt que comme rownames parce que c'est plus simple 
#à gérer dans ggplot, je crois
projets$proj <- rownames(projets)
# par contre ça a tout mélangé, je remets dans l'ordre
projets$proj <- factor(projets$proj, levels=c(
  "ANR_responsable", "ANR_membre", "ANR_non",
  "france_responsable", "france_membre", "france_non",
  "europe_responsable", "europe_membre", "europe_non",
  "inter_responsable", "inter_membre", "inter_non",
  "prive_responsable", "prive_membre", "prive_non"
))

# le graph
ggplot(projets,
       aes(x=proj, y = V1))+
  geom_segment(
    aes(x=proj, xend=proj, 
        y=0, yend=V1), color="grey")+
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("% de volants")+
  geom_hline(yintercept=50.2,  colour = "steelblue") +
  coord_flip()+
  labs(title="Au moins un vol * participation à un programme de recherche", 
       caption="La ligne rouge indique la valeur sur la totalité des répondants")


# changements profonds dans nos métiers ----

# OK !!!
# mettre sans opinion en dernier !!!

freq(climat$chgtpratique)
climat$chgtpratique <- factor(climat$chgtpratique, 
                               labels=c("",
                                 "Sans opinion",
                                 "Non, pas du tout d’accord",
                                 "Non, plutôt pas d’accord",
                                 "Montrer l'exemple (plus que les autres)",
                                 "Comme les autres"
                               ))
ggplot(climat[climat$chgtpratique !="",]) + 
  geom_bar(aes(x = chgtpratique, 
               y = 100*..prop.., group = 1 
               ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+
  labs(x="", y="% des répondants")
#Pensez-vous que l'urgence climatique exige des changements profonds dans la pratique de nos métiers ?


# un tiers de réduction des émissions ----
# OK !!!
freq(climat$solreducrech)
climat$recherche2030 <- factor(climat$solreducrech, 
                               labels=c(
                                 "Non réponse",
                                 "Bénéficier d'un statut dérogatoire \n
                                 (réduire moins que les autres)",
                                 "Montrer l'exemple \n
                                 (réduire plus que les autres)",
                                 "Réduire comme les autres"
                               ))
climat$recherche2030 <- factor(climat$recherche2030, 
                               levels=c(
                                 "Non réponse",
                                 "Bénéficier d'un statut dérogatoire \n
                                 (réduire moins que les autres)",
                                 "Réduire comme les autres",
                                 "Montrer l'exemple \n
                                 (réduire plus que les autres)"
                               ))
ggplot(climat[climat$recherche2030 !="Non réponse",]) + 
  geom_bar(aes(x = recherche2030, 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+
  labs(x="", y="% des répondants")


# La France s’est engagée à réduire d’un tiers ses émissions de gaz à effet de serre d’ici à 2030. Dans ce cadre, pensez-vous que la recherche doit : 


# diminution 5 dernières années domicile travail ----

climat$diminution_domicile_travail <- factor(climat$solevolges.domicile.,
  levels = c(
    "Fortement diminué", "Un peu diminué", "Été à peu près stables",
    "Un peu augmenté", "Fortement augmenté", "Je ne sais pas", "Non concerné·e",
    ""
  ))

ggplot(climat[climat$diminution_domicile_travail !="" &
                climat$diminution_domicile_travail!="Non concerné·e",]) + 
  geom_bar(aes(x = fct_rev(diminution_domicile_travail), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+
  labs(x="", y="% des répondants concernés")
  # OK !!!

# "En termes de déplacement domicile-travail, vos émissions :"

# visio depuis le confinement
# OK !!!
## Réordonnancement de climat$visioapresconf en climat$visioapresconf_recode
climat$visioapresconf_recode <- factor(climat$visioapresconf,
  levels = c(
    "Beaucoup plus favorable", "Un peu plus favorable", "Mon avis n’a pas changé",
    "Un peu moins favorable", "Beaucoup moins favorable", "Sans opinion",
    ""
  )
)


ggplot(climat[climat$visioapresconf_recode !="",]) + 
  geom_bar(aes(x = fct_rev(visioapresconf_recode), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+
  labs(x="", y="% des répondants")

# "Depuis le confinement, plus ou moins favorable à la visio ?"


# Selon vous, quels seraient les risques liés à une politique de réduction 
# des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel 
# destiné aux expériences et aux observations scientifiques ? 
# solrisqreducavion ----

freq(climat$solrisqreducmateriel.publi.)
climat$solrisqreducmateriel.publi_rec <- factor(climat$solrisqreducmateriel.publi.,
                                       levels = c("",
                                         "Non concerné·e",
                                         "Sans opinion",
                                         "C’est peu probable", 
                                         "C’est probable mais ce n’est pas un problème", 
                                         "C’est probable et c’est un problème"
                                       )
)

ggplot(climat[climat$solrisqreducmateriel.publi. !="" &
                climat$solrisqreducmateriel.publi.  != "Non concerné·e",]) + 
  geom_bar(aes(x = fct_rev(solrisqreducmateriel.publi.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+
  labs(x="", y="% des répondants concernés")

# OK !!!
# "Selon vous, quels seraient les risques liés à une politique de réduction 
#      des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel 
#      destiné aux expériences et aux observations scientifiques ? (sur les 50% des répondants concernés)"
# réduire votre nombre de publications


# Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?

# freq(climat$solinstit.train.)

#, title=
# "Limite de vols par personne"
ggplot(climat[climat$solinstit.limitevols. !="",]) + 
  geom_bar(aes(x = fct_rev(solinstit.limitevols.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+
  labs(x="", y="% des répondants")+
  ylim(0,65)

#Bilan GES"
ggplot(climat[climat$solinstit.bilanges. !="",]) + 
  geom_bar(aes(x = fct_rev(solinstit.bilanges.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+
  labs(x="", y="% des répondants")+
  ylim(0,65)

#, title=
# "Poids des conférences dans les évaluations de carrière"
ggplot(climat[climat$solinstit.conf. !="",]) + 
  geom_bar(aes(x = fct_rev(solinstit.conf.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+
  labs(x="", y="% des répondants")+
  ylim(0,65)



# , title=
  # "Emissions dans les critères de sélection des projets à financer"
ggplot(climat[climat$solinstit.selection. !="",]) + 
  geom_bar(aes(x = fct_rev(solinstit.selection.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "steelblue") +
  theme_minimal()+coord_flip()+ 
  labs(x="", y="% des répondants")+
  ylim(0,65)

# heures de vol approximatives par statut
group_by(climat, sitpro) %>%
  summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
  ggplot(aes(x=fct_rev(sitpro), y = volshnum)) +
  geom_segment(aes(x=fct_rev(sitpro), xend=fct_rev(sitpro), 
               y=0, yend=volshnum), color="grey")+
  geom_point(color="steelblue", size=3) +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("heures de vol par an")+
  geom_hline(yintercept=mean(climat$volshnum, na.rm=TRUE), colour = "red") +
  coord_flip()+
  scale_y_continuous(breaks=seq(0, 50, by=5)) +
  labs(caption="La ligne rouge indique la valeur sur la totalité des répondants")


# heures de vol approximatives par ANR/ERC/etc.
group_by(climat, sitpro) %>%
  summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
  ggplot(aes(x=fct_rev(sitpro), y = volshnum)) +
  geom_segment(aes(x=fct_rev(sitpro), xend=fct_rev(sitpro), 
                   y=0, yend=volshnum), color="grey")+
  geom_point(color="steelblue", size=3) +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("heures de vol par an")+
  geom_hline(yintercept=mean(climat$volshnum, na.rm=TRUE), colour = "red") +
  coord_flip()+
  scale_y_continuous(breaks=seq(0, 50, by=5)) +
  labs(caption="La ligne rouge indique la valeur sur la totalité des répondants")

# volsproj <- rbind(group_by(climat, projets.anr_r) %>%
#                     summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
#                     rename(projets=projets.anr_r),
#                   group_by(climat, projets.anr_m) %>%
#                     summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
#                     rename(projets=projets.anr_m),
#                   group_by(climat, projets.europe_r) %>%
#                     summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
#                     rename(projets=projets.europe_r),
#                   group_by(climat, projets.europe_m) %>%
#                     summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
#                     rename(projets=projets.europe_m))
# vars <- paste0("projets.", c("anr_r", "europe_r", "france_r", "inter_r", "prive_r",
#                              "anr_m", "europe_m", "france_m", "inter_m", "prive_m"), ".")
volsproj <- rbind(filter(climat, is.na(projets.anr_r.) & is.na(projets.anr_m.) & is.na(projets.europe_r.) & is.na(projets.europe_m.)) %>%
                    summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
                    mutate(projets="Ni l'un ni l'autre"),
                  filter(climat, (!is.na(projets.anr_r.) | !is.na(projets.anr_m.)) & is.na(projets.europe_r.) & is.na(projets.europe_m.)) %>%
                    summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
                    mutate(projets="Projet ANR"),
                  filter(climat, is.na(projets.anr_r.) & is.na(projets.anr_m.) & (!is.na(projets.europe_r.) | !is.na(projets.europe_m.))) %>%
                    summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
                    mutate(projets="Projet européen"),
                  filter(climat, (!is.na(projets.anr_r.) | !is.na(projets.anr_m.)) & (!is.na(projets.europe_r.) | !is.na(projets.europe_m.))) %>%
                    summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
                    mutate(projets="Projet ANR et projet européen"))

group_by(volsproj, projets) %>%
  ggplot(aes(x=fct_reorder(projets, volshnum), y = volshnum)) +
  geom_segment(aes(x=fct_reorder(projets, volshnum), xend=fct_rev(projets), 
                   y=0, yend=volshnum), color="grey")+
  geom_point(color="steelblue", size=3) +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("") + 
  ylab("heures de vol par an")+
  geom_hline(yintercept=mean(climat$volshnum, na.rm=TRUE), colour = "red") +
  coord_flip()+
  scale_y_continuous(breaks=seq(0, 50, by=5)) +
  labs(caption="La ligne rouge indique la valeur sur la totalité des répondants")



# taux de NA par question
a <- apply(climat, 2, is.na)
freq(a[,78])
sum(a[a=="TRUE"])
b <- apply(a, 2, function(a) sum(a=="TRUE"))

str(b)

tab <- as.data.frame(b)
tab <- tab[tab$b<1200,]
tab$question <- as.factor(rownames(tab))
str(tab)
ggplot(tab[1:40,], aes(x=b, y=rownames(tab[1:40,])))+
  geom_bar(stat="identity")

plot(tab[,1], type="l")

freq(climat$sitpro)

summary(climat$lastpage)

plot(climat$lastpage)
ggplot(d) +
  aes(x = heures.tv) +
  xlab("Heures") +
  ylab("Fonction de répartition cumulée")
ggplot(climat)+
  aes(x=lastpage)+
  stat_ecdf() 
  
