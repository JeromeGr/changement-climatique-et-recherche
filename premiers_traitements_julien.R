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


# programmes de recherche et vols ----

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
  geom_hline(yintercept=50.2,  colour = "red") +
  coord_flip()+
  labs(title="Au moins un vol * participation à un programme de recherche", 
       caption="La ligne rouge indique la valeur sur la totalité des répondants")


# changements profonds dans nos métiers ----

freq(climat$chgtpratique)
ggplot(climat[climat$chgtpratique !="",]) + 
  geom_bar(aes(x = chgtpratique, 
               y = 100*..prop.., group = 1 
               ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
      "Pensez-vous que l'urgence climatique exige des changements profonds dans la pratique de nos métiers ?")
  

# un tiers de réduction des émissions ----

climat$recherche2030 <- factor(climat$solreducrech, 
                               labels=c(
                                 "Non réponse",
                                 "Bénéficier d'un statut dérogatoire (moins que les autres)",
                                 "Montrer l'exemple (plus que les autres)",
                                 "Comme les autres"
                               ))
ggplot(climat[climat$recherche2030 !="",]) + 
  geom_bar(aes(x = recherche2030, 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
         "La recherche doit réduire ses émissions plus ou moins que les autres secteurs ")


# La France s’est engagée à réduire d’un tiers ses émissions de gaz à effet de serre d’ici à 2030. Dans ce cadre, pensez-vous que : 


# diminution 5 dernières années domicile travail ----

climat$diminution_domicile_travail <- factor(climat$solevolges.domicile.,
  levels = c(
    "Fortement diminué", "Un peu diminué", "Été à peu près stables",
    "Un peu augmenté", "Fortement augmenté", "Je ne sais pas", "Non concerné·e",
    ""
  ))

ggplot(climat[climat$diminution_domicile_travail !="",]) + 
  geom_bar(aes(x = fct_rev(diminution_domicile_travail), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
         "En termes de déplacement domicile-travail, vos émissions :")


# visio depuis le confinement

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
  ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
         "Depuis le confinement, plus ou moins favorable à la visio ?")



# Selon vous, quels seraient les risques liés à une politique de réduction 
# des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel 
# destiné aux expériences et aux observations scientifiques ? 
# solrisqreducavion ----

freq(climat$solrisqreducmateriel.publi.)

ggplot(climat[climat$solrisqreducmateriel.publi. !="",]) + 
  geom_bar(aes(x = fct_rev(solrisqreducmateriel.publi.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
         "Selon vous, quels seraient les risques liés à une politique de réduction 
       des émissions de gaz à effet de serre liées à la fabrication et au fonctionnement du matériel 
       destiné aux expériences et aux observations scientifiques ? (sur les 50% des répondants concernés)")


# Quelles actions les institutions et laboratoires de recherche devraient-ils mettre en œuvre pour réduire leurs émissions de gaz à effet de serre ?

freq(climat$solinstit.train.)

ggplot(climat[climat$solinstit.limitevols. !="",]) + 
  geom_bar(aes(x = fct_rev(solinstit.limitevols.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
         "Actions à mettre en oeuvre : limite de vols par personne")

ggplot(climat[climat$solinstit.bilanges. !="",]) + 
  geom_bar(aes(x = fct_rev(solinstit.bilanges.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
         "Actions à mettre en oeuvre : Bilan GES")

ggplot(climat[climat$solinstit.conf. !="",]) + 
  geom_bar(aes(x = fct_rev(solinstit.conf.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
         "Actions à mettre en oeuvre: poids des confs dans les évaluations de carrière")



ggplot(climat[climat$solinstit.selection. !="",]) + 
  geom_bar(aes(x = fct_rev(solinstit.selection.), 
               y = 100*..prop.., group = 1 
  ), fill="white", color = "orange") +
  theme_minimal()+coord_flip()+
  labs(x="", y="", title=
         "Actions à mettre en oeuvre: critères de sélection")

