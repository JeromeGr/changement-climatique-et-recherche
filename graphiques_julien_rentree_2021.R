# préambules

library(tidyverse)
library(ggplot2)
# load("climat.RData")
source("recodages.R", encoding = "UTF-8")
theme_set(theme_minimal() +
            theme(plot.title=element_text(size=11),
                  plot.caption=element_text(lineheight=1.1)))
update_geom_defaults("bar", list(fill="#045a8d"))
update_geom_defaults("col", list(fill="#045a8d"))

texte_legende <- "Source : enquête « Les personnels de la recherche face au changement climatique », Labos 1point5, 2020\nChamp : personnels affiliés à une unité du CNRS"
source_champ <- function(n=NA, extra=NA) {
  if(is.na(n))
    texte <- texte_legende
  else
    texte <- paste0(texte_legende, " (n=", n, ")")
  
  if(is.na(extra))
    labs(caption=texte)
  else
    labs(caption=paste(extra, texte, sep="\n"))
}




# sexe*enfants
climat$enfsexe <- NULL
climat$enfsexe[!is.na(climat$enfantsnb_rec)] <- 
  paste(climat$sexe[!is.na(climat$enfantsnb_rec)], 
        climat$enfantsnb_rec[!is.na(climat$enfantsnb_rec)])
freq(climat$enfsexe)

# sexe*age des enfants
climat$enfagesexe <- NULL
climat$enfagesexe[!is.na(climat$enfantsage_rec)] <- 
  paste(climat$sexe[!is.na(climat$enfantsage_rec)], 
        climat$enfantsage_rec[!is.na(climat$enfantsage_rec)])
## Réordonnancement de climat$enfagesexe
climat$enfagesexe <- factor(climat$enfagesexe,
                            levels = c(
                              "Femme Sans enfant", "Femme moins de 5 ans", "Femme Entre 5 et 15 ans",
                              "Femme Plus de 15 ans", "Homme Sans enfant", "Homme moins de 5 ans",
                              "Homme Entre 5 et 15 ans", "Homme Plus de 15 ans"
                            )
)





# # sous base seulement recherche
climat_recherche <- subset(climat, ! sitpro %in% c(
  "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
  "Chargé·e d'études/de mission","Adjoint·e technique",
  "Autre"))




# graphiques premiers résultats révisés ----

# reliquat de budget ----
#seulement sur ceux qui ont eu à en utiliser un

table(climat$reliquat.ordi)
ggplot(drop_na(climat[climat$reliquat.ordi!="Je n'ai pas eu à utiliser de reliquat de budget",], reliquat.ordi),
       aes(y=fct_relevel(reliquat.ordi), x=..prop.., group=1)) +
  geom_bar(fill=c("#74add1", "#d7191c")) +
  scale_x_continuous(labels=function(x) scales::percent(x, 1)) +
  coord_cartesian(xlim=c(0, 0.80)) +
  geom_text(aes(label=scales::percent(..prop.., accuracy=1, decimal.mark=",")),
            stat="count", position=position_dodge(.9),
            hjust=-0.2, size=3) +
  labs(x="Proportion des répondant·es", y="",
       title="Au cours des 5 dernières années vous est-il arrivé pour terminer
       un reliquat de budget d'acheter un ordinateur/écran/tablette 
       alors que ce n'était pas vraiment indispensable 
       (sur les répondant·es qui avaient un reliquat à utiliser) ?") + 
  source_champ(nrow(drop_na(climat, reliquat.ordi)),
               "Lecture : 35% des répondant·es qui avaient à utiliser un reliquat de budget l'on fait pour acheter du matériel non indispensable")






# Vols pour terrains, vols pour conférences ----

# vols par discipline (total des vols) ----

group_by(climat, discipline_agr3) %>%
  summarize(volshnum=mean(volshnum, na.rm=TRUE)) %>%
  ggplot(aes(x=fct_rev(discipline_agr3), y = volshnum)) +
  geom_segment(aes(x=fct_rev(discipline_agr3), xend=fct_rev(discipline_agr3), 
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



# vols par discipline (vols pour confs) ----

group_by(climat, discipline_agr3) %>%
  summarize(volsdist_totconf=mean(volsdist_totconf, na.rm=TRUE)) %>%
  ggplot(aes(x=fct_rev(discipline_agr3), y = volsdist_totconf)) +
  geom_segment(aes(x=fct_rev(discipline_agr3), xend=fct_rev(discipline_agr3), 
                   y=0, yend=volsdist_totconf), color="grey")+
  geom_point(color="steelblue", size=3) +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("heures de vol par an")+
  coord_flip()+
  scale_y_continuous(breaks=seq(0, 50, by=5)) +
  labs(caption="La ligne rouge indique la valeur sur la totalité des répondants")


# vols par discipline (vols pour terrain) ----

group_by(climat, discipline_agr3) %>%
  summarize(volsdist_totterrain=mean(volsdist_totterrain, na.rm=TRUE)) %>%
  ggplot(aes(x=fct_rev(discipline_agr3), y = volsdist_totterrain)) +
  geom_segment(aes(x=fct_rev(discipline_agr3), xend=fct_rev(discipline_agr3), 
                   y=0, yend=volsdist_totterrain), color="grey")+
  geom_point(color="steelblue", size=3) +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("heures de vol par an")+
  coord_flip()+
  scale_y_continuous(breaks=seq(0, 50, by=5)) +
  labs(caption="La ligne rouge indique la valeur sur la totalité des répondants")




# Pour ceux qui estiment qu'elles sont déjà très basses, par discipline----

a <- tapply(climat$volshnum[climat$solreducperso.conf=="Non, car elles sont déjà très basses"], 
            climat$discipline_agr3[climat$solreducperso.conf=="Non, car elles sont déjà très basses"], mean, na.rm=T)
b <- tapply(climat$volshnum, 
            climat$discipline_agr3, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c

# ordonné selon la moyenne pour les personnes qui pensent qu'ils ne peuvent plus réduire davantage
ggplot(table, aes(x=reorder(discipline,a), y=a))+ 
  geom_bar(stat="identity", fill="steelblue") +
  geom_segment(aes(x=reorder(discipline,a), xend=reorder(discipline,a), 
                   y=a, yend=b), color="orangered")+
  geom_point(y=b, color="orangered", size=3) +
  coord_flip() + labs(title="Seulement pour les personnes qui ne peuvent pas réduire leurs émissions 
                      liées aux vols pour se rendre à des conférences parce qu'elles sont déjà très basses:
                      Nombre moyen d'heures de vol pour se rendre à des conférences selon la discipline") +
  geom_hline(yintercept=mean(climat$volshnum, na.rm=TRUE), colour = "black", linetype="dashed") +
  labs(caption="Les points rouges indiquent le nombre moyen d'heures de vol dans chaque discipline. 
       La ligne noire en pointillés indique la valeur sur la totalité des répondants")+
  labs(y="heures de vol par an", x="Discipline")

#ordonné selon la moyenne d'ensemble par discipline
ggplot(table, aes(x=reorder(discipline,b), y=a))+ 
  geom_bar(stat="identity", fill="steelblue") +
  geom_segment(aes(x=reorder(discipline,a), xend=reorder(discipline,a), 
                   y=a, yend=b), color="orangered")+
  geom_point(y=b, color="orangered", size=3) +
  coord_flip() + labs(title="Seulement pour les personnes qui ne peuvent pas réduire leurs émissions 
                      liées aux vols pour se rendre à des conférences parce qu'elles sont déjà très basses:
                      Nombre moyen d'heures de vol pour se rendre à des conférences selon la discipline") +
  geom_hline(yintercept=mean(climat$volshnum, na.rm=TRUE), colour = "black", linetype="dashed") +
  labs(caption="Les points rouges indiquent le nombre moyen d'heures de vol dans chaque discipline. 
       La ligne noire en pointillés indique la valeur sur la totalité des répondants")+
  labs(y="heures de vol par an", x="Discipline")

# visualisation de l'écart moyenne discipline / moyenne je ne peux réduire davantage
c$écart <- c$b-c$a
ggplot(c, aes(x=reorder(discipline,b), y=écart))+ 
  geom_bar(stat="identity", fill="red") +
  coord_flip()



# Par discipline (ceux qui ne pensent pas pouvoir réduire plus)
a <- tapply(climat$volshnum[climat$solreducperso.conf=="Non, car elles sont déjà très basses"], 
            climat$discipline_agr3[climat$solreducperso.conf=="Non, car elles sont déjà très basses"], mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$discipline_agr3, mean, na.rm=T)

c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(x="Nb moyen d'heures de vol (conférences) des répondant.es l'estimant déjà très bas", 
       y="Distance (km) en avion (conférences) observée")



# tout le monde : par discipline ----

a <- tapply(climat$volsdist_totterrain, 
            climat$discipline_agr3, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$discipline_agr3, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données par discipline", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=950, y=4100, label="Moyenne (ensemble)", colour = "red", angle = 90)


# tout le monde, par statut ----
a <- tapply(climat$volsdist_totterrain, 
            climat$sitpro, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$sitpro, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données par statut", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=950, y=4100, label="Moyenne (ensemble)", colour = "red", angle = 90)


# Par degré de préoccupation ----
a <- tapply(climat$volsdist_totterrain, 
            climat$preoccupe, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$preoccupe, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données 
       en fonction du degré de préoccupation", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=250, y=2850, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=920, y=1000, label="Moyenne (ensemble)", colour = "red", angle = 90)

# selon l'évolution des émissions personnelles pour les confs ----
a <- tapply(climat$volsdist_totterrain, 
            climat$solevolges.conf, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$solevolges.conf, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données 
       selon l'évolution de l'usage de l'avion", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=750, y=3000, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=920, y=2100, label="Moyenne (ensemble)", colour = "red", angle = 90)


# selon l'impression de pouvoir diminuer ses émissions ----

a <- tapply(climat$volsdist_totterrain, 
            climat$solreducperso.conf, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$solreducperso.conf, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données 
       selon la possibilité de réduction des vols pour les conférences", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=750, y=3000, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=920, y=2100, label="Moyenne (ensemble)", colour = "red", angle = 90)




# seulement recherche

a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$solreducperso.conf, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$solreducperso.conf, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données 
       selon la possibilité de réduction des vols pour les conférences", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=750, y=3000, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=920, y=2100, label="Moyenne (ensemble)", colour = "red", angle = 90)




# selon le sexe du répondant et l'âge de ses enfants ----


a <- tapply(climat$volsdist_totterrain, 
            climat$enfagesexe, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$enfagesexe, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données 
       selon le sexe et l'âge des enfants", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=750, y=3000, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=920, y=2100, label="Moyenne (ensemble)", colour = "red", angle = 90)

# selon le fait d'avoir fait du tourisme ----
a <- tapply(climat$volsdist_totterrain, 
            climat$apportconf.tourisme, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$apportconf.tourisme, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Le dernier événement de ce type auquel vous avez participé vous a-t-il permis de visiter, de faire du tourisme ?", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1150, y=3000, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=920, y=5000, label="Moyenne (ensemble)", colour = "red", angle = 90)



# selon l'opinion sur la décroissance ----
freq(climat$opinionecolo.decroissance)
a <- tapply(climat$volsdist_totterrain, 
            climat$opinionecolo.decroissance, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$opinionecolo.decroissance, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="La décroissance est nécessaire pour faire face aux enjeux environnementaux", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1500, y=2800, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=850, y=3000, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits = c(0, 2000))


# selon l'opinion sur le fait de l'inutilité des efforts si les autres n'en font pas autant ----
a <- tapply(climat$volsdist_totterrain, 
            climat$opinionecolo.efforts, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$opinionecolo.efforts, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="[Il ne sert à rien que je fasse des efforts pour l'environnement si les autres ne font pas de même]", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1500, y=2800, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=850, y=3000, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits = c(0, 2000))

# les gens qui font que des confs sont un peu des gens de droite égoïstes


# selon le hindex ----

# recodage en tranches 1 ----
freq(climat$hindextranch)
freq(climat$hindextranch2)

a <- tapply(climat$volsdist_totterrain, 
            climat$hindextranch, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$hindextranch, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Selon le H index (recodage 1)", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1500, y=2800, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=850, y=3000, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits = c(0, 2000))+
  scale_y_continuous(limits = c(0, 6500))



# recodage en tranches 2 (pas le même résultat) ----
a <- tapply(climat$volsdist_totterrain, 
            climat$hindextranch2, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$hindextranch2, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Selon le H index (recodage 2)", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1500, y=2800, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=850, y=3000, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits = c(0, 2000))+
  scale_y_continuous(limits = c(0, 6500))

# Selon le nb de publis ----

# Recodage en tranches 1
freq(climat$nbpublistranch)
a <- tapply(climat$volsdist_totterrain, 
            climat$nbpublistranch, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$nbpublistranch, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Selon le H index", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1500, y=2800, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=850, y=3000, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits = c(0, 2000))


# Recodage en tranches 2

a <- tapply(climat$volsdist_totterrain, 
            climat$nbpublistranch2, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$nbpublistranch2, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Selon le H index", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1500, y=2800, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=850, y=3000, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits = c(0, 2000))





# Recherche seulement (sous-base) ----


# Sexe et enfants et vols conférences/données----


a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$enfagesexe, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$enfagesexe, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données 
       selon le sexe et l'âge des enfants (seulement le personnel recherche)", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat_recherche$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat_recherche$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=750, y=3400, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=1030, y=2100, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits=c(0, 1400))+
  scale_y_continuous(limits=c(0, 5500))





# Sexe  et vols conférences/données----


a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$sexe, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$sexe, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données 
       selon le sexe et l'âge des enfants (seulement le personnel recherche)", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat_recherche$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat_recherche$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=750, y=3400, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=1030, y=2100, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits=c(0, 1400))+
  scale_y_continuous(limits=c(0, 5500))



# boxplots publis et hindex ----

ggplot(climat_recherche)+
  geom_boxplot(aes(y=nbpublis, x=discipline_agr3, color=sexe))+ coord_flip()
ggplot(climat_recherche)+
  geom_boxplot(aes(y=hindex, x=discipline_agr3, color=sexe))+coord_flip()







# Part de femmes et nb d'h pour confs par discipline----


a <- cprop(table(climat_recherche$sexe, 
            climat_recherche$discipline_agr3))[1,-16]
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$discipline_agr3, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Répartition des disciplines selon la part de femmes 
       et la distance parcourue pour les conférences", 
       x="Part de femmes",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat_recherche$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=39.5, colour = "red", linetype="dashed") +
  annotate('text', x=750, y=3400, label="Moyenne (ensemble)", colour = "red")+ 
  scale_x_continuous(limits=c(0, 100))+
  scale_y_continuous(limits=c(0, 5500))





# Part de femmes et distance de vol pour terrain par discipline----


a <- cprop(table(climat_recherche$enfagesexe, 
                 climat_recherche$discipline_agr3))[2,-16]
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$discipline_agr3, mean, na.rm=T)
c <- cbind(a,b)
c <- as.data.frame(c)
c$discipline <- rownames(c)
table <- c
library(ggrepel)
ggplot(c, aes(x=a, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Répartition des disciplines selon la part de femmes avec un enfant de moins de 5 ans
       et la distance parcourue pour le recueil des données", 
       x="Part de femmes",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat_recherche$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=39.5, colour = "red", linetype="dashed") +
  annotate('text', x=750, y=3400, label="Moyenne (ensemble)", colour = "red")+ 
  scale_x_continuous(limits=c(0, 20))+
  scale_y_continuous(limits=c(0, 5500))


# ACP

summary(climat_recherche$volsdist_totconf)
summary(climat_recherche$volsdist_totterrain)
summary(climat_recherche$volsdist_totcours)
summary(climat_recherche$volsdist_totsejrech)
summary(climat_recherche$volsdist_totjury)
summary(climat_recherche$volsdist_totworkshop)
summary(climat_recherche$volsdist_toteval)
summary(climat_recherche$volsdist_totfinanc)



a <- tapply(climat$volsdist_totterrain, 
            climat$enfagesexe, mean, na.rm=T)
b <- tapply(climat$volsdist_totconf, 
            climat$enfagesexe, mean, na.rm=T)
c <- tapply(climat$volsdist_totcours, 
            climat$enfagesexe, mean, na.rm=T)
d <- tapply(climat$volsdist_totsejrech, 
            climat$enfagesexe, mean, na.rm=T)
e <- tapply(climat$volsdist_totjury, 
            climat$enfagesexe, mean, na.rm=T)
f <- tapply(climat$volsdist_totworkshop, 
            climat$enfagesexe, mean, na.rm=T)
g <- tapply(climat$volsdist_toteval, 
            climat$enfagesexe, mean, na.rm=T)
h <- tapply(climat$volsdist_totfinanc, 
            climat$enfagesexe, mean, na.rm=T)

motifs <- cbind(a, b, c, d, e, f, g, h)
colnames(motifs) <- c("confs", "terrain", "cours", "sejourrech", "jury", "workshop",
                 "eval", "financ")
library(FactoMineR)
library(explor)
library(factoextra)

acp <- PCA(motifs, scale.unit=F)


fviz_pca_biplot(acp, repel = T)+ xlim(-2.5, 4) + ylim (-3, 3)
fviz_pca_biplot(acp, repel = T)+ xlim(-2.5, 4) + ylim (-3, 3)

fviz_pca_ind(acp, repel = T)
fviz_pca_biplot(acp, axes = c(1,3), repel = T)+ xlim(-2.5, 4) + ylim (-3, 3)



freq(climat$discipline_agr3)

cprop(table(climat$preoccupe, climat$discipline_agr3))
cprop(table(climat$preoccupe, climat$sitpro))





# seulement recherche, discipline rapport terrain/conf ----

# tout le monde : par discipline ----

a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$discipline_agr3, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$discipline_agr3, mean, na.rm=T)
total <- tapply(climat_recherche$volsdist_tot, 
             climat_recherche$discipline_agr3, mean, na.rm=T)
c <- cbind(a,b,total)
c <- as.data.frame(c)
c$discipline <- rownames(c)
c$rapport <- c$b/c$a
table <- c
library(ggrepel)
ggplot(c, aes(x=total, y=rapport, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données par discipline", 
       x="Distance (km) totale en avion",
       y="Rapport entre la distance parcourue pour les conférences et la distance parcourue pour les données") +
   geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,14000))+
  scale_y_log10()


a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$discipline, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$discipline, mean, na.rm=T)
total <- tapply(climat_recherche$volsdist_tot, 
                climat_recherche$discipline, mean, na.rm=T)
c <- cbind(a,b,total)
c <- as.data.frame(c)
c$discipline <- substr(rownames(c),1)
c$rapport <- c$b/c$a
table <- c
library(ggrepel)
ggplot(c, aes(x=total, y=rapport, label = discipline))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données par discipline", 
       x="Distance (km) totale en avion",
       y="Rapport entre la distance parcourue pour les conférences et la distance parcourue pour les données") +
  geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,20000))+
  scale_y_log10()




# distance parcourue pour les conférences / distance totale

library(viridis)
ggplot(c, aes(x=total, y=b/total, label = rownames(c)))+
  geom_point(aes(size=b*2, 
                 color=b))+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences", 
       x="Distance (km) totale en avion",
       y="Part de la distance totale parcourue pour pour les conférences") +
  # geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,17000)) +
  # geom_smooth(color=black, fill=white method = "lm") +
  scale_color_viridis(option = "D")


# distance parcourue pour les données / distance totale

ggplot(c, aes(x=total, y=a/total, label = rownames(c)))+
  geom_point(aes(size=b, color=b))+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les données", 
       x="Distance (km) totale en avion",
       y="Part de la distance parcourue pour pour les données") +
  # geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,17000))+
  scale_color_viridis(option = "D")



# tout le monde : par statut ----

a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$sitpro, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$sitpro, mean, na.rm=T)
total <- tapply(climat_recherche$volsdist_tot, 
                climat_recherche$sitpro, mean, na.rm=T)
c <- cbind(a,b,total)
c <- as.data.frame(c)
c$discipline <- rownames(c)
c$rapport <- c$b/c$a
table <- c
library(ggrepel)
ggplot(c, aes(x=total, y=rapport, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données par discipline", 
       x="Distance (km) totale en avion",
       y="Rapport entre la distance parcourue pour les conférences et la distance parcourue pour les données") +
  geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,17000))+
  scale_y_log10()




a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$sitpro, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$sitpro, mean, na.rm=T)
total <- tapply(climat_recherche$volsdist_tot, 
                climat_recherche$sitpro, mean, na.rm=T)
c <- cbind(a,b,total)
c <- as.data.frame(c)
c$discipline <- rownames(c)
c$rapport <- c$b/c$a
table <- c
library(ggrepel)
ggplot(c, aes(x=total, y=rapport, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences et pour le recueil des données par discipline", 
       x="Distance (km) totale en avion",
       y="Rapport entre la distance parcourue pour les conférences et la distance parcourue pour les données") +
  geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,17000))+
  scale_y_log10()

ggplot(c, aes(x=total, y=a, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour le recueil des données et distance totale par discipline", 
       x="Distance (km) totale en avion",
       y="distance parcourue pour pour les données") +
  geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,17000))+
  scale_y_continuous(limits = c(0,17000))

ggplot(c, aes(x=total, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences", 
       x="Distance (km) totale en avion",
       y="distance parcourue pour pour les données") +
  geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,17000))



# distance parcourue pour les confs / distance totale

library(viridis)
ggplot(c, aes(x=total, y=b/total, label = rownames(c)))+
  geom_point(aes(size=b*2, 
                 color=b))+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences", 
       x="Distance (km) totale en avion",
       y="Part de la distance totale parcourue pour pour les conférences") +
  # geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,17000)) +
  # geom_smooth(color=black, fill=white method = "lm") +
  scale_color_viridis(option = "D")


# distance parcourue pour les données / distance totale

ggplot(c, aes(x=total, y=a/total, label = rownames(c)))+
  geom_point(aes(size=b, color=b))+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les données", 
       x="Distance (km) totale en avion",
       y="Part de la distance parcourue pour pour les données") +
  # geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,17000))+
  scale_color_viridis(option = "D")







# Part données * part conférences

ggplot(c, aes(x=b/total, y=a/total, label = rownames(c)))+
  geom_point(aes(size=total, color=total))+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Part de la distance parcourue pour les données et pour les conférences", 
       x="Part de la distance totale parcourue pour les conférences",
       y="Part de la distance totale parcourue pour les données") +
  # geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_x_continuous(limits = c(0,0.6))+
  scale_y_continuous(limits = c(-0.05,0.6))+
  scale_color_viridis(option = "D")


# part des motifs par discipline ----


a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totsejrech, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
c <- tapply(climat_recherche$volsdist_totworkshop, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
d <- tapply(climat_recherche$volsdist_totcours, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
e <- tapply(climat_recherche$volsdist_totjury, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
f <- tapply(climat_recherche$volsdist_totfinanc, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
g <- tapply(climat_recherche$volsdist_toteval, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
h <- tapply(climat_recherche$volsdist_totautre, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
j <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
total <- tapply(climat_recherche$volsdist_tot, 
            climat_recherche$discipline_agr4, mean, na.rm=T)

table <- cbind(a,b,c,d,e,f,g,h,j, total)

table <- cbind(a,b,c,d,e,f,g,h,j, total)
lprop(table[1:8,-10])


colnames(table) <- c("Terrain et données",
                     "Séjour de recherche",
                     "Workshop",
                     "Cours et formations",
                     "jury",
                     "financ",
                     "eval",
                     "autre",
                     "Conférences", 
                     "total")
table <- as.data.frame(as.table(table))


niveaux <- names(sort(tapply(climat_recherche$volsdist_tot, 
                             climat_recherche$discipline_agr4, mean, na.rm=T)))

table$Var1 <- fct_relevel(table$Var1, niveaux)

# table$Var1 <- fct_relevel(table$Var1, niveaux)
table$Var2 <- fct_recode(table$Var2,
                         "Autres" = "jury",
                         "Autres" = "eval",
                         "Autres" = "financ",
                         "Autres"="autre")

table$Var2 <- fct_relevel(table$Var2,
                          c("Terrain et données",
                            "Séjour de recherche",
                            "Cours et formations",
                            "Autres",
                            "Workshop",
                            "Conférences", 
                            "total")
)

#ordonnes par poids dans le total des distances parcourues
table$Var2 <- fct_relevel(table$Var2,
                          c("Autres","Cours et formations",
                            "Terrain et données",
                            "Workshop",
                            "Séjour de recherche",
                            "Conférences", 
                            "total"))


library(paletteer)

# cols <- brewer.pal(n=6, "Set2")

ggplot(table[table$Var2!="total",]) + 
  geom_bar(aes(x=Var1, y=Freq, fill=Var2), stat="identity")+
  coord_flip()+
  # scale_fill_manual(values=rev(as.vector(cols[1:6])))+
  scale_fill_paletteer_d("ggthemes::excel_Celestial")+
  labs(x="", y="Distance parcourue en avion en 2019 (km)", fill="")+
  guides(fill=guide_legend(reverse = TRUE))




ggplot(table[table$Var2=="total",],
       aes(x=Var1, y=Freq)) +
  stat_summary(geom="bar", fun=mean, na.rm=TRUE) +
  stat_summary(
    geom="errorbar", fun.data=mean_cl_normal, 
    na.rm=TRUE, width=0.2)+
  coord_flip()+
  # scale_fill_manual(values=rev(as.vector(cols[1:6])))+
  scale_fill_paletteer_d("ggthemes::excel_Celestial")+
  labs(x="", y="Distance parcourue en avion (km)")

# inquiétude sur la qualité des travaux en fonction du rapport vols terrains/confs

a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
total <- cprop(table(climat_recherche$solrisqreducavion.qual, 
                     climat_recherche$discipline_agr4))[1,-16]
total2 <- tapply(climat_recherche$volsdist_tot, 
                climat_recherche$discipline_agr4, mean, na.rm=T)
c <- cbind(a,b,total, total2)
c <- as.data.frame(c)
c$discipline <- rownames(c)
c$rapport <- c$b/c$a
library(ggrepel)
ggplot(c, aes(x=total, y=rapport, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences 
       et pour le recueil des données par discipline", 
       x="Risque de nuire à la qualité des travaux : c'est probable et c'est un problème",
       y="Rapport entre la distance parcourue pour les conférences et la distance parcourue pour les données") +
  geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_y_log10()


ggplot(c, aes(x=total, y=total2, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance totale parcourue en avion 
       et risque pour la qualité des travaux par discipline", 
       x="Risque de nuire à la qualité des travaux : c'est probable et c'est un problème",
       y="Distance totale parcourue en avion") 



ggplot(c, aes(x=total, y=a, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour le terrain 
       et risque pour la qualité des travaux par discipline", 
       x="Risque de nuire à la qualité des travaux : c'est probable et c'est un problème",
       y="Distance totale parcourue en avion pour le terrain") 

ggplot(c, aes(x=total, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences 
       et risque pour la qualité des travaux par discipline", 
       x="Risque de nuire à la qualité des travaux : c'est probable et c'est un problème",
       y="Distance totale parcourue en avion pour les conférences") 



# inquiétude sur l'accès aux données en fonction du rapport vols terrains/confs ----

a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$discipline_agr4, mean, na.rm=T)
total <- cprop(table(climat_recherche$solrisqreducavion.donnees, 
                     climat_recherche$discipline_agr4))[1,-16]
total2 <- tapply(climat_recherche$volsdist_tot, 
                 climat_recherche$discipline_agr4, mean, na.rm=T)
c <- cbind(a,b,total, total2)
c <- as.data.frame(c)
c$discipline <- rownames(c)
c$rapport <- c$b/c$a
library(ggrepel)
ggplot(c, aes(x=total, y=rapport, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences 
       et pour le recueil des données par discipline", 
       x="Risque de gener l'accès aux données : c'est probable et c'est un problème",
       y="Rapport entre la distance parcourue pour les conférences et la distance parcourue pour les données") +
  geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_y_log10()


ggplot(c, aes(x=total, y=total2, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance totale parcourue en avion 
       et risque  gener l'accès aux données", 
       x="Risque de gener l'accès aux données : c'est probable et c'est un problème",
       y="Distance totale parcourue en avion") 



ggplot(c, aes(x=total, y=a, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour le terrain 
       et risque pour la qualité des travaux par discipline", 
       x="Risque de  gener l'accès aux données : c'est probable et c'est un problème",
       y="Distance totale parcourue en avion pour le terrain") 

ggplot(c, aes(x=total, y=b, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences 
       et risque pour la qualité des travaux par discipline", 
       x="Risque de  gener l'accès aux données : c'est probable et c'est un problème",
       y="Distance totale parcourue en avion pour les conférences") 






a <- tapply(climat_recherche$volsdist_totterrain, 
            substr(climat_recherche$discipline,1,2), mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totconf, 
            substr(climat_recherche$discipline,1,2), mean, na.rm=T)
total <- cprop(table(climat_recherche$solrisqreducavion.donnees, 
                     substr(climat_recherche$discipline,1,2)), drop = F)[1,-71]
total2 <- tapply(climat_recherche$volsdist_tot, 
                 climat_recherche$discipline_agr4, mean, na.rm=T)
c <- cbind(a,b,total, total2)
c <- as.data.frame(c)
c$discipline <- rownames(c)
c$rapport <- c$b/c$a
library(ggrepel)
ggplot(c, aes(x=total, y=rapport, label = rownames(c)))+geom_point()+ 
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(title="Distance parcourue en avion pour les conférences 
       et pour le recueil des données par discipline", 
       x="Risque de gener l'accès aux données : c'est probable et c'est un problème",
       y="Rapport entre la distance parcourue pour les conférences et la distance parcourue pour les données") +
  geom_hline(yintercept=1, colour = "red", linetype="dashed") +
  # geom_vline(xintercept=mean(climat$climat_recherche, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  # annotate('text', x=3300, y=2900, label="Moyenne (ensemble)", colour = "red")+ 
  # annotate('text', x=6000, y=1, label="terrain=confs", colour = "red")+
  scale_y_log10()


# traitements par discipline ----

freq(climat$preoccupe)
freq(climat$preoccupe2)
freq(climat$pluspreoccupe)
freq(climat$changclim)
freq(climat$acthum)
freq(climat$recheco)
freq(climat$chgtpratique)
freq(climat$opinionecolo.cata)
freq(climat$opinionecolo.effondrement2)
freq(climat$solreducrech)
freq(climat$solreducperso.conf)
freq(climat$solreducperso.info)
freq(climat$solre)

names(climat)[1:50]

copie(cprop(table(climat$discipline, climat$preoccupe)))


a <- cbind(
  lprop(table(climat$discipline, climat$preoccupe2), drop = F)[,4]+
lprop(table(climat$discipline, climat$preoccupe2), drop = F)[,5],

lprop(table(climat$discipline, climat$pluspreoccupe), drop = F)[,1],

lprop(table(climat$discipline, climat$changclim), drop = F)[,1],

lprop(table(climat$discipline, climat$acthum), drop = F)[,3]+
  lprop(table(climat$discipline, climat$acthum), drop = F)[,4],

 lprop(table(climat$discipline, climat$recheco), drop = F)[,1],

lprop(table(climat$discipline, climat$chgtpratique), drop = F)[,1]+
  lprop(table(climat$discipline, climat$chgtpratique), drop = F)[,2],
lprop(table(climat$discipline, climat$chgtpratique), drop = F)[,1],

lprop(table(climat$discipline, climat$opinionecolo.cata), drop = F)[,1]+
  lprop(table(climat$discipline, climat$opinionecolo.cata), drop = F)[,2],

lprop(table(climat$discipline, climat$opinionecolo.effondrement2), drop = F)[,1]+
  lprop(table(climat$discipline, climat$opinionecolo.effondrement2), drop = F)[,2],

lprop(table(climat$discipline, climat$solreducrech), drop = F)[,1]+
  lprop(table(climat$discipline, climat$solreducrech), drop = F)[,2],
lprop(table(climat$discipline, climat$solreducrech), drop = F)[,1],

lprop(table(climat$discipline, climat$solreducperso.conf)[,-5], drop = F)[,4],
lprop(table(climat$discipline, climat$solreducperso.info)[,-5], drop = F)[,4],
lprop(table(climat$discipline, climat$solreducperso.exp)[,-5], drop = F)[,4],
lprop(table(climat$discipline, climat$solreducperso.donnees)[,-5], drop = F)[,4],
lprop(table(climat$discipline, climat$solreducperso.domicile)[,-5], drop = F)[,4]
)

copie(a)
copie(lprop(
  table(
    climat$discipline,
    climat$solreducperso.info)[,-5],
  drop = F))
copie(lprop(
  table(
    climat$discipline,
    climat$solreducperso.info),
  drop = F))

copie(lprop(
  table(
    climat$sitpro,
    climat$solreducperso.info)[,-5],
  drop = F))
copie(lprop(
  table(
    climat$sitpro,
    climat$solreducperso.info),
  drop = F))
# copie(lprop(
#   table(
#     climat$discipline, 
#     climat$solreducperso.exp)[,-5], 
#   drop = F))

freq(climat$solreducperso.donnees)
freq(climat$solreducperso.exp)

colnames(a) <- c("preoccupe : très ou extrêmement", 
                "beaucoup plus préoccupé qu'avant",
                "il y a certainement un changement climatique",
                "activités humaines causent le chgt climatique (unique ou grand rôle)",
                "a réorienté ses recherches",
                "exige des chgt profonds dans nos métiers : plutôt ou tout à fait ok",
                "dont tout à fait d'accord", 
                "catastrophe : plutôt ou tout à fait OK",
                "effonrement : plutôt ou tout à fait OK", 
                "Recherche : réduire d'au moins un tiers", 
                "Dont montrer l'exemple",
                "non à réduction des confs",
                "non à reduction du matos info", 
                "non à réduction des expériences",
                "non à réduction des vols pour données",
                "non à réduction des trajets domicile travail")

copie(a)

b <- cbind(
  lprop(table(climat$discipline_agr4, climat$preoccupe2), drop = F)[,4]+
    lprop(table(climat$discipline_agr4, climat$preoccupe2), drop = F)[,5],
  
  lprop(table(climat$discipline_agr4, climat$pluspreoccupe), drop = F)[,1],
  
  lprop(table(climat$discipline_agr4, climat$changclim), drop = F)[,1],
  
  lprop(table(climat$discipline_agr4, climat$acthum), drop = F)[,3]+
    lprop(table(climat$discipline_agr4, climat$acthum), drop = F)[,4],
  
  lprop(table(climat$discipline_agr4, climat$recheco), drop = F)[,1],
  
  lprop(table(climat$discipline_agr4, climat$chgtpratique), drop = F)[,1]+
    lprop(table(climat$discipline_agr4, climat$chgtpratique), drop = F)[,2],
  lprop(table(climat$discipline_agr4, climat$chgtpratique), drop = F)[,1],
  
  lprop(table(climat$discipline_agr4, climat$opinionecolo.cata), drop = F)[,1]+
    lprop(table(climat$discipline_agr4, climat$opinionecolo.cata), drop = F)[,2],
  
  lprop(table(climat$discipline_agr4, climat$opinionecolo.effondrement2), drop = F)[,1]+
    lprop(table(climat$discipline_agr4, climat$opinionecolo.effondrement2), drop = F)[,2],
  
  lprop(table(climat$discipline_agr4, climat$solreducrech), drop = F)[,1]+
    lprop(table(climat$discipline_agr4, climat$solreducrech), drop = F)[,2],
  lprop(table(climat$discipline_agr4, climat$solreducrech), drop = F)[,1],
  
  lprop(table(climat$discipline_agr4, climat$solreducperso.conf)[,-5], drop = F)[,4],
  lprop(table(climat$discipline_agr4, climat$solreducperso.info)[,-5], drop = F)[,4],
  lprop(table(climat$discipline_agr4, climat$solreducperso.exp)[,-5], drop = F)[,4],
  lprop(table(climat$discipline_agr4, climat$solreducperso.donnees)[,-5], drop = F)[,4],
  lprop(table(climat$discipline_agr4, climat$solreducperso.domicile)[,-5], drop = F)[,4]
)



colnames(b) <- c("preoccupe : très ou extrêmement", 
                 "beaucoup plus préoccupé qu'avant",
                 "il y a certainement un changement climatique",
                 "activités humaines causent le chgt climatique (unique ou grand rôle)",
                 "a réorienté ses recherches",
                 "exige des chgt profonds dans nos métiers : plutôt ou tout à fait ok",
                 "dont tout à fait d'accord", 
                 "catastrophe : plutôt ou tout à fait OK",
                 "effonrement : plutôt ou tout à fait OK", 
                 "Recherche : réduire d'au moins un tiers", 
                 "Dont montrer l'exemple",
                 "non à réduction des confs",
                 "non à reduction du matos info", 
                 "non à réduction des expériences",
                 "non à réduction des vols pour données",
                 "non à réduction des trajets domicile travail")

copie(b)

copie(table(climat$discipline_agr4))
copie(tapply(climat$volshnum, climat$discipline, mean, na.rm=T))

dispositifs_exp <- cbind(
lprop(table(climat$discipline_agr4, climat$solreducmateriel.lowtech)[,-5], drop = F)[,4],
lprop(table(climat$discipline_agr4, climat$solreducmateriel.util)[,-5], drop = F)[,4],
lprop(table(climat$discipline_agr4, climat$solreducmateriel.moins)[,-5], drop = F)[,4],
lprop(table(climat$discipline_agr4, climat$solreducmateriel.renouv)[,-5], drop = F)[,4]
)

climat$solrisqreducavion.insertion
colnames(dispositifs_exp) <- c("Matos expériences : non au lowtech", 
                 "Matos expériences : non à l'utiliser moins souvent",
                 "Matos expériences : en utilisant moins d'équipements",
                 "Matos expériences : en le renouvelant moins souvent")

copie(dispositifs_exp)




risques <- cbind(
  lprop(table(climat$discipline_agr4, climat$solrisqreducavion.avantages)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducavion.qual)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducavion.fin)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducavion.diffusion)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducavion.donnees)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducavion.insertion)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducavion.isoler)[,-5], drop = F)[,1]
)
lprop(table(climat$discipline_agr4, climat$solrisqreducavion.bureaucratie)[,-5], drop = F)[,1]
copie(lprop(table(climat$sitpro, climat$solrisqreducavion.bureaucratie)[,-5], drop = F)[,1])

colnames(risques) <- c("Risques : avantages du métier", 
                               "Risques : qualité des travaux",
                               "Risques : réduire financements",
                               "Risques : diffusion des travaux",
                               "Risques : gêner le recueil des données",
                               "Risques : insertion des jeunes",
                               "Risques : isoler la recherche")

copie(risques)




risques_matos <- cbind(
  lprop(table(climat$discipline_agr4, climat$solrisqreducmateriel.qual)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducmateriel.themes)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducmateriel.fin)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducmateriel.retard)[,-5], drop = F)[,1],
  lprop(table(climat$discipline_agr4, climat$solrisqreducmateriel.publi)[,-5], drop = F)[,1]
)

colnames(risques_matos) <- c("Risques_matos : qualité des travaux", 
                       "Risques_matos : chgt thèmes de recherche",
                       "Risques_matos : réduire financements",
                       "Risques_matos : faire prendre du retard",
                       "Risques_matos : réduire publis")

copie(risques_matos)



solutions_instits <- cbind(
  lprop(table(climat$discipline_agr4, climat$solinstit.compensation), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.train), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.bilanges), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.limitevols), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.selection), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.conf), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.vols6h), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.info), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.equip), drop = F)[,3],
  lprop(table(climat$discipline_agr4, climat$solinstit.vege), drop = F)[,3]
)



colnames(solutions_instits) <- c("Solutions instit. : compensation", 
                                 "Solutions instit. : train mm plus cher",
                                 "Solutions instit. : bilans ges",
                                 "Solutions instit. : limiter les vols",
                                 "Solutions instit. : critères de selection",
                                 "Solutions instit. : conf dans evaluations", 
                                 "Solutions instit. : pas de vols si moins 6h train",
                                 "Solutions instit. : pas renouveler matos info de -5ans",
                                 "Solutions instit. : equipements moins énergivores",
                                 "Solutions instit. : menus végés ou locaux")

copie(solutions_instits)






# traitements par statuts ----


freq(climat$sitp)

a <- cbind(
  lprop(table(climat$sitpro, climat$preoccupe2), drop = F)[,4]+
    lprop(table(climat$sitpro, climat$preoccupe2), drop = F)[,5],
  
  lprop(table(climat$sitpro, climat$pluspreoccupe), drop = F)[,1],
  
  lprop(table(climat$sitpro, climat$changclim), drop = F)[,1],
  
  lprop(table(climat$sitpro, climat$acthum), drop = F)[,3]+
    lprop(table(climat$sitpro, climat$acthum), drop = F)[,4],
  
  lprop(table(climat$sitpro, climat$recheco), drop = F)[,1],
  
  lprop(table(climat$sitpro, climat$chgtpratique), drop = F)[,1]+
    lprop(table(climat$sitpro, climat$chgtpratique), drop = F)[,2],
  lprop(table(climat$sitpro, climat$chgtpratique), drop = F)[,1],
  
  lprop(table(climat$sitpro, climat$opinionecolo.cata), drop = F)[,1]+
    lprop(table(climat$sitpro, climat$opinionecolo.cata), drop = F)[,2],
  
  lprop(table(climat$sitpro, climat$opinionecolo.effondrement2), drop = F)[,1]+
    lprop(table(climat$sitpro, climat$opinionecolo.effondrement2), drop = F)[,2],
  
  lprop(table(climat$sitpro, climat$solreducrech), drop = F)[,1]+
    lprop(table(climat$sitpro, climat$solreducrech), drop = F)[,2],
  lprop(table(climat$sitpro, climat$solreducrech), drop = F)[,1],
  
  lprop(table(climat$sitpro, climat$solreducperso.conf)[,-5], drop = F)[,4],
  lprop(table(climat$sitpro, climat$solreducperso.info)[,-5], drop = F)[,4],
  lprop(table(climat$sitpro, climat$solreducperso.exp)[,-5], drop = F)[,4],
  lprop(table(climat$sitpro, climat$solreducperso.donnees)[,-5], drop = F)[,4],
  lprop(table(climat$sitpro, climat$solreducperso.domicile)[,-5], drop = F)[,4]
)



colnames(a) <- c("preoccupe : très ou extrêmement", 
                 "beaucoup plus préoccupé qu'avant",
                 "il y a certainement un changement climatique",
                 "activités humaines causent le chgt climatique (unique ou grand rôle)",
                 "a réorienté ses recherches",
                 "exige des chgt profonds dans nos métiers : plutôt ou tout à fait ok",
                 "dont tout à fait d'accord", 
                 "catastrophe : plutôt ou tout à fait OK",
                 "effonrement : plutôt ou tout à fait OK", 
                 "Recherche : réduire d'au moins un tiers", 
                 "Dont montrer l'exemple",
                 "non à réduction des confs",
                 "non à reduction du matos info", 
                 "non à réduction des expériences",
                 "non à réduction des vols pour données",
                 "non à réduction des trajets domicile travail")

copie(a)



dispositifs_exp <- cbind(
  lprop(table(climat$sitpro, climat$solreducmateriel.lowtech)[,-5], drop = F)[,4],
  lprop(table(climat$sitpro, climat$solreducmateriel.util)[,-5], drop = F)[,4],
  lprop(table(climat$sitpro, climat$solreducmateriel.moins)[,-5], drop = F)[,4],
  lprop(table(climat$sitpro, climat$solreducmateriel.renouv)[,-5], drop = F)[,4]
)


colnames(dispositifs_exp) <- c("Matos expériences : non au lowtech", 
                               "Matos expériences : non à l'utiliser moins souvent",
                               "Matos expériences : en utilisant moins d'équipements",
                               "Matos expériences : en le renouvelant moins souvent")

copie(dispositifs_exp)



risques <- cbind(
  lprop(table(climat$sitpro, climat$solrisqreducavion.avantages)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducavion.qual)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducavion.fin)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducavion.diffusion)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducavion.donnees)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducavion.insertion)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducavion.isoler)[,-5], drop = F)[,1]
)

colnames(risques) <- c("Risques : avantages du métier", 
                       "Risques : qualité des travaux",
                       "Risques : réduire financements",
                       "Risques : diffusion des travaux",
                       "Risques : gêner le recueil des données",
                       "Risques : insertion des jeunes",
                       "Risques : isoler la recherche")

copie(risques)


copie(table(climat$sitpro))
copie(tapply(climat$volshnum, climat$sitpro, mean, na.rm=T))


risques_matos <- cbind(
  lprop(table(climat$sitpro, climat$solrisqreducmateriel.qual)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducmateriel.themes)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducmateriel.fin)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducmateriel.retard)[,-5], drop = F)[,1],
  lprop(table(climat$sitpro, climat$solrisqreducmateriel.publi)[,-5], drop = F)[,1]
)

colnames(risques_matos) <- c("Risques_matos : qualité des travaux", 
                             "Risques_matos : chgt thèmes de recherche",
                             "Risques_matos : réduire financements",
                             "Risques_matos : faire prendre du retard",
                             "Risques_matos : réduire publis")

copie(risques_matos)




solutions_instits <- cbind(
  lprop(table(climat$sitpro, climat$solinstit.compensation), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.train), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.bilanges), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.limitevols), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.selection), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.conf), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.vols6h), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.info), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.equip), drop = F)[,3],
  lprop(table(climat$sitpro, climat$solinstit.vege), drop = F)[,3]
)



colnames(solutions_instits) <- c("Solutions instit. : compensation", 
                             "Solutions instit. : train mm plus cher",
                             "Solutions instit. : bilans ges",
                             "Solutions instit. : limiter les vols",
                             "Solutions instit. : critères de selection",
                             "Solutions instit. : conf dans evaluations", 
                             "Solutions instit. : pas de vols si moins 6h train",
                             "Solutions instit. : pas renouveler matos info de -5ans",
                             "Solutions instit. : equipements moins énergivores",
                             "Solutions instit. : menus végés ou locaux")

copie(solutions_instits)



# part des motifs par statut ----



## Recodage de climat$sitpro en climat$sitpro_reduite
climat_recherche$sitpro_reduite <- NULL
climat_recherche$sitpro_reduite <- fct_recode(climat_recherche$sitpro,
  "Doctorant·e contractuel·le" = "Doctorant·e CIFRE"
)

a <- tapply(climat_recherche$volsdist_totterrain, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
b <- tapply(climat_recherche$volsdist_totsejrech, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
c <- tapply(climat_recherche$volsdist_totworkshop, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
d <- tapply(climat_recherche$volsdist_totcours, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
e <- tapply(climat_recherche$volsdist_totjury, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
f <- tapply(climat_recherche$volsdist_totfinanc, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
g <- tapply(climat_recherche$volsdist_toteval, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
h <- tapply(climat_recherche$volsdist_totautre, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
j <- tapply(climat_recherche$volsdist_totconf, 
            climat_recherche$sitpro_reduite, mean, na.rm=T)
total <- tapply(climat_recherche$volsdist_tot, 
                climat_recherche$sitpro_reduite, mean, na.rm=T)

table <- cbind(a,b,c,d,e,f,g,h,j, total)
lprop(table[1:8,-10])
100*table/colSums(table, na.rm=T)

colnames(table) <- c("Terrain et données",
                     "Séjour de recherche",
                     "Workshop",
                     "Cours et formations",
                     "jury",
                     "financ",
                     "eval",
                     "autre",
                     "Conférences", 
                     "total")

copie(round(100*table[,1:9]/table[,10],1))

table <- as.data.frame(as.table(table))
# niveaux <- names(sort(tapply(climat_recherche$volsdist_tot, 
                             # climat_recherche$sitpro, mean, na.rm=T)))


# table$Var1 <- fct_relevel(table$Var1, niveaux)
table$Var2 <- fct_recode(table$Var2,
                         "Autres" = "jury",
                         "Autres" = "eval",
                         "Autres" = "financ",
                         "Autres"="autre")

table$Var2 <- fct_relevel(table$Var2,
                          c("Terrain et données",
                            "Séjour de recherche",
                            "Cours et formations",
                            "Autres",
                            "Workshop",
                            "Conférences", 
                            "total")
)

#ordonnes par poids dans le total des distances parcourues
table$Var2 <- fct_relevel(table$Var2,
                          c("Autres","Cours et formations",
                            "Terrain et données",
                            "Workshop",
                            "Séjour de recherche",
                            "Conférences", 
                            "total"))



table$Var1 <- fct_relevel(table$Var1,
                          c("Directeur·rice de recherche",
                            "Professeur·e des universités",
                            "Chargé·e de recherche",
                            "Maître·sse de conférences",
                            "Ingénieur·e de recherche",
                            "Post-doctorant·e",
                            "ATER",
                            "Doctorant·e contractuel·le",
                            "Ingénieur·e d'études",
                            "Assistant ingénieur·e",
                            "Technicien·ne",
                            "Chargé·e d'études/de mission",
                            "Adjoint·e technique",
                            "Autre")
)

# install.packages("paletteer")
library(paletteer)

# cols <- brewer.pal(n=6, "Set2")

ggplot(table[table$Var2!="total"&
               table$Var1%in%c(levels(table$Var1)[1:8]),]) + 
  geom_bar(aes(x=fct_rev(Var1), y=Freq, fill=Var2), stat="identity")+
  coord_flip()+
  # scale_fill_manual(values=rev(as.vector(cols[1:6])))+
  scale_fill_paletteer_d("ggthemes::excel_Celestial")+
  labs(x="", y="Distance parcourue en avion en 2019 (km)", fill="")+
  guides(fill=guide_legend(reverse = TRUE))




#moyenne sdans l'ensemble

a <- mean(climat_recherche$volsdist_totterrain, na.rm=T)
b <- mean(climat_recherche$volsdist_totsejrech, na.rm=T)
c <- mean(climat_recherche$volsdist_totworkshop, na.rm=T)
d <- mean(climat_recherche$volsdist_totcours, na.rm=T)
e <- mean(climat_recherche$volsdist_totjury, na.rm=T)
f <- mean(climat_recherche$volsdist_totfinanc, na.rm=T)
g <- mean(climat_recherche$volsdist_toteval, na.rm=T)
h <- mean(climat_recherche$volsdist_totautre, na.rm=T)
j <- mean(climat_recherche$volsdist_totconf, na.rm=T)
total <- mean(climat_recherche$volsdist_tot, na.rm=T)


table <- cbind(a,b,c,d,e,f,g,h,j)

colnames(table) <- c("terrain",
                     "sejrech",
                     "workshop",
                     "cours",
                     "jury",
                     "financ",
                     "eval",
                     "autre",
                     "conf")
table
100*table/total



a <- sum(climat_recherche$volsdist_totterrain, na.rm=T)
b <- sum(climat_recherche$volsdist_totsejrech, na.rm=T)
c <- sum(climat_recherche$volsdist_totworkshop, na.rm=T)
d <- sum(climat_recherche$volsdist_totcours, na.rm=T)
e <- sum(climat_recherche$volsdist_totjury, na.rm=T)
f <- sum(climat_recherche$volsdist_totfinanc, na.rm=T)
g <- sum(climat_recherche$volsdist_toteval, na.rm=T)
h <- sum(climat_recherche$volsdist_totautre, na.rm=T)
j <- sum(climat_recherche$volsdist_totconf, na.rm=T)
total <- sum(climat_recherche$volsdist_tot, na.rm=T)


table <- cbind(a,b,c,d,e,f,g,h,j)

colnames(table) <- c("terrain",
                     "sejrech",
                     "workshop",
                     "cours",
                     "jury",
                     "financ",
                     "eval",
                     "autre",
                     "conf")
str(as.data.frame(table))
100*table/total
niveaux_tries <- names(sort(as.data.frame(table)))

# nombre de vols et moyenne

summary(climat_recherche$volshnum[
  climat_recherche$vols_dicho != "N'a pas volé en 2019"])
freq(climat_recherche$vols_dicho)
summary(climat_recherche$volsdist_tot[
  climat_recherche$vols_dicho != "N'a pas volé en 2019"])

a <- climat_recherche[climat_recherche$volsdist_tot >80000
                               & !is.na(climat_recherche$volsdist_tot),]

b <- cbind(a[,c("volsdist_tot","volshnum")], 
      a$volsdist_tot/a$volshnum)
colnames(b) <- c("distance", "temps", "vitesse de l'avion")

climat_recherche$vitesse_avion[climat_recherche$vols_dicho == "A volé en 2019"
                               & !is.na(climat_recherche$vols_dicho)] <- 
  climat_recherche$volsdist_tot[
    climat_recherche$vols_dicho == "A volé en 2019"
    & !is.na(climat_recherche$vols_dicho)]/
  climat_recherche$volshnum[
    climat_recherche$vols_dicho == "A volé en 2019"
    & !is.na(climat_recherche$vols_dicho)]

summary(climat_recherche$vitesse_avion)

ggplot(climat_recherche) + geom_point(
  aes(y=volshnum, x=volsdist_tot)
)

# jour de réponse et % préoccupé----

summary(climat$NumVague)

irec(climat$NumVague)
## Recodage de climat$NumVague en climat$reponse_avant_v1
climat$reponse_avant_r1 <- fct_recode(climat$NumVague,
  "oui" = "Après premier message",
  "non" = "Après première relance",
  "non" = "Après deuxième relance",
  "non" = "Après troisième relance",
  "non" = "Après quatrième relance"
)
climat$reponse_avant_r2 <- fct_recode(climat$NumVague,
                                      "oui" = "Après premier message",
                                      "oui" = "Après première relance",
                                      "non" = "Après deuxième relance",
                                      "non" = "Après troisième relance",
                                      "non" = "Après quatrième relance"
)
climat$reponse_avant_r3 <- fct_recode(climat$NumVague,
                                      "oui" = "Après premier message",
                                      "oui" = "Après première relance",
                                      "oui" = "Après deuxième relance",
                                      "non" = "Après troisième relance",
                                      "non" = "Après quatrième relance"
)
climat$reponse_avant_r4 <- fct_recode(climat$NumVague,
                                      "oui" = "Après premier message",
                                      "oui" = "Après première relance",
                                      "oui" = "Après deuxième relance",
                                      "oui" = "Après troisième relance",
                                      "non" = "Après quatrième relance"
)
climat$reponse_apres_r4 <- fct_recode(climat$NumVague,
                                      "oui" = "Après premier message",
                                      "oui" = "Après première relance",
                                      "oui" = "Après deuxième relance",
                                      "oui" = "Après troisième relance",
                                      "oui" = "Après quatrième relance"
)

cprop(table(climat$preoccupe2, climat$reponse_avant_r1))
cprop(table(climat$preoccupe2, climat$reponse_avant_r2))
cprop(table(climat$preoccupe2, climat$reponse_avant_r3))
cprop(table(climat$preoccupe2, climat$reponse_avant_r4))
cprop(table(climat$preoccupe2, climat$reponse_apres_r4))


cprop(table(climat$preoccupe2, climat$reponse_avant_r))
cprop(table(climat$preoccupe2, climat$reponse_avant_r3))
cprop(table(climat$preoccupe2, climat$reponse_avant_r4))
cprop(table(climat$preoccupe2, climat$reponse_apres_r4))

a <- cprop(table(climat$preoccupe2, climat$NumVague))
a <- as.data.frame(a[1:6,])
## Réordonnancement de a$Var1
a$Var1 <- factor(a$Var1,
  levels = c(
    "Pas du tout préoccupé·e", "Un peu préoccupé·e", "Sans opinion",
    "Assez préoccupé·e", "Très préoccupé·e", "Extrêmement préoccupé·e"
  )
)
ggplot(a) + geom_bar(aes(x=Var2, y=Freq, fill=Var1), stat="identity")+
  scale_fill_manual(
    values=c(
      "#74add1", "#fee090","light grey", "#fdae61", "#f46d43", "#d73027"))+
  theme(axis.text.x = element_text(angle = 90))  


  
# 
# pal <- RColorBrewer::brewer.pal(9, "RdYlBu")
# ggplot(drop_na(climat, preoccupe),
#        aes(y=fct_relevel(preoccupe, "Sans opinion"), x=..prop.., group=1)) +
#   geom_bar(fill=c("light grey", "#74add1", "#fee090", "#fdae61", "#f46d43", "#d73027")) +
#   scale_x_continuous(labels=function(x) scales::percent(x, 1)) +
#   coord_cartesian(xlim=c(0, 0.42)) +
#   geom_text(aes(label=scales::percent(..prop.., accuracy=0.1, decimal.mark=",")),
#             stat="count", position=position_dodge(.9),
#             hjust=-0.2, size=3) +
#   labs(x="Proportion des répondant·es", y="",
#        title="Dans quelle mesure êtes-vous préoccupé·e par le changement climatique ?") +
#   source_champ(nrow(drop_na(climat, preoccupe)),
#                "Lecture : 31,7% des répondant·es sont extrêmement préoccupé·es par le changement climatique")


# durée du séjour ----


# nb de répondants par vols (module vols) ----

freq(climat_recherche$volsmotif1)

rowSums(
table(climat_recherche$discipline_agr4,
      climat_recherche$volsmotif1)
)

freq(climat_recherche$opinionecolo.cata)









freq(climat_recherche$tpsdomtrav.avion_h)
freq(climat$tpsdomtrav.urbain_m)
freq(climat_recherche$tpsdomtrav.tgv_h)
freq(climat_recherche$tpsdomtrav.tgvMin)

freq(climat$solreducperso.conf)

climat$volsdist_totconfreu<-climat$volsdist_totconf+climatRegr$volsdist_totworkshop + climatRegr$volsdist_totjury + climatRegr$volsdist_totfinanc +climatRegr$volsdist_toteval
climat$volsdist_totconfreu_tranch2<- cut(climat$volsdist_totconfreu,
                                             include.lowest = TRUE,
                                             right = TRUE,
                                             breaks = c(0, 0.1, 1500, 7000, 30000, 100000))

freq(climat$reducrechexemp)
lprop(table(climat$solevolges.conf, climat$reducrechexemp))
lprop(table(climat$solevolges.conf, climat$volsdist_totconfreu_tranch2))

