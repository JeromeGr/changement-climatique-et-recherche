# préambules

library(tidyverse)
library(ggplot2)
load("climat.RData")
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


# selon le sexe du répondant et l'âge de ses enfants ----

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
  labs(title="Selon le H index", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1500, y=2800, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=850, y=3000, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits = c(0, 2000))



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
  labs(title="Selon le H index", 
       x="Distance (km) en avion pour le recueil des données",
       y="Distance (km) en avion pour les conférences") +
  geom_hline(yintercept=mean(climat$volsdist_totconf, na.rm=TRUE), colour = "red", linetype="dashed") +
  geom_vline(xintercept=mean(climat$volsdist_totterrain, na.rm=TRUE), colour = "red", linetype="dashed")+ 
  annotate('text', x=1500, y=2800, label="Moyenne (ensemble)", colour = "red")+ 
  annotate('text', x=850, y=3000, label="Moyenne (ensemble)", colour = "red", angle = 90)+
  scale_x_continuous(limits = c(0, 2000))

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

# # sous base seulement recherche
# climat_recherche <- climat[!climat$sitpro2 %in% c(
#   "Ingénieur·e d'études", "Assistant ingénieur·e", "Technicien·ne",
#   "Chargé·e d'études/de mission","Adjoint·e technique",
#   "Autre"
# ),]




ggplot(climat_recherche)+
  geom_boxplot(aes(y=nbpublis, x=discipline_agr3, color=sexe))+ coord_flip()
ggplot(climat_recherche)+
  geom_boxplot(aes(y=hindex, x=discipline_agr3, color=sexe))+coord_flip()


