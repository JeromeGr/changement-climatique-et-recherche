# tentative d'import des fichiers HAL par Damien 

library(tidyverse)
library(here)
library(arrow)


# création de la base des publications ------
setwd(here("HAL_in"))

d <- read.csv("2017-1.csv") |> 
  bind_rows(read.csv("2017-2.csv")) |> 
  bind_rows(read.csv("2017-3.csv")) |> 
  bind_rows(read.csv("2017-4.csv")) |> 
  bind_rows(read.csv("2017-5.csv")) |> 
  bind_rows(read.csv("2017-6.csv")) |> 
  bind_rows(read.csv("2017-7.csv")) |> 
  bind_rows(read.csv("2017-8.csv")) |> 
  bind_rows(read.csv("2017-9.csv")) |> 
  bind_rows(read.csv("2017-10.csv")) |> 
  bind_rows(read.csv("2018-1.csv")) |> 
  bind_rows(read.csv("2018-2.csv")) |> 
  bind_rows(read.csv("2018-3.csv")) |> 
  bind_rows(read.csv("2018-4.csv")) |> 
  bind_rows(read.csv("2018-5.csv")) |> 
  bind_rows(read.csv("2018-6.csv")) |> 
  bind_rows(read.csv("2018-7.csv")) |> 
  bind_rows(read.csv("2018-8.csv")) |> 
  bind_rows(read.csv("2018-9.csv")) |> 
  bind_rows(read.csv("2018-10.csv")) |> 
  bind_rows(read.csv("2019-1.csv")) |> 
  bind_rows(read.csv("2019-2.csv")) |> 
  bind_rows(read.csv("2019-3.csv")) |> 
  bind_rows(read.csv("2019-4.csv")) |> 
  bind_rows(read.csv("2019-5.csv")) |> 
  bind_rows(read.csv("2019-6.csv")) |> 
  bind_rows(read.csv("2019-7.csv")) |> 
  bind_rows(read.csv("2019-8.csv")) |> 
  bind_rows(read.csv("2019-9.csv")) |> 
  bind_rows(read.csv("2019-10.csv")) |>
  bind_rows(read.csv("2019-11.csv")) |> 
  distinct(docid, .keep_all = TRUE)  # attention je vire les doublons sur docid, mais en fait ça ne sert à rien 

write_parquet(d, "HAL_in.parquet")


d <- read_parquet("Hal_in.parquet")
# Comptage du nombre d'auteurs -----

d <- d |> 
  mutate(
  nbauteur = str_count(authFullName_s,",") +1
)

# t <- d |> 
#   filter(nbauteur > 1000) |> 
#   arrange(desc(nbauteur))
# write.csv(t,"testnbauteur.csv")

# séparation des dommaines dans autant de variables -----
d <- d |> 
  separate_wider_delim(
    domain_s, 
    ",",
    names = c(paste0("dom", 1:60)), 
    too_few = "align_start")
  
# Tidying la base en longeur pour pouvoir compter les publis par domaines ----
## pour toute la base ----
dd <- d |> 
  pivot_longer(
    cols = starts_with("dom"),
    names_to = "dom", 
    values_to = "domaines"
  ) |> 
  filter(!is.na(domaines)) |> 
  filter(substr(domaines,1,2)=="0.")

## Pour le niveau le plus agrégé 0. Niv0-----
Niv0 <- d |> 
  pivot_longer(
    cols = starts_with("dom"),
    names_to = "dom", 
    values_to = "domaines"
  ) |> 
  filter(!is.na(domaines) & (substr(domaines,1,2)=="0.")) |> 
  distinct(docid, halId_s, domaines, .keep_all = TRUE)  # je dédoublonne car manifestement bcp mettent trois fois le même code discipline ! 

## Pour le Niv1 d'agrégation ----
Niv1 <- d |> 
  pivot_longer(
    cols = starts_with("dom"),
    names_to = "dom", 
    values_to = "domaines"
  ) |> 
  filter(!is.na(domaines) & (substr(domaines,1,2)=="1.")) |> 
  distinct(docid, halId_s, domaines, .keep_all = TRUE)  # je dédoublonne car manifestement bcp mettent trois fois le même code discipline ! 


## Pour le Niv2 d'agrégation ----
Niv2 <- d |> 
  pivot_longer(
    cols = starts_with("dom"),
    names_to = "dom", 
    values_to = "domaines"
  ) |> 
  filter(!is.na(domaines) & (substr(domaines,1,2)=="2."))|> 
  distinct(docid, halId_s, domaines, .keep_all = TRUE)  # je dédoublonne car manifestement bcp mettent trois fois le même code discipline ! 


# Obtenir la liste des discipline par niveau 0, 1 et 2 ----
listNiv0 <- Niv0 |> 
  distinct(domaines) |> 
  arrange(domaines)

listNiv1 <- Niv1 |> 
  distinct(domaines) |> 
  arrange(domaines)

listNiv2 <- Niv2 |> 
  distinct(domaines) |> 
  arrange(domaines)

write.csv(listNiv0, "Niv0.csv")
write.csv(listNiv1, "Niv1.csv")
write.csv(listNiv2, "Niv2.csv")

# Création des tables avec les données agrégées de publication par niveau de publi-----
Niv0a <- Niv0 |> 
  group_by(domaines) |> 
  mutate(NbRef = row_number(),
         MoyAut = round(mean(nbauteur),1),
         MedianAuteur = median(nbauteur),
         MinAut = min(nbauteur),
         MaxAut = max(nbauteur)
         ) |> 
  arrange(domaines, desc(NbRef)) |> 
  distinct(domaines, .keep_all = TRUE) |> 
  ungroup() |> 
  select(domaines, NbRef, MoyAut, MedianAuteur, MaxAut, MinAut) 

write_csv(Niv0a, "StatsNiv0.csv") 


Niv1a <- Niv1 |> 
  group_by(domaines) |> 
  mutate(NbRef = row_number(),
         MoyAut = round(mean(nbauteur),1),
         MedianAuteur = median(nbauteur),
         MinAut = min(nbauteur),
         MaxAut = max(nbauteur)
  ) |> 
  arrange(domaines, desc(NbRef)) |> 
  distinct(domaines, .keep_all = TRUE) |> 
  ungroup() |> 
  select(domaines, NbRef, MoyAut, MedianAuteur, MaxAut, MinAut) 

write_csv(Niv1a, "StatsNiv1.csv")  

Niv2a <- Niv2 |> 
  group_by(domaines) |> 
  mutate(NbRef = row_number(),
         MoyAut = round(mean(nbauteur),1),
         MedianAuteur = median(nbauteur),
         MinAut = min(nbauteur),
         MaxAut = max(nbauteur)
  ) |> 
  arrange(domaines, desc(NbRef)) |> 
  distinct(domaines, .keep_all = TRUE) |> 
  ungroup() |> 
  select(domaines, NbRef, MoyAut, MedianAuteur, MaxAut, MinAut) 

write_csv(Niv2a, "StatsNiv2.csv") 



# Des tentatives infructueuses -------
# NE PAS FAIRE TOURNER ! ! ! -----
listdossiers<- list.files(recursive = TRUE, full.names = FALSE) |> 
  as_tibble() |> 
  mutate(id = str_glue("d{row_number()}"))


monimport <- function(entree,sortie){
  retour <<- read.csv(entree)
  return(retour)
}

monimport <- function(entree,sortie){
  {{sortie}} <<- read.csv(entree)
  # return(sortie)
}
monimport("2019-11.csv",d1)
rm(sortie)
d2 <- monimport("2019-11.csv",d1)


X2019_11b <- read_csv("2019-11.csv")

rm(d1)
rm(d2)
rbind()



setwd(here())