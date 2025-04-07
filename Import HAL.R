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
  distinct(docid, .keep_all = TRUE)  # attention je vire les doublons sur docid, mais en fait ça ne sert plus à rien 

write_parquet(d, "HAL_in.parquet")


d <- read_parquet("Hal_in.parquet")


# Comptage du nombre d'auteurs -----

d <- d |> 
  mutate(
  nbauteur = str_count(authFullName_s,",") +1
)


# séparation des dommaines dans autant de variables -----
d <- d |> 
  separate_wider_delim(
    domain_s, 
    ",",
    names = c(paste0("dom", 1:60)), 
    too_few = "align_start")

# Fonction pour faire les listes par niveau d'agrégation disciplinaire 
byniv <- function(niveau){
  toto <- d |> 
    pivot_longer(
      cols = starts_with("dom"),
      names_to = "dom", 
      values_to = "domaines"
    ) |> 
    filter(!is.na(domaines) & (substr(domaines,1,2)==str_glue("{niveau}."))) |> 
    distinct(docid, halId_s, domaines, .keep_all = TRUE) |>   # je dédoublonne car manifestement bcp mettent trois fois le même code discipline ! 
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
  
  write.csv(toto, str_glue("StatsNiv{niveau}.csv"), row.names = FALSE)
  
  titi <- toto |> select(domaines)
  write.csv(titi, str_glue("DisciplinesNiv{niveau}.csv"), row.names = FALSE)
  
  return(toto)
}

MyNiv0 <- byniv(0)
MyNiv1 <- byniv(1)
MyNiv2 <- byniv(2)











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