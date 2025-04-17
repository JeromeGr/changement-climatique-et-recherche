# tentative d'import des fichiers HAL par Damien 

library(tidyverse)
library(here)
library(arrow)
library(questionr)
library(readxl)
library(openxlsx)

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

# Fonction pour faire les listes par niveau d'agrégation disciplinaire -----
# Le plus important de cette fonction est un pivot longer pour pouvoir avoir une ligne par discipline (et donc les publis répétées autant de fois qu'il y a des disicplines)
# j'aurais bien aimé que le nom du fichier de sortie soit modifié dans la fonction mais je n'ai pas retrouvé comment faire ; de mémoire c'est assign 
# Depuis le regroupemement par section CNU cette partie ne sert plus

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

# CONSERVER LES APPELS ci-dessous mais normalement ne sert plus 
# MyNiv0 <- byniv(0)
# MyNiv1 <- byniv(1)
# MyNiv2 <- byniv(2)


# Regroupemeent par section du CNU---- 
## Base HAL en long avec tous les niveaux d'agregations des disciplines ----
HAL <- d |> 
  pivot_longer(
    cols = starts_with("dom"),
    names_to = "dom", 
    values_to = "domaines"
  ) |> 
  filter(!is.na(domaines)) |> 
  distinct(docid, halId_s, domaines, .keep_all = TRUE) |>   # je dédoublonne car manifestement bcp mettent trois fois le même code discipline ! 
  group_by(domaines) |> 
  mutate(NbRef = row_number(),
         MoyAut = round(mean(nbauteur),1),
         MedianAuteur = median(nbauteur),
         MinAut = min(nbauteur),
         MaxAut = max(nbauteur)
  ) |> 
  arrange(domaines, desc(NbRef)) |> 
  ungroup() |> 
  filter(domaines!="") 
  


## j'importe la base de Milan -----
domain <- read_excel("Recodage domaines HAL-sections CNU.xlsx", col_types = c("numeric", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text")) # j'ai modifié le fichier de Milan à la main pour n'avoir qu'une section par ligne 

## je modifie la base de Milan ------
df <- domain |> 
  mutate(
    domaine1  = str_sub(domaine1,  1, str_locate(domaine1, "=")[,"start"]-2), # je vire les libellés en clair des codes de Milan 
    domaine2  = str_sub(domaine2,  1, str_locate(domaine2, "=")[,"start"]-2),
    domaine3  = str_sub(domaine3,  1, str_locate(domaine3, "=")[,"start"]-2),
    domaine4  = str_sub(domaine4,  1, str_locate(domaine4, "=")[,"start"]-2),
    domaine5  = str_sub(domaine5,  1, str_locate(domaine5, "=")[,"start"]-2),
    domaine6  = str_sub(domaine6,  1, str_locate(domaine6, "=")[,"start"]-2),
    domaine7  = str_sub(domaine7,  1, str_locate(domaine7, "=")[,"start"]-2),
    domaine8  = str_sub(domaine8,  1, str_locate(domaine8, "=")[,"start"]-2),
    domaine9  = str_sub(domaine9,  1, str_locate(domaine9, "=")[,"start"]-2),
    domaine10 = str_sub(domaine10, 1, str_locate(domaine10,"=")[,"start"]-2),
    domaine11 = str_sub(domaine11, 1, str_locate(domaine11,"=")[,"start"]-2),
    domaine12 = str_sub(domaine12, 1, str_locate(domaine12,"=")[,"start"]-2),
      
    domaine1 = case_when(
      str_count(domaine1, fixed(".")) == 0 ~str_glue("0.{domaine1}"), # le fixed sert à dire qu'on n'est pas en regex car sinon le . correspond à n'importe quel caractère 
      str_count(domaine1, fixed(".")) == 1 ~str_glue("1.{domaine1}"), # je compte le nb de . pour en déduire le niveau d'agrégation et l'indiquer 
      str_count(domaine1, fixed(".")) == 2 ~str_glue("2.{domaine1}")),
    domaine2 = case_when(
      str_count(domaine2, fixed(".")) == 0 ~str_glue("0.{domaine2}"), 
      str_count(domaine2, fixed(".")) == 1 ~str_glue("1.{domaine2}"),
      str_count(domaine2, fixed(".")) == 2 ~str_glue("2.{domaine2}")),
    domaine3 = case_when(
      str_count(domaine3, fixed(".")) == 0 ~str_glue("0.{domaine3}"), 
      str_count(domaine3, fixed(".")) == 1 ~str_glue("1.{domaine3}"),
      str_count(domaine3, fixed(".")) == 2 ~str_glue("2.{domaine3}")),
    domaine4 = case_when(
      str_count(domaine4, fixed(".")) == 0 ~str_glue("0.{domaine4}"), 
      str_count(domaine4, fixed(".")) == 1 ~str_glue("1.{domaine4}"),
      str_count(domaine4, fixed(".")) == 2 ~str_glue("2.{domaine4}")),
    domaine5 = case_when(
      str_count(domaine5, fixed(".")) == 0 ~str_glue("0.{domaine5}"), 
      str_count(domaine5, fixed(".")) == 1 ~str_glue("1.{domaine5}"),
      str_count(domaine5, fixed(".")) == 2 ~str_glue("2.{domaine5}")),
    domaine6 = case_when(
      str_count(domaine6, fixed(".")) == 0 ~str_glue("0.{domaine6}"), 
      str_count(domaine6, fixed(".")) == 1 ~str_glue("1.{domaine6}"),
      str_count(domaine6, fixed(".")) == 2 ~str_glue("2.{domaine6}")),
    domaine7 = case_when(
      str_count(domaine7, fixed(".")) == 0 ~str_glue("0.{domaine7}"), 
      str_count(domaine7, fixed(".")) == 1 ~str_glue("1.{domaine7}"),
      str_count(domaine7, fixed(".")) == 2 ~str_glue("2.{domaine7}")),
    domaine8 = case_when(
      str_count(domaine8, fixed(".")) == 0 ~str_glue("0.{domaine8}"), 
      str_count(domaine8, fixed(".")) == 1 ~str_glue("1.{domaine8}"),
      str_count(domaine8, fixed(".")) == 2 ~str_glue("2.{domaine8}")),
    domaine9 = case_when(
      str_count(domaine9, fixed(".")) == 0 ~str_glue("0.{domaine9}"), 
      str_count(domaine9, fixed(".")) == 1 ~str_glue("1.{domaine9}"),
      str_count(domaine9, fixed(".")) == 2 ~str_glue("2.{domaine9}")),
    domaine10 = case_when(
      str_count(domaine10, fixed(".")) == 0 ~str_glue("0.{domaine10}"), 
      str_count(domaine10, fixed(".")) == 1 ~str_glue("1.{domaine10}"),
      str_count(domaine10, fixed(".")) == 2 ~str_glue("2.{domaine10}")),
    domaine11 = case_when(
      str_count(domaine11, fixed(".")) == 0 ~str_glue("0.{domaine11}"), 
      str_count(domaine11, fixed(".")) == 1 ~str_glue("1.{domaine11}"),
      str_count(domaine11, fixed(".")) == 2 ~str_glue("2.{domaine11}")),
    domaine12 = case_when(
      str_count(domaine12, fixed(".")) == 0 ~str_glue("0.{domaine12}"), 
      str_count(domaine12, fixed(".")) == 1 ~str_glue("1.{domaine12}"),
      str_count(domaine12, fixed(".")) == 2 ~str_glue("2.{domaine12}")),
    ) 


## fonction pour agréger par section du CNU----
fsection <- function(x){
toto <- HAL |> 
  filter(domaines %in% c(df$domaine1[{x}],df$domaine2[{x}], df$domaine3[{x}], df$domaine4[{x}],df$domaine5[{x}],df$domaine6[{x}],
                         df$domaine7[{x}], df$domaine8[{x}], df$domaine9[{x}], df$domaine10[{x}], df$domaine11[{x}], df$domaine12[{x}])) |> 
  # filter(domaines %in% c((!!mget(paste0("domaine",1:12, "[{{x}]") )))) |> 
  distinct(docid, .keep_all = TRUE) |> 
  mutate(section = df$code[{x}])
return(toto)
}

# pas très satisfaisant de le faire par un for... à retravailler 
for (i in 1:80) {
  assign(paste0("CNU_",i), fsection(i))
}

freqtab(CNU_35$domaines) # une simple vérif sur une section avec bcp de disciplines 

CNU_section <- bind_rows(mget(paste0("CNU_", 1:80)))

rm(list=paste0("CNU_",1:80))


CNUs <- CNU_section |> 
  group_by(section) |> 
  mutate(NbRef = row_number(),
         MoyAut = round(mean(nbauteur),1),
         MoyGeoAut = round(exp(mean(log(nbauteur))),1), 
         MedianAut = median(nbauteur),
         MinAut = min(nbauteur),
         MaxAut = max(nbauteur)
  ) |> 
  arrange(desc(NbRef)) |> 
  distinct(section, .keep_all = TRUE) |> 
  ungroup() |> 
  select(section, NbRef, MoyAut, MoyGeoAut, MedianAuteur, MinAut, MaxAut)|> 
  left_join(df |> rename(section=code) |> select(section, nom), by = "section") |> 
  relocate(section, nom) |> 
  arrange(section)


write.xlsx(CNUs,"StatsBySection.xlsx")
write.csv(CNUs,"StatsBySection.csv", row.names = FALSE)









####### pdt le zoom ----
df <- d |> 
  filter(dom1=="0.phys") |> 
  freqtable(dom2)

freq(df$dom2)

d |> 
  filter(is.na(dom2)) |> 
  count()


# Des tentatives infructueuses -------
# NE PAS FAIRE TOURNER ! ! ! -----
fNiv <-  function(x){
  str_glue("domaine{x}") = case_when(
    str_count(str_glue("domaine{x}", fixed(".")) == 0 ~str_glue("0.{domaine}{x}"), # le fixed sert à dire qu'on n'est pas en regex car sinon le . c'est n'importe quel caractère 
              str_count(domaine1, fixed(".")) == 1 ~str_glue("1.{domaine1}"),
              str_count(domaine1, fixed(".")) == 2 ~str_glue("2.{domaine1}")
    ))
}
dt <- as_tibble(map_at(df,4:6, fNiv))

# idée Milan : 
# Mais pour ton problème je me demande si le plus simple serait pas un truc du genre :
  
  domain_xlsx %>%
  group_by(section) %>%
  group_modify(function(df) {
    df$section bla bla
  })


####
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