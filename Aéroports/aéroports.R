# climat doit être chargé séparément

library(tidyverse)

airports <- read.csv("Aéroports/airports.csv", na.strings="")
# En cas d'homonymie, on prend le plus grand aéroport, puis celui qui a un code IATA,
# puis celui déclaré comme ouvert, puis l'européen
# Note : les petits aéroports français sont dans la liste fournie dans le questionnaire
unique_airports <- arrange(filter(airports,
                                  type %in% c("large_airport", "medium_airport", "small_airport")),
                           recode(type, "large"="1", "medium"="2", "small"="3"),
                           if_else(!is.na(iata_code), 1, 2),
                           if_else(scheduled_service == "yes", 1, 2),
                           if_else(continent == "EU", 1, 2))
unique_airports <- unique_airports[!duplicated(unique_airports$municipality),]

liste <- read.csv("Aéroports/liste aéroports.csv", fileEncoding="UTF-8")
liste <- left_join(liste, unique_airports, by="municipality")


traductions <- read.csv("Aéroports/grands aéroports traductions.csv",
                        header=FALSE, fileEncoding="UTF-8")
names(traductions) <- c("municipality", "traduction")
traductions <- filter(traductions, municipality != "")

liste <- left_join(liste, traductions, by="municipality")
liste$ville <- coalesce(liste$traduction, liste$municipality)

# Liste recodée à la main
# Quand la ville mentionnée n'a pas d'aéroport, ou un aéroport sans code IATA,
# on retient l'aéroport le plus proche
nonreconnus <- read.csv("Aéroports/aéroports non reconnus.csv", fileEncoding="UTF-8")

for(var in c("depart1", "arrivee1", "depart2", "arrivee2",
             "depart3", "arrivee3", "depart4", "arrivee4",
             "depart5", "arrivee5")) {
    tmp <- climat
    tmp$ville <- tmp[[paste0("vols", var)]]
    tmp <- mutate(tmp,
                  villepropre=na_if(trimws(tolower(gsub("[(/,].*$", "", ville))), ""),
                  villecorrig=if_else(villepropre %in% nonreconnus$entree,
                                      tolower(nonreconnus$ville[match(villepropre, nonreconnus$entree,
                                                                      incomparables=NA)]),
                                      villepropre),
                  code=coalesce(liste$iata_code[match(villecorrig, tolower(liste$ville),
                                                      incomparables=NA)],
                                liste$iata_code[match(villecorrig, gsub("/.*$", "", tolower(liste$ville)),
                                                      incomparables=NA)],
                                liste$iata_code[match(villecorrig, tolower(liste$municipality),
                                                      incomparables=NA)],
                                unique_airports$iata_code[match(villecorrig, tolower(unique_airports$municipality),
                                                                incomparables=NA)]))
    stopifnot(!any(is.na(tmp$code[!is.na(tmp$villepropre)])))
    climat[paste0("vols", var, "code")] <- tmp$code
    climat[paste0("vols", var, "pays")] <- airports$iso_country[match(tmp$code, airports$iata_code,
                                                                      incomparables=NA)]
    rm(tmp)
}
for(i in 1:5) {
    depart <- match(climat[[paste0("volsdepart", i, "code")]],
                    airports$iata_code, incomparables=NA)
    arrivee <- match(climat[[paste0("volsarrivee", i, "code")]],
                     airports$iata_code, incomparables=NA)
    tmp <- transmute(climat,
                     lat1=airports$latitude_deg[depart],
                     lat2=airports$latitude_deg[arrivee],
                     lon1=airports$longitude_deg[depart],
                     lon2=airports$longitude_deg[arrivee])
    climat[[paste0("vols", i, "dist")]] <-
        with(tmp, round(geosphere::distHaversine(cbind(lon1, lat1), cbind(lon2, lat2))/1000/100)*100)
}

# Vérification manuelle des risques d'ambiguïté :
# on calcule tous les noms de ville qui apparaissent plusieurs fois dans la liste,
# sont utilisés dans les données, et ont une certaine distance entre eux
# L'idée est de vérifier si des libellés incluent des précisions indiquant qu'il
# n'est pas fait référence à la ville la plus évidente, mais à une ville plus petite :
# au-dessus, on a retenu l'aéroport le plus grand en cas d'ambiguité
utilise <- data.frame(code=c(climat$volsdepart1code, climat$volsarrivee1code,
                             climat$volsdepart2code, climat$volsarrivee2code,
                             climat$volsdepart3code, climat$volsarrivee3code,
                             climat$volsdepart4code, climat$volsarrivee4code,
                             climat$volsdepart5code, climat$volsarrivee5code),
                      texte=c(climat$volsdepart1, climat$volsarrivee1,
                              climat$volsdepart2, climat$volsarrivee2,
                              climat$volsdepart3, climat$volsarrivee3,
                              climat$volsdepart4, climat$volsarrivee4,
                              climat$volsdepart5, climat$volsarrivee5))
unique_utilise <- unique(utilise$code)
ambig_airports <- airports %>%
    group_by(municipality) %>%
    filter(n() > 1 &
               any(!is.na(iata_code) & iata_code %in% unique_utilise)) %>%
    mutate(dist=sqrt(var(latitude_deg) + var(longitude_deg))) %>%
    filter(dist > 1) %>%
    arrange(dist)

sort(unique(tolower(utilise$texte[utilise$code %in% ambig_airports$iata_code])))