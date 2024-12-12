# Calcul de l'accessibilité des aéroports
# TODO: tenir compte des aéroports des DOM et des gens qui y habitent

library(tidyverse)

# Eurostat, Airline traffic data by main airport, avia_tf_ala__custom_8431504
# https://ec.europa.eu/eurostat/databrowser/view/avia_tf_ala__custom_8431504/default/table?lang=en
trafic <- read.csv("Distances/avia_tf_ala_page_tabular.csv")

airports_frlim <- read.csv("Distances/airports France et limitrophe medium-large.csv", na.strings="")
airports_frlim$gps_code_country <- paste0(airports_frlim$iso_country, "_", airports_frlim$gps_code)

airports_frlim <- left_join(airports_frlim, trafic, by=c("gps_code_country"="rep_airp.TIME_PERIOD"))

dist_airports_res <- read.csv("Distances/export_metric_osrm aéroports res 2019.csv", sep=";", dec=",")
dist_airports_res <- left_join(dist_airports_res, airports_frlim, by=c("idDst"="iata_code"))
dist_airports_res$duree <- na_if(dist_airports_res$duree, -999999)
trafic_pond <- group_by(dist_airports_res, idSrc) %>%
    summarize(trafic_bisquare60=log(sum(X2019 * spgwr::gwr.bisquare(duree^2, 60), na.rm=TRUE) + 1),
              trafic_bisquare90=log(sum(X2019 * spgwr::gwr.bisquare(duree^2, 90), na.rm=TRUE) + 1),
              trafic_bisquare120=log(sum(X2019 * spgwr::gwr.bisquare(duree^2, 120), na.rm=TRUE) + 1),
              trafic_bisquare180=log(sum(X2019 * spgwr::gwr.bisquare(duree^2, 180), na.rm=TRUE) + 1),
              trafic_binaire=log(sum(X2019 * (duree < 90), na.rm=TRUE) + 1))
climat$res.aeroport.bisquare60 <- trafic_pond$trafic_bisquare60[match(climat$res.codeinsee, trafic_pond$idSrc)]
climat$res.aeroport.bisquare90 <- trafic_pond$trafic_bisquare90[match(climat$res.codeinsee, trafic_pond$idSrc)]
climat$res.aeroport.bisquare120 <- trafic_pond$trafic_bisquare120[match(climat$res.codeinsee, trafic_pond$idSrc)]
climat$res.aeroport.bisquare180 <- trafic_pond$trafic_bisquare180[match(climat$res.codeinsee, trafic_pond$idSrc)]
climat$res.aeroport.binaire <- trafic_pond$trafic_binaire[match(climat$res.codeinsee, trafic_pond$idSrc)]

dist_airports_trav <- read.csv("Distances/export_metric_osrm aéroports trav.csv", sep=";", dec=",")
dist_airports_trav <- left_join(dist_airports_trav, airports_frlim, by=c("idDst"="iata_code"))
dist_airports_trav$duree <- na_if(dist_airports_trav$duree, -999999)
trafic_pond <- group_by(dist_airports_trav, idSrc) %>%
    summarize(trafic_bisquare60=log(sum(X2019 * spgwr::gwr.bisquare(duree^2, 60), na.rm=TRUE) + 1),
              trafic_bisquare90=log(sum(X2019 * spgwr::gwr.bisquare(duree^2, 90), na.rm=TRUE) + 1),
              trafic_bisquare120=log(sum(X2019 * spgwr::gwr.bisquare(duree^2, 120), na.rm=TRUE) + 1),
              trafic_bisquare180=log(sum(X2019 * spgwr::gwr.bisquare(duree^2, 180), na.rm=TRUE) + 1),
              trafic_binaire=log(sum(X2019 * (duree < 90), na.rm=TRUE) + 1))
climat$trav.aeroport.bisquare60 <- trafic_pond$trafic_bisquare60[match(climat$trav.codeinsee, trafic_pond$idSrc)]
climat$trav.aeroport.bisquare90 <- trafic_pond$trafic_bisquare90[match(climat$trav.codeinsee, trafic_pond$idSrc)]
climat$trav.aeroport.bisquare120 <- trafic_pond$trafic_bisquare120[match(climat$trav.codeinsee, trafic_pond$idSrc)]
climat$trav.aeroport.bisquare180 <- trafic_pond$trafic_bisquare180[match(climat$trav.codeinsee, trafic_pond$idSrc)]
climat$trav.aeroport.binaire <- trafic_pond$trafic_binaire[match(climat$trav.codeinsee, trafic_pond$idSrc)]
