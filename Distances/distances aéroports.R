# Calcul de l'accessibilité des aéroports
# TODO: tenir compte des aéroports des DOM et des gens qui y habitent

library(tidyverse)

# Eurostat, Airline traffic data by main airport, avia_tf_ala__custom_8431504
# https://ec.europa.eu/eurostat/databrowser/view/avia_tf_ala__custom_8431504/default/table?lang=en
trafic <- read.csv("Distances/avia_tf_ala_page_tabular.csv")

# Airports Council International, Airport Industry Connectivity Report 2019
# https://www.aci-europe.org/air-connectivity.html
# Direct connectivity: These are the direct air services available from the airport – measured not just in terms of destinations, but also factoring in the frequency of flights to the same destination (so for example, an airport with 5 daily flights to another airport, will register a higher score than one with only 4).
# 
# Indirect connectivity: This measures the number of places people can fly to, through a connecting flight at hub airports from a particular airport. For example, flying from Cork to a hub airport such as Amsterdam Schiphol is a direct flight from A to B. But with the vast choice of onward destinations you can fly to from there – the large number of available onward connections from these airports expands the range of destinations available from the airport of origin. Indirect connections are weighted according to their quality, based on connecting time and detours involved with the indirect routing. For example, a flight from Manchester to Johannesburg via Paris-Charles de Gaulle will register a higher score than an alternative route via Doha.
# 
# Airport connectivity: As the name suggests, this is the most comprehensive metric for airport connectivity – considering both direct and indirect connectivity from the airport in question. Airport connectivity is defined as the sum of direct and indirect connectivity – thus measuring the overall level to which an airport is connected to the rest of the World, either by direct flights or indirect connections via other airports.
# 
# Hub connectivity: Hub connectivity is the key metric for any hub airport, big or small. Essentially, it measures the number of connecting flights that can be facilitated by the hub airport in question, taking into account a minimum and maximum connecting time and weighing the quality of the connections by the detour involved and connecting times.
airport_connectivity <- read.csv("Distances/airport-connectivity.csv")
hub_connectivity <- read.csv("Distances/hub-connectivity.csv")
airport_connectivity <- full_join(select(airport_connectivity, -airport),
                                  select(hub_connectivity, -airport), by="code")
airport_connectivity <- mutate(airport_connectivity,
                               hub_connectivity=coalesce(hub_connectivity, 0))

airports_frlim <- read.csv("Distances/airports France et limitrophe medium-large.csv", na.strings="")
airports_frlim$gps_code_country <- paste0(airports_frlim$iso_country, "_", airports_frlim$gps_code)

airports_frlim <- left_join(airports_frlim, trafic, by=c("gps_code_country"="rep_airp.TIME_PERIOD"))
airports_frlim <- left_join(airports_frlim, airport_connectivity, by=c("iata_code"="code"))

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
