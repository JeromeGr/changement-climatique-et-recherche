using DataFrames, CSV, Statistics
cd("/home/milan/Recherche/Enquête climat/Enquête 2/")
airports = DataFrame(CSV.File("airports.csv"))
trads = DataFrame(CSV.File("grands aéroports traductions.csv"))
pays = Dict("AU" => "Australie", "BO" => "Bolivie", "BZ" => "Belize", "CA" => "Canada",
            "CO" => "Colombie", "CL" => "Chili",
            "DO" => "Rép. dominicaine", "ES" => "Espagne", "EG" => "Egypte",
            "GB" => "Grande-Bretagne", "GR" => "Grèce", "GY" => "Guyana", "IN" => "Inde",
            "JM" => "Jamaïque", "KY" => "Iles Caïman", "MX" =>  "Mexique", "PR" => "Porto Rico",
            "VE" => "Venezuela", "VU" => "Vanuatu", "WF" => "Wallis et Futuna",
            "US-AL" => "Alabama", "US-AR" => "Arkansas", "US-CA" => "Californie",
            "US-GA" => "Georgia", "US-IL" => "Illinois",
            "US-ME" => "Maine", "US-MO" => "Missouri", "US-MN" => "Minnesota",
            "US-MS" => "Mississippi", "US-NH" => "New Hampshire", "US-NY" => "New York",
            "US-OH" => "Ohio", "US-OR" => "Oregon", "US-SC" => "Caroline du Sud",
            "US-TX" => "Texas", "US-WI" => "Wisconsin", "US-WV" => "Virginie-Occidentale")


filter!(x -> x.scheduled_service == "yes" && x.type in ("small_airport", "medium_airport", "large_airport"), airports)
gd2 = groupby(airports, [:municipality, :iso_country])
dupsgd = gd2[[nrow(g) > 1 for g in gd2]]
dists = combine(dupsgd, [:latitude_deg, :longitude_deg] => ((x, y) -> mean(sqrt.((x .- mean(x)).^2 + (y .- mean(y)).^2))) => :mean_dist)
sort!(dists, :mean_dist, rev=true)

gd = groupby(airports, :municipality)
dupsgd = gd[[nrow(g) > 1 for g in gd]]
dupsdists = combine(dupsgd, [:latitude_deg, :longitude_deg] => ((x, y) -> mean(sqrt.((x .- mean(x)).^2 + (y .- mean(y)).^2))) => :mean_dist)
filter!(x -> x.mean_dist > 1, dupsdists)
sort!(dupsdists, :mean_dist, rev=true)
dupnames = filter!(!ismissing, unique(dupsdists.municipality))
finalairports = filter(x -> coalesce(x.type == "large_airport" || x.iso_country == "FR", false), airports)
tradsdict = Dict(trads.orig .=> trads.trad)
finalairports.municipalitytrad = get.(Ref(tradsdict), finalairports.municipality, finalairports.municipality)
finalairports.country = get.(Ref(pays), finalairports.iso_country, finalairports.iso_country)
finalairports.region = get.(Ref(pays), finalairports.iso_region, finalairports.iso_region)
finalairports.citynames = ifelse.(coalesce.(in.(finalairports.municipality, Ref(dupnames)), false),
                                  ifelse.(isequal.(finalairports.iso_country, "US") .| isequal.(finalairports.iso_country, "CN"),
                                          finalairports.municipalitytrad .* " (" .* finalairports.region .* ")",
                                          finalairports.municipalitytrad .* " (" .* finalairports.country .* ")"),
                                  finalairports.municipalitytrad)
# Risque de confusion trop grand
filter!(:citynames => !isequal("Barcelone (Venezuela)"), finalairports)
citynamesdups = view(finalairports.citynames, coalesce.(in.(finalairports.municipality, Ref(dupnames)), false))
foreach(println, sort!(unique(citynamesdups)))
citynames = filter(!ismissing, finalairports.citynames)
sort!(unique(citynames))

CSV.write("liste aéroports.csv", unique(select(finalairports, :municipality, :citynames), :citynames))
