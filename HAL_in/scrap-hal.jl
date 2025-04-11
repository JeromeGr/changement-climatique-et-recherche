using HTTP, JSON3, JSONTables, DataFrames, CSV

for y in 2017:2019
    mark = "*"
    for i in 1:typemax(Int)
        x = HTTP.get("https://api.archives-ouvertes.fr/search/?q=*:*&fq=docType_s:ART&fq=publicationDateY_i:$y&fl=docid,halId_s,docType_s,authFullName_s,publicationDate_s,domain_s,language_s,label_s&rows=10000&cursorMark=$mark&sort=docid%20asc")
        s = String(x.body)
        json = JSON3.parse(s)
        if mark == json.nextCursorMark
            break
        else
            mark = json.nextCursorMark
        end
        df = DataFrame(jsontable(json.response.docs))
        vars = [:domain_s :authFullName_s :language_s]
        transform!(df, vars .=> ByRow(passmissing(x -> join(x, ","))) .=> vars)
        CSV.write("HAL/$y-$i.csv", df)
        println(y, ' ', i)
    end
end


# Ancien essai : semble fonctionner mais donne beaucoup de doublons
# for y in 2017:2019
#     start = 1
#     for i in 1:typemax(Int)
#         file = download("https://api.archives-ouvertes.fr/search/?q=*:*&fq=docType_s:ART&fq=publicationDateY_i:$y&fl=docid,halId_s,docType_s,authFullName_s,publicationDate_s,domain_s,language_s,label_s&wt=csv&start=$start&rows=10000",
#                         "HAL/$y-$i.csv")
#         if length(read(file)) < 100
#             rm(file)
#             break
#         end
#         start += 10_000
#         println(y, ' ', i)
#     end
# end
