#Tableau récapitulatif : toutes les réponses aux solutions concernant l'avion



tsolAv<-as.data.frame(rbind(table(climat$solinstit.train.), prop.table(table(climat$solinstit.train.)), 
                       table(climat$solinstit.limitevols.), prop.table(table(climat$solinstit.limitevols.)), 
                       table(climat$solinstit.vols6h.), prop.table(table(climat$solinstit.vols6h.))))

tsolAv[c(2,4,6),]<-round(tsolAv[c(2,4,6),]*100)

Solution<-(c("Financer train mm si + cher ou mission + long (Eff)", "Financer train mm si + cher ou mission + long (%)",
             "Imposer limite nb de vols par pers (Eff)", "Imposer limite nb de vols par pers (%)", 
             "Proscrire vols qd travail en train > 6h (Eff)", "Proscrire vols qd travail en train > 6h (%)" ))

tsolAv<-cbind(Solution, tsolAv)


write.table(tsolAv, file = "/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Tableau solutions avion.text",
            sep = ",", quote = FALSE, row.names = F)

#Recette
#Copy and paste the content of the .txt file into Word.
#In Word, select the text you just pasted from the .txt file
#go to Table → Convert → Convert Text to Table…
#make sure “Commas” is selected under “Separate text at”, click OK
