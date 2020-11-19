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

#Recette pour reconstituer le tableua : Copy and paste the content of the .txt file into Word. In Word, select the text you just pasted from the .txt file
#go to Table → Convert → Convert Text to Table… make sure “Commas” is selected under “Separate text at”, click OK


#Tri à plat risque réductions des vols

tRisqRedAv<-as.data.frame(rbind(prop.table(table(climat$solrisqreducavion.qual.)),
                            prop.table(table(climat$solrisqreducavion.fin.)), prop.table(table(climat$solrisqreducavion.diffusion.)), 
                            prop.table(table(climat$solrisqreducavion.donnees.)), prop.table(table(climat$solrisqreducavion.avantages.)), 
                            prop.table(table(climat$solrisqreducavion.isoler.)), prop.table(table(climat$solrisqreducavion.avantages.)),
                            prop.table(table(climat$solrisqreducavion.bureaucratie.))))

tRisqRedAv<-round(tRisqRedAv*100)

Risque_pourcent_répondants<-(c("Diminuer qualité travaux", "Réduire accès financement", "Diminuer diffusion travaux", "Géner accès terrain/collecte données",
                               "Réduire avantg métier (voyager)", "Isoler la recherche française", "Géner insertion jeunes chercheurs", "Accroître bureaucratie"))

tRisqRedAv<-cbind(Risque_pourcent_répondants, tRisqRedAv)

write.table(tRisqRedAv, file = "/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Tableau risques avion.text",
            sep = ",", quote = FALSE, row.names = F)


#Tri à plat renoncement déplacement pro à l'étranger
climat$renon
tRenoncDepPro<-as.data.frame(rbind(prop.table(table(climat$renoncedep.prive.)),
                                prop.table(table(climat$renoncedep.pro.)), prop.table(table(climat$renoncedep.confort.)), 
                                prop.table(table(climat$renoncedep.env.)), prop.table(table(climat$renoncedep.mission.)), 
                                prop.table(table(climat$renoncedep.visio.))))

freq(climat$renoncedep.prive.)

tRenoncDepPro<-round(tRenoncDepPro*100)

Renoncements<-(c("Conflit avec des engagements privés", "Conflit avec d'autres engagements professionnels", "Raisons de confort et de santé",
                 "considérations environnementales", "difficultés à financer la mission", "possibilité de remplacer le voyage par la visioconférence"))

tRenoncDepPro<-cbind(Renoncements, tRenoncDepPro)

write.table(tRenoncDepPro, file = "/Users/jeromegreffion/Dropbox/changement-climatique-et-recherche/Resultats/Tableau renoncement déplacement pro à l'étranger.text",
            sep = ",", quote = FALSE, row.names = F)
