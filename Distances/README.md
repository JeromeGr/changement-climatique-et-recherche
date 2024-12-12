# Notes sur le calcul des distances et temps de transport

Les temps de transport et distances en voiture ont été calculées à l'aide du distancier METRIC de l'Insee, à partir des codes communes :
http://www.progedo-adisp.fr/apf_metric.php
https://metric-osrm-shiny.lab.sspcloud.fr/app/shinymetricosrm/

Pour les trajets domicile-travail, l'origine est le lieu de résidence et la destination le lieu de travail. Ces variables existent en deux versions : en prenant le chef lieu de la commune, et en prenant le centroïde. Les personnes résidant dans la même commune que leur lieu de travail se sont vu attribuer la valeur 0.
- export_metric_osrm res-trav chefs lieu.csv
- export_metric_osrm res-trav centroides.csv

Pour les aéroports proches, tous les aéroports situés dans un rayon de 300 km à vol d'oiseau ont été retenus, en partant du chef lieu de la commune. 60 aéroports français et 124 aéroports de pays limitrophes (Belgique, Luxembourg, Allemagne, Suisse, Italie, Espagne) ont été considérés, correspondant aux aéroports de taille moyenne ou grande en activité. Ces variables existent en deux versions : en prenant le lieu de résidence et en prenant le lieu de travail.
- export_metric_osrm aéroports res.csv
- export_metric_osrm aéroports trav.csv

Les lignes avec valeurs manquantes sur la ou la paire de variables utilisée ont été exclues avant de soumettre les données.
