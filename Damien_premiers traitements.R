# préambules
library(dplyr)
library(tidyverse)
library(ggplot2)
# tinytex::install_tinytex() 
library(tinytex)# pour le rmarkdown en PDF (il faut latec)
library(broom)
library(broom.helpers)
library(gtsummary)
library(GGally)


# lancement de la base 
load("climat.RData")
source("recodages.R", encoding = "UTF-8")
# super méthode de Julien pour être sûr de travailler sur les dernières données
# attention recodage prend du temps 


######### création de la dicho j'ai fortement réduit l'avion 
str(climatRegr$Evol_GesVol.conf)
describe(climatRegr$solevolges.conf)

# icut(climatRegr, solevolges.conf)

####### attention je travaille sur la base climatRegr apparemment adaptée aux régressions et avec que le personnel de recherche 

#construction dicho j'ai fortement diminué l'avion pro O/N 
climatRegr$SolevolgesFD <- climatRegr$solevolges.conf=="Fortement diminué"
climatRegr$SolevolgesFD[climatRegr$SolevolgesFD == TRUE] <- 1
climatRegr$SolevolgesFD[climatRegr$SolevolgesFD == FALSE] <- 0
climatRegr$SolevolgesFD[is.na(climatRegr$SolevolgesFD)] <- 0
describe(climatRegr$SolevolgesFD)
#  je ne sais pas pourquoi describe ne fonctionne plus d'un seul coup ! 

# climatRegr$SolevolgesFD <- recode_factor(as.character(climatRegr$solevolges.conf), "Fortement diminué"="Oui", .default="Non", .missing="Non")
# describe(climatRegr$SolevolgesFD)
# str(climatRegr$SolevolgesFD)
# autre recodage plus efficace by Milan 


# Un joli tableau croisé --------------------------------------------------

# library(gtsummary)
climat %>%
  tbl_cross(row="solevolges.conf", col = "sexe", percent = "row")

climat %>%
  tbl_cross(col="solevolges.conf", row = "sexe", percent = "row")



############################# première régression avec les options d'affichage ! ################################
res.reg01 <- glm(SolevolgesFD ~ sexe + ageAgr  + sitpro_reduite + avionperso +  revenuTete, data=climatRegr, family = binomial(link = logit))
summary(res.reg01)
tidy(res.reg01, conf.int = TRUE, exponentiate = FALSE)
view(tidy_plus_plus(res.reg01, exponentiate = FALSE))
# permet de vérifier facilement les effectifs et la référence ; view permet de tout voir 



# tbl_regression(res.reg01, exponentiate = FALSE, intercept = TRUE)
# fonctionne mais la version ci-dessous tidyverse est plus jolie avec du gras 

res.reg01 %>%
  tbl_regression(exponentiate = FALSE) %>%
  bold_labels() %>%
  bold_p(t = .1)
#ceci fait une présentation propre des résultats avec en gras les significatifs 


# ggcoef_model(res.reg01,exponentiate = TRUE)
# la représentation graphique des OR 
ggcoef_model(res.reg01,exponentiate = FALSE)
# la même avec les coefficients 



############################################# des variations du modèle ############################################## 

res.reg01 <- glm(SolevolgesFD ~ sexe + ageAgr  + sitpro2 + avionperso + avionpersochgt + revenuTete, data=climatRegr, family = binomial(link = logit))
summary(res.reg01)

res.reg02 <- glm(SolevolgesFD ~ sexe + ageAgr  + sitpro2 + avionperso +  revenuTete + visiousages.reunion15 + visiousages.seminaire + visiousages.conf  +visioprivilegiee.emissions  , data=climatRegr, family = binomial(link = logit))
summary(res.reg02)
confint(res.reg02)
tidy(res.reg02, conf.int = TRUE, exponentiate = TRUE)
tidy_plus_plus(res.reg02, exponentiate = TRUE)


climatRegr$SolevolgesFD <- recode_factor(climatRegr$solevolges.conf, "Fortement diminué"="Oui", .default="Non", .missing="Non")
#version diplyr by Milan 

rlang::last_error()

# 
# tout ça c'est pour réussir à paramétrer le https 
# Sys.getenv("PATH")
# Sys.setenv(PATH="C:\\rtools40\\usr\\bin;C:\\rtools40\\usr\\bin;C:\\Program Files\\R\\R-4.1.2\\bin\\x64;C:\\Program Files\\R\\R-4.1.2\\bin\\x64;C:\\Program Files\\R\\R-4.1.2\\bin\\x64;C:\\Program Files (x86)\\Common Files\\Oracle\\Java\\javapath;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Windows\\System32\\OpenSSH\\;C:\\Program Files\\PuTTY\\;C:\\Program Files\\SASHome\\SASFoundation\\9.4\\core\\sasexe;C:\\Program Files\\SASHome\\SASFoundation\\9.4\\ets\\sasexe;C:\\Program Files\\SASHome\\Secure\\ccme4;C:\\Users\\dcartron\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\dcartron\\AppData\\Local\\Programs\\Git\\bin")
#            
# install.packages("gitcreds")
# 
# library(gitcreds)
# gitcreds_set()
  