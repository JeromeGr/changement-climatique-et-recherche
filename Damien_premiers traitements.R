# préambules

library(tidyverse)
library(ggplot2)
load("climat.RData")
source("recodages.R", encoding = "UTF-8")
# super méthode de Julien pour être sûr de travailler sur les dernières données
# attention recodage prend du temps 

tinytex::install_tinytex()
# pour le rmarkdown en PDF (il faut latec)

# 
# tout ça c'est pour réussir à paramétrer le https 
# Sys.getenv("PATH")
# Sys.setenv(PATH="C:\\rtools40\\usr\\bin;C:\\rtools40\\usr\\bin;C:\\Program Files\\R\\R-4.1.2\\bin\\x64;C:\\Program Files\\R\\R-4.1.2\\bin\\x64;C:\\Program Files\\R\\R-4.1.2\\bin\\x64;C:\\Program Files (x86)\\Common Files\\Oracle\\Java\\javapath;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Windows\\System32\\OpenSSH\\;C:\\Program Files\\PuTTY\\;C:\\Program Files\\SASHome\\SASFoundation\\9.4\\core\\sasexe;C:\\Program Files\\SASHome\\SASFoundation\\9.4\\ets\\sasexe;C:\\Program Files\\SASHome\\Secure\\ccme4;C:\\Users\\dcartron\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\dcartron\\AppData\\Local\\Programs\\Git\\bin")
#            
# install.packages("gitcreds")
# 
# library(gitcreds)
# gitcreds_set()
  