############################################################
######## Machine Learning 1 Project - Small dataset ########
########     Kenneth Petrykowski & Mateusz Majak    ########
############################################################

#getwd()
#setwd("C:/Users/Admin/Documents/GIT/ProjectsML1")
coffee <- read.csv("arabica_data_cleaned.csv")

#library(dplyr)
glimpse(coffee)

coffee$X                     <- NULL
coffee$Species               <- NULL
coffee$Certification.Address <- NULL
coffee$Certification.Contact <- NULL
coffee$Lot.Number            <- NULL
coffee$Mill                  <- NULL
coffee$ICO.Number            <- NULL
coffee$Expiration            <- NULL
coffee$Grading.Date          <- NULL
coffee$Company               <- NULL
coffee$Farm.Name             <- NULL
coffee$Owner                 <- NULL
coffee$Producer              <- NULL
coffee$Owner.1               <- NULL
coffee$In.Country.Partner    <- NULL
coffee$Region                <- NULL

table(coffee$Quakers)
table(coffee$Category.One.Defects)
table(coffee$Category.Two.Defects)

