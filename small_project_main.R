############################################################
######## Machine Learning 1 Project - Small dataset ########
########     Kenneth Petrykowski & Mateusz Majak    ########
############################################################
library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(tibble)
library(purrr)
library(corrplot)
library(DescTools)
library(olsrr)
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
coffee$Variety               <- NULL
coffee$Altitude              <- NULL
coffee$Certification.Body    <- NULL

### !!!!!!!!! ###
# Do przemyœlenia:
table(coffee$Category.One.Defects)
table(coffee$Category.Two.Defects)
table(coffee$Processing.Method)

# Countung NAs in every column
colSums(is.na(coffee)) %>% 
  sort()


# Let's start with the column Quakers. There is one NA
table(coffee$Quakers, useNA = "ifany")

#We should probably replace the missing with the median
median(coffee$Quakers, na.rm=TRUE)
coffee$Quakers[is.na(coffee$Quakers)] <- 0


table(coffee$altitude_low_meters, useNA = "ifany")
table(coffee$altitude_high_meters, useNA = "ifany")
table(coffee$altitude_mean_meters, useNA = "ifany")

ggplot(coffee,
       aes(x = Total.Cup.Points)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()

# we delete outlier (Total.Cup.Points==0)
coffee <- coffee[!(coffee$Total.Cup.Points==0),]


###
#####
####### Now let's see which independent variables should we choose


coffees_numeric_vars <- 
  sapply(coffee, is.numeric) %>% 
  which() %>% 
  names()

coffees_numeric_vars


coffees_correlations <- 
  cor(coffee[,coffees_numeric_vars],
      use = "pairwise.complete.obs")

coffees_correlations

corrplot(coffees_correlations, 
         method = "pie")

coffee_lm1 <- lm(Total.Cup.Points ~ Aroma,
                 data = coffee)

summary(coffee_lm1)


houses_lm2 <- lm(Total.Cup.Points ~ Aroma + Flavor + Acidity +
                   Body + Balance + Sweetness + Category.One.Defects +Category.Two.Defects,
                 data = coffee)

summary(houses_lm2)
