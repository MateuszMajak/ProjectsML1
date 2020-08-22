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
library(mice)
library(VIM)
source("Mode.R")
library(verification)
library(nnet)
#getwd()
#setwd("C:/Users/Admin/Documents/GIT/ProjectsML1")
coffee_full <- read.csv("arabica_data_cleaned.csv",na.strings=c("","NA"))
glimpse(coffee_full)

#We create coffee dataset without clumsy data like row numbers, unique codes and columns with same informations.
coffee <- coffee_full
coffee[,c("X","Species","Certification.Address","Certification.Contact","Lot.Number","Mill","ICO.Number","Expiration",
          "Grading.Date","Company","Farm.Name","Owner","Producer","Owner.1","In.Country.Partner","Region",
          "Altitude","Certification.Body","Harvest.Year")] <- NULL

# Counting NAs in every column
colSums(is.na(coffee)) %>% 
  sort()

coffee %>% 
  md.pattern(rotate.names = TRUE)
# 1 NA in Quakers column and 227 NAs in each altitude column

###
#NAs in Numerical columns
# Let's start with the column Quakers. There is one NA
table(coffee$Quakers, useNA = "ifany")

#We replace the missing value with the median
coffee$Quakers[is.na(coffee$Quakers)] <- median(coffee$Quakers, na.rm=TRUE)

#Let's look on altitudes data
ggplot(coffee,
       aes(x = altitude_low_meters)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()

#There are 227 out of 1311 rows with NA altitudes
#There are also 4 clear outliers - let's see them
which(coffee$altitude_low_meters>=11000)
which(coffee$altitude_high_meters>=11000)
which(coffee$altitude_mean_meters>=11000)
#They are the same rows

#We assign outliers to the altitude_outliers vector
altitude_outliers <- which(coffee$altitude_low_meters>=11000)

coffee[altitude_outliers,]

#Interesting information - Geisha coffee is one of the best and most expensive
#coffee in the world. It is grown at high mountains.
#There is a farm Café Takesi in Bolivia, that claims to be the world’s
#highest coffee farm, at an elevation of 1,900 to 2,500 meters above sea level.

#For us, it means that we should probably treat each value above 2500m as an outlier.
#There are values very close to 2500m, so we will cut every value above 2650m
altitude_outliers <- which(coffee$altitude_low_meters>=2650)
altitude_outliers <- which(coffee$altitude_high_meters>=2650)
altitude_outliers <- which(coffee$altitude_mean_meters>=2650)

coffee[altitude_outliers,]
#The rest of the data containing altitude outliers looks reliably
#Thus, we will group outliers together with NAs - there is no 
#possibility of coffee growing at such high altitudes
coffee[altitude_outliers,"altitude_low_meters"]  <- NA
coffee[altitude_outliers,"altitude_high_meters"] <- NA
coffee[altitude_outliers,"altitude_mean_meters"] <- NA

#Now we try to deal with NAs using multiple methods
# replace missings for altitudes with sample average
coffee <- coffee %>% 
  # we create a new columns altitude_._meters.impute_mean
  mutate(altitude_low_meters.impute_mean = ifelse(is.na(altitude_low_meters), 
                                                  mean(altitude_low_meters, na.rm = TRUE),
                                                  altitude_low_meters),
         altitude_high_meters.impute_mean = ifelse(is.na(altitude_high_meters), 
                                                  mean(altitude_high_meters, na.rm = TRUE),
                                                  altitude_high_meters),
         altitude_mean_meters.impute_mean = ifelse(is.na(altitude_mean_meters), 
                                                   mean(altitude_mean_meters, na.rm = TRUE),
                                                   altitude_mean_meters)
         )


# replace missings for altitudes with sample median
coffee <- coffee %>% 
  # we create a new columns altitude_._meters.impute_median
  mutate(altitude_low_meters.impute_median = ifelse(is.na(altitude_low_meters), 
                                                  median(altitude_low_meters, na.rm = TRUE),
                                                  altitude_low_meters),
         altitude_high_meters.impute_median = ifelse(is.na(altitude_high_meters), 
                                                     median(altitude_high_meters, na.rm = TRUE),
                                                     altitude_high_meters),
         altitude_mean_meters.impute_median = ifelse(is.na(altitude_mean_meters),
                                                     median(altitude_mean_meters, na.rm = TRUE),
                                                     altitude_mean_meters)
  )

# replace missings for altitudes with means and medians in subgroups
coffee <- coffee %>% 
  # divide dataset into subgroups
  group_by(Country.of.Origin) %>%
  # we create a new column altitude_low_meters.impute.Gmean
  mutate(altitude_low_meters.impute.Gmean = ifelse(is.na(altitude_low_meters),
                                                   mean(altitude_low_meters, na.rm = TRUE),
                                                   altitude_low_meters),
         # a new column altitude_low_meters.impute.Gmed
         altitude_low_meters.impute.Gmed = ifelse(is.na(altitude_low_meters),
                                                  median(altitude_low_meters, na.rm = TRUE),
                                                  altitude_low_meters),
         # a new column altitude_high_meters.impute.Gmean
         altitude_high_meters.impute.Gmean = ifelse(is.na(altitude_high_meters),
                                                    median(altitude_high_meters, na.rm = TRUE),
                                                    altitude_high_meters),
         # a new column altitude_high_meters.impute.Gmed
         altitude_high_meters.impute.Gmed = ifelse(is.na(altitude_high_meters), 
                                                   mean(altitude_high_meters, na.rm = TRUE),
                                                   altitude_high_meters),
         # a new column altitude_mean_meters.impute.Gmean
         altitude_mean_meters.impute.Gmean = ifelse(is.na(altitude_mean_meters),
                                                    median(altitude_mean_meters, na.rm = TRUE),
                                                    altitude_mean_meters),
         # a new column altitude_mean_meters.impute.Gmed
         altitude_mean_meters.impute.Gmed = ifelse(is.na(altitude_mean_meters), 
                                                   mean(altitude_mean_meters, na.rm = TRUE),
                                                   altitude_mean_meters)
  ) %>% ungroup()

coffee %>%
  select(starts_with("altitude")) %>%
  summary()
#impute_median seems to be the closest imputations to the original dataset
#means and medians in subgroups do not give better results
#Thus, since now we will use altitude_high_meters.impute_median,
#altitude_low_meters.impute_median and altitude_mean_meters.impute_median
coffee$altitude_low_meters.impute.Gmean   <- NULL
coffee$altitude_mean_meters.impute.Gmean  <- NULL
coffee$altitude_high_meters.impute.Gmean  <- NULL
coffee$altitude_low_meters.impute.Gmed    <- NULL
coffee$altitude_high_meters.impute.Gmed   <- NULL
coffee$altitude_mean_meters.impute.Gmed   <- NULL

###
#NAs in Factor columns
colSums(is.na(coffee)) %>% 
  sort()
#Let's look on the Country.of.Origin
table(coffee$Country.of.Origin)

#There is one coffee without name of the Country of origin. We use mode in subgroups to input the name
coffee <- coffee %>% 
  group_by(Processing.Method, Color) %>%
  mutate(Country.of.Origin.impute.Gmode = if_else(is.na(Country.of.Origin), 
                                        Mode(Country.of.Origin),
                                        Country.of.Origin)) %>% 
  ungroup()

#Colombia assigned to NA
#Only one observation was changed, so the distribution of Country.of.Origin.impute.Gmode
#is very close to the the distribution of Country.of.Origin
#Thus, we will use Country.of.Origin.impute.Gmode in the future analysis

#Maybe the region will be more helpful that country, so we create Region column
table(coffee$Country.of.Origin.impute.Gmode)
coffee[,"Region"] <- NA
coffee$Region <- as.character(coffee$Region)
coffee$Country.of.Origin.impute.Gmode <- as.character(coffee$Country.of.Origin.impute.Gmode)
#East Africa group
coffee[which(coffee$Country.of.Origin.impute.Gmode=="Zambia"|
               coffee$Country.of.Origin.impute.Gmode=="Uganda"|
               coffee$Country.of.Origin.impute.Gmode=="Burundi"|
               coffee$Country.of.Origin.impute.Gmode=="Rwanda"|
               coffee$Country.of.Origin.impute.Gmode=="Malawi"|
               coffee$Country.of.Origin.impute.Gmode=="Papua New Guinea"|
               coffee$Country.of.Origin.impute.Gmode=="Mauritius"|
               coffee$Country.of.Origin.impute.Gmode=="Ethiopia"|
               coffee$Country.of.Origin.impute.Gmode=="Kenya"
             ),
       "Region"] <- "East Africa"

#West Africa
coffee[which(coffee$Country.of.Origin.impute.Gmode=="Tanzania, United Republic Of"|
               coffee$Country.of.Origin.impute.Gmode=="Cote d?Ivoire"
             ),
       "Region"] <- "West Africa"

#Asia and Oceania
coffee[which(coffee$Country.of.Origin.impute.Gmode=="Japan"|
               coffee$Country.of.Origin.impute.Gmode=="Thailand"|
               coffee$Country.of.Origin.impute.Gmode=="Vietnam"|
               coffee$Country.of.Origin.impute.Gmode=="Myanmar"|
               coffee$Country.of.Origin.impute.Gmode=="Philippines"|
               coffee$Country.of.Origin.impute.Gmode=="Laos"|
               coffee$Country.of.Origin.impute.Gmode=="China"|
               coffee$Country.of.Origin.impute.Gmode=="India"|
               coffee$Country.of.Origin.impute.Gmode=="Taiwan"|
               coffee$Country.of.Origin.impute.Gmode=="Indonesia"|
               coffee$Country.of.Origin.impute.Gmode=="Papua New Guinea"),
       "Region"] <- "Asia and Oceania"

#Central America
coffee[which(coffee$Country.of.Origin.impute.Gmode=="Costa Rica"|
               coffee$Country.of.Origin.impute.Gmode=="Guatemala"|
               coffee$Country.of.Origin.impute.Gmode=="Haiti"|
               coffee$Country.of.Origin.impute.Gmode=="Honduras"|
               coffee$Country.of.Origin.impute.Gmode=="Nicaragua"|
               coffee$Country.of.Origin.impute.Gmode=="El Salvador"|
               coffee$Country.of.Origin.impute.Gmode=="Panama"
             ),
       "Region"] <- "Central America"

#South America
coffee[which(coffee$Country.of.Origin.impute.Gmode=="Peru"|
               coffee$Country.of.Origin.impute.Gmode=="Ecuador"|
               coffee$Country.of.Origin.impute.Gmode=="Brazil"|
               coffee$Country.of.Origin.impute.Gmode=="Colombia"
             ),
       "Region"] <- "South America"
#North America
coffee[which(coffee$Country.of.Origin.impute.Gmode=="Mexico"|
               coffee$Country.of.Origin.impute.Gmode=="United States"|
               coffee$Country.of.Origin.impute.Gmode=="United States (Hawaii)"|
               coffee$Country.of.Origin.impute.Gmode=="United States (Puerto Rico)"
             ),
       "Region"] <- "North America"

coffee$Region <- as.factor(coffee$Region)
coffee$Country.of.Origin.impute.Gmode <- as.factor(coffee$Country.of.Origin.impute.Gmode)

#Now let's look on the Bag.Weight factor
table(coffee$Bag.Weight)
coffee$Bag.Weight <- as.character(coffee$Bag.Weight)

#There are two types of measurement - we should convert lbs to the kg
coffee[which(coffee$Bag.Weight=="1 lbs"),"Bag.Weight"]    <- "<=1"
coffee[which(coffee$Bag.Weight=="100 lbs"),"Bag.Weight"]  <- "45"
coffee[which(coffee$Bag.Weight=="130 lbs"),"Bag.Weight"]  <- "58.5"
coffee[which(coffee$Bag.Weight=="132 lbs"),"Bag.Weight"]  <- "59.4"
coffee[which(coffee$Bag.Weight=="150 lbs"),"Bag.Weight"]  <- "67.5"
coffee[which(coffee$Bag.Weight=="2 lbs"),"Bag.Weight"]    <- "<=1"
coffee[which(coffee$Bag.Weight=="55 lbs"),"Bag.Weight"]   <- "24.75"
coffee[which(coffee$Bag.Weight=="80 lbs"),"Bag.Weight"]   <- "36"
coffee[which(coffee$Bag.Weight=="0 kg"),"Bag.Weight"]     <- "<=1"
coffee[which(coffee$Bag.Weight=="0 lbs"),"Bag.Weight"]    <- "<=1"
coffee[which(coffee$Bag.Weight=="1"),"Bag.Weight"]        <- "<=1"
coffee[which(coffee$Bag.Weight=="1 kg"),"Bag.Weight"]     <- "<=1"
coffee[which(coffee$Bag.Weight=="1 kg,lbs"),"Bag.Weight"] <- "<=1"
coffee[which(coffee$Bag.Weight=="10 kg"),"Bag.Weight"]    <- "(1;10>"
coffee[which(coffee$Bag.Weight=="2"),"Bag.Weight"]        <- "(1;10>"
coffee[which(coffee$Bag.Weight=="2 kg"),"Bag.Weight"]     <- "(1;10>"
coffee[which(coffee$Bag.Weight=="2 kg,lbs"),"Bag.Weight"] <- "(1;10>"
coffee[which(coffee$Bag.Weight=="3 kg"),"Bag.Weight"]     <- "(1;10>"
coffee[which(coffee$Bag.Weight=="3 lbs"),"Bag.Weight"]    <- "(1;10>"
coffee[which(coffee$Bag.Weight=="4 lbs"),"Bag.Weight"]    <- "(1;10>"
coffee[which(coffee$Bag.Weight=="5 lbs"),"Bag.Weight"]    <- "(1;10>"
coffee[which(coffee$Bag.Weight=="4 kg"),"Bag.Weight"]     <- "(1;10>"
coffee[which(coffee$Bag.Weight=="5 kg"),"Bag.Weight"]     <- "(1;10>"
coffee[which(coffee$Bag.Weight=="6 kg"),"Bag.Weight"]     <- "(1;10>"
coffee[which(coffee$Bag.Weight=="6"),"Bag.Weight"]        <- "(1;10>"
coffee[which(coffee$Bag.Weight=="8 kg"),"Bag.Weight"]     <- "(1;10>"

coffee[which(coffee$Bag.Weight=="100 lbs"),"Bag.Weight"]  <- "(10;60>"
coffee[which(coffee$Bag.Weight=="130 lbs"),"Bag.Weight"]  <- "(10;60>"
coffee[which(coffee$Bag.Weight=="132 lbs"),"Bag.Weight"]  <- "(10;60>"
coffee[which(coffee$Bag.Weight=="15 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="18 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="20 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="24 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="25 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="29 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="30 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="34 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="35 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="40 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="46 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="50 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="59 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="60 kg"),"Bag.Weight"]    <- "(10;60>"
coffee[which(coffee$Bag.Weight=="45"),"Bag.Weight"]       <- "(10;60>"
coffee[which(coffee$Bag.Weight=="67.5"),"Bag.Weight"]     <- "(10;60>"
coffee[which(coffee$Bag.Weight=="59.4"),"Bag.Weight"]     <- "(10;60>"
coffee[which(coffee$Bag.Weight=="58.5"),"Bag.Weight"]     <- "(10;60>"
coffee[which(coffee$Bag.Weight=="36"),"Bag.Weight"]       <- "(10;60>"
coffee[which(coffee$Bag.Weight=="59.4"),"Bag.Weight"]     <- "(10;60>"
coffee[which(coffee$Bag.Weight=="24.75"),"Bag.Weight"]    <- "(10;60>"

coffee[which(coffee$Bag.Weight=="100 kg"),"Bag.Weight"]   <- ">60"
coffee[which(coffee$Bag.Weight=="12000 kg"),"Bag.Weight"] <- ">60"
coffee[which(coffee$Bag.Weight=="1218 kg"),"Bag.Weight"]  <- ">60"
coffee[which(coffee$Bag.Weight=="13800 kg"),"Bag.Weight"] <- ">60"
coffee[which(coffee$Bag.Weight=="150 lbs"),"Bag.Weight"]  <- ">60"
coffee[which(coffee$Bag.Weight=="1500 kg"),"Bag.Weight"]  <- ">60"
coffee[which(coffee$Bag.Weight=="18000 kg"),"Bag.Weight"] <- ">60"
coffee[which(coffee$Bag.Weight=="18975 kg"),"Bag.Weight"] <- ">60"
coffee[which(coffee$Bag.Weight=="19200 kg"),"Bag.Weight"] <- ">60"
coffee[which(coffee$Bag.Weight=="350 kg"),"Bag.Weight"]   <- ">60"
coffee[which(coffee$Bag.Weight=="66 kg"),"Bag.Weight"]    <- ">60"
coffee[which(coffee$Bag.Weight=="660 kg"),"Bag.Weight"]   <- ">60"
coffee[which(coffee$Bag.Weight=="67 kg"),"Bag.Weight"]    <- ">60"
coffee[which(coffee$Bag.Weight=="69 kg"),"Bag.Weight"]    <- ">60"
coffee[which(coffee$Bag.Weight=="70 kg"),"Bag.Weight"]    <- ">60"
coffee[which(coffee$Bag.Weight=="9000 kg"),"Bag.Weight"]  <- ">60"
coffee[which(coffee$Bag.Weight=="80 kg"),"Bag.Weight"]    <- ">60"

coffee$Bag.Weight <- factor(coffee$Bag.Weight)

# #Now let's look on the Harvest.Year
# #47 NAs in there
# table(coffee$Harvest.Year)
# #Data need some cleaning
# coffee$Harvest.Year <- addNA(coffee$Harvest.Year)
# levels(coffee$Harvest.Year)
# levels(coffee$Harvest.Year)[47] <- "missing"
# 
# coffee$Harvest.Year <- as.character(coffee$Harvest.Year)
# 
# coffee[which(coffee$Harvest.Year=="4T/2010"|
#                coffee$Harvest.Year=="23 July 2010"|
#                coffee$Harvest.Year=="4T/10"|
#                coffee$Harvest.Year=="March 2010"|
#                coffee$Harvest.Year=="4t/2010"|
#                coffee$Harvest.Year=="4T72010"|
#                coffee$Harvest.Year=="47/2010"
#              ),
#        "Harvest.Year"] <- "2010"
# 
# coffee[which(coffee$Harvest.Year=="Sept 2009 - April 2010"|
#                coffee$Harvest.Year=="December 2009-March 2010"|
#                coffee$Harvest.Year=="2009/2010"|
#                coffee$Harvest.Year=="2009-2010"|
#                coffee$Harvest.Year=="2009 - 2010"|
#                coffee$Harvest.Year=="2009 / 2010"|
#                coffee$Harvest.Year=="Fall 2009"|
#                coffee$Harvest.Year=="08/09 crop"
#              ),
#        "Harvest.Year"] <- "2009/2010"
# 
# coffee[which(coffee$Harvest.Year=="4t/2011"|
#                coffee$Harvest.Year=="January 2011"|
#                coffee$Harvest.Year=="3T/2011"|
#                coffee$Harvest.Year=="1t/2011"|
#                coffee$Harvest.Year==" 1T/2011"|
#                coffee$Harvest.Year=="Spring 2011 in Colombia."|
#                coffee$Harvest.Year=="Abril - Julio /2011"|
#                coffee$Harvest.Year=="1T/2011"
#              ),
#        "Harvest.Year"] <- "2011"
# 
# coffee[which(coffee$Harvest.Year=="2010-2011"),
#        "Harvest.Year"] <- "2010/2011"
# 
# coffee[which(coffee$Harvest.Year=="2013/2014"),
#        "Harvest.Year"] <- "2013/2014"
# 
# coffee[which(coffee$Harvest.Year=="2016 / 2017"|
#                coffee$Harvest.Year=="2016/2017"),
#        "Harvest.Year"] <- "2016/2017"
# 
# coffee[which(coffee$Harvest.Year=="2018"|
#                coffee$Harvest.Year=="2017 / 2018"),
#        "Harvest.Year"] <- "2017/2018"
# 
# other_years <- which(coffee$Harvest.Year=="Mayo a Julio"|
#                        coffee$Harvest.Year=="mmm"|
#                        coffee$Harvest.Year=="TEST"|
#                        coffee$Harvest.Year=="Abril - Julio"|
#                        coffee$Harvest.Year=="August to December"|
#                        coffee$Harvest.Year=="May-August"|
#                        coffee$Harvest.Year=="January Through April")
# coffee[other_years,]
# coffee[other_years,"Harvest.Year"] <- "missing"
# 
# coffee$Harvest.Year <- factor(coffee$Harvest.Year)
# 
# table(coffee$Harvest.Year)

#Now let's look on the  Processing.Method factor
table(coffee$Processing.Method)
#We group NA and Other together
coffee$Processing.Method <- addNA(coffee$Processing.Method)
levels(coffee$Processing.Method)
coffee[which(coffee$Processing.Method=="Other"),"Processing.Method"] <- NA
coffee$Processing.Method <- droplevels(coffee$Processing.Method)
coffee$Processing.Method <- as.character(coffee$Processing.Method)
typeof(coffee$Processing.Method)
#Now let's deal with missing values
coffee <- coffee %>% 
  group_by(Region) %>%
  mutate(Processing.Method.impute.Gmode = if_else(is.na(Processing.Method),
                                                  Mode(Processing.Method),
                                                  Processing.Method)) %>%
  ungroup()

coffee$Processing.Method <- as.factor(coffee$Processing.Method)
coffee$Processing.Method.impute.Gmode <- as.factor(coffee$Processing.Method.impute.Gmode)
coffee$Processing.Method.impute.Gmode <- addNA(coffee$Processing.Method.impute.Gmode)
coffee$Processing.Method.impute.Gmode <- droplevels(coffee$Processing.Method.impute.Gmode)

coffee$Processing.Method %>% 
  table() %>%
  prop.table()

coffee$Processing.Method.impute.Gmode %>%
  table() %>%
  prop.table()
#Distributions are similar

#There are only NAs in Color and Variety color left
#Let's look on the next factor - Color
coffee$Color <- addNA(coffee$Color)
table(coffee$Color)

#I should group NA and None together
# Blue-Green and Bluish-Green are the same colors, 
# thus I group them
coffee[which(coffee$Color=="None"),"Color"] <- NA
coffee[which(coffee$Color=="Blue-Green"),"Color"] <- "Bluish-Green"
coffee$Color <- droplevels(coffee$Color)
table(coffee$Color)
coffee$Color <- as.character(coffee$Color)
#267 NA values
coffee <- coffee %>% 
  group_by(Region,Processing.Method.impute.Gmode) %>%
  mutate(Color.impute.Gmode = if_else(is.na(Color),
                                      Mode(Color),
                                      Color)) %>%
  ungroup()

coffee$Color.impute.Gmode <- addNA(coffee$Color.impute.Gmode)
table(coffee$Color.impute.Gmode)

coffee$Color.impute.Gmode <- as.character(coffee$Color.impute.Gmode)
coffee <- coffee %>%
  mutate(Color.impute.Gmode = if_else(is.na(Color.impute.Gmode),
                                      Mode(Color),
                                      Color.impute.Gmode))
coffee$Color %>% 
  table() %>%
  prop.table()

coffee$Color.impute.Gmode %>%
  table() %>%
  prop.table()

coffee$Color.impute.Gmode <- addNA(coffee$Color.impute.Gmode)
coffee$Color.impute.Gmode <- as.factor(coffee$Color.impute.Gmode)
coffee$Color.impute.Gmode <- droplevels(coffee$Color.impute.Gmode)
table(coffee$Color.impute.Gmode)

#Now it's time to look on the last factor - Variety
coffee$Variety <- as.character(coffee$Variety)
#108 Other and 201 NA - we group them
coffee[which(coffee$Variety=="Other"),"Variety"] <- NA
coffee$Variety <- addNA(coffee$Variety)
table(coffee$Variety)

#Now we group small groups into bigger groups based on information
#from https://varieties.worldcoffeeresearch.org/
coffee$Variety <- as.character(coffee$Variety)

coffee[which(coffee$Variety=="Java"),"Variety"]                  <- "Ethiopian Landrace"
coffee[which(coffee$Variety=="Gesha"),"Variety"]                 <- "Ethiopian Landrace"
coffee[which(coffee$Variety=="Ethiopian Heirlooms"),"Variety"]   <- "Ethiopian Landrace"
coffee[which(coffee$Variety=="Ethiopian Yirgacheffe"),"Variety"] <- "Ethiopian Landrace"
coffee[which(coffee$Variety=="Ruiru 11"),"Variety"]              <- "Introgressed"
coffee[which(coffee$Variety=="Catimor"),"Variety"]               <- "Introgressed"
coffee[which(coffee$Variety=="Arusha"),"Variety"]                <- "Typica"
coffee[which(coffee$Variety=="Blue Mountain"),"Variety"]         <- "Typica"
coffee[which(coffee$Variety=="Mandheling"),"Variety"]            <- "Typica"
coffee[which(coffee$Variety=="Marigojipe"),"Variety"]            <- "Typica"
coffee[which(coffee$Variety=="Pache Comun"),"Variety"]           <- "Typica"
coffee[which(coffee$Variety=="Peaberry"),"Variety"]              <- "Typica"
coffee[which(coffee$Variety=="SL14"),"Variety"]                  <- "Typica"
coffee[which(coffee$Variety=="SL34"),"Variety"]                  <- "Typica"
coffee[which(coffee$Variety=="Sulawesi"),"Variety"]              <- "Typica"
coffee[which(coffee$Variety=="Catuai"),"Variety"]                <- "Bourbon-Typica cross"
coffee[which(coffee$Variety=="Mundo Novo"),"Variety"]            <- "Bourbon-Typica cross"
coffee[which(coffee$Variety=="Pacamara"),"Variety"]              <- "Bourbon-Typica cross"
coffee[which(coffee$Variety=="SL28"),"Variety"]                  <- "Bourbon-Typica cross"
coffee[which(coffee$Variety=="Sumatra"),"Variety"]               <- "Bourbon-Typica cross"
coffee[which(coffee$Variety=="Sumatra Lintong"),"Variety"]       <- "Bourbon-Typica cross"
coffee[which(coffee$Variety=="Moka Peaberry"),"Variety"]         <- "Bourbon"
coffee[which(coffee$Variety=="Pacas"),"Variety"]                 <- "Bourbon"

coffee <- coffee %>% 
  group_by(Region,Processing.Method.impute.Gmode) %>%
  mutate(Variety.impute.Gmode = if_else(is.na(Variety),
                                      Mode(Variety),
                                      Variety)) %>%
  ungroup()

coffee$Variety %>% 
  table() %>%
  prop.table()

coffee$Variety.impute.Gmode %>%
  table() %>%
  prop.table()

coffee$Variety <- as.factor(coffee$Variety)
coffee$Variety <- droplevels(coffee$Variety)
coffee$Variety <- addNA(coffee$Variety)
table(coffee$Variety)

coffee$Variety.impute.Gmode <- addNA(coffee$Variety.impute.Gmode)
coffee$Variety.impute.Gmode <- as.factor(coffee$Variety.impute.Gmode)
coffee$Variety.impute.Gmode <- droplevels(coffee$Variety.impute.Gmode)
table(coffee$Variety.impute.Gmode)

coffee$Variety.impute.Gmode <- as.character(coffee$Variety.impute.Gmode)
coffee <- coffee %>% 
  group_by(Bag.Weight,Color.impute.Gmode) %>%
  mutate(Variety.impute.Gmode = if_else(is.na(Variety.impute.Gmode),
                                        Mode(Variety.impute.Gmode),
                                        Variety.impute.Gmode)) %>%
  ungroup()

coffee$Variety.impute.Gmode <- addNA(coffee$Variety.impute.Gmode)
coffee$Variety.impute.Gmode <- as.factor(coffee$Variety.impute.Gmode)
coffee$Variety.impute.Gmode <- droplevels(coffee$Variety.impute.Gmode)
table(coffee$Variety.impute.Gmode)
#Distribution has changed, but Typica and Bourbon varieties are most popular 
#in the World, so it is very likely to be truth

#Now let's look for outliers considering Total.Cup.Points result
ggplot(coffee,
       aes(x = Total.Cup.Points)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()

# There is one outlier in Total.Cup.Points
coffee[which(coffee$Total.Cup.Points==0),]
#The other numeric variables are equal to 0, so I delete this observation
coffee <- coffee[-which(coffee$Total.Cup.Points==0),]

#I create new dataframe without variables which I will not use in the analysis
#I also delete Total.Cup.Points, because it is the sum of Aroma, Flavor, Aftertaste, Acidity, Body, Balance,
#Uniformity, Clean.Cup, Sweetness and Cupper.Points
coffee_final <- coffee
coffee_final[,c("Country.of.Origin","Processing.Method","Variety","altitude_low_meters","altitude_high_meters",
             "altitude_mean_meters","Color","altitude_mean_meters.impute_mean","altitude_low_meters.impute_mean",
             "altitude_high_meters.impute_mean","Total.Cup.Points")] <- NULL

# Before split the dataset, I check if there are variables
# with zero or near zero variance, because such variables 
# may need some tranformations 
nearZeroVar(coffee_final,
            saveMetrics = TRUE) -> coffees_nzv_stats

coffees_nzv_stats %>% 
  rownames_to_column("variable") %>% 
  arrange(-zeroVar, -nzv, -freqRatio)

# we havee 3 problematic variables
table(coffee_final$Quakers)
table(coffee_final$Clean.Cup)
table(coffee_final$Sweetness)

# I will group them in two groups, because in each column there is
# one hugly dominant value
quakers_not_zero_train <- which(coffee_final$Quakers>0)
coffee_final$Quakers <- as.character(coffee_final$Quakers)
coffee_final[quakers_not_zero_train,"Quakers"] <- ">0"
coffee_final$Quakers <- as.factor(coffee_final$Quakers)
table(coffee_final$Quakers)

clean_cup_not_10_train <- which(coffee_final$Clean.Cup<10)
coffee_final$Clean.Cup <- as.character(coffee_final$Clean.Cup)
coffee_final[clean_cup_not_10_train,"Clean.Cup"] <- "<10"
coffee_final$Clean.Cup <- as.factor(coffee_final$Clean.Cup)
table(coffee_final$Clean.Cup)

sweetness_not_10_train <- which(coffee_final$Sweetness<10)
coffee_final$Sweetness <- as.character(coffee_final$Sweetness)
coffee_final[sweetness_not_10_train,"Sweetness"] <- "<10"
coffee_final$Sweetness <- as.factor(coffee_final$Sweetness)
table(coffee_final$Sweetness)

#Let's check if they have still near zero variance
nearZeroVar(coffee_final,
            saveMetrics = TRUE) -> coffees_nzv_stats

coffees_nzv_stats %>% 
  rownames_to_column("variable") %>% 
  arrange(-zeroVar, -nzv, -freqRatio)
#There are no near zero variance variables now

# Now let's divide the set into learning and testing sample
coffees_numeric_vars
set.seed(987654321)

coffees_which_train <- createDataPartition(coffee_final$Cupper.Points,
                                          p = 0.7, 
                                          list = FALSE) 
head(coffees_which_train)

# we need to apply this index for data division
coffees_train <- coffee_final[coffees_which_train,]
coffees_test <- coffee_final[-coffees_which_train,]

# let's check the distribution of
# the target variable in both samples
summary(coffees_train$Cupper.Points)
summary(coffees_test$Cupper.Points)
#similar distribution in both samples

# storing the list of names of numeric variables into a vector
coffees_numeric_vars <- 
  sapply(coffees_train, is.numeric) %>% 
  which() %>% 
  names()

coffees_numeric_vars

# mutually correlated (irrelevant) variables
coffee_correlations <- 
  cor(coffees_train[,coffees_numeric_vars],
      use = "pairwise.complete.obs")

corrplot(coffee_correlations, 
         method = "pie")

coffees_numeric_vars_order <- 
  coffee_correlations[,"Cupper.Points"] %>% 
  sort(decreasing = TRUE) %>%
  names()

corrplot.mixed(coffee_correlations[coffees_numeric_vars_order, 
                                   coffees_numeric_vars_order],
               upper = "circle",
               lower = "number",
               tl.col="black",
               tl.pos = "lt")
# We can see very high correlation between altitude variables 
# Number of bags and Category.One.Defects seem to have very small correlation between 
# them and the other variables - we will skip them
corrplot.mixed(coffee_correlations[coffees_numeric_vars_order[-12:-13], 
                                   coffees_numeric_vars_order[-12:-13]],
               upper = "circle",
               lower = "number",
               tl.col = "black",
               tl.pos = "lt")


### The relation of the target variable with the most strongly correlated variables
#Flavour
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Flavor)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
# 5 coffees clearly not matching the relationship - 2 coffees with the flavour < 8 but Cupper.Points = 10 
# 3 coffees with Cupper.Points < 5.5 and Flavor > 7.5
# maybe exclude these observations from the sample
coffees_outliers <- NULL
coffees_outliers_flavor <- which(coffees_train$Flavor < 8 & coffees_train$Cupper.Points == 10)
coffees_outliers_flavor <- append(coffees_outliers_flavor,
                                  which(coffees_train$Flavor > 7.5 & coffees_train$Cupper.Points < 5.5))

coffees_outliers <- append(coffees_outliers,coffees_outliers_flavor)

#Aftertaste
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Aftertaste)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# 5 coffees clearly notmatching the relationship - 
#they look similar to the previous values from Flavor 
coffees_outliers_aftertaste <- which(coffees_train$Aftertaste >= 7.5 & coffees_train$Cupper.Points < 5.5)
coffees_outliers_aftertaste <- append(coffees_outliers_aftertaste,
                                      which(coffees_train$Aftertaste < 8 & coffees_train$Cupper.Points == 10))
identical(sort(coffees_outliers_flavor),sort(coffees_outliers_aftertaste))
# They were exactly the same outliers 

#Balance
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Balance)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
#4 clear outliers - Cupper.Points = 10 and Balance = 7 ; Balance > 7.5 and Cupper.Points < 5.5
coffees_outliers_balance <- which(coffees_train$Balance == 7 & coffees_train$Cupper.Points == 10)
# Actually there are 2 outliers with exact value of Balance and Cupper.Points
coffees_outliers_balance <- append(coffees_outliers_balance,
                                   which(coffees_train$Balance > 7.5 & coffees_train$Cupper.Points < 5.5))
identical(sort(coffees_outliers_flavor),sort(coffees_outliers_balance))
# They were exactly the same outliers

#Aroma
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Aroma)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
#4 clear new outliers - Cupper.Points = 10 and Aroma < 7.75 ; Cupper.Points < 5.5 and Aroma > 7.5
coffees_outliers_aroma <- which(coffees_train$Aroma < 7.75 & coffees_train$Cupper.Points == 10)
coffees_outliers_aroma <- append(coffees_outliers_aroma,
                                   which(coffees_train$Aroma > 7.5 & coffees_train$Cupper.Points < 5.5))
#These outliers were marked earlier

#Acidity
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Acidity)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
# At least 2 clear outliers
which(coffees_train$Acidity < 7.5 & coffees_train$Cupper.Points == 10)
# 2 outliers marked earlier
coffees_outliers_acidity <- which(coffees_train$Acidity < 5.5 & coffees_train$Cupper.Points > 7.5)
# 1 new outlier
coffees_outliers <- append(coffees_outliers,coffees_outliers_acidity)

#Body
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Body)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
#At least 2 clear outliers
which(coffees_train$Body == 7 & coffees_train$Cupper.Points == 10)
# 2 outliers marked earlier
coffees_outliers_body <- which(coffees_train$Body < 5.5 & coffees_train$Cupper.Points > 7.5)
# 1 new outlier
coffees_outliers <- append(coffees_outliers,coffees_outliers_body)

#Uniformity
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Uniformity)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
#Relationship looks more like categorical relationship

#Moisture
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Moisture)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
#many values far from relationship

#Category.One.Defects
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = Category.One.Defects)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
#3 clear outliers - Defects >= 15 and Cupper.Points above 6.5
coffees_outliers_cat1_defects <- which(coffees_train$Category.One.Defects >= 15 & coffees_train$Cupper.Points > 6.5)
coffees_outliers <- append(coffees_outliers,coffees_outliers_cat1_defects)

#altitude_mean_meters.impute_median
ggplot(coffees_train,
       aes(x = Cupper.Points,
           y = altitude_mean_meters.impute_median)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
#There is some positive relationship, but very unclear - many values far from relationship

#Now we try to find highly correlated variables
findCorrelation(coffee_correlations,
                cutoff = 0.80,
                names = TRUE)
# these are potential candidates to be excluded from the model because of the high correlation between them

### qualitative (categorical) variables ###
coffees_categorical_vars <-
  sapply(coffees_train, is.factor) %>% 
  which() %>% 
  names()

coffees_categorical_vars

#We use the function that retrieves F statistic value
# for the explanatory categorical variable provided 
# as the function argument

coffees_F_anova <- function(categorical_var) {
  anova_ <- aov(coffees_train$Cupper.Points ~ 
                  coffees_train[[categorical_var]]) 
  
  return(summary(anova_)[[1]][1, 4])
}

# Relationship between coffees_categorical_vars and Cupper.Points
sapply(coffees_categorical_vars,
       coffees_F_anova) %>% 
  sort(decreasing = TRUE) -> coffees_anova_all_categorical

coffees_anova_all_categorical

#relation between Color.impute.Gmode levels and Cupper.Points
ggplot(coffees_train,
       aes(x = Color.impute.Gmode,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#Green has many outliers
#Bluish-Green has slightly higher values 

#relation between Country.of.Origin.impute.Gmode levels and Cupper.Points
ggplot(coffees_train,
       aes(x = Country.of.Origin.impute.Gmode,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#there can be seen some relationships between these variables but there are countries with only few coffees
#Thus, we probably choose the region 
ggplot(coffees_train,
       aes(x = Region,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#It can be seen that there are differences between Regions

#the relationship between Cupper.Points and Variety.impute.Gmode
ggplot(coffees_train,
       aes(x = Variety.impute.Gmode,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#There are some differences between varieties of coffee, but not very clear

#the relationship between Cupper.Points and Processing.Method.impute.Gmode
ggplot(coffees_train,
       aes(x = Processing.Method.impute.Gmode,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#There is no clear differences betweend processing methods
#There are many values far from mean and median in Washed / Wet

#the relationship between Cupper.Points and Clean.Cup
ggplot(coffees_train,
       aes(x = Clean.Cup,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#Clean.Cup = 10 has higher values than <10, but also many outliers there

#the relationship between Cupper.Points and Sweetness
ggplot(coffees_train,
       aes(x = Sweetness,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#Sweetness = 10 has slightly higher values than <10, but also many outliers there

#the relationship between Cupper.Points and Quakers
ggplot(coffees_train,
       aes(x = Quakers,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#Very Similar mean and similar distributions, but many outliers for Quakers = 0

#the relationship between Cupper.Points and unit_of_measurement
ggplot(coffees_train,
       aes(x = unit_of_measurement,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()

#the relationship between Cupper.Points and Bag.Weight
ggplot(coffees_train,
       aes(x = Bag.Weight,
           y = Cupper.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#There are diffrences between Bag.Weights, but not so big

# Bag.Weight and unit_of_measurement
#can be also higly correlated with Processing_method and Region

#Let's check it with the Cramer's V coefficient
DescTools::CramerV(coffees_train$unit_of_measurement,
                   coffees_train$Region)
#moderate association between unit_of_measurement and Region
DescTools::CramerV(coffees_train$unit_of_measurement,
                   coffees_train$Processing.Method.impute.Gmode)
#low association between unit_of_measurement and Processing.Method.impute.Gmode

DescTools::CramerV(coffees_train$Bag.Weight,
                   coffees_train$Region)
#moderate association between Bag.Weight and Region

DescTools::CramerV(coffees_train$Bag.Weight,
                   coffees_train$Processing.Method.impute.Gmode)
#low association between Bag.Weight and Processing.Method.impute.Gmode

DescTools::CramerV(coffees_train$Bag.Weight,
                   coffees_train$unit_of_measurement)
#low association between Bag.Weight and unit_of_measurement

#There is moderate association between Bag.Weight and Region and also
#between unit_of_measurement and Region
#I do not exclude them from the analysis for now

#I check if there is some linear combination of other variables
( findLinearCombos(coffees_train[, coffees_numeric_vars] ) ->
    coffees_linearCombos )
#there is no such combination

#all coffees_train variables
coffees_variables_all <- names(coffees_train)

#altitudes are very highly correlated to each other, so I chose only altitude_mean_meters.impute_median
coffees_variables_all <-
  coffees_variables_all[!coffees_variables_all %in% 
                          c("altitude_low_meters.impute_median",
                            "altitude_high_meters.impute_median",
                            "Category.One.Defects",
                            "Number.of.Bags")]

#I delete outliers form training set
coffees_train <- coffees_train[-coffees_outliers,]

options(contrasts = c("contr.treatment",
                      "contr.treatment"))

#estimation of the model with all variables
coffees_lm1 <- lm(Cupper.Points ~ ., 
                 data = coffees_train %>% 
                   dplyr::select(coffees_variables_all))


summary(coffees_lm1)
#R-squared 0.8079
#There are some missing coefficients
coef(coffees_lm1)[is.na(coef(coffees_lm1))]

#Unfortunately all of Region dummies seems to be multicollinear.
#Thus, I delete this variable from the analysis
coffees_variables_all2 <-
  coffees_variables_all[-which(coffees_variables_all %in% 
                                c("Region"))]

coffees_lm1a <- lm(Cupper.Points ~ .,
                  data = coffees_train %>% 
                    dplyr::select(coffees_variables_all2))

summary(coffees_lm1a)
#after delete the Region variable R-squared has not changed

# backward eliminationbased on p-value = 0.1
ols_step_backward_p(coffees_lm1a,
                    prem = 0.1,
                    progress = TRUE) -> coffees_lm1_backward_p

# final model details
summary(coffees_lm1_backward_p$model)

coffees_predicted <- predict(coffees_lm)

ggplot(data.frame(error = coffees_train$Cupper.Points - coffees_predicted),
       aes(x = error)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()


ggplot(data.frame(real = coffees_train$Cupper.Points,
                  predicted = coffees_predicted),
       aes(x = predicted, 
           y = real)) +
  geom_point(col = "blue") +
  theme_bw()

cor(coffees_train$Cupper.Points,
    coffees_predicted)


### Evaluation
#Residuals
head(coffees_lm$residuals)

hist(coffees_lm$residuals, breaks = 30)





