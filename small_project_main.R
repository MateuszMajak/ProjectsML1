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
coffee_full <- read.csv("arabica_data_cleaned.csv")
#library(dplyr)
glimpse(coffee_full)

coffee <- coffee_full
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

# Counting NAs in every column
colSums(is.na(coffee)) %>% 
  sort()

glimpse(coffee)

#Let's look if the factors have some hidden NAs
table(coffee$Color)
#216 empty values and 51 of None level

table(coffee$Country.of.Origin)
#1 empty value

table(coffee$Bag.Weight)
#No NAs

table(coffee$Harvest.Year)
#47 empty values

table(coffee$Processing.Method)
#152 empty values

table(coffee$unit_of_measurement)
#No NAs

# Let's start with the column Quakers. There is one NA
table(coffee$Quakers, useNA = "ifany")

#We should probably replace the missing with the median
median(coffee$Quakers, na.rm=TRUE)
coffee$Quakers[is.na(coffee$Quakers)] <- 0

#Let's look on NAs in altitudes columns
table(coffee$altitude_low_meters, useNA = "ifany")
table(coffee$altitude_high_meters, useNA = "ifany")
table(coffee$altitude_mean_meters, useNA = "ifany")

ggplot(coffee,
       aes(x = altitude_low_meters)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()
#There are 227 out of 1311 rows with NA altitudes
#There are also 4 outliers - let's see them
which(coffee$altitude_low_meters>=11000)
which(coffee$altitude_high_meters>=11000)
which(coffee$altitude_mean_meters>=11000)
#They are the same rows
altitude_outliers <- which(coffee$altitude_low_meters>=11000)
coffee[altitude_outliers,]
#The rest of the data containing altitude outliers looks reliably
#Thus, we will keep it and find out if the altitude play the role in the analysis

#Let's look for outliers considering Total.Cup.Point result
ggplot(coffee,
       aes(x = Total.Cup.Points)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()
# We delete outlier (Total.Cup.Points==0)
coffee <- coffee[!(coffee$Total.Cup.Points==0),]

#Let's look on the Country.of.Origin
table(coffee$Country.of.Origin)

#There is one coffee without name of the Country of origin. Let's delete it
coffee <- coffee[!coffee$Country.of.Origin=="",]

#Maybe the region will be more helpful that country, so we create Region column
table(coffee$Country.of.Origin)
coffee[,"Region"] <- NA

#East Africa group
coffee[which(coffee$Country.of.Origin=="Zambia"|
               coffee$Country.of.Origin=="Uganda"|
               coffee$Country.of.Origin=="Burundi"|
               coffee$Country.of.Origin=="Rwanda"|
               coffee$Country.of.Origin=="Malawi"|
               coffee$Country.of.Origin=="Papua New Guinea"|
               coffee$Country.of.Origin=="Mauritius"|
               coffee$Country.of.Origin=="Ethiopia"|
               coffee$Country.of.Origin=="Kenya"
             ),
       "Region"] <- "East Africa"

#West Africa
coffee[which(coffee$Country.of.Origin=="Tanzania, United Republic Of"|
               coffee$Country.of.Origin=="Cote d?Ivoire"
             ),
       "Region"] <- "West Africa"

#Asia and Oceania
coffee[which(coffee$Country.of.Origin=="Japan"|
               coffee$Country.of.Origin=="Thailand"|
               coffee$Country.of.Origin=="Vietnam"|
               coffee$Country.of.Origin=="Myanmar"|
               coffee$Country.of.Origin=="Philippines"|
               coffee$Country.of.Origin=="Laos"|
               coffee$Country.of.Origin=="China"|
               coffee$Country.of.Origin=="India"|
               coffee$Country.of.Origin=="Taiwan"|
               coffee$Country.of.Origin=="Indonesia"|
               coffee$Country.of.Origin=="Papua New Guinea"),
       "Region"] <- "Asia and Oceania"

#Central America
coffee[which(coffee$Country.of.Origin=="Costa Rica"|
               coffee$Country.of.Origin=="Guatemala"|
               coffee$Country.of.Origin=="Haiti"|
               coffee$Country.of.Origin=="Honduras"|
               coffee$Country.of.Origin=="Nicaragua"|
               coffee$Country.of.Origin=="El Salvador"|
               coffee$Country.of.Origin=="Panama"
             ),
       "Region"] <- "Central America"

#South America
coffee[which(coffee$Country.of.Origin=="Peru"|
               coffee$Country.of.Origin=="Ecuador"|
               coffee$Country.of.Origin=="Brazil"|
               coffee$Country.of.Origin=="Colombia"
             ),
       "Region"] <- "South America"
#North America
coffee[which(coffee$Country.of.Origin=="Mexico"|
               coffee$Country.of.Origin=="United States"|
               coffee$Country.of.Origin=="United States (Hawaii)"|
               coffee$Country.of.Origin=="United States (Puerto Rico)"
             ),
       "Region"] <- "North America"

coffee$Region <- as.factor(coffee$Region)

#Now let's look on the Harvest.Year
table(coffee$Harvest.Year)
#Data need some cleaning
coffee$Harvest.Year <- coffee$Harvest.Year
coffee$Harvest.Year <- as.character(coffee$Harvest.Year)
table(coffee$Harvest.Year)

coffee[which(coffee$Harvest.Year=="4T/2010"|
               coffee$Harvest.Year=="23 July 2010"|
               coffee$Harvest.Year=="4T/10"|
               coffee$Harvest.Year=="March 2010"|
               coffee$Harvest.Year=="4t/2010"|
               coffee$Harvest.Year=="4T72010"|
               coffee$Harvest.Year=="47/2010"
             ),
       "Harvest.Year"] <- "2010"

coffee[which(coffee$Harvest.Year=="Sept 2009 - April 2010"|
               coffee$Harvest.Year=="December 2009-March 2010"|
               coffee$Harvest.Year=="2009/2010"|
               coffee$Harvest.Year=="2009-2010"|
               coffee$Harvest.Year=="2009 - 2010"|
               coffee$Harvest.Year=="2009 / 2010"|
               coffee$Harvest.Year=="Fall 2009"|
               coffee$Harvest.Year=="08/09 crop"
             ),
       "Harvest.Year"] <- "2009/2010"

coffee[which(coffee$Harvest.Year=="4t/2011"|
               coffee$Harvest.Year=="January 2011"|
               coffee$Harvest.Year=="3T/2011"|
               coffee$Harvest.Year=="1t/2011"|
               coffee$Harvest.Year==" 1T/2011"|
               coffee$Harvest.Year=="Spring 2011 in Colombia."|
               coffee$Harvest.Year=="Abril - Julio /2011"|
               coffee$Harvest.Year=="1T/2011"
             ),
       "Harvest.Year"] <- "2011"

coffee[which(coffee$Harvest.Year=="2010-2011"),"Harvest.Year"] <- "2010/2011"

coffee[which(coffee$Harvest.Year=="2011/2012"),"Harvest.Year"] <- "Others"

coffee[which(coffee$Harvest.Year=="2013/2014"),"Harvest.Year"] <- "2013/2014"

coffee[which(coffee$Harvest.Year=="2016 / 2017"|
               coffee$Harvest.Year=="2016/2017"),"Harvest.Year"] <- "2016/2017"

coffee[which(coffee$Harvest.Year=="2018"|
               coffee$Harvest.Year=="2017 / 2018"),"Harvest.Year"] <- "2017/2018"

other_years <- which(coffee$Harvest.Year=="Mayo a Julio"|
                       coffee$Harvest.Year=="mmm"|
                       coffee$Harvest.Year=="TEST"|
                       coffee$Harvest.Year=="Abril - Julio"|
                       coffee$Harvest.Year=="August to December"|
                       coffee$Harvest.Year=="May-August"|
                       coffee$Harvest.Year=="January Through April"|
                       coffee$Harvest.Year=="")
coffee[other_years,]

coffee[other_years,"Harvest.Year"] <- "Others"

coffee$Harvest.Year <- factor(coffee$Harvest.Year)



#Now let's look on the next factor - Color
table(coffee$Color)
#We should group NA and None together
coffee[which(coffee$Color==""),"Color"] <- "None"

coffee$Color <- factor(coffee$Color)

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

#Now let's look on the  Processing.Method factor
table(coffee$Processing.Method)


# Now let's divide the set into learning and testing sample
set.seed(987654321)

coffees_which_train <- createDataPartition(coffee$Total.Cup.Points,
                                          p = 0.7, 
                                          list = FALSE) 
head(coffees_which_train)

# we need to apply this index for data division
coffees_train <- coffee[coffees_which_train,]
coffees_test <- coffee[-coffees_which_train,]

# let's check the distribution of
# the target variable in both samples
summary(coffees_train$Total.Cup.Points)
summary(coffees_test$Total.Cup.Points)

# storing the list of names of numeric variables into a vector
coffees_numeric_vars <- 
  # check if variable is numeric
  sapply(coffee, is.numeric) %>% 
  # select those which are
  which() %>% 
  # and keep just their names
  names()

coffees_numeric_vars

# mutually correlated (irrelevant) variables
coffee_correlations <- 
  cor(coffees_train[,coffees_numeric_vars],
      use = "pairwise.complete.obs")

corrplot(coffee_correlations, 
         method = "pie")

coffees_numeric_vars_order <- 
  coffee_correlations[,"Total.Cup.Points"] %>% 
  sort(decreasing = TRUE) %>%
  names()

corrplot.mixed(coffee_correlations[coffees_numeric_vars_order, 
                                   coffees_numeric_vars_order],
               upper = "square",
               lower = "number",
               tl.col="black",
               tl.pos = "lt")

#altitude stats, number of bags and quakers seem to have very small correlation between 
#them and the other variables - we will skip them
corrplot.mixed(coffee_correlations[coffees_numeric_vars_order[-12:-16], 
                                   coffees_numeric_vars_order[-12:-16]],
               upper = "circle",
               lower = "number",
               tl.col = "black",
               tl.pos = "lt")

#The relation of the target variable with the most strongly correlated variables

#Flavour
ggplot(coffees_train,
       aes(x = Total.Cup.Points,
           y = Flavor)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
# 3 coffees not 
# matching the relationship - two have flavour above 6.5 but Total.Cup.Points below 65 
#and one has Flavor close to 6 and the Total.Cup.Points close to 78 - maybe exclude
#these observations from the sample
coffees_which_outliers <- NULL

coffees_outliers_lower <- which(coffees_train$Flavor > 6.5 & coffees_train$Flavor < 7 & coffees_train$Total.Cup.Points < 65)
coffees_outliers_higher <- which(coffees_train$Flavor < 6.25 & coffees_train$Total.Cup.Points > 77.5)

coffees_which_outliers <- append(coffees_which_outliers,coffees_outliers_lower)
coffees_which_outliers <- append(coffees_which_outliers,coffees_outliers_higher)
coffees_which_outliers

#Aftertaste
ggplot(coffees_train,
       aes(x = Total.Cup.Points,
           y = Aftertaste)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# 3 coffees not 
# matching the relationship - the new one has Aftertaste result above 7.5 and total.Cup.Points below 75 
coffees_outliers_lower_aftertaste <- which(coffees_train$Aftertaste > 7.5 & coffees_train$Aftertaste < 8 & coffees_train$Total.Cup.Points < 75)
which(coffees_train$Aftertaste < 7 & coffees_train$Total.Cup.Points < 65) #these are already marked as outliers in the analysis
                                                                          #of relationship between Flavour and Total.Cup.Points
coffees_which_outliers <- append(coffees_which_outliers,coffees_outliers_lower_aftertaste)

#Balance
ggplot(coffees_train,
       aes(x = Total.Cup.Points,
           y = Balance)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

#It is more difficult to see outliers here. One point seems to be not matching the relationship for sure - 
#Total.Cup.Points < 60 and Balance > 6.5 - It was marked as outlier earlier

findCorrelation(coffee_correlations,
                cutoff = 0.75,
                names = TRUE)

# these are potential candidates to be excluded from the model because of the high correlation between them


### qualitative (categorical) variables ###
coffees_categorical_vars <-
  sapply(coffee, is.factor) %>% 
  which() %>% 
  names()

coffees_categorical_vars

# The function that retrieves F statistic value
# for the explanatory categorical variable provided 
# as the function argument

coffees_F_anova <- function(categorical_var) {
  anova_ <- aov(coffees_train$Total.Cup.Points ~ 
                  coffees_train[[categorical_var]]) 
  
  return(summary(anova_)[[1]][1, 4])
}

# Relationship between coffees_categorical_vars and Total.Cup.Points
sapply(coffees_categorical_vars,
       coffees_F_anova) %>% 
  sort(decreasing = TRUE) -> coffees_anova_all_categorical

coffees_anova_all_categorical

#relation between Color levels and Total.Cup.Points
ggplot(coffees_train,
       aes(x = Color,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#there is no clear relationship between these variables
#Also there are many NAs

#relation between Country.of.Origin levels and Total.Cup.Points
ggplot(coffees_train,
       aes(x = Country.of.Origin,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#there can be seen some relationships between these variables but there are countries with only few coffees
#Thus, we probably choose the region 
ggplot(coffees_train,
       aes(x = Region,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#It can be seen that there are differences between Regions

#the relationship between Total.Cup.Points and Harvest.Year
ggplot(coffees_train,
       aes(x = Harvest.Year,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()
#There is no big differences between years and it will not be helpful for predicting
#Total.Cup.Points in the future, so we will ommit it in the analysis

ggplot(coffees_train,
       aes(x = unit_of_measurement,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()

ggplot(coffees_train,
       aes(x = Bag.Weight,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()

#There are no clear relationships between variables and Bag.Weight and unit_of_measurement
#can be higly correlated with Processing_method and Region
#Let's check it with the Cramer's V coefficient
DescTools::CramerV(coffees_train$unit_of_measurement,
                   coffees_train$Region)
#moderate association between unit_of_measurement and Region

DescTools::CramerV(coffees_train$unit_of_measurement,
                   coffees_train$Processing.Method)
#low association between unit_of_measurement and Processing.Method

DescTools::CramerV(coffees_train$Bag.Weight,
                   coffees_train$Region)
#high association between Bag.Weight and Region

DescTools::CramerV(coffees_train$Bag.Weight,
                   coffees_train$Processing.Method)
#low association between Bag.Weight and Processing.Method

DescTools::CramerV(coffees_train$Bag.Weight,
                   coffees_train$unit_of_measurement)
#low association between Bag.Weight and unit_of_measurement


#We will not use unit_of_measurement and Bag.Weight also, because th



ggplot(coffees_train,
       aes(x = Processing.Method,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()







# strength of relation between two CATEGORICAL variables
# can be tested, e.g. using the Cramer's V coefficient
# (calculated on the basis of Chi2 test statistic)
# - Cramer's V  takes values from 0 to 1, where
# higher values mean a stronger relationship
# (if both variables have only two levels 
# Cramer's V take values from -1 to 1)





###
#####
####### Now let's see which independent variables should we choose

# coffees_numeric_vars <- 
#   sapply(coffee, is.numeric) %>% 
#   which() %>% 
#   names()
# 
# coffees_numeric_vars
# 
# 
# coffees_correlations <- 
#   cor(coffee[,coffees_numeric_vars],
#       use = "pairwise.complete.obs")
# 
# coffees_correlations
# 
# corrplot(coffees_correlations, 
#          method = "pie")
# 
# coffee_lm1 <- lm(Total.Cup.Points ~ Aroma,
#                  data = coffee)
# 
# summary(coffee_lm1)
# 
# 
# coffee_lm2 <- lm(Total.Cup.Points ~ Aroma + Flavor + Acidity +
#                    Body + Balance + Sweetness + Category.One.Defects +Category.Two.Defects,
#                  data = coffee)
# 
# summary(coffee_lm2)
# 
# # calculating predicted values based on the last model
# predict(coffee_lm2)
# 
# ggplot(data.frame(error = coffee$Total.Cup.Points - predict(coffee_lm2)),
#        aes(x = error)) +
#   geom_histogram(fill = "blue",
#                  bins = 100) +
#   theme_bw()
# 
# # real values against the predicted
# ggplot(data.frame(real = coffee$Total.Cup.Points,
#                   predicted = predict(coffee_lm2)),
#        aes(x = predicted, 
#            y = real)) +
#   geom_point(col = "blue") +
#   theme_bw()
# 
# #Their correlation
# cor(coffee$Total.Cup.Points,
#     predict(coffee_lm2))
# 
# #All data
# houses_lm3 <- lm(Total.Cup.Points ~ .,
#                  data = coffee %>% 
#                    dplyr::select(houses_variables_all)) # training data
# 
# 
# summary(houses_lm4)
