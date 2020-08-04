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

# Countung NAs in every column
colSums(is.na(coffee)) %>% 
  sort()


# Let's start with the column Quakers. There is one NA
table(coffee$Quakers, useNA = "ifany")

#We should probably replace the missing with the median
median(coffee$Quakers, na.rm=TRUE)
coffee$Quakers[is.na(coffee$Quakers)] <- 0

#227 NAs here
table(coffee$altitude_low_meters, useNA = "ifany")
table(coffee$altitude_high_meters, useNA = "ifany")
table(coffee$altitude_mean_meters, useNA = "ifany")

ggplot(coffee,
       aes(x = Total.Cup.Points)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()

# We delete outlier (Total.Cup.Points==0)
coffee <- coffee[!(coffee$Total.Cup.Points==0),]

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

# these are potential candidates to be excluded from the model


# qualitative (categorical) variables
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

# Relationship between Country.of.Origin and Total.Cup.Points
sapply(coffees_categorical_vars,
       coffees_F_anova) %>% 
  # in addition lets sort them
  # in the decreasing order of F
  #  and store as an object
  sort(decreasing = TRUE) -> coffees_anova_all_categorical

coffees_anova_all_categorical

ggplot(coffees_train,
       aes(x = Color,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()

ggplot(coffees_train,
       aes(x = Country.of.Origin,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()

ggplot(coffees_train,
       aes(x = Bag.Weight,
           y = Total.Cup.Points)) +
  geom_boxplot(fill = "red") +
  theme_bw()







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


coffee_lm2 <- lm(Total.Cup.Points ~ Aroma + Flavor + Acidity +
                   Body + Balance + Sweetness + Category.One.Defects +Category.Two.Defects,
                 data = coffee)

summary(coffee_lm2)

# calculating predicted values based on the last model
predict(coffee_lm2)

ggplot(data.frame(error = coffee$Total.Cup.Points - predict(coffee_lm2)),
       aes(x = error)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()

# real values against the predicted
ggplot(data.frame(real = coffee$Total.Cup.Points,
                  predicted = predict(coffee_lm2)),
       aes(x = predicted, 
           y = real)) +
  geom_point(col = "blue") +
  theme_bw()

#Their correlation
cor(coffee$Total.Cup.Points,
    predict(coffee_lm2))

#All data
houses_lm3 <- lm(Total.Cup.Points ~ .,
                 data = coffee %>% 
                   dplyr::select(houses_variables_all)) # training data


summary(houses_lm4)
