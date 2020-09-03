############################################################
########  Machine Learning 1 Project - Big dataset  ########
########               Mateusz Majak                ########
############################################################
library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(class)
library(tibble)
library(purrr)
library(corrplot)
library(DescTools)
library(olsrr)
library(nnet)
library(lmtest)
library(mice)
library(VIM)
library(verification)
library(janitor)
library(nnet)
library(stringr)
library(AER)
#getwd()
#setwd("C:/Users/mateu/Documents/GIT/ProjectsML1")
players_full <- read.csv("players_20.csv",na.strings=c("","NA"))
glimpse(players_full)

#I create players dataset without data which can not be predictor
#for the player position
#there is also a challenge - I have to choose player position dependent variable
#from 3 possible columns: team_position, player_positions, nation_position
#Not every player plays in nation cups, so I will choose between team_position
#and player_positions.
players <- 
  players_full %>% 
  dplyr::select(c(7:8,15,16,18:20,24:25,32:78))

glimpse(players)
#55 columns after deleting columns not good for being predictors

#I will choose player_positions as a dependent variable, because it has no NAs
#There are multiple positions for some players, so I divide such players into
#different rows
players <- 
  players %>%
  tidyr::separate_rows(player_positions, sep = ",")

players$player_positions <- gsub(" ","",players$player_positions)
# There are 15 positions. I will group them into 4 main positions:
#Attacker, Midfielder, Defender and Goalkeeper

players[which(
  players$player_positions=="ST"|
    players$player_positions=="LW"|
    players$player_positions=="RW"|
    players$player_positions=="CF"),
  "player_positions"]  <- "Attacker"

players[which(
  players$player_positions=="CB"|
    players$player_positions=="LB"|
    players$player_positions=="RB"|
    players$player_positions=="RWB"|
    players$player_positions=="LWB"),
  "player_positions"]  <-  "Defender"

players[which(
  players$player_positions=="CM"|
    players$player_positions=="RM"|
    players$player_positions=="LM"|
    players$player_positions=="CAM"|
    players$player_positions=="CDM"),
  "player_positions"]  <- "Midfielder"

players[which(
  players$player_positions=="GK"),
  "player_positions"]  <- "Goalkeeper"

players <- unique(players)
#Now there are 21747 rows in the dataset - there are some players who played
#at different places on the field, which will for sure will be problematic
#considering accuration of predictions, but in fact I will use more realistic
#data, so maybe errors in prediction will show some similarities between
#seemingly different positions in the field

#I delete gk_kicking, gk_positioning, gk_diving, gk_handling,  gk_reflexes and
#gk_speed because they are already represented by other columns: 
#goalkeeping_kicking, goalkeeping_positioning, goalkeeping_reflexes,
#goalkeeping_diving, goalkeeping_handling and movement_acceleration /
#movement_sprint_speed
#player_tags and player_traits are also unique values for some players -
# other players do not have tags and traits - that's why I will also delete them
#I delete team_position also, because it does not provide any additional
#information about position
players <- 
  players %>% 
  dplyr::select(-c("team_position","gk_kicking","gk_positioning","gk_diving",
                   "gk_handling","gk_reflexes","gk_speed","player_tags",
                   "player_traits"))

# Counting NAs in every column
colSums(is.na(players)) %>% 
  sort()

players %>% 
  md.pattern(rotate.names = TRUE)
#Now there are 2036 rows with missing values in 6 columns: pace, shooting,
#passing, dribbling, defending and physic. It's almost 10% of observation 
# so i will omit them in the future analysis.
players <- 
  players %>% 
  dplyr::select(-c("pace","shooting","passing","dribbling","defending",
                   "physic"))

summary(players)

#Most of the numerical variables are integers in 0-100 range
#I will convert skill_moves and weak_foot from 1-5 to 0-100 range and also
# height_cm and weight_kg to 0-100 range to normalize them. It can be helpful
#in the future analysis
players$weak_foot   <- players$weak_foot*100/5
players$skill_moves <- players$skill_moves*100/5
players$height_cm   <- players$height_cm/(max(players$height_cm))*100
players$weight_kg   <- players$weight_kg/(max(players$weight_kg))*100

glimpse(players)
#There are 3 character variables in the dataset: dependent variable
#player_positions, preferred_foot and work_rate. I convert them into factors
players$player_positions <- as.factor(players$player_positions)
players$preferred_foot   <- as.factor(players$preferred_foot)
players$work_rate        <- as.factor(players$work_rate)

#and I create list of factor and numerical predictors
players_numeric_vars <- 
  sapply(players, is.numeric) %>% 
  which() %>% 
  names()

players_factor_vars <- 
  sapply(players, is.factor) %>% 
  which() %>% 
  names()

# Now let's divide the set into learning and testing sample
set.seed(987654321)

players_which_train <- createDataPartition(players$player_positions,
                                           p = 0.7, 
                                           list = FALSE) 
head(players_which_train)

players_train <- players[players_which_train,]
players_test <- players[-players_which_train,]

#The distribution of the target variable in both samples
players_train$player_positions %>% 
  table() %>%
  prop.table()

players_test$player_positions %>% 
  table() %>%
  prop.table()
#Very similar distributions
players_correlations <- 
  cor(players_train[,players_numeric_vars],
      use = "pairwise.complete.obs")

corrplot(players_correlations, 
         method = "pie",tl.cex = 0.5)
#Goalkeeping and defending variables are very highly correlated to each other

#Highly correlated variables - candidates to be excluded from the analysis
highly_correlated_variables <- findCorrelation(players_correlations,
                cutoff = 0.90,
                names = TRUE)

###logit model by multinom()
players_mlogit1 <- multinom(player_positions ~ .,
                    data = players_train)

players_mlogit1_fitted <- predict(players_mlogit1) 

table(players_mlogit1_fitted,
      players_train$player_positions)

lrtest(players_mlogit1)
#I can reject the null hypothesis on the 0.001 level


# Comparison of real and predicted values
accuracy_multinom <- function(predicted, real) {
  ctable_m <- table(predicted, 
                    real)
  accuracy <- (100 * sum(diag(ctable_m)) / sum(ctable_m))
  base_ <- diag(ctable_m) / colSums(ctable_m)
  balanced_accuracy <- mean(100 * ifelse(is.na(base_), 0, base_))
  base_2 <- diag(ctable_m) / rowSums(ctable_m)
  correctly_predicted <- mean(100 * ifelse(is.na(base_2), 0, base_2))
  
  return(c(accuracy = accuracy, 
           balanced_accuracy = balanced_accuracy,
           balanced_correctly_predicted = correctly_predicted))
}

accuracy_multinom(predicted = players_mlogit1_fitted, 
                  real = players_train$player_positions)

ctrl_nocv <- trainControl(method = "none")

churn_logit2_train <- 
  train(players_test ~ .,
        data = churn_train %>% 
          # we exclude customerID
          dplyr::select(-customerID),
        # model type
        method = "glm",
        # family of models
        family = "binomial",
        # train control
        trControl = ctrl_nocv)

# lets see the result

churn_logit2_train

summary(churn_logit2_train)
