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
source("functions/F_accuracy_multinom.R")
library(MLmetrics)

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

#I will choose player_positions 
z = players$player_positions
players$player_positions <- 
  unname(sapply(z, function(z) {
    paste(sort(trimws(strsplit(z[1], ',')[[1]])), collapse=',')} ))


players[which(
  players$player_positions=="ST"|
    players$player_positions=="LW"|
    players$player_positions=="RW"|
    players$player_positions=="CF"|
    players$player_positions=="RM,ST"|
    players$player_positions=="LW,RW"|
    players$player_positions=="LM,ST"|
    players$player_positions=="CAM,ST"|
    players$player_positions=="LM,RM,ST"|
    players$player_positions=="LW,ST"|
    players$player_positions=="RW,ST"|
    players$player_positions=="LW,RW,ST"|
    players$player_positions=="CF,ST"|
    players$player_positions=="CAM,CF"|
    players$player_positions=="LM,LW,ST"|
    players$player_positions=="CAM,LM,ST"|
    players$player_positions=="RM,RW,ST"|
    players$player_positions=="CAM,CF,ST"|
    players$player_positions=="CAM,RM,ST"|
    players$player_positions=="CAM,LW,ST"|
    players$player_positions=="CAM,CM,ST"|
    players$player_positions=="CF,RW,ST"|
    players$player_positions=="CM,ST"|
    players$player_positions=="CF,LW,ST"|
    players$player_positions=="CF,LW,RW"|
    players$player_positions=="CF,LW"|
    players$player_positions=="CAM,RW,ST"|
    players$player_positions=="CAM,CF,CM"|
    players$player_positions=="CAM,CF,LW"|
    players$player_positions=="CF,LM,ST"|
    players$player_positions=="CAM,CF,RW"|
    players$player_positions=="CF,RW"|
    players$player_positions=="CF,LM,LW"|
    players$player_positions=="CF,RM,RW"|
    players$player_positions=="LM,RW,ST"|
    players$player_positions=="CF,RM,ST"|
    players$player_positions=="LW,RM,ST"|
    players$player_positions=="CF,CM,ST"|
    players$player_positions=="CM,LW,ST"|
    players$player_positions=="CF,CM,LW"|
    players$player_positions=="LW,LWB,ST"|
    players$player_positions=="CM,RW,ST"|
    players$player_positions=="CF,LWB,ST"|
    players$player_positions=="CF,LM,RW"|
    players$player_positions=="CDM,LW,ST"),
  "player_positions"]  <- "Attacker"

players[which(
  players$player_positions=="CB"|
    players$player_positions=="LB"|
    players$player_positions=="RB"|
    players$player_positions=="RWB"|
    players$player_positions=="LWB"|
    players$player_positions=="CB,RB"|
    players$player_positions=="CB,LB"|
    players$player_positions=="CB,CDM"|
    players$player_positions=="LB,RB"|
    players$player_positions=="LB,LM"|
    players$player_positions=="RB,RM"|
    players$player_positions=="LB,LWB"|
    players$player_positions=="CB,CDM,CM"|
    players$player_positions=="RB,RWB"|
    players$player_positions=="CB,LB,RB"|
    players$player_positions=="LB,LM,LWB"|
    players$player_positions=="RB,RM,RWB"|
    players$player_positions=="CB,CDM,RB"|
    players$player_positions=="CDM,RB"|
    players$player_positions=="CDM,CM,RB"|
    players$player_positions=="LB,RB,RM"|
    players$player_positions=="CB,CM"|
    players$player_positions=="CM,RB"|
    players$player_positions=="CM,LB"|
    players$player_positions=="LM,LW,RW"|
    players$player_positions=="CB,RB,RM"|
    players$player_positions=="LM,RB,RM"|
    players$player_positions=="CM,LB,LM"|
    players$player_positions=="CDM,LB"|
    players$player_positions=="LB,LM,RM"|
    players$player_positions=="CB,LB,LWB"|
    players$player_positions=="LB,LW"|
    players$player_positions=="CB,LB,LM"|
    players$player_positions=="LB,LM,LW"|
    players$player_positions=="CDM,LB,RB"|
    players$player_positions=="LB,LM,RB"|
    players$player_positions=="CB,RB,RWB"|
    players$player_positions=="CB,CDM,LB"|
    players$player_positions=="LB,RB,RWB"|
    players$player_positions=="CB,LWB"|
    players$player_positions=="CDM,CM,LB"|
    players$player_positions=="RB,RW"|
    players$player_positions=="CDM,RB,RM"|
    players$player_positions=="LM,RM,RWB"|
    players$player_positions=="CB,RWB"|
    players$player_positions=="RB,RM,RW"|
    players$player_positions=="LB,LWB,RB"|
    players$player_positions=="CDM,LB,LM"|
    players$player_positions=="RB,RM,ST"|
    players$player_positions=="LB,RM"|
    players$player_positions=="CAM,RB"|
    players$player_positions=="CM,LB,RB"|
    players$player_positions=="CB,CM,RB"|
    players$player_positions=="CAM,LB,LM"|
    players$player_positions=="CDM,RB,RWB"|
    players$player_positions=="LB,LW,RB"|
    players$player_positions=="CB,RM"|
    players$player_positions=="CB,CM,LB"|
    players$player_positions=="LB,RB,RW"|
    players$player_positions=="CM,RB,RWB"|
    players$player_positions=="CDM,LB,LWB"|
    players$player_positions=="CB,LM,LWB"|
    players$player_positions=="LWB,RB,RWB"|
    players$player_positions=="LM,RB,RWB"|
    players$player_positions=="LB,LW,LWB"|
    players$player_positions=="CM,LB,LWB"|
    players$player_positions=="CB,LWB,RWB"|
    players$player_positions=="CB,LM,RB"|
    players$player_positions=="CB,LB,RM"|
    players$player_positions=="CB,CM,RWB"|
    players$player_positions=="CAM,CB,RB"),
  "player_positions"]  <-  "Defender"

players[which(
  players$player_positions=="CM"|
    players$player_positions=="RM"|
    players$player_positions=="LM"|
    players$player_positions=="CAM"|
    players$player_positions=="CDM"|
    players$player_positions=="CDM,CM"|
    players$player_positions=="LM,RM"|
    players$player_positions=="CAM,CM"|
    players$player_positions=="CAM,CDM,CM"|
    players$player_positions=="CAM,LM,RM"|
    players$player_positions=="CAM,LM"|
    players$player_positions=="CAM,RM"|
    players$player_positions=="RM,RW"|
    players$player_positions=="CAM,CM,LM"|
    players$player_positions=="CM,RM"|
    players$player_positions=="LM,LW"|
    players$player_positions=="CM,LM"|
    players$player_positions=="CAM,CM,RM"|
    players$player_positions=="CM,LM,RM"|
    players$player_positions=="CAM,LW"|
    players$player_positions=="CAM,RW"|
    players$player_positions=="LM,RM,RW"|
    players$player_positions=="CAM,LW,RW"|
    players$player_positions=="CAM,CM,RW"|
    players$player_positions=="LM,LW,RM"|
    players$player_positions=="CM,RB,RM"|
    players$player_positions=="CDM,CM,RM"|
    players$player_positions=="CAM,CM,RM"|
    players$player_positions=="CDM,CM,LM"|
    players$player_positions=="CAM,CDM"|
    players$player_positions=="LW,RM,RW"|
    players$player_positions=="CF,LM,RM"|
    players$player_positions=="CAM,CM,LW"|
    players$player_positions=="CAM,RM,RW"|
    players$player_positions=="CM,RW"|
    players$player_positions=="RM,RWB"|
    players$player_positions=="LM,LWB"|
    players$player_positions=="CM,LW,RW"|
    players$player_positions=="CM,LW"|
    players$player_positions=="CAM,LM,LW"|
    players$player_positions=="CAM,CF,RM"|
    players$player_positions=="CAM,CF,LM"|
    players$player_positions=="CF,RM"|
    players$player_positions=="CF,LM"|
    players$player_positions=="LW,RM"|
    players$player_positions=="LM,RW"|
    players$player_positions=="LM,LWB,RM"|
    players$player_positions=="CM,RWB"|
    players$player_positions=="CM,LM,LW"|
    players$player_positions=="LWB,RWB"|
    players$player_positions=="CM,RM,RW"|
    players$player_positions=="CM,LM,ST"|
    players$player_positions=="RM,RWB,ST"|
    players$player_positions=="RM,RW,RWB"|
    players$player_positions=="CM,LWB"|
    players$player_positions=="CF,CM,RM"|
    players$player_positions=="CF,CM"|
    players$player_positions=="CDM,LM"|
    players$player_positions=="CAM,LM,RW"|
    players$player_positions=="LW,LWB"|
    players$player_positions=="LM,RWB"|
    players$player_positions=="LM,LW,LWB"|
    players$player_positions=="CM,RM,ST"|
    players$player_positions=="CM,RM,RWB"|
    players$player_positions=="CM,LM,RW"|
    players$player_positions=="CM,LM,LWB"|
    players$player_positions=="CDM,RWB"|
    players$player_positions=="CDM,RM"|
    players$player_positions=="CAM,RM,RWB"|
    players$player_positions=="LM,LWB,RWB"|
    players$player_positions=="CDM,LM,RM"|
    players$player_positions=="CDM,CM,RWB"|
    players$player_positions=="CDM,CM,LW"|
    players$player_positions=="CAM,LW,RM"|
    players$player_positions=="CAM,LM,LWB"|
    players$player_positions=="CAM,CDM,RM"|
    players$player_positions=="CAM,CDM,LM"|
    players$player_positions=="LWB,RM,RWB"|
    players$player_positions=="LWB,RM"|
    players$player_positions=="LM,RWB,ST"|
    players$player_positions=="CM,LWB,RWB"|
    players$player_positions=="CM,LW,RM"|
    players$player_positions=="CM,LW,LWB"|
    players$player_positions=="CF,CM,LM"|
    players$player_positions=="CDM,LM,LWB"|
    players$player_positions=="CDM,LM,LW"|
    players$player_positions=="CDM,CM,ST"|
    players$player_positions=="CDM,CM,RW"|
    players$player_positions=="CDM,CM,LWB"|
    players$player_positions=="CDM,CF,CM"),
  "player_positions"]  <- "Midfielder"

players[which(
  players$player_positions=="GK"),
  "player_positions"]  <- "Goalkeeper"

players[which(
  players$player_positions!="Goalkeeper" &
    players$player_positions!="Midfielder" &
    players$player_positions!="Defender" &
    players$player_positions!="Attacker"),
  "player_positions"]  <- "Others"

sort(table(players$player_positions))

players <- players[-which(players$player_positions=="Others"),]

# I delete gk_kicking, gk_positioning, gk_diving, gk_handling,  gk_reflexes and
# gk_speed because they are already represented by other columns:
# goalkeeping_kicking, goalkeeping_positioning, goalkeeping_reflexes,
# goalkeeping_diving, goalkeeping_handling and movement_acceleration /
# movement_sprint_speed
# player_tags and player_traits are also unique values for some players -
# other players do not have tags and traits - that's why I will also delete them
# I delete team_position also, because it does not provide any additional
# information about position
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
#The null hypothesis can be rejected on the 0.001 level

# Comparison of real and predicted values
accuracy_multinom(predicted = players_mlogit1_fitted, 
                  real = players_train$player_positions)
###KNN model by train ()
modelLookup("knn")

ctrl_cv10 <- trainControl(method = "cv",
                          number = 10,
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary)

set.seed(987654321)

players_train_knn <- 
  train(player_positions ~ .,
        data = players_train,
        method = "knn",
        trControl = ctrl_cv10)

#result
players_train_knn
#package automatically chose the k = 9 based on the highest possible accuracy

#I will try other K values
test_k <- data.frame(k = seq(1, 99, 4))

players_train_knn2 <- 
  train(player_positions ~ .,
        data = players_train,
        method = "knn",
        trControl = ctrl_cv10,
        tuneGrid = test_k)


plot(players_train_knn2)

players_train_knn2_scaled <- 
  train(player_positions ~ .,
        data = players_train,
        method = "knn",
        trControl = ctrl_cv10,
        tuneGrid = test_k,
        preProcess = c("range"),
        metric = "logLoss")

players_train_knn2_scaled
#logLoss was used to select the optimal model using the smallest value.
#The final value used for the model was k = 97.


plot(players_train_knn2_scaled)
#Here the best accuracy for k = 97, but it looks like it is not much higher
# than for k = 20



players_test_forecasts <- 
  data.frame(players_mlogit1 = predict(players_mlogit1,
                                         players_test),
             players_train_knn = predict(players_train_knn,
                                        players_test),
             players_train_knn2 = predict(players_train_knn2,
                                          players_test),
             players_train_knn2_scaled = predict(players_train_knn2_scaled,
                                                 players_test))

head(players_test_forecasts)

sapply(players_test_forecasts,
       function(x) accuracy_multinom(predicted = x,
                                        real = players_test$player_positions))










