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
library(mice)
library(VIM)
library(verification)
library(janitor)
library(nnet)
library(stringr)
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

# Counting NAs in every column
colSums(is.na(players)) %>% 
  sort()
#I will choose team_position as a dependent variable, because it has only one
#position for each players. There are 240 missings, so I check if there are only
#one poisition in player_positions for them and delete those who have more than
#1 position.
#Each position contains 2 or 3 letters, so I will choose only rows with max 3
#characters in player_positions column
NA_player_positions <- which(is.na(players$team_position))

players <- players %>% 
  dplyr::mutate(team_position = if_else(is.na(team_position),
                                        player_positions,
                                        team_position))

more_than_1_pos <- which(str_length(players[,"team_position"])>3)

players <- players[-more_than_1_pos,]
#Now there is no NAs in dependent variable

players %>% 
  md.pattern(rotate.names = TRUE)


