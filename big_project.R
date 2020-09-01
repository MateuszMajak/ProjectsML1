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
#getwd()
#setwd("C:/Users/Admin/Documents/GIT/ProjectsML1")
players_full <- read.csv(".csv",na.strings=c("","NA"))
glimpse(players_full)

#We create players dataset without clumsy data like !!!  [[XXXXXXXXXXXXXXXXXXXXXXXXXXXX]]  !!!
players <- players_full
players[,c("X","Species","Certification.Address","Certification.Contact","Lot.Number","Mill","ICO.Number","Expiration",
          "Grading.Date","Company","Farm.Name","Owner","Producer","Owner.1","In.Country.Partner","Region",
          "Altitude","Certification.Body","Harvest.Year")] <- NULL

# Counting NAs in every column
colSums(is.na(players)) %>% 
  sort()

players %>% 
  md.pattern(rotate.names = TRUE)

a
