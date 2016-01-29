############ Airbnb Kaggle project ################
#
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

#
 setwd("~/Documents/personal/kaggle/airbnb contest") 
#
 countries <- read.csv( file = "countries.csv", header = TRUE)
#
 age_gender <- read.csv( file = "age_gender_bkts.csv", header = TRUE)
#
 train.org <- read.csv( file = "train_users_2.csv", header = TRUE, stringsAsFactors = FALSE)
# 
 test.org <- read.csv( file = "test_users.csv", header = TRUE, stringsAsFactors = FALSE)
#
 sessions <- read.csv( file = "sessions.csv", header = TRUE)
#
# how do I merge sessions with train? and also test?
library(sqldf)
# use sql quary to merge tables
#
################# exploratory data analysis #################
#
 str(sessions)
#
 str(age_gender)
#
 str(countries)
#
 str(train.org)
#
 str(test.org)
#
#
################ modifications on data sets #################
#
# fix time stamps in test and train sets
#


summary(train.org$age)
# clean up, unlikely age values in train:
 train.org$age[train.org$age<14 | train.org$age >90] = -1
 train.org$age[ which (is.na(train.org$age))] = -1
#
table(train.org$gender)
table(train.org$signup_method)
table(train.org$signup_app)
table(train.org$signup_flow)
table(train.org$language)
#
#
table(train.org$country_destination) #NDF the customer has not booked yet.
#
# create a binary outcome, booked or NDF
#
# time stemp
head(train.org$date_account_created)
#
options("scipen" = 10) # 
head(train.org$timestamp_first_active)
#
# extract year, month and day as features and put in the model
#
#
head(train.org$date_first_booking) # have empty cells... not booked yet
#
head(train.org$country_destination)
#
# what kind of model can we use?
#
# if the outcome is binary, choose logistic regression
















# end