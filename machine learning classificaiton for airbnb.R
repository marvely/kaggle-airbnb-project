###### airbnb competetion r code classification multiclass case #############
# what algorithm should i use?
#
 setwd("~/Documents/personal/kaggle/airbnb contest") 
#
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(pROC)
#
# 1.Massaging data sets 
#
######################## training set #############################
train.org <- read.csv( file = "train_users_2.csv", header = TRUE, stringsAsFactors = FALSE)
# 
train.org <- train.org[, - which(colnames(train.org) == 'date_first_booking')] # 
#
# this extraction part would be helpful in my algorithm:
# for date_account_created, split into 3 cols, year, month and day:
#
dac <- as.data.frame(str_split_fixed(train.org$date_account_created, "-", 3))
train.org <- train.org[, - which(colnames(train.org) == 'date_account_created')] # ok
train.org$dacYear <- dac[, 1]
train.org$dacMonth <- dac[, 2]
train.org$dacDay <- dac[, 3]
#
# for timestamp_first_active, also split into year, month and day:
#
train.org$tfaYear <- substring(as.character(train.org$timestamp_first_active), 1, 4)
train.org$tfaMonth <- substring(as.character(train.org$timestamp_first_active), 5, 6)
train.org$tfaDay <- substring(as.character(train.org$timestamp_first_active), 7, 8)
train.org <- train.org[, - which(colnames(train.org) == 'timestamp_first_active')] 
#
# fix the age inputs
col_num_age <- which(colnames(train.org) == "age")
current_year <- as.numeric( substring( as.character( Sys.Date()), 1, 4))
#
# 
for (i in 1: dim(train.org)[1])
{ 
  if ( !is.na(train.org[i, col_num_age]))
  {
    if (train.org[i, col_num_age]> 1900) 
    {
      train.org[i, col_num_age] <- current_year - train.org[i, col_num_age]
    }
  }
}
#
# convert chr to factors:
#
train.names <- colnames(train.org)[-3]
train.names <- train.names[-1]
train.names <- train.names[-c(12:14)]
train.org [train.names] <- lapply( train.org[train.names], as.factor)
levels(train.org$first_affiliate_tracked)[1] <- "-unknown-"

#
################### do the same thing to test #############################
# 
test.org <- read.csv( file = "test_users.csv", header = TRUE, stringsAsFactors = FALSE)
test.org <- test.org[, - which(colnames(test.org) == 'date_first_booking')] # 
#
#
test.dac <- as.data.frame(str_split_fixed(test.org$date_account_created, "-", 3))
test.org <- test.org[, - which(colnames(test.org) == 'date_account_created')] # ok
test.org$dacYear <- test.dac[, 1]
test.org$dacMonth <- test.dac[, 2]
test.org$dacDay <- test.dac[, 3]
#
# for timestamp_first_active, also split into year, month and day:
#
test.org$tfaYear <- substring(as.character(test.org$timestamp_first_active), 1, 4)
test.org$tfaMonth <- substring(as.character(test.org$timestamp_first_active), 5, 6)
test.org$tfaDay <- substring(as.character(test.org$timestamp_first_active), 7, 8)
test.org <- test.org[, - which(colnames(test.org) == 'timestamp_first_active')] 
#
#
test.col_num_age <- which(colnames(test.org) == "age")
#
for (j in 1: dim(test.org)[1])
{ 
  if ( !is.na(test.org[j, test.col_num_age]))
  {
    if (test.org[j, test.col_num_age]> 1900) 
    {
      test.org[j, test.col_num_age] <- current_year - test.org[j, test.col_num_age]
    }
  }
}

#
# convert chr to factors:
#
test.names <- colnames(test.org)[-1]
test.names <- test.names[-2]
test.names <- test.names[-c(11:13)]
test.org [test.names] <- lapply( test.org[test.names], as.factor)
#
# fix the empty first level in first_affiliate_tracked:
levels(test.org$first_affiliate_tracked)[1] <- "-unknown-"









#######################################################################################
# 2. Build first model on training set
# should i split the training set into sub-train and sub-test? or should i just use the ensemble or something machine learning boosting stuffs?
#
##################### randomize the train set ###############################
#

train.org_random_ <- train.org[ sample( dim(train.org)[1]), ]
split_n <- round(0.7*dim(train.org)[1])
train_random <- train.org_random_ [1: split_n, ]
test_random <- train.org_random_ [(split_n + 1):dim(train.org)[1], ]
train_random_noid <- train_random[, -1]
test_random_noid <- train_random[, -1]
ls()
rm("train.org", "train_random", "test_random", "dac", "train.org_random_")
#
################### desicion tree ##########################################
# col13 is coutnry_destination
library(C50)
library(caret)
train.model.1<- train( country_destination ~. , data = train_random_noid, method = "C5.0")

#
# model1 desicion tree try 1:
train.model.3<- C5.0( train_random_noid[-12], train_random_noid$country_destination, trails = 10)
# 
# see a lot of mislabel for NDF, and US. How to improve the model?
remove_from_model.1 <- c("tfaMonth", "age", "tfaDay", "dacDay")
train_random_subset <- train_random_noid[, -which(colnames(train_random_noid) %in% remove_from_model.1)]
train.model.2<- C5.0( train_random_subset[-11], train_random$country_destination )

#

# becasue of the limitations of one single decision tree model, need to use ensemble method!
#
###################### random forest #####################################
#
library(randomForest)
library(foreach)
feature_names <- colnames(train_random_noid)[-12]
train.model.rf.1 <- randomForest(train_random_noid[, feature_names], 
                                train_random_noid$coutry_destination, 
                                ntree= 10, na.action = na.omit)
# random forest has a function just for combining the result in foreach: combine
feature_names<- colnames(train_random_noid)
train.model.rf <- foreach(ntree=rep(50, 5), .combine=combine, 
                          .packages='randomForest') %dopar% {
                            randomForest(feature_names , train_random_noid$coutry_destination, 
                                         ntree=ntree, na.action = na.omit)
}
# 
# if omit NA, how many obs are there left?
# took too long to run and no result 
#
#
################# support machine vector ############################################
#
#


#################### multinomial class response logistic regression ######################
#
train_random_noid$country_destination <-relevel( train_random_noid$country_destination, ref = "NDF")
library(nnet)
train.model.log <- multinom( country_destination ~ gender+age+signup_method+signup_flow+language+affiliate_channel+affiliate_provider, data = train_random_noid)
# Error in nnet.default(X, Y, w, mask = mask, size = 0, skip = TRUE, softmax = TRUE,  : 
#  too many (2784) weights
z <- summary(train.model.log)$coefficients/summary(train.model.log)$standard.errors
#
################## GBM ###############################################
# GBM what is GBM?



# end