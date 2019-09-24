rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(pROC)
library(stringr)
library(caret)
library(gbm)
library(Metrics)
library(RANN)
library(mice)

setwd("C:\\Users\\sbortnem\\Desktop\\Spring 2019\\APAN 5200\\Kaggle Competition")




data = read.csv('analysisData.csv',na.strings=c("","N/A"))
scoringData = read.csv('scoringData.csv',na.strings=c("","N/A"))

#what are the different levels of property_type?
table(data$property_type)
table(scoringData$property_type)

#move property_type less than 50 to Other -- DATA
level_prop_type = levels(data$property_type)
table_prop_type = table(data$property_type)
for(level in level_prop_type){
  if(table_prop_type[level] < 134){
    data$property_type[data$property_type == level] = "Other"
  } 
}
data$property_type = droplevels(data$property_type)

#move property_type less than 50 to Other -- SCORINGDATA
level_prop_type1 = levels(scoringData$property_type)
table_prop_type1 = table(scoringData$property_type)
for(level in level_prop_type1){
  if(table_prop_type1[level] < 50){
    scoringData$property_type[scoringData$property_type == level] = "Other"
  } 
}
scoringData$property_type = droplevels(scoringData$property_type)



#make variable for length of name,summary,space and description -- DATA
data$name_nchar = nchar(as.character(data$name))
data$summary_nchar = nchar(as.character(data$summary))
data$space_nchar = nchar(as.character(data$space))
data$description_nchar = nchar(as.character(data$description))

#make variable for length of name,summary,space and description -- SCORINGDATA
scoringData$name_nchar = nchar(as.character(scoringData$name))
scoringData$summary_nchar = nchar(as.character(scoringData$summary))
scoringData$space_nchar = nchar(as.character(scoringData$space))
scoringData$description_nchar = nchar(as.character(scoringData$description))




#count number of amenities -- DATA
for(x in 1:nrow(data)){
  amenities = (str_count(data$amenities[x], ',') + 1)
  data$amenities_number[x] = amenities
}

#check for certain amenities, make to binary variables-- DATA
data$amenities = as.character(data$amenities)
data$AirConditioning = as.numeric(str_detect(data$amenities, "Air conditioning"))
data$FamilyFriendly = as.numeric(str_detect(data$amenities, "Family/kid friendly"))
data$PetFriendly = as.numeric(str_detect(data$amenities, "Pets allowed"))
data$SmokerFriendly = as.numeric(str_detect(data$amenities, "Smoking allowed"))


#count number of amenities -- SCORINGDATA
for(x in 1:nrow(scoringData)){
  amenities = (str_count(scoringData$amenities[x], ',') + 1)
  scoringData$amenities_number[x] = amenities
}

#check for certain amenities, make to binary variables -- SCORINGDATA
scoringData$amenities = as.character(scoringData$amenities)
scoringData$AirConditioning = as.numeric(str_detect(scoringData$amenities, "Air conditioning"))
scoringData$FamilyFriendly = as.numeric(str_detect(scoringData$amenities, "Family/kid friendly"))
scoringData$PetFriendly = as.numeric(str_detect(scoringData$amenities, "Pets allowed"))
scoringData$SmokerFriendly = as.numeric(str_detect(scoringData$amenities, "Smoking allowed"))




#convert weekly price and monthly price to numerical -- DATA
data$weekly_price = as.numeric(gsub('[$,]', '', data$weekly_price))   #check these
data$monthly_price = as.numeric(gsub('[$,]','', data$monthly_price))

#if na, replace weekly price from monthly price and monthly price from weekly price - DATA
weekly_price_NA = which(is.na(data$weekly_price) == TRUE & is.na(data$monthly_price) == FALSE)
data[weekly_price_NA,]$weekly_price = (data[weekly_price_NA,]$monthly_price * (7/30))
monthly_price_NA = which(is.na(data$monthly_price) == TRUE & is.na(data$weekly_price) == FALSE)
data[monthly_price_NA,]$monthly_price = (data[monthly_price_NA,]$weekly_price * (30/7))

#convert weekly price and monthly price to numerical -- SCORINGDATA
scoringData$weekly_price = as.numeric(gsub('[$,]', '', scoringData$weekly_price))   #check these
scoringData$monthly_price = as.numeric(gsub('[$,]','', scoringData$monthly_price))

#if na, replace weekly price from monthly price and monthly price from weekly price - SCORINGDATA
weekly_price_NA1 = which(is.na(scoringData$weekly_price) == TRUE & is.na(scoringData$monthly_price) == FALSE)
scoringData[weekly_price_NA1,]$weekly_price = (scoringData[weekly_price_NA1,]$monthly_price * (7/30))
monthly_price_NA1 = which(is.na(scoringData$monthly_price) == TRUE & is.na(scoringData$weekly_price) == FALSE)
scoringData[monthly_price_NA1,]$monthly_price = (scoringData[monthly_price_NA1,]$weekly_price * (30/7))


#make feature for whether there is an about host -- DATA
data$host_about = as.character(data$host_about)
for(x in 1:nrow(data)){
  if(is.na(data$host_about[x]) | data$host_about[x] == ""){
    data$host_about_avail[x] = FALSE
    data$host_about_avail[x] = as.numeric(data$host_about_avail[x])
  } else {
    data$host_about_avail[x] = TRUE
    data$host_about_avail[x] = as.numeric(data$host_about_avail[x])
  }
}

#make feature for whether there is an about host -- SCORINGDATA
scoringData$host_about = as.character(scoringData$host_about)
for(x in 1:nrow(scoringData)){
  if(is.na(scoringData$host_about[x]) | scoringData$host_about[x] == ""){
    scoringData$host_about_avail[x] = FALSE
    scoringData$host_about_avail[x] = as.numeric(scoringData$host_about_avail[x])
  } else {
    scoringData$host_about_avail[x] = TRUE
    scoringData$host_about_avail[x] = as.numeric(scoringData$host_about_avail[x])
  }
}

#make feature for whether there is a transit -- DATA
data$transit = as.character(data$transit)
for(x in 1:nrow(data)){
  if(is.na(data$transit[x]) | data$transit[x] == ""){
    data$transit_avail[x] = FALSE
    data$transit_avail[x] = as.numeric(data$transit_avail[x])
  } else {
    data$transit_avail[x] = TRUE
    data$transit_avail[x] = as.numeric(data$transit_avail[x])
  }
}

#make feature for whether there is a transit -- SCORINGDATA
scoringData$transit = as.character(scoringData$transit)
for(x in 1:nrow(scoringData)){
  if(is.na(scoringData$transit[x]) | scoringData$transit[x] == ""){
    scoringData$transit_avail[x] = FALSE
    scoringData$transit_avail[x] = as.numeric(scoringData$transit[x])
  } else {
    scoringData$transit_avail[x] = TRUE
    scoringData$transit_avail[x] = as.numeric(scoringData$transit[x])
  }
}



#convert factors into numeric if possible -- DATA
data$price = as.numeric(gsub('[$,]', '', data$price))
data$security_deposit = as.numeric(gsub('[$,]','', data$security_deposit))
data$cleaning_fee = as.numeric(gsub('[$,]','', data$cleaning_fee))
data$extra_people = as.numeric(gsub('[$,]','', data$extra_people))



#convert factors into numeric if possible -- SCORINGDATA
scoringData$price = as.numeric(gsub('[$,]', '', scoringData$price))
scoringData$security_deposit = as.numeric(gsub('[$,]','', scoringData$security_deposit))
scoringData$cleaning_fee = as.numeric(gsub('[$,]','', scoringData$cleaning_fee))
scoringData$extra_people = as.numeric(gsub('[$,]','', scoringData$extra_people))




#select features to not keep
column_drop = c("listing_url", "scrape_id", "last_scraped", "name","summary", "space", "description","experiences_offered","neighborhood_overview","transit","notes","access","interaction","house_rules",
                "thumbnail_url","medium_url", "picture_url","xl_picture_url","host_id","host_url","host_name","host_since","host_location","host_about","host_response_rate","host_acceptance_rate",
                "host_thumbnail_url","host_picture_url","host_neighbourhood","host_verifications","street", "neighbourhood","neighbourhood_cleansed","city","state","zipcode","market","smart_location",
                "country_code","country","latitude","longitude","amenities","square_feet","calendar_updated","has_availability","calendar_last_scraped","requires_license","license","jurisdiction_names","is_business_travel_ready",
                "host_is_superhost","host_listings_count","host_has_profile_pic","is_location_exact","first_review","last_review","instant_bookable","cancellation_policy","require_guest_profile_picture",
                "require_guest_phone_verification","host_total_listings_count","number_of_reviews","availability_30","availability_60","availability_90","availability_365","name_nchar","space_nchar"
)
                #after reviewing correlation -- cor < 0.1
                #"host_is_superhost","host_listings_count","host_total_listings_count","host_has_profile_pic","host_identity_verified","is_location_exact",
                #"extra_people","minimum_nights","maximum_nights","availability_30","availability_60","availability_90","availability_365","number_of_reviews","first_review",
                #"last_review","review_scores_accuracy","review_scores_cleanliness","review_scores_checkin","review_scores_communication","review_scores_location","instant_bookable","require_guest_profile_picture",
                #"require_guest_phone_verification"
                

data_clean = data[,!(names(data) %in% column_drop)]
scoringData_clean = scoringData[,!(names(scoringData) %in% column_drop)]


#run mice to impute NAs, add id and price back in 
#mice_imputed_data = complete(mice(data_clean[,-(1:2)],m=1, maxit=50, method='cart', seed=500))
#mice_imputed_data$id = data_clean$id
#mice_imputed_data$price = data_clean$price
#write.csv(mice_imputed_data, 'mice_imputed_data.csv', row.names = T)
data_mice = read.csv('mice_imputed_data.csv')


#run mice to impute NAs, add id back
#mice_imputed_scoringData = complete(mice(scoringData_clean[,-1], m=1, maxit = 50, method = 'cart', seed=500))
#mice_imputed_scoringData$id = scoringData_clean$id
#write.csv(mice_imputed_scoringData, 'mice_imputed_scoringData.csv', row.names = T)
scoringData_mice = read.csv('mice_imputed_scoringData.csv')



#columns to run model on
model_columns_data = c("price","neighbourhood_group_cleansed","room_type","accommodates",
                       "bathrooms", "bedrooms","beds","weekly_price","monthly_price","cleaning_fee","guests_included",
                       "extra_people","minimum_nights","maximum_nights","review_scores_accuracy",
                       "review_scores_cleanliness","review_scores_location","calculated_host_listings_count",
                       "reviews_per_month","summary_nchar","description_nchar","amenities_number","FamilyFriendly","id")
data_mice_model = data_mice[names(data_mice) %in% model_columns_data]
model_columns_scoringData = c("price","neighbourhood_group_cleansed","room_type","accommodates",
                              "bathrooms", "bedrooms","beds","weekly_price","monthly_price","cleaning_fee","guests_included",
                              "extra_people","minimum_nights","maximum_nights","review_scores_accuracy",
                              "review_scores_cleanliness","review_scores_location","calculated_host_listings_count",
                              "reviews_per_month","summary_nchar","description_nchar","amenities_number","FamilyFriendly","id")
scoringData_mice_model = scoringData_mice[names(scoringData_mice) %in% model_columns_scoringData]

#USE TO FIGURE OUT IMPORTANCE OF ALL OF THE VARIABLES
#data_mice_model = data_mice[,-36]
#scoringData_mice_model = scoringData_mice[,-36]



library(rsample)
library(randomForest)
library(ranger)

#make dummy variables
#data_clean_dummy = dummyVars(" ~.", data = data_mice_model)
#data_mice_model = data.frame(predict(data_clean_dummy, newdata = data_mice_model))
#data_mice_model

#scoringData_clean_dummy = dummyVars(" ~.", data = scoringData_mice_model)
#scoringData_mice_model = data.frame(predict(scoringData_clean_dummy, newdata = scoringData_mice_model))
#scoringData_mice_model

#look at structure of dummy data frame
#str(data_mice_model)
#str(scoringData_mice_model)

#set.seed(100)
#split = createDataPartition(y = data_mice_model$price, p=0.7,list = F, groups = 1450)
#train = data_mice_model[split,]
#test = data_mice_model[-split,]
#nrow(data_mice_model) == nrow(train) + nrow(test)


#boosting                                                                                 IF YOU GET FEWER VARIABLES, MAKE MORE TREES
#boost = gbm(price ~., data = train, distribution = "gaussian",
#            n.trees = 500, interaction.depth = 5, shrinkage = 0.1)

#predict on train
#predBoostTrain = predict(boost, n.trees = 500)
#rmse(predBoostTrain, train$price)                                             #140.6259

#predict on test
#predBoostTest = predict(boost, newdata = test, n.trees = 500)
#rmse(predBoostTest, test$price)                                               #186.3304

#now running boost on the entire data set -- DATA_DUMMY
#boostData_dummy = gbm(price ~., data=data_mice_model, distribution = "gaussian",
#                      n.trees = 500,interaction.depth = 8, shrinkage = 0.1)

#predBoostData_dummy = predict(boostData_dummy, n.trees = 500)
#rmse(predBoostData_dummy, data_mice_model$price)                                   #133.471

#pred boost on large data set -- SCORINGDATA_DUMMY
#predBoostScoring_dummy = predict(boostData_dummy, newdata = scoringData_mice_model, n.trees = 500)


#check for NAs in predBoostScoringData_dummy
#sum(is.na(predBoostScoring_dummy))

#summary of boost on entire model
#summary(boostData_dummy)

#write to submission file
#submissionFileBoost = data.frame(id = scoringData_mice_model$id, price = predBoostScoring_dummy)
#sum(is.na(submissionFileBoost)) == 0
#write.csv(submissionFileBoost, 'sample_submission_boost.csv', row.names = F)















#dummy encode
dmy1 = dummyVars(" ~ .", data = data_mice_model)
data_dummy = data.frame(predict(dmy1, newdata = data_mice_model))
dmy2 = dummyVars(" ~ .", data = scoringData_mice_model)
scoringData_dummy = data.frame(predict(dmy2, newdata = scoringData_mice_model))

set.seed(100)
split = createDataPartition(y = data_dummy$price, p=0.7,list = F, groups = 1450)
train = data_dummy[split,]
test = data_dummy[-split,]
nrow(data_dummy) == nrow(train) + nrow(test)

model_ranger = ranger (
      formula         = price ~ .,
      data            = train,
      num.trees       = 1000,
      mtry            = 6,
      min.node.size   = 4,
      importance      = 'impurity',
      respect.unordered.factors = TRUE
)

sqrt(model_ranger$prediction.error)                                                  #191.5532      #192.2229

#find RMSE of test set 
pred_ranger = predict(model_ranger, test, num.trees = 1000)
rmse_ranger = rmse(pred_ranger$predictions, test$price); rmse_ranger                 #178.0164      #177.8356

#find RMSE of entire data set 
pred_ranger3 = predict(model_ranger, data_dummy, num.trees = 1000)
rmse_ranger3 = rmse(pred_ranger3$predictions, data_dummy$price); rmse_ranger3        #123.9874      #125.1166

#pred on scoringData
pred_ranger2 = predict(model_ranger, data = scoringData_dummy , num.trees = 1000) 

#generate submission file data frame, check for NAs, and generate file
submissionFile = data.frame(id = scoringData_dummy$id, price = pred_ranger2$predictions)
sum(is.na(submissionFile)) == 0
write.csv(submissionFile, 'sample_submission_ranger_dummy.csv', row.names = F)



#NEED TO ADD DUMMY ENCODING BEFORE RUN BELOW

##ranger grid search -- 96 total combos -- RUN THIS BEFORE I GO TO SLEEP
#hyper_grid = expand.grid(
#  mtry = seq(10, 19, by = 2),
#  node_size = seq(3, 9, by =2),
#  sample_size = c(.55, .632, .70, .80),
#  OOB_RMSE = 0
#)

#for(i in 1:nrow(hyper_grid)){
  
# #train model
#  model = ranger(
#    formula         = price ~ .,
#    data            = train,
#    num.trees       = 500,
#    mtry            = hyper_grid$mtry[i],
#    min.node.size   = hyper_grid$node_size[i],
#    sample.fraction = hyper_grid$sample_size[i],
#    seed            = 123
#  )
  
#  #add OOB error to grid
#  hyper_grid$OOB_RMSE[i] = sqrt(model$prediction.error)
#}

#hyper_grid %>%
#  dplyr::arrange(OOB_RMSE) %>%
#  head(10)


#one-hot encode the categorical variables
#one_hot = dummyVars(~.,train,fullRank = FALSE)
#train_hot = predict(one_hot, train) %>% as.data.frame()

##make ranger compatible names
#names(train_hot) = make.names(names(train_hot), allow_ = FALSE)

##hyperparameter grid search
#hyper_grid_2 = expand.grid(
#  mtry     = seq(10, 25, by = 6),
#  node_size = seq(3, 9, by = 2),
#  sample_size = c(.55, .632, .70, .80),
#  OOB_RMSE = 0
#)

##perform grid search
#for(i in 1:nrow(hyper_grid_2)) {
  
#  #train model
#  model = ranger(
#    formula         = price ~ .,
#    data            = train_hot,
#    num.trees       = 500,
#    mtry            = hyper_grid_2$mtry[i],
#    min.node.size   = hyper_grid_2$node_size[i],
#    sample.fraction = hyper_grid_2$sample_size[i],
#    seed            = 123
#  )
  
#  #add OOB error to grid
#  hyper_grid_2$OOB_RMSE[i] = sqrt(model$prediction.error)
#}

#hyper_grid_2 %>%
#  dplyr::arrange(OOB_RMSE) %>%
#  head(10)
