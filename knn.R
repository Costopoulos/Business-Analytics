# ************************************************
#  PRACTICAL BUSINESS ANALYTICS 2021
#  COM3018
#
# Marilia Sinopli
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 22 NOVEMBER 2021
#
# UPDATE
# 1.00      09/11/2021    Initial Version   (Uliks Sekiraqa)
# 1.01      22/11/2021    Updated K variable (Uliks Sekiraqa)
# 1.02      23/11/2021    Return performance measures (Dominic Adams)
# 1.03      26/11/2021    Removed target from training data, incorporated generic getModelClassifications (Emilia Lukose)
# 1.04      23/11/2021    Print maximum optimal K (Emilia Lukose)
# ************************************************


# ************************************************
# knnModel() :
#
#
# INPUT   :   data frame         - train        - training dataset
#             data frame         - test         - testing dataset
#
# ************************************************
knnModel<-function(train,test,plot){
  
  K_VALUE = 7
  
  myTitle<-(paste("Preprocessed Dataset KNN. K=",K_VALUE))
  print(myTitle)
  
  # # Remove fields that we do not need the mean values for
  # TO_DROP <- c("genres","production_companies")
  # train <- train[, !(names(train) %in% TO_DROP)]
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train <- train[!(train$adult == ""), ]
  # train <- train[!(train$adult == "[]"), ]
  # train <- train[!(train$budget == ""), ]
  # train <- train[!(train$budget == "[]"), ]
  # train <- train[!(train$genres == ""), ]
  # train <- train[!(train$genres == "[]"), ]
  # train <- train[!(train$original_language == ""), ]
  # train <- train[!(train$original_language == "[]"), ]
  # train <- train[!(train$popularity == ""), ]
  # train <- train[!(train$popularity == "[]"), ]
  # train <- train[!(train$production_companies == ""), ]
  # train <- train[!(train$production_companies == "[]"), ]
  # train <- train[!(train$production_countries == ""), ]
  # train <- train[!(train$production_countries == "[]"), ]
  # train <- train[!(train$revenue == ""), ]
  # train <- train[!(train$revenue == "[]"), ]
  # train <- train[!(train$runtime == ""), ]
  # train <- train[!(train$runtime == "[]"), ]
  # train <- train[!(train$vote_count == ""), ]
  # train <- train[!(train$vote_count == "[]"), ]
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expected output
  train_expected<-train[,positionClassOutput]
  print(table(train_expected))
  str(train_expected)
  browser()
  
  # train_expected<-factor(train_expected)
  # print(table(train_expected))
  # str(train_expected)
  
  # Remove rows with NA fields
  # train_inputs <- na.omit(train_inputs)
  # train_inputs[!apply(train_inputs == "", 3, all),]
  # train_inputs[!apply(train_inputs == "[]", 3, all),]
  
  # train_expected<-factor(train_expected)
  # levels(train_expected)<-c("0","1")
  
  browser()
  knnClassifier <- caret::knn3(train_inputs, factor(train_expected), k=K_VALUE)
  
  # ************************************************
  # Use the created KNN classifier with the test dataset
  measures<-getModelClassifications(myModel=knnClassifier,
                                    testDataset=test,
                                    title=myTitle,
                                    plot=plot)
  
  # ************************************************
  # For loop iterating through values k from 1 to 100
  # Plots a graph showing the trend of accuracy as k rises
  # This code was sourced from:
  # https://api.rpubs.com/kskand/316172
  # by Kumar Skand, October 8 2017
  #
  # ************************************************
  # i=1
  # k.optm=1
  # k_list <- c()
  # for (i in 1:100) {
  #    knn.mod <- knn(train=train_inputs, test=test_inputs, cl=train_expected, k=i)
  #    k.optm[i] <- 100 * sum(test.predictors_labels == knn.mod)/NROW(test.predictors_labels)
  #    k=i
  #    # cat(k,'=',k.optm[i],'')
  #    k_list[i] <- k.optm[i]
  # }
  # plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
  # print(paste("Best K:", which(k_list==max(k_list)), max(k_list)))
  # ************************************************ 
  
  
  return (measures)
}

