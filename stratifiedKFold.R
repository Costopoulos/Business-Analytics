# ************************************************
#  PRATICAL BUSINESS ANALYTICS 2021
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
# 1.00      09/11/2021    Initial Version   (Dominic Adams)
# ************************************************

# ************************************************
# Global variables
# ************************************************

KFOLDS <- 5 # Number of folds to use in stratified k-fold cross-validation

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# allocateFoldID() :
#
# Append a column called "foldID" that indicates the fold number
#
# INPUT   :   data frame         - df        - dataset
#
# OUTPUT  :   data frame         - df        - dataset with foldID added
#
# ************************************************
allocateFoldID<-function(df){
  
  recordsPerFold<-ceiling(nrow(df)/KFOLDS)
  
  foldIds<-rep(seq(1:KFOLDS),recordsPerFold)
  
  foldIds<-foldIds[1:nrow(df)]
  
  df$foldId<-foldIds
  
  return(df)
} #endof allocateFoldID()

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# stratifiedDataset() :
#
# Split dataset by the class (assume 2-class)
# Calculate the number of records that will appear in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# The dataset now has a foldID from which we can select the data
# for the experiments
#
# INPUT   :   data frame - originalDataset - dataset
#
# OUTPUT  :   data frame - newDataset      - dataset with foldID added
#
# ************************************************
stratifiedDataset<-function(originalDataset){
  
  positionClassOutput=which(names(originalDataset)==OUTPUT_FIELD)
  
  # Get the unique class values
  classes<-unique(originalDataset[,positionClassOutput])
  
  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1<-which(originalDataset[,positionClassOutput]==classes[1])
  split1<-originalDataset[indexClass1,]
  split2<-originalDataset[-indexClass1,]
  
  # Append a column that indicates the fold number for each class
  split1<-allocateFoldID(split1)
  split2<-allocateFoldID(split2)
  
  # Combine the two datasets
  
  newDataset<-rbind(split1,split2)
  
  # Randomise the classes
  newDataset<-newDataset[order(runif(nrow(newDataset))),]
  
  return(newDataset)
}

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# stratifiedSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit<-function(dataset,fold){
  
  test<-subset(dataset, subset= foldId==fold, select=-foldId)
  train<-subset(dataset, subset= foldId!=fold,select=-foldId)
  
  return(list(
    train=train,
    test=test))
}