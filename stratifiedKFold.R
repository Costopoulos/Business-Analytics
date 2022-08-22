# ************************************************
#
# [Based on Lab 4's code]
#
# allocateFoldID() :
#
# Extend df by a column called "foldID" that contains the fold number
#
# INPUT   :   data frame         - df        - dataset
#             list               - config    - list of configurations
#
# OUTPUT  :   data frame         - df        - dataset with foldID
# ************************************************
allocateFoldID<-function(df, config){
  
  recordsPerFold<-ceiling(nrow(df)/config$KFOLDS)
  
  foldIds<-rep(seq(1:config$KFOLDS),recordsPerFold)
  
  foldIds<-foldIds[1:nrow(df)]
  
  # Append column
  df$foldId<-foldIds
  
  return(df)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# stratifiedDataframe() :
#
# Split df by classOutput
# Calculate the number of records that are present in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# df now has a foldID from which we can select the data
# for the training
#
# INPUT   :   data frame - df              - df of movies
#             list       - config          - list of configurations
#
# OUTPUT  :   data frame - newDf           - dataset with foldID added
#
# ************************************************
stratifiedDataframe<-function(df, config){
  
  positionClassOutput=which(names(df)==config$OUTPUT_FIELD)
  
  # Get the unique class values
  classes<-unique(df[,positionClassOutput])
  
  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1<-which(df[,positionClassOutput]==classes[1])
  split1<-df[indexClass1,]
  split2<-df[-indexClass1,]
  
  # Append a column that indicates the fold number for each class
  split1<-allocateFoldID(split1, config)
  split2<-allocateFoldID(split2, config)
  
  # Combine the two splits
  newDf<-rbind(split1,split2)
  
  # Randomise the classes
  newDf<-newDf[order(runif(nrow(newDf))),]
  
  return(newDf)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# stratifiedSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - df        - dataframe of preprocessed movies
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit<-function(df,fold){
  
  test<-subset(df, subset= foldId==fold, select=-foldId)
  train<-subset(df, subset= foldId!=fold,select=-foldId)
  
  return(list(
    train=train,
    test=test))
}
