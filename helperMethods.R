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
# 1.00      09/11/2021    Initial Version   (Emilia Lukose)
# ************************************************

# ************************************************
# Global variables
# ************************************************

# The output field of the model is measured from 0-1, thus we shift the hit 
# threshold
FACTORED_HIT_THRESHOLD = HIT_THRESHOLD/10

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# runExperiment() :
#
#
# INPUT   :   data frame         - dataset        - dataset
#             object function    - FUN            - name of function
#             ...                - optional        - parameters are passed on
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
runExperiment<-function(dataset,FUN, ...){
  
  allResults<-data.frame()
  
  for (k in 1:KFOLDS){
    
    splitData<-stratifiedSplit(dataset=dataset,fold=k)
    
    measures<-FUN(train=splitData$train,
                  test=splitData$test,
                  plot=(k==KFOLDS),...)
    
    allResults<-rbind(allResults,data.frame(measures))
  } #endof for()
  
  # Return the means from all the experiments back as a list
  getMeans<-colMeans(allResults)
  getMeans[1:4]<-as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints
  
  return(as.list(colMeans(allResults)))
} #endof runExperiment()

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# NConvertClass() :
#
# In original dataset, $target is the classification label
# We need to convert this to give the minority class a value of 0
# this just makes it easiert to define the confusion matrix!
# for the UCI-G this is a class of {0,1} being {flop, hit}
#
# INPUT   :
#             Data Frame        - dataset
#
# OUTPUT  :
#             Data Frame        - dataset
#
# ************************************************
NConvertClass<-function(dataset){
  positionClassOutput<-which(names(dataset)==OUTPUT_FIELD)
  classes<-sort(table(dataset[,positionClassOutput])) #smallest class will be first
  minority<-names(classes[1])
  indexToStatus2<-which(dataset[positionClassOutput]==minority)
  dataset[positionClassOutput][indexToStatus2,]<-0
  dataset[positionClassOutput][-indexToStatus2,]<-1
  return(dataset)
}

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# metricsToRealWorld() :
#
# Adjust metrics to an assumed real-world sample
# Here we assume that the dataset has been sampled and needs to be 'corrected'
# We have already calculated for the training dataset
# classBalance = model(flop)/model(hit)
#
# INPUT   :   Data Frame     - dataset   - original dataset
#         :   Data Frame     - measures  - performance metrics
#         :   double         - natural   - flop rate in natural population
#
# OUTPUT  :
#         :   Data Frame     - measures  - adjusted performance metrics
#
# ************************************************
metricsToRealWorld<-function(dataset,measures,natural){
  
  positionClassOutput=which(names(dataset)==OUTPUT_FIELD)
  
  #target is encoded as: 1 = Flop, 0 = Hit
  #Calculate class balance ratio, rm = flop / hit
  ClassHit<-length(which(dataset[,positionClassOutput]>=FACTORED_HIT_THRESHOLD))
  ClassFlop<-length(which(dataset[,positionClassOutput]<FACTORED_HIT_THRESHOLD))
  browser()
  classBalance<-ClassFlop/ClassHit
  print(paste("Class balance, flop:hit=",round(classBalance,digits=2)))
  
  # This equates to a flop rate of...
  print(paste("Flop rate in dataset=",round((ClassFlop/nrow(dataset))*100,digits=2),"%",sep=""))
  
  # sg = rm / rn
  sg<-classBalance/natural
  
  # Adjust the results to reflect the natural dataset
  # P (being hits) = TP+TN
  # So we will increase P * sg, which will then give the estimated natural class balance
  # in the results
  
  TP<-round(measures$TP*sg,digits=0)
  FN<-round(measures$FN*sg,digits=0)
  TN=measures$TN
  FP=measures$FP
  
  adjustedMeasures<-getModelMeasures(TP=TP, FN=FN, FP=FP, TN=TN)
  
  NprintMeasures(results=adjustedMeasures,
                 title=paste("Real world adjust",natural))
  
  # You'd expect (FP+TN)/(TP+FN) to be close to naturalClassBalance
  
  adjustedMeasures$threshold=measures$threshold
  
  return(adjustedMeasures)
} #endof metricsToRealWorld()

# ************************************************
# realWorldMetrics() :
#
# Adds results by taking real world metrics into account
#
# INPUT: data frame - dataset - dataset of pre-processed tracks
#        list       - measures - measures from a model
#        data frame - allResults - results from a model
#        character  - modelName - name of the model
# OUTPUT: data frame - allResults - combination of results from different models, incl realWorldMetrics
# ************************************************
realWorldMetrics<-function(dataset, measures, allResults, modelName){
  # REAL WORLD METRICS
  # Assume that the dataset has been sampled and needs to be 'corrected'
  # 50% of songs are flops and 50% are hits, but this is highly unlikely
  # Assume the natural population has a 70% chance of having a flop
  naturalClassBalance<-70/100
  measures<-metricsToRealWorld(dataset=dataset,
                               measures = measures,
                               natural=naturalClassBalance)
  
  # Name results modelName_adjust
  tempResults <- data.frame(placeholder_name=unlist(measures))
  names(tempResults)[names(tempResults) == "placeholder_name"] <- paste(modelName, "adjust", sep="_")
  allResults<-cbind(allResults,tempResults)
  return (allResults)
}