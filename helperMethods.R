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
# classBalanceRatio = model(fail)/model(success)
#
# INPUT   :   Data Frame     - df        - original data frame
#         :   Data Frame     - measures  - performance metrics
#             list           - config    - list of configurations
#         :   double         - real      - failure rate in real world scenario
#
# OUTPUT  :
#         :   Data Frame     - measures  - adjusted performance metrics
#
# ************************************************
metricsToRealWorld<-function(df,measures,config,real){
  
  positionClassOutput=which(names(df)==config$OUTPUT_FIELD)
  
  # Class Balance
  success<-length(which(df[,positionClassOutput]==1))
  failure<-length(which(df[,positionClassOutput]==0))
  
  classBalance<-failure/success
  print(paste("Class balance, failure/success=",round(classBalance,digits=2)))
  
  # This equates to a flop rate of...
  print(paste("Failure rate in dataset=",round((failure/nrow(df))*100,digits=2),"%",sep=""))
  
  # sg = rm / rn
  sg<-classBalance/real
  
  # Adjust the results to reflect the real df
  # P (being successes) = TP+TN
  # So we will increase P * sg, which will then give the estimated real class balance
  # in the results
  
  TP<-round(measures$TP*sg,digits=0)
  FN<-round(measures$FN*sg,digits=0)
  TN=measures$TN
  FP=measures$FP
  
  adjustedMeasures<-calculateModelMeasures(TP=TP, FN=FN, FP=FP, TN=TN)
  
  NprintMeasures(results=adjustedMeasures,
                 title=paste("Real world adjust",real))
  
  adjustedMeasures$threshold=measures$threshold
  
  return(adjustedMeasures)
}

# ************************************************
# realWorldMetrics() :
#
# Adds results by taking real world metrics into account
#
# INPUT: data frame - df          - dataset of pre-processed tracks
#        list       - measures    - measures from a model
#        data frame - allResults  - results from a model
#        list       - config      - list of configurations
#        character  - modelName   - name of the model
#
# OUTPUT: data frame - allResults - combination of results from different models, incl realWorldMetrics
# ************************************************
realWorldMetrics<-function(df, measures, allResults, config, modelName){
  # REAL WORLD METRICS
  # Half of the movies are failures and half are successful, but in a real world
  # scenario it would be 70% fails and 30% successes
  realWorldClassBalance<-70/100
  measures<-metricsToRealWorld(df=df,
                               measures=measures,
                               config=config,
                               real=realWorldClassBalance)
  
  # Name results modelName_adjust
  tempResults <- data.frame(placeholder_name=unlist(measures))
  names(tempResults)[names(tempResults) == "placeholder_name"] <- paste(modelName, "adjust", sep="_")
  allResults<-cbind(allResults,tempResults)
  return (allResults)
}
