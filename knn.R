# ************************************************
# knnModel() :
#
#
# INPUT   :   data frame         - train        - training dataset
#             data frame         - test         - testing dataset
#             list               - config       - list of configurations
#
# OUTPUT  :
#             Data Frame         - measures     - performance metrics
# ************************************************
knnModel<-function(train,test,config,plot){
  
  myTitle<-(paste("KNN. K =",config$K_VALUE))
  print(myTitle)
  
  positionClassOutput<-which(names(train)==config$OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expected output
  train_expected<-train[,positionClassOutput]

  knnClassifier <- caret::knn3(train_inputs, factor(train_expected), k=config$K_VALUE)
  
  # Use the created KNN classifier with the test dataset
  measures<-testModel(myModel=knnClassifier,
                      testDataset=test,
                      title=myTitle,
                      config=config,
                      plot=plot)
 
  return (measures)
}

