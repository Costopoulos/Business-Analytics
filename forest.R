# ************************************************
#
# [Based on Lab 4's code]
#
# simpleDT() :
#
# Create C5 Decision Tree on the raw dataset
# A decision tree may not need the dataset to be pre-processed
#
# INPUT   :
#             Data Frame     - train       - original train dataset
#             Data Frame     - test        - original test dataset
#             list           - config      - list of configurations
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
simpleDT<-function(train,test,config,plot=TRUE){
  
  train<-NConvertClass(train,config)
  test<-NConvertClass(test,config)
  
  positionClassOutput<-which(names(train)==config$OUTPUT_FIELD)
  
  tree<-C50::C5.0(x=train[-positionClassOutput],
                  y=factor(train[,positionClassOutput]),
                  rules=TRUE,
                  trials=1)

  measures<-testModel(myModel = tree,
                      testDataset = test,
                      title="Original Dataset. DT C5.0",
                      config=config)
  
  return(measures)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# fullDT() :
#
# Create C5 Decision Tree on pre-processed dataset
#
# INPUT   :
#             Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             list           - config      - list of configurations
#             int            - boost       - number of trees to boost
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
fullDT<-function(train,test,config,boost=1,plot=TRUE){
  
  positionClassOutput<-which(names(train)==config$OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  # ************************************************
  # Create a standard Decision Tree using the C5.0 algorithm
  # Uses library C50
  # Outputs the tree in the format of rules
  
  myTitle<-"Preprocessed Dataset. DT C5.0"
  if (boost>1)
    myTitle<-paste(myTitle,"BOOSTED=",boost)
  
  print(myTitle)
  
  tree<-C50::C5.0(x=train_inputs,
                  factor(train_expected),
                  rules=TRUE,
                  trials=boost)
  
  # Use the created decision tree with the test dataset
  # to determine best classification threshold & calculate metrics
  measures<-testModel(myModel = tree,
                                    testDataset = test,
                                    config = config,
                                    title=myTitle,
                                    plot=plot)
  
  if (plot==TRUE){
    
    print(summary(tree))
    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=myTitle)

    
    # Function to output the tree as rules to a file
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
    
    # ************************************************
    # Creates the same  C5.0 decision tree & output as a tree structure, plot it
    # The "partykit" library requires the variables (wrongly) to be global
    print("Plot decision tree to file called diagram.pdf")
    
    Global_train_inputs<<-train_inputs
    Global_train_expected<<-train_expected
    
    # :: is used to specify a function within the named package to avoid confusion
    tree<-C50::C5.0(x=Global_train_inputs,
                    factor(Global_train_expected),
                    trials=boost)
    
    # ::: is used to directly access a member of a package that is internal
    graphtree<-C50:::as.party.C5.0(tree)
    
    # The plot is large - so print to a big PDF file
    pdf(config$TREE_FILENAME, width=100, height=50, paper="special", onefile=F)
    
    # The number is the node level of the tree to print
    plot(graphtree[config$NODE_DEPTH])
    
    #This closes the PDF file
    dev.off()
  }
  return(measures)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# randomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             list           - config      - list of configurations
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForest<-function(train,test,config,plot=TRUE){
  
  myTitle<-(paste("Random Forest =",config$TREE_NUMBER,"trees"))
  print(myTitle)
  
  positionClassOutput<-which(names(train)==config$OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expected output
  train_expected<-train[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=config$TREE_NUMBER ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  
  # Use the created decision tree with the test dataset
  measures<-testModel(myModel = rf,
                      testDataset = test,
                      config=config,
                      title=myTitle,
                      plot=plot)
  
  if (plot==TRUE){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importance)))
  }
  
  return(measures)
}
