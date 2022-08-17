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
#
# [This function consists of modified code from Lab 4]
#
# simpleDT() :
#
# Create C5 Decision Tree on the raw dataset
# A decision tree may not need the dataset to be pre-processed
#
# INPUT   :
#             Data Frame     - train       - original train dataset
#             Data Frame     - test        - original test dataset
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
simpleDT<-function(train,test,plot=TRUE){
  
  # In original dataset, $target is the classification label
  # We need to convert this to give the minority class a value of 0
  # this just makes it easier to define the confusioon matrix!
  # for the UCI-G this is a class of {0,1} being {flop, hit}
  train<-NConvertClass(train)
  test<-NConvertClass(test)
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  tree<-C50::C5.0(x=train[-positionClassOutput],
                  y=factor(train[,positionClassOutput]),
                  rules=TRUE,
                  trials=1)
  
  measures<-getModelClassifications(myModel = tree,
                                    testDataset = test,
                                    title="Original Dataset. DT C5.0")
  
  return(measures)
} #endof simpleDT()

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# fullDT() :
#
# Create C5 Decision Tree on pre-processed dataset
#
# INPUT   :
#             Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             int            - boost       - number of trees to boost
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
fullDT<-function(train,test,boost=1,plot=TRUE){
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
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
  measures<-getModelClassifications(myModel = tree,
                                    testDataset = test,
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
    
    # ************************************************
    # We can visualise the tree
    
    #Function to output the tree as rules to a file
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
    
    # ************************************************
    # Creates the same  C5.0 decision tree & output as a tree structure, plot it
    # The "partykit" library requires the variables (wrongly) to be global
    print("Plot decision tree to file called tree.pdf")
    
    Global_train_inputs<<-train_inputs
    Global_train_expected<<-train_expected
    
    # :: is used to specify a function within the named package to avoid confusion
    tree<-C50::C5.0(x=Global_train_inputs,
                    factor(Global_train_expected),
                    trials=boost)
    
    # ::: is used to directly access a member of a package that is internal
    graphtree<-C50:::as.party.C5.0(tree)
    
    # The plot is large - so print to a big PDF file
    pdf(PDF_FILENAME, width=100, height=50, paper="special", onefile=F)
    
    # The number is the node level of the tree to print
    plot(graphtree[NODE_LEVEL])
    
    #This closes the PDF file
    dev.off()
  }
  return(measures)
} #endof fullDT()

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# randomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForest<-function(train,test,plot=TRUE){
  
  myTitle<-(paste("Preprocessed Dataset. Random Forest=",FOREST_SIZE,"trees"))
  print(myTitle)
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expected output
  train_expected<-train[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  
  # ************************************************
  # Use the created decision tree with the test dataset
  measures<-getModelClassifications(myModel = rf,
                                    testDataset = test,
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
} #endof randomForest()