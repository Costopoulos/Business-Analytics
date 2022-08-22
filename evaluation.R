# ************************************************
# runEvaluation() :
#
# Outputs results of models and writes to a CSV
#
# INPUT: data frame - allResults - results of models
#        list       - config     - list of configurations
# OUTPUT: NONE
# ************************************************
runEvaluation<-function(allResults, config){
  
  # Combine all results into a dataframe
  allResults<-data.frame(t(allResults))
  
  # Sort by highest MCC
  allResults<-allResults[order(allResults$MCC,decreasing = TRUE),]
  
  # Output results to compare all classifiers
  allResults[,1:4]<-sapply(allResults[,1:4], as.integer)
  allResults$folds<-config$KFOLDS
  print(formattable::formattable(allResults))
  
  # Write frame to a CSV files
  write.csv(allResults,file=config$EVALUATION_RESULTS)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# testModel() :
#
# Test dataset
# Determine the threshold, plot the results and calculate metrics
#
# INPUT   :   object         - myMode       - model (tree or KNN)
#         :   Data Frame     - testDataset  - dataset to evaluate
#         :   string         - title        - string to plot as the chart title
#         :   list           - config       - list of configurations
#         :   int            - classLabel   - label given to the positive (TRUE) class
#         :   boolean        - plot         - TRUE to output results/charts
#
# OUTPUT  :   List       - Named evaluation measures
#
# ************************************************
testModel<-function(myModel,
                    testDataset,
                    title,
                    config,
                    classLabel=1,
                    plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==config$OUTPUT_FIELD)
  
  # Test data is the dataframe of all columns except vote_average
  test_inputs<-testDataset[-positionClassOutput]
  
  # Predict
  testPredictedClassProbs<-predict(myModel, test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Success probabilities
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  # Test expected is the dataframe with only vote_average column
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE)
    NprintMeasures(measures,title)
  
  return(measures)
}

# ************************************************
#
# calculateModelMeasures() :
#
# Calculate measures to evaluate a model given values from confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: list - containing:
#                   TP        - double - number of true positives
#                   FP        - double - number of false positives
#                   TN        - double - number of true negatives
#                   FN        - double - number of false negatives
#                   accuracy  - double - accuracy of model
#                   p_success - double - precision for "success" / 1 classification
#                   p_fail    - double - precision for "failure" / 0 classification
#                   FPR       - double - false positive rate
#                   TPR       - double - true positive rate
#                   TNR       - double - true negative rate
#                   MCC       - double - Matthew's Correlation Coefficient
#
# ************************************************
calculateModelMeasures<-function(TP,FN,FP,TN){
  
  # Calculate model performance
  measures <- list()
  
  measures$TP <- TP
  measures$FP <- FP
  measures$FN <- FN
  measures$TN <- TN
  
  measures$accuracy <- 100 * (TP + TN) / (TP + FP + FN + TN)
  
  measures$p_success <- 100 * TP / (TP + FP)
  measures$p_fail    <- 100 * TN / (TN + FN)
  
  measures$FNR <- 100 * FN / (TN + FN)
  measures$FPR <- 100 * FP / (TN + FP)
  measures$TPR <- 100 * TP / (FN + TP)
  measures$TNR <- 100 * TN / (TN + FP)
  
  measures$MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP)*(TP + FN)*(TN + FP)*(TN + FN))
  
  return (measures)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# NcalcConfusion() :
#
# Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#
# OUTPUT: A list with the entries from NcalcMeasures()
#
#                         ACTUAL
#               -------------------------
# PREDICTED     SUCCESS=1   |  FAILURE=0
#               -------------------------
#     SUCCESS=1      TP     |    FP
#               =========================
#     FAILURE=0      FN     |    TN
#
#
# ************************************************
NcalcConfusion<-function(expectedClass,predictedClass){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # Convert to wanted type
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  return(calculateModelMeasures(TP,FN,FP,TN))
  
}

# ************************************************
#
# [Based on Lab 4's code]
#
# NprintMeasures()
#
# Output measures to the Viewer
#
# INPUT:    list -   results - results from NcalcConfusion()
#           string - title   - title of the table
#
# OUTPUT :  NONE
# ************************************************
NprintMeasures<-function(results,title){
  
  #This outputs our results into the "Viewer" in RStudio
  tidyTable<-data.frame(t(t(results)))
  names(tidyTable)[1]<-title
  
  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# Convert DT rules to ASCII rules
#
# <anticedent 1> AND <anticedent 2> ...
# Each anticedent is: [field][comparision][value]
#
# INPUT: Object - tree - Trained tree
#
# OUTPUT: data frame of rules, class and anticedents
# ************************************************
NDT5RuleOutput<-function(tree){
  x<-summary(tree)[1]
  x<-substr(x,regexpr("Rules:",x)[1]+8,nchar(x))
  x<-substr(x,1,regexpr("Evaluation on training data",x)[1]-1)
  x<-gsub("[\n\t]", "*", x)
  df_of_rules<-data.frame(matrix(ncol=3,nrow=tree$size),stringsAsFactors = FALSE)
  df_of_rules<-setNames(df_of_rules,c("Rule","Class","Anti"))
  
  numberofrules<-tree$size
  if (length(numberofrules)>1){
    numberofrules<-numberofrules[1]
    warning("More than one tree found. Extracting rules for just the first")
  }
  
  totalAnticedents<-0
  for (ruleNumber in 1:numberofrules){
    start<-regexpr("\\*\\*",x)[1]+2
    end<-regexpr("->",x)[1]-3
    onerule<-substr(x,start,end) #Single rule, anticedents seperated by '**'
    onerule<-gsub("\\*\\*"," AND ",onerule) #Rule now has "AND" between anticedents
    NumAnticedents<-str_count(onerule,"AND")+1
    totalAnticedents=totalAnticedents+NumAnticedents
    classpos<-regexpr("class ",x)+6
    classID<-as.numeric(substr(x,classpos,classpos))  #This has the class of the rule, i.e. {0,1}
    df_of_rules$Rule[ruleNumber]<-onerule
    df_of_rules$Class[ruleNumber]<-ifelse(classID==0,"Failure","Success") # Convert class to label
    df_of_rules$Anti[ruleNumber]<-NumAnticedents
    x<-substr(x,classpos,nchar(x))
    st<-regexpr("\\*\\*",x)[1]+2 #move past the rule ID
    x<-substr(x,st,nchar(x))
  }
  return(df_of_rules)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# NEvaluateClassifier() :
#
# Evaluate classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
NEvaluateClassifier<-function(test_predicted,test_expected,threshold) {
  
  predictedClass<-ifelse(test_predicted<threshold,0,1)
  
  results<-NcalcConfusion(expectedClass=test_expected,
                          predictedClass=predictedClass)
  
  return(results)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# NdetermineThreshold() :
#
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Deterime "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - probs        - probability of being class 1
#         :   Data Frame     - testing_data - dataset to evaluate
#         :   boolean        - plot         - TRUE=create charts otherwise don't
#         :   string         - title        - string to plot as the chart title
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
# ************************************************
NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
  
  # Helper local scope function
  getFirst<-function(values){
    if (length(values)>1){
      return(values[1])
    } else
      return(values)
  }
  
  toPlot<-data.frame()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-NEvaluateClassifier(test_predicted=test_predicted,
                                 test_expected=test_expected,
                                 threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  indexToBest<-getFirst(which(toPlot$youdan==max(toPlot$youdan)))
  maxYoudan<-toPlot$x[indexToBest]
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  #241019 select just the first min distance, as might be more
  mindist<-getFirst(toPlot$x[which(toPlot$distance==min(toPlot$distance))])
  
  # ************************************************
  # Plot threshold graph
  
  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))
    
    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
    
    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
    
    if (!is.na(crosspoint)){
      if (crosspoint<1)
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }
    
    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
    
    # Plot the min distance, as might be more
    abline(v=mindist,col="green",lty=3,lwd=2)
    
    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
    
    abline(v=maxYoudan,col="purple",lty=3,lwd=2)
    
    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),
           col=c("blue","red","green","purple"),
           lty=c(1,1,3,3),
           lwd=2)
    
    text(x=0,y=50, adj = c(-0.2,2),cex=1,
         col="black",
         paste("THRESHOLDS:\nDistance=",mindist,"\nYoudan=",maxYoudan))
    
    
    rr<-pROC::roc(response=test_expected,
                  predictor=test_predicted,
                  plot=TRUE,
                  auc=TRUE,
                  auc.polygon=TRUE,
                  percent=TRUE,
                  grid=TRUE,
                  print.auc=TRUE,
                  main=paste("ROC for Classifier Model",title),
                  xlab="Specificity (1-FPR) %",
                  ylab="Sensitivity (TPR) %")
    
    # Selects the "best" threshold based on distance
    analysis<-coords(rr, x="best",transpose = FALSE,
                     best.method="closest.topleft",
                     ret=c("threshold",
                           "specificity",
                           "sensitivity"))
    
    fpr<-round(100.0-analysis["specificity"],digits=2)
    
    #Add crosshairs to the graph
    abline(h=analysis["sensitivity"],col="red",lty=3,lwd=2)
    abline(v=analysis["specificity"],col="red",lty=3,lwd=2)
    
    #Annote with text
    annotate<-paste("Threshold: ",round(analysis["threshold"],digits=4L),
                    " TPR: ",round(analysis["sensitivity"],digits=2L),
                    "% FPR: ",fpr,"%",sep="")
    
    text(x=analysis["specificity"],
         y=analysis["sensitivity"], adj = c(-0.2,2),cex=1,
         col="red",annotate)
    
  } # endof if plotting
  
  # Euclidean distance threshold
  myThreshold<-mindist      # Min Distance should be the same as analysis["threshold"]
  
  #Use the "best" distance threshold to evaluate classifier
  results<-NEvaluateClassifier(test_predicted=test_predicted,
                               test_expected=test_expected,
                               threshold=myThreshold)
  
  results$threshold<-myThreshold
  
  return(results)
}
