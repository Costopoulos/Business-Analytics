# ************************************************
#  PRACTICAL BUSINESS ANALYTICS 2021
#  COM3018
# ************************************************


# Import libraries
LIBRARIES<-c(
  "ggplot2",
  "lubridate",
  "data.table",
  "stringr",
  "gridExtra",
  "formattable",
  "plyr",
  "cluster",
  "caret",
  "class",
  "outliers",
  "corrplot",
  "MASS",
  "pROC",
  "stats",
  "PerformanceAnalytics",
  "partykit",
  "C50",
  "randomForest")


setConfig<-function() {
  DATASET_FILENAME <- "movies_metadata.csv"
  
  OUTPUT_FIELD <- "vote_average" # Field name of the output class to predict
  
  # Indicates the type of each field
  PROBLEMATIC_FIELDS <- c("belongs_to_collection","homepage","tagline") 
  # these are fields with an absurdly large number of
  # NA values, that will be dropped
  
  UNUSED_FIELDS     <- c("homepage","id","imdb_id","original_title",
                         "belongs_to_collection","overview", "poster_path",
                         "production_countries","release_date", "spoken_languages",
                         "status","tagline","title","video")
  SYMBOLIC_FIELDS   <- c("adult","genres","original_language",
                         "production_companies","vote_average")
  ORDINAL_FIELDS    <- c("popularity","runtime") # vote_average was ordinal, but
                                                 # I changed it to ease the modelling
  DISCREET_FIELDS   <- c("budget","revenue","vote_count")
  
  TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
  TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
  TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
  TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
  
  OUTLIER_CONF      <- 0.95                 # Confidence p-value for detecting outliers
                                            # Set to negative means "analyse but 
                                            # do not replace outliers"
  
  TREE_FILENAME         <- "diagram.pdf"      # Graphical Tree
  EVALUATION_RESULTS    <- "evaluations.csv"  # CSV results file
  NODE_DEPTH            <- 1                  # Νode level of the tree to print
  TREE_NUMBER           <- 200                # Trees in the forest
  
  BLOCKBUSTER_THRESHOLD <- 6.5                # Any movie with a vote_average above
                                              # this threshold is considered a 
                                              # success
  
  MEAN_WORTHY_FIELDS <- c("popularity","budget","revenue","runtime") # Fields to
                                                                     # get the mean
                                                                     # values from
  
  KFOLDS <- 5 # Number of folds to use in stratified k-fold cross-validation
  
  # Initialize empty list to keep key-value pairs
  config <- list()
  
  # Pairs
  config[['DATASET_FILENAME']]     <- DATASET_FILENAME
  config[['OUTPUT_FIELD']]         <- OUTPUT_FIELD
  config[['PROBLEMATIC_FIELDS']]   <- PROBLEMATIC_FIELDS
  config[['UNUSED_FIELDS']]        <- UNUSED_FIELDS
  config[['SYMBOLIC_FIELDS']]      <- SYMBOLIC_FIELDS
  config[['ORDINAL_FIELDS']]       <- ORDINAL_FIELDS
  config[['DISCREET_FIELDS']]      <- DISCREET_FIELDS
  config[['TYPE_ORDINAL']]         <- TYPE_ORDINAL
  config[['TYPE_DISCREET']]        <- TYPE_DISCREET
  config[['TYPE_SYMBOLIC']]        <- TYPE_SYMBOLIC
  config[['TYPE_NUMERIC']]         <- TYPE_NUMERIC
  config[['OUTLIER_CONF']]         <- OUTLIER_CONF
  config[['TREE_FILENAME']]        <- TREE_FILENAME
  config[['EVALUATION_RESULTS']]   <- EVALUATION_RESULTS
  config[['NODE_DEPTH']]           <- NODE_DEPTH
  config[['TREE_NUMBER']]          <- TREE_NUMBER
  config[['BLOCKBUSTER_THRESHOLD']]<- BLOCKBUSTER_THRESHOLD
  config[['MEAN_WORTHY_FIELDS']]   <- MEAN_WORTHY_FIELDS
  config[['KFOLDS']]               <- KFOLDS
  
  return(config)
}

# ************************************************
# runModels() :
#
# Runs experiments on models with the dataset
# Models used:
# KNN, Random Forest
#
# INPUT: data frame - dataset - dataset of pre-processed tracks
#        data frame - normalized_dataset - normalized dataset   
# OUTPUT: data frame - allResults - combination of results from different models
# ************************************************
runModels<-function(dataset, normalized_dataset){
  
  # ************************************************
  # Modelling: Random Forest
  # ************************************************
  
  # RANDOM FOREST
  RFmeasures<-runExperiment(dataset = dataset,FUN = randomForest)
  # Create a data frame to compare results from different experiments
  allResults<-data.frame(RandomForest=unlist(RFmeasures))
  allResults<-realWorldMetrics(dataset, RFmeasures, allResults, "RF")
  
  # # Uncomment if we want to experiment with more DTs:
  # allResults <- runDTModels(dataset, allResults)

  # # ************************************************
  # # Modelling: KNN
  # # ************************************************
  # 
  # # Use stratified k-fold cross-validation with the KNN algorithm
  # KNNmeasures<-runExperiment(dataset = normalized_dataset,FUN = knnModel)
  # allResults<-cbind(allResults,data.frame(KNN=unlist(KNNmeasures)))
  # allResults<-realWorldMetrics(dataset, KNNmeasures, allResults, "KNN")
  
  return (allResults)
}

# ************************************************
# main() :
#
# Entry point to execute our program execution
#
# INPUT: None
# OUTPUT :None
# ************************************************
main<-function(){
  # Get config
  config <- setConfig()
  
  # Dataset after each file has been semi-preprocessed
  movies <- initialPreprocessing(config)
  cat("The number of rows in the 'movies' object is now", nrow(movies),"\n")
  
  # Visualize data
  visualizeData(movies, config)
  
  # Df after preprocessing
  df <- preprocessing(movies, config)
  movies <- df$movies
  movies_normalized <- df$movies_normalized

  # Prepare df for use in stratified k-fold cross-validation
  dataset <- stratifiedDataframe(movies, config)
  dataset_normalized <- stratifiedDataframe(movies_normalized, config)

  # Run experiments on the models
  allResults <- runModels(dataset_normalized, normalized_dataset=dataset_normalized)

  # Perform evaluation
  runEvaluation(allResults)
}

gc() # Garbage collection

# Clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# Clear console
cat("\f")

# Load libraries
library(pacman)
pacman::p_load(char=LIBRARIES,install=TRUE,character.only=TRUE)

# Source all other code
source("preprocessing.R")
source("visualizations.R")
source("stratifiedKFold.R")
source("knn.R")
source("forest.R")
source("helperMethods.R")
source("evaluation.R")

set.seed(42)

main()

