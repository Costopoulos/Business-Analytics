# ************************************************
#  PRATICAL BUSINESS ANALYTICS 2021
#  COM3018
#
# Marilia Sinopli
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 09 NOVEMBER 2021
#
# UPDATE
# 1.00      09/11/2021    Initial Version   (Emilia Lukose)
# 1.01      11/11/2021    Add main() and create dataset object    (Dominic Adams)
# 1.02      16/11/2021    Read in all data files and use for data exploration/pre-processing   (Dominic Adams)
# 1.03      21/11/2021    Separate semi-pre-processing on each file and full dataset pre-processing  (Emilia Lukose)
# 1.04      22/11/2021    Added preliminary KNN Model (Uliks Sekiraqa)
# 1.05      22/11/2021    Added decision tree models (Emilia Lukose)
# 1.06      22/11/2021    Added evaluation of different models (Emilia Lukose)
# 1.07      25/11/2021    Added runDTModels for experiments on decision tree models (Emilia Lukose)
# 1.08      25/11/2021    Separated function for real world metrics comparison (Emilia Lukose)
# ************************************************

# ************************************************
# Global variables
# ************************************************

DATASET_FILENAME <- "movies_metadata.csv"

OUTPUT_FIELD <- "vote_average" # Field name of the output class to predict

# Indicates the type of each field
PROBLEMATIC_FIELDS <- c("belongs_to_collection","homepage","tagline") 
                      # these are fields with an absurdly large number of
                      # NA values, that will be dropped

UNUSED_FIELDS     <- c("adult","homepage","id","imdb_id","original_title",
                       "belongs_to_collection","original_language", "overview",
                       "poster_path","production_country","release_date",
                       "spoken_languages","status","tagline","title","video")
ORDINAL_FIELDS    <- c("popularity","runtime")
DISCREET_FIELDS   <- c("budget", "revenue")

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric

OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection
# Set to negative means "analyse but do not replace outliers"

# These are the supervised model constants

PDF_FILENAME      <- "tree.pdf"           # Name of PDF with graphical tree diagram
RESULTS_FILENAME  <- "results.csv"        # Name of the CSV results file
NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 200                  # Number of trees in the forest

# Libraries that will be used in the project
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


# ************************************************
# main() :
#
# Entry point to execute our program execution
#
# INPUT: None
# OUTPUT :None

# ************************************************
main<-function(){
  # Dataset after each file has been semi-preprocessed
  movies <- initialPreprocessing(DATASET_FILENAME)

  cat("The number of rows in the 'movies' object is", nrow(movies),"\n")
  
  # Data exploration
  #
  # If plot(s) are not visible in the RStudio Plots tab, comment out everything 
  # in main() after plotMeanHitFlopValues(tracks) is called
  plotAllMeanGraphs(movies, rangeBars = FALSE, yLine=0, indivPlots = FALSE) # Mean values across the years
  plotHistograms(movies, indivPlots = FALSE) # Distribution of field values for hits and flops
  plotMeanHitFlopValues(movies) # Table showing mean field values for hits and flops
  # 
  # # Dataset after entire dataset has been preprocessed
  # datasets <- preprocessDataset(tracks)
  # tracks <- datasets$tracks
  # tracks_normalised <- datasets$tracks_normalised
  # 
  # print(paste("Number of total records=",nrow(tracks)))
  # 
  # # Prepare the dataset for use in stratified k-fold cross-validation
  # dataset <- stratifiedDataset(tracks)
  # dataset_normalised <- stratifiedDataset(tracks_normalised)
  # 
  # # Run experiments on the models 
  # allResults <- runModels(dataset_normalised, normalised_dataset=dataset_normalised)
  # 
  # # Perform evaluation
  # runEvaluation(allResults)
}

# ************************************************
# Utility functions from the labs
# ************************************************

gc() # Garbage collection to automatically release memory

# Clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# Clears the console area
cat("\f")

# Load libraries
library(pacman)
pacman::p_load(char=LIBRARIES,install=TRUE,character.only=TRUE)

# Import other functions
source("preprocessing.R")
source("dataPlot.R")
# source("stratifiedKFold.R")
# source("KNNFunctions.R")
# source("forestFunctions.R")
# source("helperFunctions.R")
# source("evaluationFunctions.R")

set.seed(123)

main()

