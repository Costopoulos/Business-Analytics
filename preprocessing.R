# ************************************************
#  PRATICAL BUSINESS ANALYTICS 2021
#  COM3018
#
# Marilia Sinopli
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 11 NOVEMBER 2021
#
# UPDATE
# 1.00      11/11/2021    Initial Version   (Dominic Adams)
# 1.01      13/11/2021    Add readDataset()   (Dominic Adams)
# 1.02      16/11/2021    Finish handleReleaseDateField()   (Dominic Adams)
# 1.03      16/11/2021    Add removeRowsWithNA()   (Emilia Lukose)
# 1.04      16/11/2021    Add removeUnusedFields()   (Emilia Lukose)
# 1.05      21/11/2021    Add preprocessDatasetFiles(), preprocessDataset() (Emilia Lukose)
# 1.06      23/11/2021    Added ordinal outlier detection, ordinal scaling  (Emilia Lukose)
# 1.07      23/11/2021    Removed duplicate tracks based on uri and track name + artist  (Emilia Lukose)
# ************************************************

# ************************************************
# Global variables
# ************************************************

# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()

# ************************************************
# readDataset() :
#
# Read a CSV file and return it as a dataset
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - CSV file as a workable dataset
# ************************************************
readDataset<-function(csvFilename){
  
  dataset<-read.csv(file=csvFilename,encoding="UTF-8",stringsAsFactors = FALSE, na.strings=c("","NA"))
  print(paste(csvFilename,"has been read. Number of records=",nrow(dataset)))
  
  return(dataset)
}

# ************************************************
# naValuesPerColumn() :
#
# Read a dataframe and print its NA values per column
#
# INPUT: dataframe - df 
naValuesPerColumn<-function(df){
  for (i in 1:ncol(df))
    cat("Column ", i, "has this many NA values:", sum(is.na(df[,i])), "\n")
}

# ************************************************
# removeFields() :
#
# Remove certain columns from the dataframe
#
# INPUT: data frame - dataset - original (raw) dataset
# vector - fields - column names that should be dropped
#
# OUTPUT : data frame - dataset with fields removed
# ************************************************
removeFields<-function(dataset, fields) {
  drops <- fields
  dataset <- dataset[ , !(names(dataset) %in% drops)]
  return (dataset)
}

# ************************************************
# initialPreprocessing() :
#
# Perform semi-preprocessing on csv files
#
# INPUT: file - datasetFile - csv filename
#
# OUTPUT : data frame - movies - initially processed movies
# ************************************************
initialPreprocessing<-function(datasetFile){
  # Read the CSV file
  dataset <- readDataset(datasetFile)
  
  # Check how many NA values there are, per column
  naValuesPerColumn(dataset)
  
  # Remove rows with NA fields
  movies <- na.omit(dataset)
  
  # Remove any duplicate movies
  movies <- movies[!duplicated(movies[c("original_title","homepage",
                                        "id","imdb_id")]),]
  
  return (movies)
}

# ************************************************
# preprocessDataset() :
#
# Perform preprocessing on entire dataset
#
# INPUT: data frame - dataset - dataset of tracks
#
# OUTPUT : list - datasets - (preprocessed dataset of tracks, preprocessed and scaled dataset of tracks)
# ************************************************
preprocessDataset<-function(dataset){
  # ************************************************
  # Remove rows where loudness > 0
  dataset = dataset[dataset$loudness <= 0,]
  
  # ************************************************
  # Remove more fields from the dataset to prepare for modelling
  dataset <- removeUnusedFields(dataset, UNUSED_FIELDS)
  
  # ************************************************
  # Get field types
  field_types <- determineFieldTypes(dataset)
  
  # ************************************************
  # Test if any ordinals are outliers and replace with mean values
  # Null hyposis is there are no outliers
  # We reject this if the p-value<significance (i.e. 0.05), confidence=95%
  ordinals<-dataset[,which(field_types=="ORDINAL"),drop=FALSE]
  ordinals_outliers_replaced<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)
  dataset[,which(field_types=="ORDINAL")]<-ordinals_outliers_replaced
  
  # ************************************************
  # Normalisation - copy rescaled values to a new dataframe
  # z-scale the ordinals
  zscaled<-apply(ordinals_outliers_replaced, MARGIN = 2,
                 FUN = function(X) (scale(X,center=TRUE,
                                          scale=TRUE)))
  
  # Scale in this case to be [0.0,1.0]
  ordinalReadyforML<-Nrescaleentireframe(as.data.frame(zscaled))
  dataset_scaled<-data.frame(dataset)
  dataset_scaled[,which(field_types=="ORDINAL")]<-ordinalReadyforML
  
  return (list(tracks=dataset, tracks_normalised=dataset_scaled))
}

# ************************************************
# determineFieldTypes() :
#
# INPUT: data frame - combined preprocessed datasets of tracks
#
# OUTPUT : vector strings - with type per field [SYMBOLIC, DISCREET, ORDINAL}]
# ************************************************
determineFieldTypes<-function(dataset) {
  symbolic_field_names = SYMBOLIC_FIELDS
  ordinal_field_names = ORDINAL_FIELDS
  
  field_types<-vector()
  # For every field
  for(field in 1:(ncol(dataset))){
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    # Assign symbolics
    if (names(dataset)[field] %in% symbolic_field_names){
      field_types[field]<-TYPE_SYMBOLIC
    }
    # Assign numerics
    else {
      field_types[field]<-TYPE_NUMERIC
      # Assign ordinals and discreets
      if (names(dataset)[field] %in% ordinal_field_names){
        field_types[field]<-TYPE_ORDINAL
      }
      else {
        field_types[field]<-TYPE_DISCREET
      }
    }
  }
  
  # View the field types on the console
  ordinal_fields<-names(dataset)[field_types=="ORDINAL"]
  print(paste("ORDINAL FIELDS=",length(ordinal_fields)))
  print(ordinal_fields)
  
  discreet_fields<-names(dataset)[field_types=="DISCREET"]
  print(paste("DISCREET FIELDS=",length(discreet_fields)))
  print(discreet_fields)
  
  symbolic_fields<-names(dataset)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)
  
  results<-data.frame(field=names(dataset),type=field_types)
  print(formattable::formattable(results))
  return (field_types)
}

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

NPREPROCESSING_outlier<-function(ordinals,confidence){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# NplotOutliers() :
#
# Scatter plot of field values and colours outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
#
# OUTPUT : None
# ************************************************
NplotOutliers<-function(sorted,outliers,fieldName){
  
  plot(1:length(sorted),sorted,
       pch=1,
       xlab="Unique records",
       ylab=paste("Sorted values",fieldName),
       bty="n")
  
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}

# ************************************************
#
# [This function consists of modified code from Lab 4]
#
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}