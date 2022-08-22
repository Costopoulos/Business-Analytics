# ************************************************
# parseDataset() :
#
# Read a CSV file and return it as a data frame
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - df - data frame
# ************************************************
parseDataset<-function(csvFilename){
  
  df<-read.csv(file=csvFilename,encoding="UTF-8",stringsAsFactors = FALSE, na.strings=c("","NA"))
  print(paste(csvFilename,"'s number of rows are",nrow(df)))
  
  return(df)
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
# INPUT: data frame - df - data frame with data of the dataset
# vector - fields - column names that should be dropped
#
# OUTPUT : data frame - df with fields removed
# ************************************************
removeFields<-function(df, fields) {
  drops <- fields
  df <- df[ , !(names(df) %in% drops)]
  return (df)
}

# ************************************************
# convertTypes() :
#
# Convert types to the wanted ones
#
# INPUT: data frame - df - dataframe from the dataset
#
# OUTPUT : data frame - df with updated types
# ************************************************
convertTypes<-function(df) {
  # Convert to double
  df$budget <- as.numeric(df$budget)
  df$popularity <- as.double(df$popularity)
  # Convert all date values to YYYY format
  df$release_date <- year(as.Date(df$release_date))
  return(df)
}

# *******************************************************
# classifyMoviePerformance() :
#
# Converts "vote_average" column to 0 or 1, if the 
# value is equal or more than blockbuster_threshold
#
# INPUT: data frame - df                     - dataframe from the dataset
#        num        - blockbuster_threshold  - value above which the movie is 
#                                              considered a success
#
# OUTPUT : data frame - df with updated vote_average col
# *******************************************************
classifyMoviePerformance<-function(df, blockbuster_threshold) {
  for (i in 1:nrow(df))
  {
    if (df[i,"vote_average"] >= blockbuster_threshold)
    {
      df[i,"vote_average"]<-1
    }
    else
    {
      df[i,"vote_average"]<-0
    }
  }
  return(df)
}

# ************************************************
# initialPreprocessing() :
#
# Perform semi-preprocessing on csv files
#
# INPUT: config - list of configurations
#
# OUTPUT : data frame - movies - initially processed movies
# ************************************************
initialPreprocessing<-function(config){
  # Read the CSV file
  df <- parseDataset(config$DATASET_FILENAME)
  
  # Check how many NA values there are, per column
  naValuesPerColumn(df)
  
  # Drop problematic columns
  movies <- removeFields(df, config$PROBLEMATIC_FIELDS)
  cat("Removing problematic columns\n")
  
  # Remove rows with NA fields
  movies <- na.omit(movies)
  
  # Update datatypes
  movies <- convertTypes(movies)
  
  # Movie is a blockbuster if score is >= 6.5
  movies <- classifyMoviePerformance(movies, config$BLOCKBUSTER_THRESHOLD)
  
  # Remove any duplicate movies
  movies <- movies[!duplicated(movies[c("original_title","id","imdb_id")]),]
  
  return (movies)
}

# ************************************************
# preprocessDataset() :
#
# Perform preprocessing on entire dataset
#
# INPUT: data frame - df     - data frame of movies
#        list       - config - list of configurations
#
# OUTPUT : list - datasets - (preprocessed dataset of tracks, preprocessed and scaled dataset of tracks)
# ************************************************
preprocessing<-function(df, config){
  
  # Remove more fields from the df to prepare for training
  df <- removeFields(df, config$UNUSED_FIELDS)
  
  # Get field types
  field_types <- determineFieldTypes(df, config)
  
  # If ordinals are outliers, replace them with mean values
  # If p-value < (1-confidence), reject it
  ordinals<-df[,which(field_types=="ORDINAL"),drop=FALSE]
  ordinals_outliers_replaced<-determineOutliers(ordinals=ordinals,confidence=config$OUTLIER_CONF)
  df[,which(field_types=="ORDINAL")]<-ordinals_outliers_replaced
  
  # Normalisation and z-scaling of ordinals
  zscaled <- apply(ordinals_outliers_replaced, MARGIN = 2,
                   FUN = function(X) (scale(X,center=TRUE,
                                            scale=TRUE)))
  
  # Scale to be [0.0,1.0]
  ordinalReadyforML<-rescaleDataframe(as.data.frame(zscaled))
  df_scaled<-data.frame(df)
  df_scaled[,which(field_types=="ORDINAL")]<-ordinalReadyforML
  
  return (list(movies=df, movies_normalized=df_scaled))
}

# ************************************************
# determineFieldTypes() :
#
# INPUT: data frame - df     - semi-preprocessed movies
#        list       - config - list of configurations
# OUTPUT : vector strings with type
# ************************************************
determineFieldTypes<-function(df, config) {
  
  field_types<-vector()
  # For all columns
  for(field in 1:(ncol(df))){
    entry<-which(data.frame()$name==names(df)[field])
    if (length(entry)>0){
      field_types[field]<-data.frame()$type[entry]
      next
    }
    
    # Assign symbolics
    if (names(df)[field] %in% config$SYMBOLIC_FIELDS){
      field_types[field]<-config$TYPE_SYMBOLIC
    }
    # Assign nums
    else {
      field_types[field]<-config$TYPE_NUMERIC
      # Assign ordinals and discreets
      if (names(df)[field] %in% config$ORDINAL_FIELDS){
        field_types[field]<-config$TYPE_ORDINAL
      }
      else {
        field_types[field]<-config$TYPE_DISCREET
      }
    }
  }
  
  # View the field types on the console
  ordinal_fields<-names(df)[field_types=="ORDINAL"]
  print(paste("ORDINAL FIELDS=",length(ordinal_fields)))
  print(ordinal_fields)
  
  discreet_fields<-names(df)[field_types=="DISCREET"]
  print(paste("DISCREET FIELDS=",length(discreet_fields)))
  print(discreet_fields)
  
  symbolic_fields<-names(df)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)
  
  results<-data.frame(field=names(df),type=field_types)
  print(formattable::formattable(results))
  return (field_types)
}

# ************************************************
#
# [Based on Lab 4's code]
#
# determineOutliers() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

determineOutliers<-function(ordinals,confidence){
  
  # For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    # If found records with outlier values
    if ((length(outliers>0))){
      
      # If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone # Put in the values with the outliers replaced by means
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
# [Based on Lab 4's code]
#
# NplotOutliers() :
#
# Scatter plot of field values and colours outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  -  list of above points that are considered outliers
#        String - fieldName -  name of field to plot
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
# [Based on Lab 4's code]
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
# [Based on Lab 4's code]
#
# rescaleDataframe() :
#
# Rescale dataframe to [0.0,1.0]
#
# INPUT:   data frame - df - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
rescaleDataframe<-function(df){
  scaled<-sapply(as.data.frame(df),Nrescale)
  return(scaled)
}
