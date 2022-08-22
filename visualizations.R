# ************************************************
# visualizeData() :
#
# Create all data plots
#
# INPUT: data frame - df - movies' data frame
#        list       - config     - list of configurations
#        bool       - rangeBars  - If true, plot mean values with standard deviation
#        num        - yLine      - Point in y axis to intercept the plot
#        bool       - indivPlots - If true, plots graphs separately too
# ************************************************
visualizeData<-function(df, config, rangeBars = FALSE, yLine = 0, indivPlots = FALSE) {
  visualizeMeanGraphs(df, config, rangeBars, yLine, indivPlots) # Mean values across the years
  visualizeHistograms(df) # Distribution of field values for successful and fail movies
  visualizeMeanSuccessFailureValues(df) # Table with mean field values for successful and fail movies
}

# ************************************************
# visualizeMeanGraphs() :
#
# Plot line graphs to show the change of mean values over time
#
# INPUT: data frame - df         - movies' data frame
#        list       - config     - list of configurations
#        bool       - rangeBars  - If true, plot mean values with standard deviation
#        num        - yLine      - Point in y axis to intercept the plot
#        bool       - indivPlots - If true, plots graphs separately too
# ************************************************
visualizeMeanGraphs<-function(df, config, rangeBars = FALSE, yLine = 0, indivPlots = FALSE){
  p <- list()
  for(i in 1:length(config$MEAN_WORTHY_FIELDS)){
    p[[i]] <- ggplot(data=df, aes_string(y=config$MEAN_WORTHY_FIELDS[i], x="release_date", group=1)) +
      ggtitle(paste("Mean values for", config$MEAN_WORTHY_FIELDS[i])) +
      scale_x_continuous(name="Release date") +
      theme(
        plot.title = element_text(color="black", size=11, face="bold"),
        panel.background = element_rect(fill="white", colour="blue", size=0.5, 
                                        linetype="solid", color="grey"),
        axis.title.x = element_text(color="black", size=11),
        axis.title.y = element_text(color="#993333", size=11, face="bold")
      )
    
    # Range bar showcasing max value as mean+1 standard deviation and min as 
    # mean-1 standard deviation
    if (rangeBars == TRUE) {
      p[[i]] <- p[[i]] + stat_summary(aes_string(y = config$MEAN_WORTHY_FIELDS[i],group=1), fun=mean, fun.min = function(x) mean(x) - sd(x),
                                      fun.max = function(x) mean(x) + sd(x), colour="black", geom = "pointrange",group=1)
    } else {
      p[[i]] <- p[[i]] + stat_summary(aes_string(y = config$MEAN_WORTHY_FIELDS[i],group=1), fun=mean, colour="black", geom = "line",group=1)
    }
    
    # Draw a vertical line at a specific point of time in the y axis
    if (yLine != 0) {
      p[[i]] <- p[[i]] + geom_vline(xintercept = yLine, color = "red")
    }
    
    # Plot graphs individually
    if (indivPlots == TRUE) {
      plot(p[[i]])
    }
  }
  do.call(grid.arrange,p)
}

# ************************************************
# plotHistogram() :
#
# Plots an individual histogram for the input field and bin width
#
# INPUT: data frame - df         - data frame of movies
#        various    - field      - field to plot histogram for
#        num        - binWidth   - width of the bars
#        bool       - indivPlots - If true, plots graphs separately too
#
# OUTPUT: ggplot histogram - histogram object which can be printed
# ************************************************
plotHistogram<-function(df, field, binWidth = 30, indivPlots = FALSE){
  p<-ggplot(df, aes_string(x=field, fill="vote_average", color="vote_average")) +
    ggtitle(paste(field, "histogram")) +
    geom_histogram(binwidth = binWidth,position="identity",aes_string(fill="vote_average"),alpha=0.5)+
    scale_color_manual(values=c("#999999", "#E69F00"))+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    theme(
      plot.title = element_text(color="black", size=11, face="bold"),
      panel.background = element_rect(fill="white", colour="blue", size=0.5, 
                                      linetype="solid", color="grey"),
      axis.title.x = element_text(color="black", size=11),
      axis.title.y = element_text(color="#993333", size=11, face="bold")
    )
  
  # Plot graphs individually
  if (indivPlots == TRUE) {
    plot(p)
  }
  
  return (p)
}

# ************************************************
# visualizeHistograms() :
#
# Plot histograms to see the frequency distribution of each field in the dataset
#
# INPUT: data frame - df - movies' dataset
# ************************************************
visualizeHistograms<-function(df, indivPlots = FALSE){
  
  # Plot histograms for each field
  p1 <- plotHistogram(df=df, field="vote_count", binWidth=1000, indivPlots=indivPlots)
  p2 <- plotHistogram(df=df, field="runtime", binWidth=100, indivPlots=indivPlots)
  p3 <- plotHistogram(df=df, field="revenue", binWidth=100000000, indivPlots=indivPlots)
  p4 <- plotHistogram(df=df, field="budget", binWidth=20000000, indivPlots=indivPlots)
  p5 <- plotHistogram(df=df, field="popularity", binWidth=30, indivPlots=indivPlots)
  p6 <- plotHistogram(df=df, field="release_date", binWidth=10, indivPlots=indivPlots)
  
  # # Plot histograms for each field
  # p1 <- plotHistogram(df=df, field="vote_count")
  # p2 <- plotHistogram(df=df, field="runtime")
  # p3 <- plotHistogram(df=df, field="revenue")
  # p4 <- plotHistogram(df=df, field="budget")
  # p5 <- plotHistogram(df=df, field="popularity")
  
  grid.arrange(p1, p2, p3, p4, p5, p6)
}

# ************************************************
# visualizeMeanSuccessFailureValues() :
#
# Table with mean field values for successful and fail movies
#
# INPUT: data frame - df - movies' data frame
# ************************************************
visualizeMeanSuccessFailureValues<-function(df){
  # Remove fields that we do not need the mean values for
  TO_DROP <- c("release_date")
  df <- df[, !(names(df) %in% TO_DROP)]
  
  # Data frame with the mean success values of all num columns in df
  success <- as.data.frame(lapply(list(df), function(x)x[x$vote_average=="1",]))
  mean_success <- colMeans(success[sapply(success, is.numeric)])
  
  # Data frame with the mean failure values of all num columns in df
  failure <- as.data.frame(lapply(list(df), function(x)x[x$vote_average=="0",]))
  mean_failure <- colMeans(failure[sapply(failure, is.numeric)])
  
  combined_means <- data.frame(cbind(mean_failure, mean_success))
  # Round all values to two decimal places
  combined_means <- round(combined_means, 2)
  
  # Add labels to the columns
  names(combined_means)[1]<-"Mean failure value"
  names(combined_means)[2]<-"Mean success value"
  # Delete the vote_average row
  combined_means<-combined_means[-5,]
  combined_means <- cbind("Field" = rownames(combined_means), as.data.frame(combined_means))
  rownames(combined_means) <- c()
  
  t<-formattable(combined_means, align=c("l",rep("r", NCOL(combined_means))))
  print(t)
}

