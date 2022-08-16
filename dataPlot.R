# ************************************************
#  PRATICAL BUSINESS ANALYTICS 2021
#  COM3018
#
# Marilia Sinopli
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 13 NOVEMBER 2021
#
# UPDATE
# 1.00      13/11/2021    Initial plotHistograms()   (Dominic Adams)
# 1.01      17/11/2021    Add plotAllMeanGraphs()   (Dominic Adams)
# 1.02      18/11/2021    Add plotHistograms()   (Dominic Adams)
# 1.02      20/11/2021    Add plotMeanHitFlopValues()   (Dominic Adams)
# ************************************************

# ************************************************
# Global variables
# ************************************************

MEAN_WORTHY_FIELDS <- c("popularity","budget","revenue","runtime",
                        "vote_average")
HIT_THRESHOLD <- 6.5

# ************************************************
# plotAllMeanGraphs() :
#
# Plot line graphs to see how the mean values change over time
#
# INPUT: data frame - df - movies' data frame
# ************************************************
plotAllMeanGraphs<-function(df, rangeBars = FALSE, yLine = 0, indivPlots = FALSE){
  p <- list()
  for(i in 1:length(MEAN_WORTHY_FIELDS)){
    p[[i]] <- ggplot(data=df, aes_string(y=MEAN_WORTHY_FIELDS[i], x="release_date", group=1)) +
      ggtitle(paste("Mean values for", MEAN_WORTHY_FIELDS[i])) +
      scale_x_continuous(name="Release date") +
      theme(
        plot.title = element_text(color="black", size=11, face="bold"),
        panel.background = element_rect(fill="white", colour="blue", size=0.5, 
                                        linetype="solid", color="grey"),
        axis.title.x = element_text(color="black", size=11),
        axis.title.y = element_text(color="#993333", size=11, face="bold")
      )
    
    # Can add range bars showing mean value +/- 1 standard deviation as max/min values
    if (rangeBars == TRUE) {
      p[[i]] <- p[[i]] + stat_summary(aes_string(y = MEAN_WORTHY_FIELDS[i],group=1), fun=mean, fun.min = function(x) mean(x) - sd(x),
                                      fun.max = function(x) mean(x) + sd(x), colour="black", geom = "pointrange",group=1)
    } else {
      p[[i]] <- p[[i]] + stat_summary(aes_string(y = MEAN_WORTHY_FIELDS[i],group=1), fun=mean, colour="black", geom = "line",group=1)
    }
    
    # Can add a vertical line on to the graph at a specific year
    if (yLine != 0) {
      p[[i]] <- p[[i]] + geom_vline(xintercept = yLine, color = "red")
    }
    
    # Can plot each graph to its own plot in addition to the grouped plot display
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
# INPUT: data frame - df - data frame of movies
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
  
  if (indivPlots == TRUE) {
    plot(p)
  }
  
  return (p)
}

# ************************************************
# plotAllHistograms() :
#
# Plot histograms to see the frequency distribution of each field in the dataset
#
# INPUT: data frame - df - movies' dataset
# ************************************************
plotHistograms<-function(df, indivPlots = FALSE){
  
  # Plot histograms for each field
  p1 <- plotHistogram(df=df, field="vote_count", binWidth=1000, indivPlots=indivPlots)
  p2 <- plotHistogram(df=df, field="vote_average", binWidth=1, indivPlots=indivPlots)
  p3 <- plotHistogram(df=df, field="runtime", binWidth=100, indivPlots=indivPlots)
  p4 <- plotHistogram(df=df, field="revenue", binWidth=100000000, indivPlots=indivPlots)
  p5 <- plotHistogram(df=df, field="budget", binWidth=20000000, indivPlots=indivPlots)
  p6 <- plotHistogram(df=df, field="popularity", binWidth=30, indivPlots=indivPlots)
  
  grid.arrange(p1, p2, p3, p4, p5, p6)
}

# ************************************************
# plotMeanHitFlopValues() :
#
# Plot a table showing the mean field values for hit and flop movies
#
# INPUT: data frame - df - movies' data frame
# ************************************************
plotMeanHitFlopValues<-function(df){
  
  # Remove fields that we do not need the mean values for
  TO_DROP <- c("release_date")
  df <- df[, !(names(df) %in% TO_DROP)]
  
  # Create a data frame for the mean hit values of all numeric columns in df
  hits <- as.data.frame(lapply(list(df), function(x)x[x$vote_average>=HIT_THRESHOLD,]))
  mean_hits <- colMeans(hits[sapply(hits, is.numeric)])
  
  # Create a data frame for the mean flop values of all numeric columns in df
  flops <- as.data.frame(lapply(list(df), function(x)x[x$vote_average<HIT_THRESHOLD,]))
  mean_flops <- colMeans(flops[sapply(flops, is.numeric)])
  
  combined_means <- data.frame(cbind(mean_flops, mean_hits))
  # Round all values to two decimal places
  combined_means <- round(combined_means, 2)
  
  # Add labels to the columns
  names(combined_means)[1]<-"Mean flop value"
  names(combined_means)[2]<-"Mean hit value"
  combined_means <- cbind("Field" = rownames(combined_means), as.data.frame(combined_means))
  rownames(combined_means) <- c()
  
  t<-formattable(combined_means, align=c("l",rep("r", NCOL(combined_means)-1)))
  print(t)
}

