## Function that plots midpeak beads in selected APD
# Author Shelly Lin 191220

  ## Filename = "Aurora 4L Midpeak bead track v04.csv"
  ## startDate <- "200110"
  ## endDate <- "200123"

midPeakPLOT <- function(Filename, startDate, endDate, APD, plotTrend, vline = NULL){
  library(ggplot2)
  library(stringr)
  
  DF <- read.csv(paste0("./data/", Filename))
  DF$Date <- as.character(DF$Date)
  DF$indices <- c(1:nrow(DF))
  
  index_Date_i <- min(which(DF$Date == startDate))
  index_Date_f <- max(which(DF$Date == endDate))
  if(is.null(vline)){
    index_vline <- index_Date_i
  }else{
    index_vline <- which(DF$Date == vline)
  }
    
  plotRangeDF <- DF[index_Date_i:index_Date_f,]
  
  p <- ggplot(plotRangeDF, aes(plotRangeDF$indices, plotRangeDF[[APD]])) +
    geom_point() +
    geom_path()+
    geom_vline(xintercept = index_vline, linetype="dotdash") +
#    ylim(0, max(plotRangeDF[[APD]])) +
    xlab("Date") +
    scale_x_continuous(breaks = c(plotRangeDF$indices), labels = c(plotRangeDF$Date)) +
    ylab("Midpeak Value") +
    ggtitle(APD) +
    theme(axis.text.x = element_text( 
      angle = 90, 
      color="black")
    )
  
  # fixing the y axis
  if(str_detect(APD, "Median") == T){
    maxVal <- max(plotRangeDF[[APD]])
    p = p +
      scale_y_continuous(breaks=seq(0, maxVal, 5000), limits = c((maxVal-10*5000),maxVal))
  }else{
    maxVal <- max(plotRangeDF[[APD]])
    p = p +
      scale_y_continuous(breaks=seq(0, maxVal, 0.5), limits = c(maxVal-10*0.5,maxVal))
  }
  # plotting trendline
  if(plotTrend ==T){
    p <- p +
      geom_smooth(method = "lm", se=F)
  }else{
    p <- p
  }

  print(p)
}
