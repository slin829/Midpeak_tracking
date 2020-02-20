savePlots <- function(Filename, startDate, endDate, plotTrend = T, vline = NULL){
  ### Version 4 ###
  #Filename = "Aurora 4L Midpeak bead track v05.csv"
  #startDate <- "190321"
  #endDate <- "190815"
  
  # Make sure to set current dir to souce file location
  library(reshape2)
  library(ggplot2)
  library(stringr)
  options(stringsAsFactors = FALSE)
  
  midPeakPLOT <- function(Filename, startDate, endDate, APD, plotTrend, vline = NULL){
    DF <- read.csv(paste0("../data/", Filename))
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
  
  # ---------------------------- creating single plots and save ---------------------------------
  DF <- read.csv(paste0("../data/", Filename))
  non_numAPD <- as.data.frame(which(lapply(DF, class)=="character"))
  APDs <- colnames(DF)
  APDs <- APDs[-c(non_numAPD$`which(lapply(DF, class) == "character")`)]
  APDs <- APDs[!APDs %in% c("Date")]

  saveFolder <- paste0(startDate, "_", endDate)
  if(saveFolder %in% dir("../output")){
    print("A folder with the current start and end date already exists. Any existing files will be over written!")
    readline(prompt="Press any key to continue or Esc to cancel")
  }else{
    dir.create(paste0("../output/", saveFolder))
  }

  for(i in APDs){
    Midpeakplot =midPeakPLOT(Filename, startDate, endDate, i, plotTrend = T, vline = NULL)
    ggsave(paste0("../output/",saveFolder,"/", i, ".png"), width = 10, height =10)
    print(paste0("saved ", i))
  } 
  
  # ----------------------------- creating multiplots and save --------------------------------------
  
  multiPLOT <- function(laser, type){
    #laser = "V_laser" ("B_laser", "YG_laser", "R_laser")
    #type = "Median" 
    
    if(laser == "B_laser"){
      laserGrep <- grep("B", colnames(DF), value = TRUE)
    }else if(laser == "R_laser"){
      laserGrep <- grep("R", colnames(DF), value = TRUE)
    }else if(laser == "YG_laser"){
      laserGrep <- grep("YG", colnames(DF), value = TRUE)
    }else if(laser == "V_laser"){
      laserGrep <- grep("V", colnames(DF), value = TRUE)
      remove_V <-  c(grep("R", laserGrep, value = TRUE),
                     grep("YG", laserGrep, value = TRUE),grep("B", laserGrep, value = TRUE))
      laserGrep <- laserGrep[-which(laserGrep %in% remove_V)]
    }
    subset <- grep(type, laserGrep, value = TRUE)
    subDF <- DF[, c("Date", subset)]
    index_Date_i <- min(which(subDF$Date == startDate))
    index_Date_f <- max(which(subDF$Date == endDate))
    if(is.null(vline)){
      index_vline <- index_Date_i
    }else{
      index_vline <- which(subDF$Date == vline)
    }
    subDF <- subDF[index_Date_i:index_Date_f,]
    subDF$indices <- c(1:nrow(subDF))
    multiDF <- melt(subDF, id = c("Date", "indices"))
    
    sp <- ggplot(multiDF, aes(multiDF$indices, multiDF$value)) +
      geom_point() +
      geom_path()+
      xlab("Date") +
      scale_x_continuous(breaks = c(multiDF$indices), labels = c(multiDF$Date)) +
      ylab("Midpeak Value") +
      theme(axis.text.x = element_text( 
        angle = 90, 
        color="black")
      )
    
    # fixing the y axis
    if(type == "Median"){
      maxVal <- max(multiDF$value)
      sp = sp +
        scale_y_continuous(breaks=seq(0, maxVal, 50000), limits = c(0,maxVal))
    }else{
      maxVal <- max(multiDF$value)
      sp = sp +
        scale_y_continuous(breaks=seq(0, maxVal, 0.5), limits = c(0,maxVal))
    }
    
    sp + facet_wrap(~ multiDF$variable, ncol=4)
  }
  
  for(i in c("B_laser", "R_laser", "YG_laser", "V_laser")){
    type = "Median"
    meidanSummary <- multiPLOT(i, type)
    ggsave(paste0("../output/",saveFolder,"/# Median_summary_", i, ".png"), width = 10, height =10)
    print(paste0("saved Median Summary ", i))
  }
  for(i in c("B_laser", "R_laser", "YG_laser", "V_laser")){
    type = "rCV"
    meidanSummary <- multiPLOT(i, type)
    ggsave(paste0("../output/",saveFolder,"/# rCV_summary", i, ".png"), width = 10, height =10)
    print(paste0("saved rCV Summary ", i))
  }
  
  print("====== Save completed =====")
}