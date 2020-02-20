
multiPLOT <- function(Filename, startDate, endDate, laser, type){
  #laser = "V_laser" ("B_laser", "YG_laser", "R_laser")
  #type = "Median" 
  
  DF <- read.csv(paste0("./data/", Filename))
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