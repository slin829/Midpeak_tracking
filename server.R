# midpeak_plot_app server v4.R

server <- function(input, output){
  source("./functions/midPeakPLOT.R")
  source("./functions/multiPLOT.R")
  output$APD <- renderUI({
    DF <- read.csv(paste0("./data/",input$Filename))
    non_numAPD <- as.data.frame(which(lapply(DF, class)=="character"))
    APDs <- colnames(DF)
    APDs <- APDs[-c(non_numAPD$`which(lapply(DF, class) == "character")`)]
    avail_APDs <- APDs[!APDs %in% c("Date")]
    radioButtons("APD", label = "APD", choices = avail_APDs,
                selected = avail_APDs[1], inline = T) 
  })

  output$Graph <- renderPlot({
    midPeakPLOT(input$Filename, input$startDate, input$endDate, input$APD, input$Treandline, input$Vline)
  })
  output$Summary <- renderPlot({
    multiPLOT(input$Filename, input$startDate, input$endDate, input$Laser, input$Type)
  })
  output$avail_Dates <- renderTable({
    DF <- read.csv(paste0("./data/", input$Filename))
    avail_Dates <- as.character(DF$Date)
    while(length(avail_Dates) %% 3 > 0){
      avail_Dates <- c(avail_Dates, "NA")
    }
    avail_Dates <- matrix(avail_Dates, ncol=3, byrow = F) 
  })
  
}