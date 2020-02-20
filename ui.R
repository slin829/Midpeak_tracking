# midpeak_plot_app ui v4.R

avail_Files <- dir("./data")
ui<-fluidPage(
  h1("Midpeak bead tracking"),
  h4("Version 3, Release Date: 2020-01-24, Authors: SL"),
  hr(),

  sidebarPanel(
    selectInput("Filename", label= "File name" , choices = avail_Files, selected = avail_Files[3]),
    textInput("startDate", label= "Start Date", "200110"),
    textInput("endDate", label= "End Date", "200212"),
    textInput("Vline", label= "Vline"),
    hr(),
    radioButtons("Laser", label = "Laser Summary", choices = c("V_laser", "B_laser", "YG_laser", "R_laser")),
    radioButtons("Type", label = "Plot Type", choices = c("Median", "rCV")),
    hr(),
    checkboxInput("Treandline", "Plot Trendline"),
    uiOutput("APD"),
    width = 3
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Laser Summary", plotOutput("Summary", width = 1400, height = 800)),
      tabPanel("APD", plotOutput("Graph", width = 1400, height = 800)),
      tabPanel("Dates", tableOutput("avail_Dates"))
    )
  )
)

