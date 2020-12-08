library(shiny)
library(DT)

appDataMaster <- readRDS('appDataMaster.rds')
appDataToday <- readRDS('appDataToday.rds')
metrics <- readRDS('metrics.rds')

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("2020-2021 NCAA Men's Basketball Predictions"),

  sidebarLayout(
    sidebarPanel(textOutput("modelAdequacy"),width = 3.5),
    # Show the data table
    mainPanel(
      h1("Today's Games Predictions"),
      dataTableOutput(outputId = "todayData"),
      h1("All Game Predictions"),
      dataTableOutput(outputId = "predData")
      #h1("NCAA Tournament Predictions"),
      #dataTableOutput(outputId = "predTournamentData")
    )
  )
)



# Define server logic required to draw build table
server <- function(input, output){

  output$todayData <- DT::renderDataTable(
    appDataToday
  )
  
  output$predData <- DT::renderDataTable(
    appDataMaster
  )

  # output$predTournamentData <- DT::renderDataTable(
  #   tournamentData
  # )

  output$modelAdequacy <- renderText(paste0("Trained from games through ", Sys.Date() - 1,"; Game Winner Accuracy: ",round(100*metrics[1,1],2),"%; Average Error Spread: ",round(metrics[1,2],1)," pts; Average Error Totals: ",round(metrics[1,3],1)," pts"))
}

# Run the application 
shinyApp(ui = ui, server = server)

