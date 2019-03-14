library(shiny)
library(shinythemes)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)

HeatMap1 <- read.csv("/Users/abhavluthra/desktop/dic/abhavlutLab1/part2/HeatMapGraph.csv")
stateFreqTwoDF <- read.csv(file="/Users/abhavluthra/desktop/dic/abhavlutLab1/part3/stateFreqTwoDF.csv")
stateFreqMultiDF <- read.csv(file="/Users/abhavluthra/desktop/dic/abhavlutLab1/part3/stateFreqMultiDF.csv")
# Combine Two Twitter Data
stateFreq <- cbind(stateFreqTwoDF, stateFreqMultiDF$Freq)

colnames(stateFreq) <- c("region", "Two Query", "Multiple Query")

map_data <- HeatMap1

ui <- fluidPage(title = 'Lets See Some Maps',
                theme = shinythemes::shinytheme('cerulean'),
                
                sidebarLayout(
                  sidebarPanel(width = 3,
                               sliderInput("num_colors",
                                           label = "Number of colors:",
                                           min = 1,
                                           max = 9,
                                           value = 7),
                               selectInput("select", 
                                           label = "Select Tweets Count:", 
                                           choices = colnames(stateFreq)[2:3], 
                                           selected = 1)),
                  
                  mainPanel(width = 9, 
                            tabsetPanel( 
                              tabPanel(title = 'Comparison Map',
                                       plotOutput(outputId = "map1"),
                                       plotOutput(outputId = "map2")
                                       ),
                              tabPanel(title = 'Tweet Table', 
                                       dataTableOutput(outputId = 'table'))))))

server <- function(input, output, session) {
  
  output$map1 <- renderPlot({
    
    statelabel1 <- data.frame(stateFreq[1])
    statelabel <- cbind(statelabel1, stateFreq[input$select])
    colnames(statelabel) <- c("region", "value")
    statelabel$value <- as.numeric(statelabel$value)
    statelabel$region <- tolower(statelabel$region)
    statelabel$region <- gsub(" (u.s. state)", "", statelabel$region, fixed=TRUE)
    
    state_choropleth(statelabel, title = "Twitter Map", num_colors = input$num_colors)
  })
  
  output$map2 <- renderPlot({
    
    statelabel <- map_data[,1:2]
    colnames(statelabel) <- c("region", "value")
    statelabel$value <- as.numeric(statelabel$value)
    statelabel$region <- tolower(statelabel$region)
    statelabel$region <- gsub(" (u.s. state)", "", statelabel$region, fixed=TRUE)
    
    state_choropleth(statelabel, title = "CDC Map", num_colors = input$num_colors)
  })
  
  output$table <- renderDataTable({
    stateFreq[order(stateFreq[input$select]), ]
    
  })
}

shinyApp(ui, server)